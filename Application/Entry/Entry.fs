module DrizzleCarton.Application.Entry

open DrizzleCarton.Model
open DrizzleCarton.Application.Common
open DrizzleCarton.Application.EntryRepositoryContract

type StoreResult =
  | EntryStored of Entry
  | DataFailure of string

let add (entryRepo: IEntryRepository) (name, parent, kind, size) =
  failwith "TODO"

let storeNewRootFolder (entryRepo: IEntryRepository) =
  add entryRepo (EntryName.root, EntryParent.none, EntryKind.Folder, EntrySize.zero)

type FindByIdResult =
  | EntryFound of Entry
  | EntryNotFound
  | DataFailure of string

let findById (entryRepo: IEntryRepository) (id: EntryId) =
  match entryRepo.FindEntryById id with
  | Error(ReadEntryFailure.DataAccessError s) -> DataFailure s
  | Error(ModelValidationError s) ->
    DataFailure
      $"Illegal state: Entry with id %d{EntryId.toRaw id} could not be validated when read from storage! Message: '%s{s}'"
  | Ok(Some entry) -> EntryFound entry
  | Ok None -> EntryNotFound

type GetSubEntriesResult =
  | SubEntriesFound of Entry list
  | ZeroSubEntries
  | DataFailure of string

let getSubEntries (entryRepo: IEntryRepository) (id: EntryId) : GetSubEntriesResult =
  match entryRepo.GetSubEntries id with
  | Error(ReadEntryFailure.DataAccessError s) -> DataFailure s
  | Error(ModelValidationError s) ->
    DataFailure
      $"Illegal state: One or more of the sub-entries of entry with id %d{EntryId.toRaw id} could not be validated when read from storage! Message: '%s{s}'"
  | Ok subEntries when subEntries.IsEmpty -> ZeroSubEntries
  | Ok subEntries -> SubEntriesFound subEntries

type GetParentResult =
  | ParentFound of Entry
  | NoParent
  | NonexistentParent of EntryId
  | DataFailure of string

let getParent (entryRepo: IEntryRepository) (entry: Entry) =
  let _, _, parent, _, _ = Entry.toTuple entry

  match EntryParent.toRaw parent with
  | None -> NoParent
  | Some id ->
    match findById entryRepo id with
    | FindByIdResult.DataFailure s -> DataFailure s
    | EntryNotFound -> NonexistentParent id
    | EntryFound e -> ParentFound e

type CountAncestorsResult =
  | AncestorCount of int
  | ZeroAncestors
  | HasNonexistentAncestor of
    {| AncestorId: EntryId
       CountUntilAncestorExcluded: int |}
  | DataFailure of string

let countAncestors (entryRepo: IEntryRepository) (entry: Entry) : CountAncestorsResult =
  let rec count (counter: int) =
    match getParent entryRepo entry with
    | GetParentResult.DataFailure s -> DataFailure s
    | NonexistentParent parentId ->
      HasNonexistentAncestor
        {| AncestorId = parentId
           CountUntilAncestorExcluded = counter |}
    | NoParent when counter = 0 -> ZeroAncestors
    | NoParent -> AncestorCount counter
    | ParentFound _ -> count (counter + 1)

  count (0)

module Validation =
  let maxLegalAncestors = 6

  let nonFile invalid entry =
    if entry |> Entry.isFolder then Ok entry else Error invalid

  let legalAncestorCount (entryRepo: IEntryRepository) invalid (maxAncestorCount: int) entry =
    match countAncestors entryRepo entry with
    | DataFailure s -> Error invalid
    | HasNonexistentAncestor x when x.CountUntilAncestorExcluded >= maxAncestorCount -> Error invalid
    | HasNonexistentAncestor _ -> Ok entry
    | ZeroAncestors -> Ok entry
    | AncestorCount c when c > maxAncestorCount -> Error invalid
    | AncestorCount _ -> Ok entry

  let validateAncestors (entryRepo: IEntryRepository) (entry: Entry) : Result<Entry, ValidationError> =
    (match getParent entryRepo entry with
     | GetParentResult.DataFailure s -> Error s
     | NonexistentParent _ -> Error "Entry may not point to a parent that doesn't exist."
     | NoParent -> Ok entry
     | ParentFound parent ->
       // Now the parent validations.
       nonFile "Entry parents must be folders." parent
       |> Result.bind (
         legalAncestorCount
           entryRepo
           $"Entry may not have more than %d{maxLegalAncestors} ancestors."
           (maxLegalAncestors - 1)
       )
       |> Result.map (fun _ -> entry)) // Return the entry, not the parent.
    |> Result.mapError ValidationError

  let validate (entryRepo: IEntryRepository) (entry: Entry) : Result<Entry, ValidationError> =
    entry |> validateAncestors entryRepo |> Result.map (fun _ -> entry) // Make sure to always return the original entry on successful validation
