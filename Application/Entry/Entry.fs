module DrizzleCarton.Application.Entry

open DrizzleCarton.Model
open DrizzleCarton.Application.Common
open DrizzleCarton.Application.EntryRepositoryContract

type FindByIdResult =
  | EntryFound of Entry
  | EntryNotFound
  | DataRetrievingError of Message

let private findById (entryRepo: IEntryRepository) id =
  match entryRepo.FindEntryById id with
    | Error(ReadEntryFailure.DataAccessError msg) -> DataRetrievingError msg
    | Error(ModelValidationError msg) ->
      DataRetrievingError(
        Message
          $"Illegal state: Entry with id %d{(EntryId.toRaw id)} could not be validated when read from storage! '%s{msg}'"
      )
    | Ok(Some entry) -> EntryFound entry
    | Ok None -> EntryNotFound

type FindByRawIdResult =
  | EntryFound of Entry
  | EntryNotFound
  | InvalidIdError of Message
  | DataRetrievingError of Message

let findByRawId (entryRepo: IEntryRepository) rawId =
  match EntryId.ofRaw rawId with
  | Error(Validation.ValidationError msg) -> InvalidIdError msg
  | Ok id ->
    match findById entryRepo id with
    | FindByIdResult.DataRetrievingError msg -> DataRetrievingError msg
    | FindByIdResult.EntryNotFound -> EntryNotFound
    | FindByIdResult.EntryFound entry -> EntryFound entry

type GetSubEntriesResult =
  | SubEntriesFound of Entry list
  | ZeroSubEntries
  | InvalidIdError of Message
  | DataRetrievingError of Message

let getSubEntries (entryRepo: IEntryRepository) rawId : GetSubEntriesResult =
  match EntryId.ofRaw rawId with
  | Error(Validation.ValidationError msg) -> InvalidIdError msg
  | Ok id ->
    match entryRepo.GetSubEntries id with
    | Error(ReadEntryFailure.DataAccessError msg) -> DataRetrievingError msg
    | Error(ModelValidationError msg) ->
      DataRetrievingError
        $"Illegal state: One or more of the sub-entries of entry with id %d{rawId} could not be validated when read from storage! Message: '%s{msg}'"
    | Ok subEntries when subEntries.IsEmpty -> ZeroSubEntries
    | Ok subEntries -> SubEntriesFound subEntries

type GetParentResult =
  | ParentFound of Entry
  | NoParent
  | NonexistentParent of EntryId
  | DataRetrievingError of Message

let getParent (entryRepo: IEntryRepository) (entryData: EntryData) =
  let _, parent, _, _ = Entry.EntryData.toTuple entryData

  match EntryParent.toRaw parent with
  | None -> NoParent
  | Some id ->
    match findById entryRepo id with
    | FindByIdResult.DataRetrievingError msg -> DataRetrievingError msg
    | FindByIdResult.EntryNotFound -> NonexistentParent id
    | FindByIdResult.EntryFound entry -> ParentFound entry

module Validation =
  let maxLegalAncestors = 6

  type CountAncestorsResult =
    | AncestorCount of int
    | ZeroAncestors
    | HasNonexistentAncestor of
      {| AncestorId: EntryId
         CountUntilAncestorExcluded: int |}
    | DataRetrievingError of Message

  let countAncestors (entryRepo: IEntryRepository) (entryData: EntryData) : CountAncestorsResult =
    let rec count (counter: int) =
      match getParent entryRepo entryData with
      | GetParentResult.DataRetrievingError s -> DataRetrievingError s
      | NonexistentParent parentId ->
        HasNonexistentAncestor
          {| AncestorId = parentId
             CountUntilAncestorExcluded = counter |}
      | NoParent when counter = 0 -> ZeroAncestors
      | NoParent -> AncestorCount counter
      | ParentFound _ -> count (counter + 1)

    count (0)

  let legalAncestorCount (entryRepo: IEntryRepository) invalid maxAncestorCount (entryData: EntryData) =
    match countAncestors entryRepo entryData with
    | DataRetrievingError _ -> Error invalid
    | HasNonexistentAncestor x when x.CountUntilAncestorExcluded >= maxAncestorCount -> Error invalid
    | HasNonexistentAncestor _ -> Ok entryData
    | ZeroAncestors -> Ok entryData
    | AncestorCount c when c > maxAncestorCount -> Error invalid
    | AncestorCount _ -> Ok entryData

  let validateAncestors (entryRepo: IEntryRepository) (entry: EntryData) : Result<EntryData, ValidationError> =
    (match getParent entryRepo entry with
     | GetParentResult.DataRetrievingError msg -> Error msg
     | NonexistentParent _ -> Error "Entry may not point to a parent that doesn't exist."
     | NoParent -> Ok entry
     | ParentFound parent ->
       let parentData = Entry.getData parent
       // Now the parent validations.
       Entry.Validation.nonFile "Entry parents must be folders." parentData
       |> Result.bind (
         legalAncestorCount
           entryRepo
           $"Entry may not have more than %d{maxLegalAncestors} ancestors."
           (maxLegalAncestors - 1)
       )
       |> Result.map (fun _ -> entry)) // Return the entry, not the parent.
    |> Result.mapError ValidationError

  let validate (entryRepo: IEntryRepository) (entryData: EntryData) : Result<EntryData, ValidationError> =
    entryData |> validateAncestors entryRepo |> Result.map (fun _ -> entryData) // Make sure to always return the original entry on successful validation

type AddResult =
  | DataStoringError of Message
  | InvalidEntryError of Message
  | Stored of Entry

let add (entryRepo: IEntryRepository) (name, parent, kind, size) =
  match Entry.EntryData.ofRaw (name, parent, kind, size) with
  | Error(Validation.ValidationError msg) -> InvalidEntryError msg
  | Ok entryData ->
    match Validation.validate entryRepo entryData with
    | Error(Common.ValidationError msg) -> InvalidEntryError msg
    | Ok validatedEntryData ->
      match entryRepo.StoreEntry validatedEntryData with
      | Error(WriteEntryFailure.DataAccessError msg) -> DataStoringError msg
      | Ok storedEntryId -> Entry.withId validatedEntryData storedEntryId |> Stored

let storeNewRootFolder (entryRepo: IEntryRepository) =
  let data = Entry.EntryData.root

  entryRepo.StoreEntry data
  |> Result.map (fun storedEntryId -> Entry.withId data storedEntryId)
