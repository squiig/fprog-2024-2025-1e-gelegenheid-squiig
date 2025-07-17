namespace DrizzleCarton.Application

open DrizzleCarton.Model
open DrizzleCarton.Model.Entry
open DrizzleCarton.Application.EntryRepositoryContract

module Entry =

  type StoreResult =
    | EntryStored of Entry
    | DataFailure of string

  let newRoot (dataAccess: IEntryDataAccess) =
    match dataAccess.StoreNewRootFolder() with
    | Error(WriteEntryFailure.DataAccessError s) -> DataFailure s
    | Ok entry -> EntryStored entry

  type FindByIdResult =
    | EntryFound of Entry
    | EntryNotFound
    | DataFailure of string

  let findById (dataAccess: IEntryDataAccess) (id: EntryId) =
    match dataAccess.FindEntryById id with
    | Error(ReadEntryFailure.DataAccessError s) -> DataFailure s
    | Error(ValidationError s) ->
      DataFailure
        $"Illegal state: Entry with id %d{EntryId.toRaw id} could not be validated when read from storage! Message: '%s{s}'"
    | Ok(Some entry) -> EntryFound entry
    | Ok None -> EntryNotFound

  type GetSubEntriesResult =
    | SubEntriesFound of Entry list
    | ZeroSubEntries
    | DataFailure of string

  let getSubentries (dataAccess: IEntryDataAccess) (id: EntryId) : GetSubEntriesResult =
    match dataAccess.GetSubEntries id with
    | Error(ReadEntryFailure.DataAccessError s) -> DataFailure s
    | Error(ValidationError s) ->
      DataFailure
        $"Illegal state: One or more of the sub-entries of entry with id %d{EntryId.toRaw id} could not be validated when read from storage! Message: '%s{s}'"
    | Ok subEntries when subEntries.IsEmpty -> ZeroSubEntries
    | Ok subEntries -> SubEntriesFound subEntries

  type GetParentResult =
    | ParentFound of Entry
    | NoParent
    | NonexistentParent of EntryId
    | DataFailure of string

  let getParent (dataAccess: IEntryDataAccess) (entry: Entry) =
    let _, _, parentId, _, _ = toTuple entry

    match EntryParent.toRaw parentId with
    | None -> NoParent
    | Some id ->
      match findById dataAccess id with
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

  let countAncestors (dataAccess: IEntryDataAccess) (entry: Entry) : CountAncestorsResult =
    let rec count (counter: int) =
      match getParent dataAccess entry with
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
      if entry |> isFolder then Ok entry else Error invalid

    let legalAncestorCount (dataAccess: IEntryDataAccess) invalid (maxAncestorCount: int) entry =
      match countAncestors dataAccess entry with
      | DataFailure s -> Error invalid
      | HasNonexistentAncestor x when x.CountUntilAncestorExcluded >= maxAncestorCount -> Error invalid
      | HasNonexistentAncestor _ -> Ok entry
      | ZeroAncestors -> Ok entry
      | AncestorCount c when c > maxAncestorCount -> Error invalid
      | AncestorCount _ -> Ok entry

  let validate (dataAccess: IEntryDataAccess) (entry: Entry) : Result<Entry, string> =
    match getParent dataAccess entry with
    | GetParentResult.DataFailure s -> Error s
    | NonexistentParent _ -> Error "Entry may not point to a parent that doesn't exist."
    | NoParent -> Ok entry
    | ParentFound parent ->
      // Now the parent validations.
      Validation.nonFile "Entry parents must be folders." parent
      |> Result.bind (
        Validation.legalAncestorCount
          dataAccess
          $"Entry may not have more than %d{Validation.maxLegalAncestors} ancestors."
          (Validation.maxLegalAncestors - 1)
      )
      |> Result.map (fun _ -> entry)
