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
  | Error(ReadEntryFailure.ModelValidationError msg) ->
    DataRetrievingError(
      Message
        $"Illegal state: Entry with id %d{(EntryId.toRaw id)} could not be validated when read from storage! '%s{msg}'"
    )
  | Error(ReadEntryFailure.PermissionDenied) -> DataRetrievingError "Permission denied."
  | Ok(Some entry) -> EntryFound entry
  | Ok None -> EntryNotFound

type GetSubEntriesResult =
  | SubEntriesFound of Entry list
  | ZeroSubEntries
  | DataRetrievingError of Message

let getSubEntries (entryRepo: IEntryRepository) entryId : GetSubEntriesResult =
  match entryRepo.GetSubEntries entryId with
  | Error(ReadEntryFailure.DataAccessError msg) -> DataRetrievingError msg
  | Error(ReadEntryFailure.ModelValidationError msg) ->
    DataRetrievingError
      $"Error: One or more of the sub-entries of entry with id %d{EntryId.toRaw entryId} could not be validated when read from storage! Details: '%s{msg}'"
  | Error(ReadEntryFailure.PermissionDenied) -> DataRetrievingError "Permission denied."
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

let sumEntryBytes (entries: Entry list) : ByteCount =
  entries |> List.sumBy Entry.size |> ByteCount

type GetLargestSubEntryResult =
  | Found of Entry
  | ZeroSubEntries
  | DataRetrievingError of Message

let getLargestSubEntry (entryRepo: IEntryRepository) entryId : GetLargestSubEntryResult =
  match getSubEntries entryRepo entryId with
  | GetSubEntriesResult.DataRetrievingError msg -> DataRetrievingError msg
  | GetSubEntriesResult.ZeroSubEntries -> ZeroSubEntries
  | GetSubEntriesResult.SubEntriesFound subEntries -> subEntries |> List.maxBy Entry.size |> Found

module Validation =
  let maxLegalAncestors = 6

  type AncestorCount = int

  type CountAncestorsResult =
    | AncestorCount of AncestorCount
    | ZeroAncestors
    | HasNonexistentAncestor of
      {| AncestorId: EntryId
         CountUntilAncestorExcluded: AncestorCount |}
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

type CreateResult =
  | DataStoringError of Message
  | InvalidEntryError of Message
  | Stored of Entry

let create (entryRepo: IEntryRepository) entryData =
  match Validation.validate entryRepo entryData with
  | Error(Common.ValidationError msg) -> InvalidEntryError msg
  | Ok validatedEntryData ->
    match entryRepo.StoreEntry validatedEntryData with
    | Error(WriteEntryFailure.DataAccessError msg) -> DataStoringError msg
    | Error(WriteEntryFailure.PermissionDenied) -> DataStoringError "Permission denied."
    | Error(WriteEntryFailure.UnexpectedResultError msg) ->
      DataStoringError $"Encountered unexpected result after storing new entry: %s{msg}"
    | Ok storedEntryId -> Entry.withId validatedEntryData storedEntryId |> Stored

let storeNewRootFolder (entryRepo: IEntryRepository) =
  let data = Entry.EntryData.root

  entryRepo.StoreEntry data
  |> Result.map (fun storedEntryId -> Entry.withId data storedEntryId)

let createSubFolder entryRepo name parentFolder =
  match Entry.EntryData.make (name, parentFolder, EntryKind.Folder, EntrySize.zero) with
  | Error(Validation.ValidationError msg) -> CreateResult.InvalidEntryError msg
  | Ok folderData -> create entryRepo folderData
