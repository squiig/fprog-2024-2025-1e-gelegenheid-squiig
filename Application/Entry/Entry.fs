namespace DrizzleCarton.Application

open DrizzleCarton.Model
open DrizzleCarton.Model.Entry

/// Any error that may come from the data access implementation when attempting to read Entries.
type ReadEntryFailure =
  | ValidationError of string
  | DataAccessError of string

/// Any error that may come from the data access implementation when attempting to write Entries.
type WriteEntryFailure = DataAccessError of string

/// Defines the data operations for Entry functionality to be implemented by some data access dependency.
type IEntryDataAccess =
  abstract GetAllEntries: unit -> Result<Entry list, ReadEntryFailure>
  abstract GetSubEntries: EntryId -> Result<Entry list, ReadEntryFailure>
  abstract FindEntryById: EntryId -> Result<Entry option, ReadEntryFailure>
  abstract StoreEntry: EntryName * EntryParent * EntryKind * EntrySize -> Result<Entry, WriteEntryFailure>
  abstract StoreNewRootFolder: unit -> Result<Entry, WriteEntryFailure>
  abstract UpdateEntry: Entry -> Result<Entry, WriteEntryFailure>

module Entry =

  type StoreResult =
    | EntryStored of Entry
    | DataFailure of string

  let newRoot (dataAccess: IEntryDataAccess) =
    match dataAccess.StoreNewRootFolder() with
    | Error(WriteEntryFailure.DataAccessError s) -> DataFailure s
    | Ok entry -> EntryStored entry

  type GetByIdResult =
    | EntryFound of Entry
    | EntryNotFound
    | DataAccessFailure of string

  let findById (dataAccess: IEntryDataAccess) (id: EntryId) =
    match dataAccess.FindEntryById id with
    | Error(ReadEntryFailure.DataAccessError s) -> DataAccessFailure s
    | Error(ValidationError s) -> DataAccessFailure s
    | Ok(Some entry) -> EntryFound entry
    | Ok None -> EntryNotFound

  type GetSubEntriesResult =
    | SubEntriesFound of Entry list
    | ZeroSubEntries
    | DataAccessFailure of string

  let findSubentries (dataAccess: IEntryDataAccess) (id: EntryId) : GetSubEntriesResult =
    match dataAccess.GetSubEntries id with
    | Error(ReadEntryFailure.DataAccessError s) -> DataAccessFailure s
    | Error(ValidationError s) ->
      DataAccessFailure
        $"Illegal state: One or more of the sub-entries of entry with id %d{EntryId.toRaw id} could not be validated when read from storage! Message: '%s{s}'"
    | Ok subEntries when subEntries.IsEmpty -> ZeroSubEntries
    | Ok subEntries -> SubEntriesFound subEntries

  type GetParentResult =
    | ParentFound of Entry
    | NoParent
    | NonexistentParent
    | DataAccessFailure of string

  let getParent (dataAccess: IEntryDataAccess) (entry: Entry) =
    let _, _, parentId, _, _ = toTuple entry

    match EntryParent.toRaw parentId with
    | None -> NoParent
    | Some id ->
      match findById dataAccess id with
      | GetByIdResult.DataAccessFailure s -> DataAccessFailure s
      | EntryNotFound -> NonexistentParent
      | EntryFound e -> ParentFound e

  module Validation =
    let nonFileParent (dataAccess: IEntryDataAccess) invalid parent =
      if parent |> isFolder then Ok parent else Error invalid

  let validate (dataAccess: IEntryDataAccess) (entry: Entry) : Result<Entry, string> =
    match getParent dataAccess entry with
    | DataAccessFailure s -> Error s
    | NonexistentParent -> Error "Entry may not point to a parent that doesn't exist."
    | NoParent -> Ok entry
    | ParentFound parent ->
      Validation.nonFileParent dataAccess "Entry parents must be folders." parent
      |> Result.map (fun _ -> entry)
