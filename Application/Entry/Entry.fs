namespace DrizzleCarton.Application

open DrizzleCarton.Model
open DrizzleCarton.Model.Entry

// TODO: make exhaustive?
/// Any error that may come from the Data Access implementation when attempting to access Entries.
type EntryDataAccessFailure = EntryDataAccessFailure of string

/// Defines data access operations for entry functionality.
type IEntryDataAccess =
  abstract GetAllEntries: unit -> Result<Entry list, EntryDataAccessFailure>
  abstract FindEntryById: EntryId -> Result<Entry option, EntryDataAccessFailure>
  abstract StoreEntry: EntryName * EntryParent * EntryKind * EntrySize -> Result<Entry, EntryDataAccessFailure>
  abstract StoreNewRootFolder: unit -> Result<Entry, EntryDataAccessFailure>
  abstract UpdateEntry: Entry -> Result<Entry, EntryDataAccessFailure>

module Entry =

  type StoreResult =
    | EntryStored of Entry
    | DataAccessFailure of string

  let newRoot (dataAccess: IEntryDataAccess) =
    match dataAccess.StoreNewRootFolder() with
    | Error(EntryDataAccessFailure s) -> DataAccessFailure s
    | Ok entry -> EntryStored entry

  type GetByIdResult =
    | EntryFound of Entry
    | EntryNotFound
    | DataAccessFailure of string

  let findById (dataAccess: IEntryDataAccess) (id: EntryId) =
    match dataAccess.FindEntryById id with
    | Error(EntryDataAccessFailure s) -> DataAccessFailure s
    | Ok(Some entry) -> EntryFound entry
    | Ok None -> EntryNotFound

  // TODO: fix this function
  let fetchSubentries (dataAccess: IEntryDataAccess) (id: int) =
    Database.subEntries db id |> Result.defaultValue [] |> List.map Entry.ofRaw

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
