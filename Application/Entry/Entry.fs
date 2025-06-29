namespace DrizzleCarton.Application

open DrizzleCarton.Model

// TODO: make all these failure types exhaustive

type DAGetAllEntriesFailure = | DataAccessFailed

type DAFindEntryByIdFailure = | DataAccessFailed

type DAStoreEntryFailure = | DataAccessFailed

type DAUpdateEntryFailure =
  | UserDoesNotExist
  | DataAccessFailed

/// Defines data access operations for entry functionality.
type IEntryDataAccess =
  abstract GetAllEntries: unit -> Result<Entry list, DAGetAllEntriesFailure>
  abstract FindEntryById: EntryId -> Result<Entry option, DAFindEntryByIdFailure>
  abstract StoreEntry: EntryName * EntryParent * EntryKind * EntrySize -> Result<Entry, DAStoreEntryFailure>
  abstract StoreNewRootFolder: unit -> Result<Entry, DAStoreEntryFailure>
  abstract UpdateEntry: Entry -> Result<Entry, DAUpdateEntryFailure>

module Entry =

  type StoreResult =
    | Stored of Entry
    | DataAccessFailed

  let newRoot (dataAccess: IEntryDataAccess) =
    match dataAccess.StoreNewRootFolder() with
    | Error DAStoreEntryFailure.DataAccessFailed -> DataAccessFailed
    | Ok entry -> Stored entry

  type GetByIdResult =
    | NotFound
    | Found of Entry
    | DataAccessFailed

  let findById (dataAccess: IEntryDataAccess) (id: EntryId) =
    match dataAccess.FindEntryById id with
    | Error DAFindEntryByIdFailure.DataAccessFailed -> DataAccessFailed
    | Ok(Some entry) -> Found entry
    | Ok None -> NotFound

  // TODO: fix this function
  let fetchSubentries (dataAccess: IEntryDataAccess) (id: int) =
    Database.subEntries db id |> Result.defaultValue [] |> List.map Entry.toEntry
