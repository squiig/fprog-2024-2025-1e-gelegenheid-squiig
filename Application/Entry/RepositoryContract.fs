module DrizzleCarton.Application.EntryRepositoryContract

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
