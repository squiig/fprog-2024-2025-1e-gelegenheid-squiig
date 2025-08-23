module DrizzleCarton.Application.EntryRepositoryContract

open DrizzleCarton.Model
open DrizzleCarton.Application.Common

/// Any error that may come from the data access implementation when attempting to read Entries.
type ReadEntryFailure =
  | ModelValidationError of Message
  | DataAccessError of Message

/// Any error that may come from the data access implementation when attempting to write Entries.
type WriteEntryFailure = DataAccessError of Message

/// Defines the data operations for Entry functionality to be implemented by some data access dependency.
type IEntryRepository =
  abstract GetAllEntries: unit -> Result<Entry list, ReadEntryFailure>
  abstract GetSubEntries: EntryId -> Result<Entry list, ReadEntryFailure>
  abstract FindEntryById: EntryId -> Result<Entry option, ReadEntryFailure>
  abstract StoreEntry: EntryData -> Result<EntryId, WriteEntryFailure>
  abstract UpdateEntry: Entry -> Result<Entry, WriteEntryFailure>
