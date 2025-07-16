module DrizzleCarton.RAMDataAccess.Entry

open DrizzleCarton.Application
open DrizzleCarton.Model
open DrizzleCarton.Model.Entry

let entryPersistence: IEntryDataAccess =
  { new IEntryDataAccess with
      member this.FindEntryById(id: EntryId) : Result<Entry option, EntryDataAccessFailure> =
        raise (System.NotImplementedException())

      member this.GetAllEntries() : Result<Entry list, EntryDataAccessFailure> =
        raise (System.NotImplementedException())

      member this.StoreEntry
        (name: EntryName, parent: EntryParent, kind: EntryKind, size: EntrySize)
        : Result<Entry, EntryDataAccessFailure> =
        raise (System.NotImplementedException())

      member this.StoreNewRootFolder() : Result<Entry, EntryDataAccessFailure> =
        raise (System.NotImplementedException())

      member this.UpdateEntry(entry: Entry) : Result<Entry, EntryDataAccessFailure> =
        raise (System.NotImplementedException())

  }
