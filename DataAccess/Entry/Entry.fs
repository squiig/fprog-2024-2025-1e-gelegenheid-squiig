module DrizzleCarton.DataAccess.Entry

open DrizzleCarton.Application
open DrizzleCarton.Model

let entryPersistence: IEntryDataAccess =
  { new IEntryDataAccess with
      member this.FindEntryById(arg1: EntryId) : Result<Entry option, DAFindEntryByIdFailure> =
        raise (System.NotImplementedException())

      member this.GetAllEntries() : Result<Entry list, DAGetAllEntriesFailure> =
        raise (System.NotImplementedException())

      member this.StoreEntry
        (arg1: EntryName, arg2: EntryParent, arg3: EntryKind, arg4: EntrySize)
        : Result<Entry, DAStoreEntryFailure> =
        raise (System.NotImplementedException())

      member this.StoreNewRootFolder() : Result<Entry, DAStoreEntryFailure> =
        raise (System.NotImplementedException())

      member this.UpdateEntry(arg1: Entry) : Result<Entry, DAUpdateEntryFailure> =
        raise (System.NotImplementedException())

  }
