module DrizzleCarton.RAMDataAccess.Entry

open DrizzleCarton.Application
open DrizzleCarton.Model
open DrizzleCarton.Model.Entry
open DrizzleCarton.RAMDataAccess
open DrizzleCarton.ResultHelper

let entryPersistence: IEntryDataAccess =
  { new IEntryDataAccess with
      member this.GetSubEntries(id: EntryId) : Result<Entry list, ReadEntryFailure> =
        let rawId = EntryId.toRaw id

        match RAMDataAccess.subEntries RAMDataAccess.defaultDb rawId with
        | Error _ ->
          Error(ReadEntryFailure.DataAccessError $"Unknown error while requesting sub-entries for entry id %d{rawId}")
        | Ok entries ->
          entries
          |> List.map Entry.ofRaw
          |> sequenceResult
          |> Result.mapError (function
            | Validation.ValidationError e -> ValidationError e)

      member this.FindEntryById(id: EntryId) : Result<Entry option, ReadEntryFailure> =
        raise (System.NotImplementedException())

      member this.GetAllEntries() : Result<Entry list, ReadEntryFailure> =
        raise (System.NotImplementedException())

      member this.StoreEntry
        (name: EntryName, parent: EntryParent, kind: EntryKind, size: EntrySize)
        : Result<Entry, WriteEntryFailure> =
        raise (System.NotImplementedException())

      member this.StoreNewRootFolder() : Result<Entry, WriteEntryFailure> =
        raise (System.NotImplementedException())

      member this.UpdateEntry(entry: Entry) : Result<Entry, WriteEntryFailure> =
        raise (System.NotImplementedException())

  }
