namespace DrizzleCarton.RAMDataAccess

type UserTuple = int * string * int * int
type EntryTuple = int * string * Option<int> * string * int

type RAMDataAccess =
  private
    { mutable Users: Map<int, UserTuple>
      mutable Entries: Map<int, EntryTuple> }

type RAMDataAccessError = NotFound of int

module RAMDataAccess =
  let connect () =
    let users =
      [ 1, "janne", 1073741824, 1
        2, "camiel", 1073741824, 5
        3, "eleanor", 1048576, 14 ]
      |> List.map (fun (id, name, quota, rootFolder) -> id, (id, name, quota, rootFolder))
      |> Map.ofList


    let entries =
      [ 1, "files", None, "folder", 0
        2, "important", Some 1, "folder", 0
        3, "taxreturns2024.txt", Some 2, "file", 1024
        4, "install77.img", Some 1, "file", 1641200
        5, "files", None, "folder", 0
        6, "a", Some 5, "folder", 0
        7, "b", Some 6, "folder", 0
        8, "1.txt", Some 7, "file", 1024
        9, "9.txt", Some 7, "file", 1025
        10, "10.txt", Some 7, "file", 1026
        11, "oud", Some 1, "folder", 0
        12, "windows95.txt", Some 11, "file", 33
        13, "command.com", Some 11, "file", 512
        14, "files", None, "folder", 0
        15, "a", Some 14, "folder", 0
        16, "b", Some 15, "folder", 0
        17, "c", Some 16, "folder", 0
        18, "d", Some 17, "folder", 0
        19, "e", Some 18, "folder", 0
        20, "hallo.txt", Some 19, "file", 1048576 ]
      |> List.map (fun (id, name, parentId, kind, size) -> id, (id, name, parentId, kind, size))
      |> Map.ofList

    { RAMDataAccess.Users = users
      RAMDataAccess.Entries = entries }

  let defaultDb = connect ()


  let user (id: int) (db: RAMDataAccess) : Result<UserTuple, RAMDataAccessError> =
    match db.Users |> Map.tryFind id with
    | Some user -> Ok user
    | None -> Error(NotFound id)

  let addUser
    (db: RAMDataAccess)
    (username: string, quota: int, rootFolder: int)
    : Result<UserTuple, RAMDataAccessError> =
    let id =
      if Map.isEmpty db.Users then
        1
      else
        db.Users |> Map.maxKeyValue |> fst |> (+) 1

    let user = id, username, quota, rootFolder
    db.Users <- Map.add id user db.Users
    Ok user

  let users (db: RAMDataAccess) : Result<UserTuple list, RAMDataAccessError> =
    db.Users |> Map.values |> List.ofSeq |> Ok

  let updateUser (db: RAMDataAccess) (id, username, quota, rootFolder) : Result<UserTuple, RAMDataAccessError> =
    match db.Users |> Map.tryFind id with
    | Some user ->
      let updatedUser = id, username, quota, rootFolder
      db.Users <- Map.add id updatedUser db.Users
      Ok updatedUser
    | None -> Error(NotFound id)

  let entries (db: RAMDataAccess) : Result<List<EntryTuple>, RAMDataAccessError> =
    db.Entries |> Map.values |> List.ofSeq |> Ok


  let entry (db: RAMDataAccess) (id: int) =
    db.Entries
    |> Map.tryFind id
    |> Option.map Ok
    |> Option.defaultWith (fun _ -> Error(NotFound id))

  let addEntry (db: RAMDataAccess) (name, parent, kind, size) =
    let nextId =
      if Map.isEmpty db.Entries then
        1
      else
        Map.maxKeyValue db.Entries |> fst |> (+) 1

    let entry = nextId, name, parent, kind, size
    db.Entries <- Map.add nextId entry db.Entries
    Ok entry



  let subEntries (db: RAMDataAccess) (id: int) : Result<List<EntryTuple>, RAMDataAccessError> =
    db.Entries
    |> Map.filter (fun _ (_, _, parentId, _, _) -> parentId = Some id)
    |> Map.values
    |> List.ofSeq
    |> Ok
