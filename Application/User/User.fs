module DrizzleCarton.Application.User

open DrizzleCarton.Model
open DrizzleCarton.Model.User
open DrizzleCarton.Application.Common
open DrizzleCarton.Application.UserRepositoryContract
open DrizzleCarton.Application.EntryRepositoryContract

type GetAllResult =
  | DataFailure of string
  | Found of User list

let getAll (userRepo: IUserRepository) =
  match userRepo.GetAllUsers() with
  | Error(ReadUserFailure.DataAccessError s) -> DataFailure s
  | Error(ReadUserFailure.ModelValidationError s) -> DataFailure s
  | Ok users -> Found users

type FindByIdResult =
  | DataFailure of string
  | NotFound
  | Found of User

let findById (userRepo: IUserRepository) (id: UserId) =
  match userRepo.FindUserById id with
  | Error(ReadUserFailure.ModelValidationError e) -> DataFailure e
  | Error(ReadUserFailure.DataAccessError e) -> DataFailure e
  | Ok None -> NotFound
  | Ok(Some user) -> Found user

// TODO: fix function
let add (userRepo: IUserRepository) username quota rootFolder =
  Database.addUser db (username, quota, rootFolder) |> Result.map toUser

// TODO: fix function
let update (userRepo: IUserRepository) id user =
  Database.updateUser db (toTuple user) |> Result.map toUser

module Validation =
  let validUserRoot entryRepo invalid user =
    let _, _, _, root = User.toTuple user
    let rootId = UserRoot.toRaw root

    match Entry.findById entryRepo rootId with
    | Entry.FindByIdResult.DataFailure s -> Error s
    | Entry.EntryNotFound -> Error invalid
    | Entry.EntryFound e -> if Entry.isRootFolder e then Ok user else Error invalid
    |> Result.mapError ValidationError

let validate (entryRepo: IEntryRepository) user =
  user
  |> Validation.validUserRoot entryRepo "Users must have a valid root folder."
  |> Result.map (fun _ -> user) // Whatever previous validations returned, return the input user if all succeeded
