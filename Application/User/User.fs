module DrizzleCarton.Application.User

open DrizzleCarton.Model
open DrizzleCarton.Model.User
open DrizzleCarton.Application.Common
open DrizzleCarton.Application.UserRepositoryContract

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
  let notOverQuota invalid user =
    let _, _, quota, _ = User.toTuple user
    // looks like this is gonna be cross validation too...
    failwith "todo"

let validate (userRepo: IUserRepository) user = failwith "todo"
