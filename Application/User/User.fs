module DrizzleCarton.Application.User

open DrizzleCarton.Model
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

type AddResult =
  | DataAccessError of string
  | RootFolderError of string
  | InvalidUserError of string
  | Stored of User

let add (userRepo: IUserRepository) (entryRepo: IEntryRepository) (rawUserName, rawUserQuota) =
  match Entry.storeNewRootFolder entryRepo with
  | Entry.StoreResult.DataFailure msg -> RootFolderError msg
  | Entry.EntryStored rootFolder ->
    match UserData.ofRaw (rawUserName, rawUserQuota, rootFolder) with
    | Error(Validation.ValidationError msg) -> InvalidUserError msg
    | Ok userData ->
      match userRepo.StoreUser userData with
      | Error(WriteUserFailure.DataAccessError msg) -> DataAccessError msg
      | Ok storedUserId -> User.withId userData storedUserId |> Stored

let update (userRepo: IUserRepository) user = userRepo.UpdateUser user

module Validation =

  let validate (entryRepo: IEntryRepository) (user: User) =
    user
    // Potential validations...
    |> Ok
    |> Result.map (fun _ -> user) // Whatever previous validations returned, return the input user if all succeeded
