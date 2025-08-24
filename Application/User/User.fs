module DrizzleCarton.Application.User

open DrizzleCarton.Model
open DrizzleCarton.Application.Common
open DrizzleCarton.Application.UserRepositoryContract
open DrizzleCarton.Application.EntryRepositoryContract

type GetAllResult =
  | DataRetrievingError of Message
  | Found of User list

let getAll (userRepo: IUserRepository) =
  match userRepo.GetAllUsers() with
  | Error(ReadUserFailure.DataAccessError s) -> DataRetrievingError s
  | Error(ReadUserFailure.ModelValidationError s) -> DataRetrievingError s
  | Ok users -> Found users

type FindByIdResult =
  | DataRetrievingError of Message
  | InvalidIdError of Message
  | NotFound
  | Found of User

let findById (userRepo: IUserRepository) rawId =
  match UserId.ofRaw rawId with
  | Error(Validation.ValidationError msg) -> InvalidIdError msg
  | Ok userId ->
    match userRepo.FindUserById userId with
    | Error(ReadUserFailure.ModelValidationError e) -> DataRetrievingError e
    | Error(ReadUserFailure.DataAccessError e) -> DataRetrievingError e
    | Ok None -> NotFound
    | Ok(Some user) -> Found user

type AddResult =
  | RootFolderStoringError of Message
  | UserDataStoringError of Message
  | InvalidUserError of Message
  | Stored of User

let add (userRepo: IUserRepository) (entryRepo: IEntryRepository) (rawUserName, rawUserQuota) =
  match Entry.storeNewRootFolder entryRepo with
  | Error(WriteEntryFailure.DataAccessError msg) -> RootFolderStoringError msg
  | Error(WriteEntryFailure.PermissionDenied) -> RootFolderStoringError "Permission denied."
  | Error(WriteEntryFailure.UnexpectedResultError msg) ->
    RootFolderStoringError $"Encountered unexpected result after storing new root folder: %s{msg}"
  | Ok rootFolder ->
    // Try to combine everything into valid user data.
    match UserData.ofRaw (rawUserName, rawUserQuota, rootFolder) with
    | Error(Validation.ValidationError msg) -> InvalidUserError msg // TODO: Make sure the root folder gets removed.
    | Ok userData ->
      // Try to store the user data.
      match userRepo.StoreUser userData with
      | Error(WriteUserFailure.DataAccessError msg) -> UserDataStoringError msg
      | Error(WriteUserFailure.UnexpectedResultError msg) -> 
        UserDataStoringError $"Encountered unexpected result after storing new user data: %s{msg}"
      // If stored, assign the new id to the user data and return as a valid and stored User entity.
      | Ok storedUserId -> User.withId userData storedUserId |> Stored

type RenameResult =
  | DataRetrievingError of Message
  | DataStoringError of Message
  | InvalidIdError of Message
  | InvalidNameError of Message
  | InvalidNameForUserError of Message
  | UserNotFoundError
  | UserUpdated of User

let rename (userRepo: IUserRepository) rawId rawNewName =
  match findById userRepo rawId with
  | FindByIdResult.DataRetrievingError msg -> DataRetrievingError msg
  | FindByIdResult.InvalidIdError msg -> InvalidIdError msg
  | FindByIdResult.NotFound -> UserNotFoundError
  | FindByIdResult.Found oldUser ->
    match UserName.ofRaw rawNewName with
    | Error(Validation.ValidationError msg) -> InvalidNameError msg
    | Ok newName ->
      let id, _, quota, root = User.toTuple oldUser

      match User.make (id, newName, quota, root) with
      | Error(Validation.ValidationError msg) -> InvalidNameForUserError msg
      | Ok dirtyUser ->
        match userRepo.UpdateUser dirtyUser with
        | Error(WriteUserFailure.DataAccessError msg) -> DataStoringError msg
        | Error(WriteUserFailure.UnexpectedResultError msg) -> 
        DataStoringError $"Encountered unexpected result after updating user: %s{msg}"
        | Ok updatedUser -> UserUpdated updatedUser
