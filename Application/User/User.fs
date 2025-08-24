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
  | Error(ReadUserFailure.PermissionDenied) -> DataRetrievingError "Permission denied."
  | Ok users -> Found users

type FindByIdResult =
  | DataRetrievingError of Message
  | NotFound
  | Found of User

let findById (userRepo: IUserRepository) (id: UserId) =
  match userRepo.FindUserById id with
  | Error(ReadUserFailure.ModelValidationError e) -> DataRetrievingError e
  | Error(ReadUserFailure.DataAccessError e) -> DataRetrievingError e
  | Error(ReadUserFailure.PermissionDenied) -> DataRetrievingError "Permission denied."
  | Ok None -> NotFound
  | Ok(Some user) -> Found user

type AddResult =
  | RootFolderStoringError of Message
  | UserDataStoringError of Message
  | InvalidUserError of Message
  | Stored of User

let add (userRepo: IUserRepository) (entryRepo: IEntryRepository) ((name: UserName), (quota: UserQuota)) =
  match Entry.storeNewRootFolder entryRepo with
  | Error(WriteEntryFailure.DataAccessError msg) -> RootFolderStoringError msg
  | Error(WriteEntryFailure.PermissionDenied) -> RootFolderStoringError "Permission denied."
  | Error(WriteEntryFailure.UnexpectedResultError msg) ->
    RootFolderStoringError $"Encountered unexpected result after storing new root folder: %s{msg}"
  | Ok rootFolder ->
    // Try to combine everything into valid user data.
    match UserData.ofRaw (UserName.toRaw name, UserQuota.toRaw quota, rootFolder) with
    | Error(Validation.ValidationError msg) -> InvalidUserError msg // TODO: Make sure the root folder gets removed.
    | Ok userData ->
      // Try to store the user data.
      match userRepo.StoreUser userData with
      | Error(WriteUserFailure.DataAccessError msg) -> UserDataStoringError msg
      | Error(WriteUserFailure.UnexpectedResultError msg) ->
        UserDataStoringError $"Encountered unexpected result after storing new user data: %s{msg}"
      | Error(WriteUserFailure.PermissionDenied) -> UserDataStoringError "Permission denied."
      // If stored, assign the new id to the user data and return as a valid and stored User entity.
      | Ok storedUserId -> User.withId userData storedUserId |> Stored

type RenameResult =
  | DataRetrievingError of Message
  | DataStoringError of Message
  | InvalidNameForUserError of Message
  | UserNotFoundError
  | UserUpdated of User

let rename (userRepo: IUserRepository) (id: UserId) (newName: UserName) =
  match findById userRepo id with
  | FindByIdResult.DataRetrievingError msg -> DataRetrievingError msg
  | FindByIdResult.NotFound -> UserNotFoundError
  | FindByIdResult.Found oldUser ->
    let id, _, quota, root = User.toTuple oldUser

    match User.make (id, newName, quota, root) with
    | Error(Validation.ValidationError msg) -> InvalidNameForUserError msg
    | Ok dirtyUser ->
      match userRepo.UpdateUser dirtyUser with
      | Error(WriteUserFailure(WriteUserFailure.DataAccessError msg)) -> DataStoringError msg
      | Error(WriteUserFailure(WriteUserFailure.UnexpectedResultError msg)) ->
        DataStoringError $"Encountered unexpected result after updating user: %s{msg}"
      | Error(WriteUserFailure(WriteUserFailure.PermissionDenied)) -> DataStoringError "Permission denied."
      | Error(UpdateUserFailure.UserNotFound) -> UserNotFoundError
      | Ok updatedUser -> UserUpdated updatedUser

type GetTotalBytesResult =
  | UserDataRetrievingError of Message
  | EntryDataRetrievingError of Message
  | UserNotFound
  | ZeroEntries
  | TotalBytesCounted of ByteCount

let getTotalBytesStoredByUserId userRepo entryRepo userId =
  match findById userRepo userId with
  | FindByIdResult.DataRetrievingError msg -> UserDataRetrievingError msg
  | NotFound -> UserNotFound
  | Found user ->
    let _, _, _, userRoot = User.toTuple user
    let rootEntry = UserRoot.toRaw userRoot

    match Entry.getSubEntries entryRepo (Entry.getId rootEntry) with
    | Entry.GetSubEntriesResult.DataRetrievingError msg -> EntryDataRetrievingError msg
    | Entry.GetSubEntriesResult.ZeroSubEntries -> ZeroEntries
    | Entry.GetSubEntriesResult.SubEntriesFound entries -> Entry.sumEntryBytes entries |> TotalBytesCounted
