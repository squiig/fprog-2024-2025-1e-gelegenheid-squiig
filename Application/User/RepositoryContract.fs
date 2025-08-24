module DrizzleCarton.Application.UserRepositoryContract

open DrizzleCarton.Model
open DrizzleCarton.Application.Common

/// Any error that may come from the data access implementation when attempting to read Users.
type ReadUserFailure =
  | ModelValidationError of Message
  | DataAccessError of Message
  | PermissionDenied

/// Any error that may come from the data access implementation when attempting to write Users.
type WriteUserFailure =
  | DataAccessError of Message
  | UnexpectedResultError of Message
  | PermissionDenied

type UpdateUserFailure =
  | WriteUserFailure of WriteUserFailure
  | UserNotFound

/// Defines the data operations for User functionality to be implemented by some data access dependency.
type IUserRepository =
  abstract GetAllUsers: unit -> Result<User list, ReadUserFailure>
  abstract FindUserById: UserId -> Result<User option, ReadUserFailure>
  abstract StoreUser: UserData -> Result<UserId, WriteUserFailure>
  abstract UpdateUser: User -> Result<User, UpdateUserFailure>
  abstract GetTotalBytesStored: UserId -> Result<ByteCount, ReadUserFailure>
