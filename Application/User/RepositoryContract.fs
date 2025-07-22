module DrizzleCarton.Application.UserRepositoryContract

open DrizzleCarton.Model
open DrizzleCarton.Model.User

/// Any error that may come from the data access implementation when attempting to read Users.
type ReadUserFailure =
  | ModelValidationError of string
  | DataAccessError of string

/// Any error that may come from the data access implementation when attempting to write Users.
type WriteUserFailure = DataAccessError of string

/// Defines the data operations for User functionality to be implemented by some data access dependency.
type IUserRepository =
  abstract GetAllUsers: unit -> Result<User list, ReadUserFailure>
  abstract FindUserById: UserId -> Result<User option, ReadUserFailure>
  abstract StoreUser: User -> Result<unit, WriteUserFailure>
  abstract UpdateUser: User -> Result<User, WriteUserFailure>
