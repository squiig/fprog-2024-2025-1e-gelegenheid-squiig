module DrizzleCarton.Application.Util

let resToOpt<'T, 'Error, 'U> (f: 'T -> 'U) (res: Result<Option<'T>, 'Error>) : Option<'U> =
  res |> Result.defaultValue None |> Option.map f
