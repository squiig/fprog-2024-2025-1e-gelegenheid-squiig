module DrizzleCarton.Application.ResultHelper

let sequenceResult (results: Result<'a, 'e> list) : Result<'a list, 'e> =
  let folder accumulator next =
    match accumulator, next with
    | Ok xs, Ok x -> Ok(x :: xs)
    | Error e, _ -> Error e
    | _, Error e -> Error e

  List.fold folder (Ok []) results |> Result.map List.rev
