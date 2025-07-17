/// Provides reusable combinators for basic data validation.
module DrizzleCarton.Model.Validation

open System
open System.Text.RegularExpressions

type ValidationError = ValidationError of string

/// Validate that the given string is not empty or indicate that the value is invalid by returning the provided value.
let nonEmpty invalid s =
  if String.IsNullOrWhiteSpace s then Error invalid else Ok s

/// Validate that the given string contains only alphanumeric characters or indicate otherwise by returning the provided value.
let alphaNumeric invalid s =
  if String.forall Char.IsLetterOrDigit s then
    Ok s
  else
    Error invalid

/// Validate that the given string matches the provided regular expression or indicate otherwise by returning the provided value.
let matches (re: Regex) invalid (s: string) =
  if re.IsMatch s then Ok s else Error invalid

let noForwardSlashes invalid (s: string) =
  if s.Contains '/' then Error invalid else Ok s

let noBackwardSlashes invalid (s: string) =
  if s.Contains '\\' then Error invalid else Ok s

let nonNegativeNumber invalid n = if n < 0 then Error invalid else Ok n

let noSpaces invalid s =
  if String.forall Char.IsWhiteSpace s then
    Error invalid
  else
    Ok s

let notBelowOne invalid i = if i < 1 then Error invalid else Ok i
