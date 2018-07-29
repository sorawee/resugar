open Belt;
open Printf;
open Structs;

type errorRep = string => term;
type fn = (errorRep, term) => term;
/* I want to make the above type polymorphic but don't know how */

let map: MutableMap.String.t((fn, fn)) = MutableMap.String.make();

let addFunction = MutableMap.String.set(map);

let lookupCommon = (f, op, arg) =>
  switch (MutableMap.String.get(map, op)) {
  | None => raise(RuntimeError(sprintf("biject: %s doesn't exist", op)))
  | Some(fs) =>
    let error = s =>
      sprintf("%s: %s\n\narg: %s", op, s, string_of_term(arg))
      |. RuntimeError
      |> raise;
    f(fs, error, arg);
  };

let lookupForward = lookupCommon(((x, _)) => x);
let lookupBackward = lookupCommon(((_, x)) => x);

addFunction(
  "name-to-str",
  (
    error =>
      fun
      | TermVar(v) => TermPrim(PrimStr(v))
      | _ => error("the argument is not a variable"),
    error =>
      fun
      | TermPrim(PrimStr(v)) => TermVar(v)
      | _ => error("the argument is not a string"),
  ),
);

addFunction(
  "reverse",
  {
    let rev = error =>
      fun
      | TermList(lst) => TermList(List.reverse(lst))
      | _ => error("the argument is not a list");
    (rev, rev);
  },
);
