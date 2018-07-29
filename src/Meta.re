open Belt;
open Printf;
open Structs;

let map = MutableMap.String.make();

let addFunction = MutableMap.String.set(map);

let lookup = (op, args) =>
  switch (MutableMap.String.get(map, op)) {
  | None => raise(RuntimeError(sprintf("meta: %s doesn't exist", op)))
  | Some(f) =>
    f(
      s =>
        sprintf(
          "%s: %s\n\nargs: %s",
          op,
          s,
          String.concat(" ", List.map(args, string_of_term)),
        )
        |. RuntimeError
        |> raise,
      args,
    )
  };

addFunction("get-loc", error =>
  fun
  | [TermCore(_, [loc, ..._])] => loc
  | [_] => error("the argument is not a TermCore")
  | _ => error("arity mismatches")
);

addFunction("string-append", error =>
  fun
  | [TermPrim(PrimStr(a)), TermPrim(PrimStr(b))] =>
    TermPrim(PrimStr(a ++ b))
  | [_, _] => error("one of the two arguments is not a string")
  | _ => error("arity mismatches")
);

addFunction("loc-to-string", error =>
  fun
  | [TermPrim(PrimLoc(l))] => TermPrim(PrimStr(l))
  | [_] => error("the argument is not a Loc")
  | _ => error("arity mismatches")
);
