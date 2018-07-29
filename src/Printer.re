open Belt;
open Printf;
open Utils;
open Structs;
open Abbrev;

let jsonPrim = (p: primitive) : Js.Json.t =>
  Js.Json.object_(
    switch (p) {
    | PrimStr(s) =>
      Js.Dict.empty()
      |. dictSet(aTYPE, Js.Json.string(aSTRING))
      |. dictSet(aVALUE, Js.Json.string(s))
    | PrimNum(n) =>
      Js.Dict.empty()
      |. dictSet(aTYPE, Js.Json.string(aNUMBER))
      |. dictSet(aVALUE, Js.Json.string(n))
    | PrimBool(b) =>
      Js.Dict.empty()
      |. dictSet(aTYPE, Js.Json.string(aBOOL))
      |. dictSet(aVALUE, Js.Json.boolean(b))
    | PrimLoc(l) =>
      Js.Dict.empty()
      |. dictSet(aTYPE, Js.Json.string(aLOC))
      |. dictSet(aVALUE, Js.Json.string(l))
    },
  );

let rec jsonTerm = (t: term) : Js.Json.t => {
  let jsonTerms = args =>
    Js.Json.array(List.toArray(List.map(args, jsonTerm)));

  switch (t) {
  | TermPrim(p) => jsonPrim(p)
  | TermCore(op, args) =>
    Js.Dict.empty()
    |. dictSet(aTYPE, Js.Json.string(aCORE))
    |. dictSet(aNAME, Js.Json.string(op))
    |. dictSet(aPATTERNS, jsonTerms(args))
    |> Js.Json.object_
  | TermSurf(_, _, _) => failwith("Surface is not completely desugared")
  | TermAux(_, _) =>
    RuntimeError(sprintf("Auxiliary is left: %s", string_of_term(t)))
    |> raise
  | TermVar(var) =>
    Js.Dict.empty()
    |. dictSet(aTYPE, Js.Json.string(aVAR))
    |. dictSet(aVALUE, Js.Json.string(var))
    |> Js.Json.object_
  | TermList(args) =>
    Js.Dict.empty()
    |. dictSet(aTYPE, Js.Json.string(aLIST))
    |. dictSet(aVALUE, jsonTerms(args))
    |> Js.Json.object_
  | TermOption(opt) =>
    Js.Json.object_(
      switch (opt) {
      | None => Js.Dict.empty() |. dictSet(aTYPE, Js.Json.string(aNONE))
      | Some(v) =>
        Js.Dict.empty()
        |. dictSet(aTYPE, Js.Json.string(aSOME))
        |. dictSet(aVALUE, jsonTerm(v))
      },
    )
  | TermTag(_, _, body) => jsonTerm(body)
  };
};
