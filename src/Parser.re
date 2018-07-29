open Belt;
open Utils;
open Structs;
open Abbrev;

[@bs.module "../lib/PegParser"]
external parse : (string, Js.Dict.t(string)) => Js.Json.t = "parse";

let get = (value, field) =>
  Js.Json.classify(Js.Dict.unsafeGet(value, field));

let getString = (value, field) : string =>
  switch (get(value, field)) {
  | JSONString(s) => s
  | x =>
    let value = Js.Json.stringifyAny(value);
    failwith({j|expect a $(field) of type string, found: $(x) in $(value)|j});
  };

let getObject = (value, field) : Js.Dict.t('a) =>
  switch (get(value, field)) {
  | JSONObject(obj) => obj
  | x =>
    let value = Js.Json.stringifyAny(value);
    failwith({j|expect a $(field) of type object, found: $(x) in $(value)|j});
  };

let getArray = (value, field) : Js.Array.t('a) =>
  switch (get(value, field)) {
  | JSONArray(arr) => arr
  | x =>
    let value = Js.Json.stringifyAny(value);
    failwith({j|expect a $(field) of type array, found: $(x) in $(value)|j});
  };

let rec parsePattern = (obj: Js.Json.tagged_t) : pattern => {
  let parseConstruct = (obj, constr: (string, list(pattern)) => pattern) =>
    constr(
      getString(obj, aNAME),
      List.map(
        List.fromArray(getArray(obj, aPATTERNS)),
        parsePattern @@@ Js.Json.classify,
      ),
    );

  switch (obj) {
  | Js.Json.JSONObject(obj) =>
    switch (getString(obj, aTYPE)) {
    | "Drop" => PatDrop
    | "Var" => PatVar(getString(obj, aVALUE))
    | "None" => PatOption(None)
    | "Some" => PatOption(Some(parsePattern(get(obj, aVALUE))))
    | "B" =>
      PrimBool(
        switch (get(obj, aVALUE)) {
        | Js.Json.JSONFalse => false
        | Js.Json.JSONTrue => true
        | x => failwith({j|expect a boolean, found: $(x)|j})
        },
      )
      |. PatPrim
    | "N" => PrimNum(getString(obj, aVALUE)) |. PatPrim
    | "St" => PrimStr(getString(obj, aVALUE)) |. PatPrim
    | "Lo" => PrimLoc(getString(obj, aVALUE)) |. PatPrim
    | "PVar" =>
      let obj = getObject(obj, aVALUE);
      PatPVar(
        getString(obj, aNAME),
        getArray(obj, "labels")
        |> Array.map(_, obj =>
             switch (Js.Json.classify(obj)) {
             | Js.Json.JSONString(s) => s
             | x => failwith({j|expect a label, found $(x)|j})
             }
           )
        |> Set.String.fromArray,
      );
    | "Fresh" =>
      let obj = getObject(obj, aVALUE);
      PatFresh(
        getString(obj, "varItem"),
        parsePattern(get(obj, aPATTERN)),
      );
    | "Capture" =>
      let obj = getObject(obj, aVALUE);
      PatCapture(
        getString(obj, "varItem"),
        parsePattern(get(obj, aPATTERN)),
      );
    | "Meta" => parseConstruct(obj, (x, y) => PatMeta(x, y))
    | "Biject" =>
      PatBiject(getString(obj, aNAME), parsePattern(get(obj, aPATTERN)))
    | "C" => parseConstruct(obj, (x, y) => PatCore(x, y))
    | "S" => parseConstruct(obj, (x, y) => PatSurf(x, y, false))
    | "Aux" => parseConstruct(obj, (x, y) => PatAux(x, y))
    | "L" =>
      PatList(
        getArray(obj, aVALUE)
        |> List.fromArray
        |> List.map(_, parsePattern @@@ Js.Json.classify),
        EllipsisNone,
      )
    | "Ellip" =>
      PatList(
        getArray(obj, aVALUE)
        |> List.fromArray
        |> List.map(_, parsePattern @@@ Js.Json.classify),
        EllipsisPattern(
          parsePattern(get(obj, "pat")),
          getString(obj, "label"),
        ),
      )
    | "Ext" => PatExtension(parsePattern(get(obj, aVALUE)))
    | x => failwith({j|unexpected $(x)|j})
    }
  | _ => failwith({j|expect a pattern, found: $(obj)|j})
  };
};

let parseProgram =
  fun
  | Js.Json.JSONArray(arr) =>
    Array.reduce(arr, Map.String.empty, (acc, obj) =>
      switch (Js.Json.classify(obj)) {
      | Js.Json.JSONObject(obj) =>
        let name = getString(obj, aNAME);
        let cases =
          getArray(obj, "cases")
          |> List.fromArray
          |> List.map(_, obj =>
               switch (Js.Json.classify(obj)) {
               | Js.Json.JSONObject(obj) => {
                   lhs: parsePattern(get(obj, "lhs")),
                   rhs: parsePattern(get(obj, "rhs")),
                 }
               | x => failwith({j|expect a case, found: $(x)|j})
               }
             );
        Map.String.set(acc, name, cases);
      | x => failwith({j|expect a rule, found: $(x)|j})
      }
    )
  | x => failwith({j|expect rules, found $(x)|j});

let program = s => {
  let obj = parse(s, Js.Dict.fromList([("startRule", "ProgramTop")]));
  parseProgram(Js.Json.classify(obj));
};

let term = s => {
  let obj = parse(s, Js.Dict.fromList([("startRule", "PatternTop")]));
  let ret = patternToTerm(parsePattern(Js.Json.classify(obj)));
  ret;
};
