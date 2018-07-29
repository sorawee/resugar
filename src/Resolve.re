open Belt;
open Structs;

let resolvePattern =
    (p: pattern, keepSet: option(Set.String.t))
    : (pattern, Set.String.t) => {
  let boundSet = MutableSet.String.make();

  let visitor = {
    inherit class patternVisitor(Set.String.t);
    pub! visitPVar = (_, pvar, labels) => {
      let toKeep =
        switch (keepSet) {
        | None => true
        | Some(keepSet) => Set.String.has(keepSet, pvar)
        };
      if (toKeep) {
        MutableSet.String.add(boundSet, pvar);
        PatPVar(pvar, labels);
      } else {
        PatDrop;
      };
    };
    pub! visitFresh = (env, item, p) =>
      PatFresh(item, this#visit(Set.String.add(env, item), p));
    pub! visitCapture = (env, item, p) =>
      PatCapture(item, this#visit(Set.String.add(env, item), p));
    pub! visitVar = (env, v) =>
      if (Set.String.has(env, v)) {
        PatVar(v);
      } else {
        this#visit(env, PatPVar(v, Set.String.empty));
      }
  };

  let result = visitor#visit(Set.String.empty, p); /* has effect */
  (result, boundSet |> MutableSet.String.toArray |> Set.String.fromArray);
};

let resolve = (p: desugaringRules) : desugaringRules =>
  List.map(
    _,
    ({lhs, rhs}) => {
      let (rhs, rightBoundSet) = resolvePattern(rhs, None);
      let (lhs, _) = resolvePattern(lhs, Some(rightBoundSet));
      {lhs, rhs};
    },
  )
  |> Map.String.map(p, _);
