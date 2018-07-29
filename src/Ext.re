open Belt;
open Structs;

module Srcloc = {
  let transformPattern =
      (p: pattern, augment: list(pattern) => list(pattern))
      : pattern => {
    let visitor = {
      inherit class patternVisitor(unit);
      pub! visitCore = (env, op, args) =>
        PatCore(op, List.map(augment(args), this#visit(env)));
      pub! visitSurf = (env, op, args, fromUser) =>
        PatSurf(op, List.map(augment(args), this#visit(env)), fromUser);
      pub! visitExtension = (_, p) => p
    };
    visitor#visit((), p);
  };

  let leftAugment =
    fun
    | [PatExtension(p), ...rest] => [p, ...rest]
    | lst => [PatDrop, ...lst];

  let rightAugment = (topSrcloc: pattern) =>
    fun
    | [PatExtension(p), ...rest] => [p, ...rest]
    | lst => [topSrcloc, ...lst];

  let topAugment =
    fun
    | PatSurf(op, [PatDrop, ...rest], fromUser) => {
        let topSrcloc = PatVar("__top_loc");
        (PatSurf(op, [topSrcloc, ...rest], fromUser), topSrcloc);
      }
    | PatSurf(_, [topSrcloc, ..._], _) as p => (p, topSrcloc)
    | _ => raise(RuntimeError("Top level is not a PatSurf"));

  let transform = (p: desugaringRules) : desugaringRules =>
    List.map(
      _,
      ({lhs, rhs}) => {
        let (lhs, topSrcloc) =
          topAugment(transformPattern(lhs, leftAugment));
        {lhs, rhs: transformPattern(rhs, rightAugment(topSrcloc))};
      },
    )
    |> Map.String.map(p, _);
};
