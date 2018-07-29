open Belt;
open Utils;
open Structs;
open Printf;

let generatePvars =
  List.makeBy(_, i => PatPVar(sprintf("__pvar_%d", i), Set.String.empty));
let gensym = newGensym();

let assignName = (map: Map.String.t(string), item: string, generateNew: bool) =>
  if (generateNew) {
    Map.String.set(map, item, gensym("__var_"));
  } else {
    Map.String.set(map, item, item);
  };

let desugar = (e: term, rules: desugaringRules, settings: settings) : term => {
  let rec desugarSurf = (op, args, fromUser) : term =>
    switch (Map.String.get(rules, op)) {
    | None =>
      /*
       * This should eventually throw an error. Right now allow it to work
       * so that we can add sugars incrementally
       */
      let e: term = TermCore(op, args);
      if (! (settings |> resugarGet)) {
        e;
      } else {
        let pvars = generatePvars(List.length(args));
        TermTag(PatSurf(op, pvars, fromUser), PatCore(op, pvars), e);
      };
    | Some(cases) =>
      let opt =
        findBy(cases, ({lhs, rhs}: desugaringRuleCase) =>
          switch (Match.matchPattern(TermSurf(op, args, fromUser), lhs)) {
          | None => None
          | Some((env, p)) => Some((env, p, rhs))
          }
        );
      switch (opt) {
      | None =>
        sprintf(
          "No case matched in %s",
          string_of_term(TermSurf(op, args, fromUser)),
        )
        |. RuntimeError
        |> raise
      | Some((env, plhs, rhs)) =>
        let e = subsPattern(env, rhs, Map.String.empty, Set.String.empty);
        if (! (settings |> resugarGet)) {
          e;
        } else {
          TermTag(plhs, rhs, e);
        };
      };
    }
  and subsPattern = (env: Env.t, p, varMap, currentLabels) : term => {
    let rec loop = p => {
      let error = s =>
        sprintf("%s\n\nin substitution to %s", s, string_of_pattern(p))
        |. RuntimeError
        |> raise;
      let loops = List.map(_, loop);
      switch (p) {
      | PatPVar(pvar, labels) =>
        if (Set.String.subset(labels, currentLabels)) {
          switch (Map.String.get(env.pvar, pvar)) {
          | None => error(sprintf("No pattern variable %s", pvar))
          | Some(t) => t
          };
        } else {
          sprintf(
            "Mismatch: %s has %s but scope has %s",
            pvar,
            string_of_set_string(labels),
            string_of_set_string(currentLabels),
          )
          |> error;
        }
      | PatPrim(p) => TermPrim(p)
      | PatCore(op, args) => TermCore(op, loops(args))
      | PatDrop => error("Wildcard not supported in the RHS")
      | PatSurf(op, args, _) =>
        /* This is not a PatSurf created from users */
        desugarSurf(op, loops(args), false)
      | PatAux(op, args) => TermAux(op, loops(args))
      | PatMeta(op, args) => Meta.lookup(op, loops(args))
      | PatBiject(op, arg) => loop(arg) |> Biject.lookupForward(op, _)
      | PatOption(opt) => TermOption(Option.map(opt, loop))
      | PatVar(v) =>
        switch (Map.String.get(varMap, v)) {
        | None => error(sprintf("Variable %s not found in variable map", v))
        | Some(s) => TermVar(s)
        }
      | PatFresh(item, body) =>
        subsPattern(env, body, assignName(varMap, item, true), currentLabels)
      | PatCapture(item, body) =>
        subsPattern(
          env,
          body,
          assignName(varMap, item, false),
          currentLabels,
        )
      | PatTag(_, _, body) =>
        sprintf(
          "Unexpected tag in substitution in %s",
          string_of_pattern(body),
        )
        |> error
      | PatList(ps, EllipsisNone) => TermList(loops(ps))
      | PatList(ps, EllipsisPattern(p, l)) =>
        switch (Map.String.get(env.ellipsis, l)) {
        | None => error(sprintf("Label %s not found during substitution", l))
        | Some(envs) =>
          let ts =
            List.map(envs, subenv =>
              subsPattern(
                Env.unify(subenv, env),
                p,
                varMap,
                Set.String.add(currentLabels, l),
              )
            );
          TermList(loops(ps) @ ts);
        }
      | PatList(_, EllipsisList(_, _)) =>
        error("Unexpected ellipsis list in substitution")
      | PatExtension(_) =>
        error("PatExtension should have been desugared already")
      };
    };
    loop(p);
  };

  let rec loop = (e: term) => {
    let loops = List.map(_, loop);
    switch (e) {
    | TermPrim(_) => e
    | TermVar(_) => e
    | TermCore(op, args) => TermCore(op, loops(args))
    | TermAux(op, args) => TermAux(op, loops(args))
    | TermList(args) => TermList(loops(args))
    | TermOption(opt) => TermOption(Option.map(opt, loop))
    | TermTag(_, _, _) =>
      /* It is impossible to have a `TermTag` during the desugaring process. */
      sprintf("Does not expect a tag, found: %s", string_of_term(e))
      |> failwith
    | TermSurf(op, args, fromUser) =>
      loops(args) |> desugarSurf(op, _, fromUser)
    };
  };

  let startTime = Js.Date.now();
  let result = loop(e);
  sprintf("Desugaring finished in %f ms", Js.Date.now() -. startTime)
  |> Js.log;
  result;
};
