open Belt;
open Utils;
open Structs;
open Printf;

type matchResult = option((Env.t, pattern));

let rec loop =
        (t, p: pattern, fresh: Set.String.t, currentLabels: Set.String.t)
        : matchResult => {
  let matchConstruct = (top: string, pop: string, targs, pargs, constr) =>
    if (top != pop) {
      None;
    } else if (List.length(targs) != List.length(pargs)) {
      None;
    } else {
      let env = ref(Env.empty);
      let pats = ref([]);
      let result =
        List.every2(targs, pargs, (t, p) =>
          switch (loop(t, p, fresh, currentLabels)) {
          | None => false
          | Some((subenv, subp)) =>
            env := Env.unify(subenv, env^);
            pats := [subp, ...pats^];
            true;
          }
        );
      if (result) {
        Some((env^, constr(pop, List.reverse(pats^))));
      } else {
        None;
      };
    };

  switch (p, t) {
  | (PatPVar(pvar, labels), _) =>
    if (Set.String.subset(labels, currentLabels)) {
      Some((Env.(bindPVar(empty, pvar, t)), p));
    } else {
      let labelsStr = string_of_set_string(labels);
      let currentLabelsStr = string_of_set_string(currentLabels);
      sprintf(
        "Mismatch: %s has %s but scope has %s",
        pvar,
        labelsStr,
        currentLabelsStr,
      )
      |. RuntimeError
      |> raise;
    }

  | (PatDrop, _) => Some((Env.empty, termToPattern(t)))

  | (PatTag(_, _, _), _) => failwith("Can't match against a tag")

  | (_, TermTag(lhs, rhs, body)) =>
    /*
     * A pattern can match against a tag. Consider:
     *
     * (A x) => {B x}
     * (C {B x}) => x
     *
     * Then:
     *
     * (C (A 1)) ==> (C {B 1}) ==> 1
     *
     * Under the hood, we have:
     *
     * (C (A 1)) ==> (C <Tag: A -> B | {B 1}>)
     *
     * To match (C {B x}) with (C <Tag: A -> B | {B 1}>),
     * we need to be able to match against a tag
     *
     * We still would like to be able to retain the old tag information,
     * so it must be kept in a new tag
     */

    loop(body, p, fresh, currentLabels)
    |. Option.map(((env, p)) => (env, PatTag(lhs, rhs, p)))

  | (PatPrim(p1), TermPrim(p2)) =>
    if (p1 == p2) {
      Some((Env.empty, p));
    } else {
      None;
    }
  | (PatPrim(_), _) => None

  | (PatVar(v1), TermVar(v2)) =>
    /*
     * `PatVar` could only be matched when we run resugaring
     * Note that the returned pattern doesn't matter
     */
    if (Set.String.has(fresh, v1)) {
      /*
       * Actually, `PatVar` shouldn't contribute to the `pvar` map.
       * But we use it for the purpose of making sure that `PatVar`s
       * will unify properly
       */
      Some((
        Env.(bindPVar(empty, {j|__coerced_var_$(v1)|j}, t)),
        p,
      ));
    } else if (v1 == v2) {
      Some((Env.empty, p));
    } else {
      None;
    }
  | (PatVar(_), _) => None

  | (PatCore(pop, pargs), TermCore(top, targs)) =>
    matchConstruct(top, pop, targs, pargs, (op, args) => PatCore(op, args))
  | (PatCore(_, _), _) => None

  | (PatAux(pop, pargs), TermAux(top, targs)) =>
    matchConstruct(top, pop, targs, pargs, (op, args) => PatAux(op, args))
  | (PatAux(_, _), _) => None

  | (PatOption(None), TermOption(None)) => Some((Env.empty, p))
  | (PatOption(Some(p)), TermOption(Some(t))) =>
    loop(t, p, fresh, currentLabels)
    |. Option.map(((env, p)) => (env, PatOption(Some(p))))
  | (PatOption(_), _) => None

  | (PatSurf(pop, pargs, _), TermSurf(top, targs, _)) =>
    /*
     * `PatSurf` could only be matched when we run resugaring
     * so it will act just like `PatCore` in this case.
     * Note that the returned pattern doesn't matter
     */
    matchConstruct(top, pop, targs, pargs, (_, _) => p)
  | (PatSurf(_, _, _), _) => None

  | (PatFresh(item, body), t) =>
    /*
     * `PatFresh` could only be matched when we run resugaring
     * Note that the returned pattern doesn't matter
     */
    loop(t, body, Set.String.add(fresh, item), currentLabels)
  | (PatCapture(_, body), t) =>
    /*
     * `PatCapture` could only be matched when we run resugaring
     * Note that the returned pattern doesn't matter
     */
    loop(t, body, fresh, currentLabels)

  | (PatMeta(_, _), _) =>
    /*
     * `PatMeta` could only be matched when we run resugaring
     * Note that the returned pattern doesn't matter
     */
    Some((Env.empty, p))

  | (PatBiject(op, p), t) =>
    /*
     * `PatBiject` could only be matched when we run resugaring
     * Note that the returned pattern doesn't matter
     *
     * Note that we perform finv on t first, not later. Consider
     * the following example:
     *
     * ... => (biject to-string (blah pvar))
     *
     * The result of the rewrite with this rule is a string.
     * In resugaring, the input `s` is a string. Thus, it doesn't make
     * sense to match `s` against (blah pvar). Instead, we should
     * calculate to-string^{-1}(s) first and then match it against
     * (blah pvar)
     */
    Biject.lookupBackward(op, t) |. loop(p, fresh, currentLabels)

  | (PatList(ps, EllipsisNone), TermList(ts)) =>
    matchConstruct("", "", ts, ps, (_, args) => PatList(args, EllipsisNone))
  | (PatList(ps, EllipsisPattern(p, l)), TermList(ts)) =>
    let tLen = List.length(ts);
    let pLen = List.length(ps);
    if (pLen > tLen) {
      None;
    } else {
      /* That is, tLen >= pLen */
      let (tPrefix, tSuffix) =
        switch (List.splitAt(ts, pLen)) {
        | None =>
          sprintf(
            "%d is greater than the length of ts: %d",
            pLen,
            List.length(ts),
          )
          |> failwith
        | Some(v) => v
        };
      matchConstruct("", "", tPrefix, ps, (_, ps) =>
        PatList(ps, EllipsisNone)
      )
      |. Option.flatMap(((env, plhs)) =>
           switch (plhs) {
           | PatList(ps, _) =>
             let envs = ref([]);
             let plhss = ref([]);
             let foo = tSuffix;
             let result =
               List.every(foo, t =>
                 switch (loop(t, p, fresh, Set.String.add(currentLabels, l))) {
                 | None => false
                 | Some((subenv, p)) =>
                   envs := [subenv, ...envs^];
                   plhss := [p, ...plhss^];
                   true;
                 }
               );
             if (result) {
               Some((
                 Env.bindEllipsis(env, l, List.reverse(envs^)),
                 PatList(ps, EllipsisList(List.reverse(plhss^), l)),
               ));
             } else {
               None;
             };
           | _ => failwith("expect a PatList")
           }
         );
    };
  | (PatList(_, EllipsisList(_, _)), _) =>
    failwith("Can't match against an ellipsis list")
  | (PatList(_, _), _) => None

  | (PatExtension(_), _) =>
    failwith("PatExtension should have been desugared already")
  };
};

/**
 * Match a term against a pattern.
 */
let matchPattern = (t: term, p: pattern) : option((Env.t, pattern)) =>
  /*
   * NOTE: This implementation is a little bit naive.
   * One possible optimization is to pass in an Env.t and merge
   * the producing Env.t with it.
   */
  loop(t, p, Set.String.empty, Set.String.empty);
