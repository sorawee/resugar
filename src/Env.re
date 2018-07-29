open Belt;
open Printf;
open Utils;
open Structs;

type t = {
  pvar: Map.String.t(term),
  ellipsis: Map.String.t(list(t)),
};

let empty: t = {pvar: Map.String.empty, ellipsis: Map.String.empty};

/**
   * Unify `env1` into `env2`
   */
let rec unify = (env1: t, env2: t) : t => {
  let pvar =
    mapMerge(env1.pvar, env2.pvar, (key, v1, v2) =>
      if (v1 == v2) {
        v1;
      } else {
        RuntimeError(
          sprintf(
            "Failed unification of %s: %s and %s",
            key,
            string_of_term(v1),
            string_of_term(v2),
          ),
        )
        |> raise;
      }
    );
  /*
   * Unlike `pvar`, `ellipsis`'s `v1` and `v2` are allowed to be different. For example:
   *
   * (A [1 2 3] [4 5 6]) / (A [x_{l} ...l] [y_{l} ...l]) =
   *
   * {l: [{x: 1}, {x: 2}, {x: 3}] ++ {l: [{y: 4}, {y; 5}, {y: 6}] =
   *
   * {l: [{x: 1, y: 4}, {x: 2, y; 5}, {x: 3, y: 6}]}
   *
   * But notice that the length of lists must be the same, and things still
   * need to unify properly
   */
  let ellipsis =
    mapMerge(
      env1.ellipsis,
      env2.ellipsis,
      (key, lst1, lst2) => {
        let l1 = List.length(lst1);
        let l2 = List.length(lst2);
        if (l1 != l2) {
          RuntimeError(
            sprintf("Mismatched length of %s: %d and %d", key, l1, l2),
          )
          |> raise;
        } else {
          List.reduce2(lst1, lst2, [], (acc, e1, e2) =>
            [unify(e1, e2), ...acc]
          )
          |> List.reverse;
        };
      },
    );
  {pvar, ellipsis};
};

let bindPVar = (env: t, pvar: string, t: term) : t =>
  unify(
    {
      pvar: Map.String.(set(Map.String.empty, pvar, t)),
      ellipsis: Map.String.empty,
    },
    env,
  );

let bindEllipsis = (env: t, label: string, envs: list(t)) : t =>
  unify(
    {
      pvar: Map.String.empty,
      ellipsis: Map.String.(set(Map.String.empty, label, envs)),
    },
    env,
  );

let rec toString = (env: t) : string => {
  let pvarToString = pvars => {
    let lst = Map.String.toList(pvars);
    String.concat(
      ", ",
      List.map(lst, ((k, v)) => sprintf("%s: %s", k, string_of_term(v))),
    );
  };
  let ellipsisToString = ellipsis => {
    let lst = Map.String.toList(ellipsis);
    String.concat(
      ",\n",
      List.map(lst, ((k, v)) =>
        sprintf("%s: [%s]", k, String.concat(", ", List.map(v, toString)))
      ),
    );
  };
  sprintf(
    "{\npvar: {\n%s\n},\nellipsis: {\n%s\n}\n}",
    pvarToString(env.pvar),
    ellipsisToString(env.ellipsis),
  );
};
