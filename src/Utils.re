open Belt;
open Printf;

/**
 * Function composition operator
 */
let (@@@) = (f: 'y => 'z, g: 'x => 'y) : ('x => 'z) => (x: 'x) => f(g(x));

/**
 * Find the first element `e` in `lst` such that `f(e)` returns
 * positively (`Some`) and returns `f(e)`. If there is no
 * such element, returns `None`.
 */
let rec findBy = (lst: list('a), f: 'a => option('b)) : option('b) =>
  switch (lst) {
  | [] => None
  | [e, ...rest] =>
    switch (f(e)) {
    | None => findBy(rest, f)
    | x => x
    }
  };

/**
 * Merge `m1` into `m2`. When conflicted, consult `f`.
 */
let mapMerge =
    (m1: Map.String.t('a), m2: Map.String.t('a), f: (string, 'a, 'a) => 'a)
    : Map.String.t('a) =>
  Map.String.reduce(m1, m2, (acc, k, v) =>
    switch (Map.String.get(acc, k)) {
    | None => Map.String.set(acc, k, v)
    | Some(v2) => Map.String.set(acc, k, f(k, v, v2))
    }
  );

/**
 * Gensym generator
 */
let newGensym = () => {
  let counter = ref(0);
  (prefix: string) => {
    counter := counter^ + 1;
    let counter = counter^;
    sprintf("%s%d", prefix, counter);
  };
};

/**
 * Set to string
 */
let string_of_set_string = (s: Set.String.t) : string =>
  Set.String.toList(s) |> String.concat(" ", _);

/**
* Set dict compositionally
*/
let dictSet = (d, k, v) => {
  Js.Dict.set(d, k, v);
  d;
};
