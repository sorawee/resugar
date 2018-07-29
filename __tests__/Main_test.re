open Jest;
open Expect;

describe("desugaring", () => {
  let rules = {j|
sugar s-let:
  | (s-let {s-binding x v} body) => (s-app (s-lam x body) v)
end
sugar s-and:
  | (s-and x y) => (s-if x y (s-bool false))
end
sugar s-binding:
  | (s-binding x y) => {s-binding x y}
end
sugar s-not:
  | (s-not x) => (s-if x (s-bool false) (s-bool true))
end
    |j};

  test("no resugar, no srcloc", () => {
    let f =
      Main.runTerm(
        rules,
        Structs.settings(~resugar=false, ~srclocExt=false),
      );
    let tIn =
      {j|
(s-let (s-binding asd (s-and (s-bool true) (s-bool false)))
       (s-not (s-id asd)))
      |j}
      |> Parser.term;
    let tOut =
      {j|
<s-app <s-lam asd <s-if <s-id asd> <s-bool false> <s-bool true>>>
       <s-if <s-bool true> <s-bool false> <s-bool false>>>
      |j}
      |> Parser.term;
    expect(f(tIn)) |> toEqual(tOut);
  });

  test("no resugar, with implicit srcloc", () => {
    let f =
      Main.runTerm(rules, Structs.settings(~resugar=false, ~srclocExt=true));
    let tIn =
      {j|
(s-let 1 (s-binding 2 asd (s-and 3 (s-bool 4 true) (s-bool 5 false)))
       (s-not 6 (s-id 7 asd)))
      |j}
      |> Parser.term;
    let tOut =
      {j|
<s-app 1 <s-lam 1 asd <s-if 6 <s-id 7 asd> <s-bool 6 false> <s-bool 6 true>>>
       <s-if 3 <s-bool 4 true> <s-bool 5 false> <s-bool 3 false>>>
      |j}
      |> Parser.term;
    expect(f(tIn)) |> toEqual(tOut);
  });

  test("no resugar, with explicit srcloc", () => {
    let rules = {j|
sugar s-when:
  | (s-when @l cond body) =>
    (fresh x (s-let x
                    cond
                    (s-if (s-is-boolean (s-id x))
                          (s-if (s-id x) body (s-nothing))
                          (s-thrown l))))
end
|j};

    let f =
      Main.runTerm(rules, Structs.settings(~resugar=false, ~srclocExt=true));
    let tIn =
      {j|
(s-when 1 (s-not 2 true) (s-print 3 (s-str 4 "blah")))
      |j}
      |> Parser.term;
    let tOut =
      {j|
<s-let 1 var1 <s-not 2 true>
       <s-if 1 <s-is-boolean 1 <s-id 1 var1>>
               <s-if 1 <s-id 1 var1> <s-print 3 <s-str 4 "blah">> <s-nothing 1>>
               <s-thrown 1 1>>>
      |j}
      |> Parser.term
      |. Structs.renameTermVar("var1", "__var_1");
    expect(f(tIn)) |> toEqual(tOut);
  });

  test("fail", () => {
    let rules = {j|
sugar row:
| (row blah) => {row blah}
end

sugar table:
| (table [col_{i} ...i] [{row [val_{i j} ...i]} ...j]) =>
  (t [(r [(c val_{i j} col_{i}) ...i]) ...j])
end
    |j};
    let f =
      Main.runTerm(
        rules,
        Structs.settings(~resugar=false, ~srclocExt=false),
      );
    let tIn =
      {j|
(table ["a" "b"] [(row [1 2]) (row [3 4]) (row [5 6])])
|j}
      |> Parser.term;
    let tOut =
      {j|
<t [<r [<c 1 "a"> <c 2 "b">]> <r [<c 3 "a"> <c 4 "b">]> <r [<c 5 "a"> <c 6 "b">]>]>
      |j}
      |> Parser.term;
    expect(f(tIn)) |> toEqual(tOut);
  });
});
