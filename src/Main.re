open Printf;
open Structs;

let runCommon =
    (
      f: (desugaringRules, settings, 'a) => 'a,
      rules: string,
      settings: settings,
    )
    : ('a => 'a) => {
  sprintf(
    "Settings = {resugar: %s, srclocExt: %s}",
    string_of_bool(settings |> resugarGet),
    string_of_bool(settings |> srclocExtGet),
  )
  |> Js.log;
  let rules = ref(Parser.program(rules));
  if (settings |> srclocExtGet) {
    rules := Ext.Srcloc.transform(rules^);
  };
  f(Resolve.resolve(rules^), settings);
};

let runJSON =
  runCommon((rules, settings, obj) =>
    Js.Json.classify(obj)
    |> Parser.parsePattern
    |> patternToTerm
    |. Desugar.desugar(rules, settings)
    |> Printer.jsonTerm
  );

let runTerm =
  runCommon((rules, settings) => Desugar.desugar(_, rules, settings));
