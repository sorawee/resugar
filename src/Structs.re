open Belt;
open Printf;

exception RuntimeError(string);

type primitive =
  | PrimStr(string)
  | PrimNum(string)
  | PrimBool(bool)
  | PrimLoc(string);

type pattern =
  | PatPVar(string, Set.String.t)
  | PatDrop
  | PatPrim(primitive)
  | PatCore(string, list(pattern))
  | PatFresh(string, pattern)
  | PatCapture(string, pattern)
  | PatSurf(string /* op */, list(pattern) /* args */, bool /* fromUser */)
  | PatAux(string, list(pattern))
  | PatMeta(string, list(pattern))
  | PatBiject(string, pattern)
  | PatList(list(pattern) /* args */, ellipsis)
  | PatOption(option(pattern))
  | PatVar(string)
  | PatTag(pattern /* lhs */, pattern /* rhs */, pattern /* body */)
  | PatExtension(pattern)
and ellipsis =
  | EllipsisNone
  | EllipsisPattern(pattern, string)
  | EllipsisList(list(pattern), string);

class patternVisitor ('env) = {
  pub visitPVar = (_, pvar, labels) => PatPVar(pvar, labels);
  pub visitDrop = _ => PatDrop;
  pub visitPrim = (_, prim) => PatPrim(prim);
  pub visitCore = (env, op, ps) =>
    PatCore(op, List.map(ps, this#visit(env)));
  pub visitSurf = (env, op, ps, fromUser) =>
    PatSurf(op, List.map(ps, this#visit(env)), fromUser);
  pub visitAux = (env, op, ps) => PatAux(op, List.map(ps, this#visit(env)));
  pub visitMeta = (env, op, ps) =>
    PatMeta(op, List.map(ps, this#visit(env)));
  pub visitBiject = (env, op, p) => PatBiject(op, this#visit(env, p));
  pub visitFresh = (env, item, p) => PatFresh(item, this#visit(env, p));
  pub visitCapture = (env, item, p) => PatCapture(item, this#visit(env, p));
  pub visitVar = (_, v) => PatVar(v);
  pub visitTag = (env, l, r, p) =>
    PatTag(this#visit(env, l), this#visit(env, r), this#visit(env, p));
  pub visitList = (env, ps, el) =>
    PatList(
      List.map(ps, this#visit(env)),
      switch (el) {
      | EllipsisPattern(p, l) => EllipsisPattern(this#visit(env, p), l)
      | EllipsisList(ps, label) =>
        EllipsisList(List.map(ps, this#visit(env)), label)
      | EllipsisNone => EllipsisNone
      },
    );
  pub visitOption = (env, opt) =>
    PatOption(Option.map(opt, this#visit(env)));
  pub visitExtension = (env, p) => PatExtension(this#visit(env, p));
  pub visit = (env: 'env, p) =>
    switch (p) {
    | PatPVar(pvar, labels) => this#visitPVar(env, pvar, labels)
    | PatDrop => this#visitDrop(env)
    | PatPrim(prim) => this#visitPrim(env, prim)
    | PatCore(op, ps) => this#visitCore(env, op, ps)
    | PatSurf(op, ps, fromUser) => this#visitSurf(env, op, ps, fromUser)
    | PatAux(op, ps) => this#visitAux(env, op, ps)
    | PatMeta(op, ps) => this#visitMeta(env, op, ps)
    | PatBiject(op, p) => this#visitBiject(env, op, p)
    | PatFresh(item, p) => this#visitFresh(env, item, p)
    | PatCapture(item, p) => this#visitCapture(env, item, p)
    | PatVar(v) => this#visitVar(env, v)
    | PatTag(l, r, p) => this#visitTag(env, l, r, p)
    | PatList(ps, el) => this#visitList(env, ps, el)
    | PatOption(opt) => this#visitOption(env, opt)
    | PatExtension(p) => this#visitExtension(env, p)
    };
};

type term =
  | TermPrim(primitive)
  | TermCore(string, list(term))
  | TermSurf(string, list(term), bool)
  | TermAux(string, list(term))
  | TermVar(string)
  | TermList(list(term))
  | TermOption(option(term))
  | TermTag(pattern, pattern, term);

let string_of_prim =
  fun
  | PrimStr(s) => sprintf("\"%s\"", s)
  | PrimNum(n) => n
  | PrimBool(b) => string_of_bool(b)
  | PrimLoc(_) => ".";

let rec string_of_term = (t: term) => {
  let string_of_args = (ts: list(term)) =>
    String.concat(" ", List.map(ts, string_of_term));

  switch (t) {
  | TermPrim(p) => string_of_prim(p)
  | TermCore(op, args) => sprintf("<%s %s>", op, string_of_args(args))
  | TermSurf(op, args, fromUser) =>
    sprintf("(%s%s %s)", fromUser ? "%" : "", op, string_of_args(args))
  | TermAux(op, args) => sprintf("{%s %s}", op, string_of_args(args))
  | TermList(args) => sprintf("[%s]", string_of_args(args))
  | TermVar(v) => v
  | TermOption(None) => "none"
  | TermOption(Some(t)) => sprintf("{some %s}", string_of_term(t))
  | TermTag(_, _, body) => sprintf("#%s", string_of_term(body))
  };
};

let rec string_of_pattern = (t: pattern) => {
  let string_of_args = (ts: list(pattern)) =>
    String.concat(" ", List.map(ts, string_of_pattern));

  switch (t) {
  | PatPrim(p) => string_of_prim(p)
  | PatCore(op, args) => sprintf("<%s %s>", op, string_of_args(args))
  | PatSurf(op, args, fromUser) =>
    sprintf("(%s%s %s)", fromUser ? "%" : "", op, string_of_args(args))
  | PatAux(op, args) => sprintf("{%s %s}", op, string_of_args(args))
  | PatMeta(op, args) => sprintf("(meta %s %s)", op, string_of_args(args))
  | PatBiject(op, p) => sprintf("(biject %s %s)", op, string_of_pattern(p))
  | PatList(args, EllipsisNone) => sprintf("[%s]", string_of_args(args))
  | PatList(args, EllipsisPattern(p, l)) =>
    sprintf(
      "[%s | %s ... %s]",
      string_of_args(args),
      string_of_pattern(p),
      l,
    )
  | PatList(args, EllipsisList(ps, l)) =>
    sprintf(
      "[%s | <<%s>> with label %s ]",
      string_of_args(args),
      string_of_args(ps),
      l,
    )
  | PatDrop => "_"
  | PatVar(s) => sprintf("(Var | %s)", s)
  | PatFresh(item, body) =>
    sprintf("(Fresh %s | %s)", item, string_of_pattern(body))
  | PatCapture(item, body) =>
    sprintf("(Capture %s | %s)", item, string_of_pattern(body))
  | PatPVar(v, labels) =>
    if (Set.String.isEmpty(labels)) {
      v;
    } else {
      sprintf("%s_{%s}", v, String.concat(" ", Set.String.toList(labels)));
    }
  | PatOption(None) => "none"
  | PatOption(Some(t)) => sprintf("{some %s}", string_of_pattern(t))
  | PatTag(_, _, body) => sprintf("#%s", string_of_pattern(body))
  | PatExtension(p) => sprintf("@%s", string_of_pattern(p))
  };
};

type desugaringRuleCase = {
  lhs: pattern,
  rhs: pattern,
};
type desugaringRules = Map.String.t(list(desugaringRuleCase));

let string_of_program = (prog: desugaringRules) : string =>
  List.map(Map.String.toList(prog), ((rule, cases)) =>
    List.map(cases, ({lhs, rhs}) =>
      sprintf(
        "  | %s => %s",
        string_of_pattern(lhs),
        string_of_pattern(rhs),
      )
    )
    |> String.concat("\n")
    |> sprintf("sugar %s:\n%s\nend", rule, _)
  )
  |> String.concat("\n\n");

[@bs.deriving abstract]
type settings = {
  resugar: bool,
  srclocExt: bool,
};

let rec termToPattern = (t: term) : pattern => {
  let termToPatterns = List.map(_, termToPattern);
  switch (t) {
  | TermPrim(p) => PatPrim(p)
  | TermCore(op, args) => PatCore(op, termToPatterns(args))
  | TermSurf(op, args, fromUser) =>
    PatSurf(op, termToPatterns(args), fromUser)
  | TermAux(op, args) => PatAux(op, termToPatterns(args))
  | TermVar(v) => PatVar(v)
  | TermList(args) => PatList(termToPatterns(args), EllipsisNone)
  | TermOption(opt) => PatOption(Option.map(opt, termToPattern))
  | TermTag(lhs, rhs, body) => PatTag(lhs, rhs, termToPattern(body))
  };
};

let rec patternToTerm = (p: pattern) : term => {
  let patternToTerms = List.map(_, patternToTerm);
  switch (p) {
  | PatPrim(p) => TermPrim(p)
  | PatCore(op, args) => TermCore(op, patternToTerms(args))
  | PatSurf(op, args, _) => TermSurf(op, patternToTerms(args), true)
  | PatAux(op, args) => TermAux(op, patternToTerms(args))
  | PatList(args, EllipsisNone) => TermList(patternToTerms(args))
  | PatOption(opt) => TermOption(Option.map(opt, patternToTerm))
  | PatVar(var) => TermVar(var)
  | PatMeta(_, _)
  | PatBiject(_, _)
  | PatList(_, EllipsisPattern(_, _))
  | PatList(_, EllipsisList(_, _))
  | PatFresh(_, _)
  | PatCapture(_, _)
  | PatPVar(_, _)
  | PatDrop
  | PatTag(_, _, _)
  | PatExtension(_) =>
    raise(RuntimeError(sprintf("Syntax error: %s", string_of_pattern(p))))
  };
};

let renamePatternPVar = (p: pattern, before: string, after: string) : pattern => {
  let visitor = {
    as _this; /* suppress warning about unused this */
    inherit class patternVisitor(unit);
    pub! visitVar = (_, v) =>
      PatVar(
        if (v == before) {
          after;
        } else {
          v;
        },
      )
  };

  visitor#visit((), p);
};

/* NOTE: a lazy approach. Using a term visitor is better. */
let renameTermVar = (t, before, after) =>
  t |> termToPattern |. renamePatternPVar(before, after) |> patternToTerm;
