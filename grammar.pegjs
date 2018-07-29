/**
 * Based on https://github.com/pegjs/pegjs/blob/master/examples/javascript.pegjs
 */

ProgramTop
  = _ p:Program _ { return p; }

Program
  = r:Rule rs:(__ Rule)* {
    return [r].concat(rs.map(e => e[1]));
  }
  / _ { return []; }

Rule
  = "sugar" __ n:Ident _ ":" cases:(__ Case)* __ "end" {
    return {n, cases: cases.map(e => e[1])};
  }

Case
  = "|" __ lhs:Pattern __ "=>" __ rhs:Pattern {
    return {lhs, rhs};
  }

PatternTop
  = _ p:Pattern _ { return p; }

Pattern
  = v:Prim { return v; }
  / "_" { return {t: aDROP}; }
  / v:PVar { return {t: aPVAR, v}; }
  / v:Var { return {t: aVAR, v}; }
  / v:Option { return v; }
  / v:Fresh { return {t: aFRESH, v}; }
  / v:Capture { return {t: aCAPTURE, v}; }
  / v:Biject { return {t: aBIJECT, n: v.n, pat: v.pat}; }
  / v:Meta { return {t: aMETA, n: v.n, ps: v.ps}; }
  / v:Core { return {t: aCORE, n: v.n, ps: v.ps}; }
  / v:Surf { return {t: aSURFACE, n: v.n, ps: v.ps}; }
  / v:Aux { return {t: aAUX, n: v.n, ps: v.ps}; }
  / v:List { return v; }
  / v:Ext { return {t: aEXT, v}; }

Ext
  = "@" p:Pattern { return p; }

IdentList
  = first:Ident rest:(__ Ident)* {
    return [first].concat(rest.map(e => e[1]));
  }
  / _ { return []; }

PVar
  = n:Ident "_{" _ labels:IdentList _ "}" {
    return {n, labels};
  }

Var
  = v:Ident { return v; }

Meta
  = "(" _ "meta" __ n:Ident ps:(__ Pattern)* _ ")" {
    return {n, ps: ps.map(e => e[1])};
  }

Biject
  = "(" _ "biject" __ n:Ident __ pat:Pattern _ ")" {
    return {n, pat};
  }

Core
  = "<" _ n:Ident ps:(__ Pattern)* _ ">" {
    return {n, ps: ps.map(e => e[1])};
  }

Surf
  = "(" _ n:Ident ps:(__ Pattern)* _ ")" {
    return {n, ps: ps.map(e => e[1])};
  }

Aux
  = "{" _ n:Ident ps:(__ Pattern)* _ "}" {
    return {n, ps: ps.map(e => e[1])};
  }

Fresh
  = "(" _ "fresh" __ item:VarItem __ pat:Pattern _ ")" {
    return {varItem: item, pat};
  }

Capture
  = "(" _ "capture" __ item:VarItem __ pat:Pattern _ ")" {
    return {varItem: item, pat};
  }

VarItem = Ident

List
  = "[" _ "]" {
    return {t: aLIST, v: []};
  }
  / "[" p:Pattern ps:(__ Pattern)* ellipsis:(__ "..." _ Ident)? _ "]" {
    const pats = [p].concat(ps.map(e => e[1]));
    if (ellipsis) {
       const lastP = pats.pop();
       return {
         t: aELLIP,
         v: pats,
         label: ellipsis[3],
         pat: lastP,
       };
    } else {
       return {
         t: aLIST,
         v: pats,
       };
    }
}

Option
  = "none" { return {t: aNONE}; }
  / "{" _ "some" _ p:Pattern _ "}" {
    return {t: aSOME, v: p};
  }

Prim
  = b:Bool  { return {t: aBOOL, v: b}; }
  / n:Number { return {t: aNUMBER, v: n}; }
  / s:String { return {t: aSTRING, v: s}; }

Ident "identifier"
  = !Keyword [a-zA-Z][a-zA-Z0-9-]* { return text(); }

IdentPart
  = [a-zA-Z0-9-]

Keyword
  = "none" !IdentPart
  / "some" !IdentPart
  / "sugar" !IdentPart
  / "true" !IdentPart
  / "false" !IdentPart
  / "capture" !IdentPart
  / "fresh" !IdentPart
  / "meta" !IdentPart

Number "number"
  = [0-9]+ { return text(); }

String "string"
  = '"' chars:DoubleStringCharacter* '"' {
      return chars.join("");
    }

Bool
  = "true" { return true; }
  / "false" { return false; }

LineTerminator = "\n"

DoubleStringCharacter
  = !('"' / "\\" / LineTerminator) . { return text(); }
  / "\\" sequence:EscapeSequence { return sequence; }

EscapeSequence
  = '"'
  / "\\"
  / "n" { return "\n"; }

_ "whitespace"
  = (" " / LineTerminator / Comment)*

__ "whitespace"
  = (" " / LineTerminator / Comment)+

Comment "comment"
  = "#" (!LineTerminator .)*
