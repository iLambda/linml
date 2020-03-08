%{
  open Lang.Sort
  open Syntax
  open Utils

  (* Optional type annotation *)
  let optional_ty_annot t = function
    | None -> t
    | Some ty -> SynTeTyAnnot (t, ty)

  (* Optional type annotation in pattern *)
  let optional_pat_ty_annot p = function
    | None -> p
    | Some ty -> SynPatTyAnnot (p, ty)

  (* Make a give *)
  let rec give ?ty p value body = match p, ty with 
    (* Desugar into simple give, with type annot if necessary *)
    | SynPatVar x, _-> SynTeGive (x, optional_ty_annot value ty, body)
    (* Desugar the type annot into a return type for match *)
    | SynPatTyAnnot (p, t), _ -> give p value body ~ty:t
    (* Destruct locations *)
    | SynPatLoc (_, p), None -> give p value body
    | SynPatLoc (_, p), Some ty -> give ~ty:ty p value body
    (* Default case is : make a match out of it*)
    | _ -> SynTeMatch (value, ty, [SynClause (p, body)])
    
  (* Accumulate value *)
  let accumulate_nonempty f l = match List.rev l with 
    | [] -> assert false 
    | t::ts -> List.fold_right f (List.rev ts) t

  (* Apply an injection *)
  let rec inject (before, after_oty) t = 
    match before, after_oty with 
      | [], None -> t
      | [], Some after_ty -> SynTeUnionLeft (t, after_ty) 
      | ty::tl, _ -> SynTeUnionRight (ty, inject (tl, after_oty) t)

  (* Make the pattern to extract an injection *)

  (* Make an abstraction *)
  let linear_abs arguments t = 
    List.fold_right (fun (x, ty) t -> SynTeLinAbs (x, ty, t)) arguments t

%}

(*
 * TOKENS
 *)
%token EOF

(* Identifier, tags and operator*)
%token <string * Lexing.position * Lexing.position> IDENTIFIER TAG
/* %token <string * Lexing.position * Lexing.position> IDENTIFIER_PREFIX 
%token <string * Lexing.position * Lexing.position> 
  IDENTIFIER_OP0 IDENTIFIER_OP1 IDENTIFIER_OP2 
  IDENTIFIER_OP3 IDENTIFIER_OP4 */
(* Literals *)
%token <Syntax.integer * string> INTEGER
(* Keywords *)
%token (*KEYWORD_LET*) KEYWORD_GIVE KEYWORD_IN
%token KEYWORD_FUN
%token KEYWORD_MATCH KEYWORD_RETURN KEYWORD_WITH KEYWORD_END KEYWORD_EITHER
%token KEYWORD_ZERO 
%token KEYWORD_TYPE 
(* Operators *)
%token OPERATOR_FATARROW OPERATOR_LOLLIPOP 
%token OPERATOR_INJECT OPERATOR_EXTRACT
(* Punctuation *)
%token PUNCTUATION_LPAREN PUNCTUATION_RPAREN 
%token PUNCTUATION_LANGLE PUNCTUATION_RANGLE  
%token PUNCTUATION_STAR PUNCTUATION_AND PUNCTUATION_PLUS
%token (*PUNCTUATION_SEMICOLON*) PUNCTUATION_COLON PUNCTUATION_MINUS
%token PUNCTUATION_BAR 
%token PUNCTUATION_BANG
%token PUNCTUATION_COMMA 
%token PUNCTUATION_EQUAL PUNCTUATION_UNDERSCORE
(* Entry point *)
%start<Syntax.program> program


(*
 * PRIORITY RULES
 *)
%right OPERATOR_LOLLIPOP PUNCTUATION_RANGLE
%right PUNCTUATION_PLUS
%right PUNCTUATION_STAR PUNCTUATION_AND PUNCTUATION_MINUS
%nonassoc PUNCTUATION_BANG
%%

(*
 * GRAMMAR RULES
 *)

(* Location *)
loc(t):
  | t = t
    { SynTeLoc (($startpos, $endpos), t) }

locp(p):
  | p = p
    { SynPatLoc (($startpos, $endpos), p) }

(* Custom operators *)
/* operator_prefix: id = IDENTIFIER_PREFIX { Identifier.mak term_sort id }
operator_infix_0: id = IDENTIFIER_OP0 { Identifier.mak term_sort id }
operator_infix_1: id = IDENTIFIER_OP1 { Identifier.mak term_sort id }
operator_infix_2: id = IDENTIFIER_OP2 { Identifier.mak term_sort id }
operator_infix_3: id = IDENTIFIER_OP3 { Identifier.mak term_sort id }
operator_infix_4: id = IDENTIFIER_OP4 { Identifier.mak term_sort id }

operator_id: 
  | id = operator_prefix  { id }
  | id = operator_infix_0 { id }
  | id = operator_infix_1 { id }
  | id = operator_infix_2 { id }
  | id = operator_infix_3 { id }
  | id = operator_infix_4 { id } */

(* Identifiers *)
term_variable:
  | id = IDENTIFIER
    { Identifier.mak term_sort id }
  /* | PUNCTUATION_LPAREN id=operator_id PUNCTUATION_RPAREN
    { id } */

(* Arguments *)
term_argument:
  (* (x y ... z : A) *)
  | PUNCTUATION_LPAREN xs=term_variable+ 
    PUNCTUATION_COLON domain=typ
    PUNCTUATION_RPAREN
    { List.map (fun x -> x, domain) xs }

term_arguments:
  (* (x:A) *)
  | a=term_argument { a }
  (* (x:A) ... (x:A) *)
  | a=term_argument args=term_arguments { a @ args }

(* Type constants *)
%inline typconst:
  | i=INTEGER 
    { match i with 
        | _, "0" -> SynTyZero
        | _, "1" -> SynTyOne
        | _ -> $syntaxerror }
  | t=TAG 
    { match t with 
        | "T", _, _ -> SynTyTop 
        | _ -> $syntaxerror }

(* Types *)
typm1:
  (* Constants *)
  | ty=typconst { ty }
  (* A *)
  | id=IDENTIFIER  { SynTyVarOrTyCon (id, []) }  
  (* A! *)
  | PUNCTUATION_BANG ty=typm1 { SynTyBang ty }
  (* (A) *)
  | PUNCTUATION_LPAREN ty=typ PUNCTUATION_RPAREN { ty }

typ0:
  (* A x x x *)
  | id=IDENTIFIER tys=typm1+ { SynTyVarOrTyCon (id, tys) }  
  (* A * B *)
  | ty1=typ0 PUNCTUATION_STAR ty2=typ0 { SynTyTensor (ty1, ty2) }
  (* A & B *)
  | ty1=typ0 PUNCTUATION_AND ty2=typ0 { SynTyWith (ty1, ty2) }
  (* t *)
  | ty=typm1 { ty }

typ1:
  (* A + B *)
  | ty1=typ1 PUNCTUATION_PLUS ty2=typ1 { SynTyPlus (ty1, ty2) }
  (* t *)
  | ty=typ0 { ty }

typ:  
  (* A -o B *)
  | ty1=typ OPERATOR_LOLLIPOP ty2=typ { SynTyLollipop (ty1, ty2) }
  (* A -> B *)
  | ty1=typ /*OPERATOR_ARROW*/ PUNCTUATION_MINUS PUNCTUATION_RANGLE ty2=typ 
    { SynTyArrow (ty1, ty2) }
  (* t *)
  | ty=typ1 { ty }

(* Injection context *)
injection:
  (* Hole left *)
  /* | PUNCTUATION_UNDERSCORE PUNCTUATION_PLUS ty=noplus_typ
    { fun t -> SynTeUnionLeft (t, ty) }
  (* Hole right *)
  | ty=typ PUNCTUATION_PLUS PUNCTUATION_UNDERSCORE
    { fun t -> SynTeUnionRight (ty, t) } */

  (* _ + ... *)
  | PUNCTUATION_UNDERSCORE PUNCTUATION_PLUS after_ty=typ
    { [], (Some after_ty) }
    
  (* .. + a + _ + a + .. *)
  | before_tys=nonempty_list(terminated(typ0, PUNCTUATION_PLUS))
    PUNCTUATION_UNDERSCORE
    after_ty=option(preceded(PUNCTUATION_PLUS, typ))
    { before_tys, after_ty }

(* Patterns *)
alt_pair_pattern:
  (* p (,-)+ *)
  | p=pattern0 a=nonempty_list(preceded(PUNCTUATION_COMMA, PUNCTUATION_MINUS))
    { 0, p, (List.length a) }
  (* (-,)+ p (,-)* *)
  | b=nonempty_list(terminated(PUNCTUATION_MINUS, PUNCTUATION_COMMA)) p=pattern0 
    a=loption(preceded(PUNCTUATION_COMMA, separated_nonempty_list(PUNCTUATION_COMMA, PUNCTUATION_MINUS)))
  { (List.length b), p, (List.length a) }

pattern1:
  (* * *)
  | PUNCTUATION_STAR { SynPatOne }
  (* _ *)
  | PUNCTUATION_UNDERSCORE { SynPatWildcard }
  (* x *)
  | x=term_variable { SynPatVar x }
  (* !p *)
  | PUNCTUATION_BANG p=pattern1 { SynPatBang p }
  (* (p : t) *)
  | PUNCTUATION_LPAREN p=locp(pattern) PUNCTUATION_COLON t=typ PUNCTUATION_RPAREN 
    { SynPatTyAnnot (p, t) }
  (* (p) *)
  | PUNCTUATION_LPAREN p=locp(pattern) PUNCTUATION_RPAREN { p }
  (* <-,-,p,-> *)
  | PUNCTUATION_LANGLE ps=alt_pair_pattern PUNCTUATION_RANGLE
    { let b,p,a = ps in SynPatAltPair (b,p,a)  }


pattern0:
  (* p *)
  | p=pattern1 { p }
  (* p <: A + _ + A *)
  | p=locp(pattern1) OPERATOR_EXTRACT inj=injection
    { SynPatUnion (p, inj) }

pattern:
  (* p *)
  | p=pattern0 { p }
  (* p, p *)
  | p1=locp(pattern0) PUNCTUATION_COMMA p2=locp(pattern)
    { SynPatSimPair (p1, p2) }
  (* p | p *)
  | p1=locp(pattern0) PUNCTUATION_BAR p2=locp(pattern)
    { SynPatOr (p1, p2) }

(* Pattern clauses *)
clause: 
  | p=locp(pattern) OPERATOR_FATARROW t=loc(term) { SynClause (p, t) }

clauses:
  | PUNCTUATION_BAR? cs=separated_nonempty_list(PUNCTUATION_BAR, clause) { cs }

(* Terms *)
constants: 
  (* Multiplicative unit *)
  | PUNCTUATION_STAR { SynTeOne }
  (* Additive unit *)
  | PUNCTUATION_LANGLE PUNCTUATION_RANGLE { SynTeTop } 

/* termOp:  */
  (* t OPERATOR u *)

term0: 
  (* Constant *)
  | c=constants { SynTeConst c }
  (* x *)
  | x=term_variable { SynTeVar x }
  (* x! *)
  | PUNCTUATION_BANG t=loc(term0) { SynTeBang t }
  (* <t> *)
  | PUNCTUATION_LANGLE t=term1 PUNCTUATION_RANGLE
    { t }
  (* <t1, t2> *)
  | PUNCTUATION_LANGLE t=loc(term1) PUNCTUATION_COMMA 
    ts=separated_nonempty_list(PUNCTUATION_COMMA, loc(term1))
    PUNCTUATION_RANGLE
    { accumulate_nonempty (fun t a -> SynTeAltPair(t, a)) (t::ts)  }
  (* (t) *)
  | PUNCTUATION_LPAREN t=loc(term) ty=preceded(PUNCTUATION_COLON, typ) PUNCTUATION_RPAREN 
    { SynTeTyAnnot (t, ty) }
  (* (t) *)
  | PUNCTUATION_LPAREN t=term PUNCTUATION_RPAREN { t }

term1:
  (* t *)
  | t=term0 { t }
  (* t t' *)
  | t1=loc(term1) t2=loc(term0) { SynTeApp (t1, t2) }

term:
  (* t *)
  | t=term1 { t }
  (* t1, t2 *)
  | t=loc(term1) PUNCTUATION_COMMA 
    ts=separated_nonempty_list(PUNCTUATION_COMMA, loc(term1))
    { accumulate_nonempty (fun t a -> SynTeSimPair(t, a)) (t::ts)  }
  (* (x:A) -o t *)
  | KEYWORD_FUN arguments=term_arguments OPERATOR_LOLLIPOP body=loc(term)
    { linear_abs arguments body }
  (* give x : A = t *)
  | KEYWORD_GIVE p=locp(pattern) codomain=preceded(PUNCTUATION_COLON, typ)?
    PUNCTUATION_EQUAL t1=loc(term) KEYWORD_IN t2=loc(term)
    { give (optional_pat_ty_annot p codomain) t1 t2 }
  (* match t return T with | p => t *)
  | KEYWORD_MATCH t=loc(term) 
    ty=preceded(KEYWORD_RETURN, typ)? KEYWORD_WITH 
    c=clauses 
    KEYWORD_END
    { SynTeMatch (t, ty, c) }
  (* either | x | y *)
  | KEYWORD_EITHER PUNCTUATION_BAR? ts=separated_list(PUNCTUATION_BAR, loc(term))
    KEYWORD_END
    { match ts with 
        (* No clauses is top *)
        | [] -> SynTeConst (SynTeTop)
        (* One clause is identity *)
        | [t] -> t
        (* Multiple clauses *)
        | t::ts -> accumulate_nonempty (fun t a -> SynTeAltPair(t, a)) (t::ts)
    }
  (* refute T with t *)
  | KEYWORD_ZERO ty=option(typ) KEYWORD_WITH t=loc(term)
    { optional_ty_annot (SynTeZero t) ty }
  (* x :> A + _ *)
  | t=loc(term1) OPERATOR_INJECT inj=injection
    { inject inj t }

(* Type declaration *)
/* type_decl:
  KEYWORD_TYPE name=IDENTIFIER  */
  


(* Program *)
program: t=term EOF
  { SynProg t }
