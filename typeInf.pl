/* match functions by unifying with arguments 
    and infering the result
*/
typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(Args, [T], FType), /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpList(FType, TArgs). /* recurisvely match types */

/* propagate types */
typeExp(T, T).

/* list version to allow function mathine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(Hin, Hout), /* type infer the head */
    typeExpList(Tin, Tout). /* recurse */

/* TODO: add statements types and their type checking */
/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */

typeStatement(gvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    typeCode([Code], T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add definition to database */

typeStatement(lvLetIn(Name, T, Code, Func), unit):-
    % 1. Check if var is valid 
    atom(Name), /* make sure we have a bound name */
    typeCode([Code], T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */

    % 2. Add var to special local variable cache (stack?)
    asserta(lvar(Name, T)), /* add definition to local stack */

    % 3. Run Func (?)
    typeCode(Func,_),

    % 4. Remove var from local cache
    retract(lvar(Name, T)).
    
/* gfLet for functions */
/* idk what im doing */
/* need something for parameters */
/* do we even need this ? */
/* should this even be a statement ? */
/* stay tuned to find out */

/* i think this works right now */
typeStatement(gfLet(Name, Args, T, Code), T):-
    atom(Name),
    typeCode(Code, T),
    is_list(Args),
    append(Args, [T], X),
    asserta(gvar(Name, X)).


/* If statement */
typeStatement(if(Cond, TCode, FCode), T):-
    typeExp(Cond, bool),
    typeCode(TCode, T),
    typeCode(FCode, T),
    bType(T).

/* For loop */
typeStatement(for(Assign, Cond, Code), T):-
    typeExp(Assign, int), % ensures assign is of type int
    typeExp(Cond, bool),
    typeCode(Code, _T3), % we dont care about this
    typeExp(T, unit).


typeStatement(Expr, T):-
    typeExp(Expr, T),
    bType(T).

typeStatement(gvar(Name, T), T):-
    gvar(Name, T).

typeStatement(lvar(Name, T), T):-
    lvar(Name, T).

typeStatement(leval(Name, T), T):-
    typeCode([lvar(Name, T)],T).


/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
typeCode([], T):- bType(T).
typeCode([S], T):-typeStatement(S, T).
typeCode([S, S2|Code], T):-
    typeStatement(S,_T),
    typeCode([S2|Code], T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(Code, T).

/* Basic types
    TODO: add more types if needed
 */
bType(bool).
bType(int).
bType(float).
bType(string).
bType(unit). /* unit type for things that are not expressions */
/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/* allow alpha types */
bType(T):-
    var(T).

/*
    TODO: as you encounter global variable definitions
    or global functions add their definitions to 
    the database using:
        asserta( gvar(Name, Type) )
    To check the types as you encounter them in the code
    use:
        gvar(Name, Type) with the Name bound to the name.
    Type will be bound to the global type
    Examples:
        g

    Call the predicate deleveGVars() to delete all global 
    variables. Best wy to do this is in your top predicate
*/

deleteGVars():-retractall(gvar), asserta(gvar(_X,_Y):-false()).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type

    TODO: add more functions
*/
fType('=', [int, int, bool]).
fType('==', [float, float, bool]).
fType('<', [float, float, bool]).
fType(iminus, [int, int, int]).
fType(itimes, [int, int, int]).
fType(idivide, [int, int, int]).
fType(iplus, [int,int,int]).
fType(fplus, [float, float, float]).
fType(fminus, [float, float, float]).
fType(ftimes, [float, float, float]).
fType(fdivide, [float, float, float]).
fType(fToInt, [float,int]).
fType(iToFloat, [int,float]).
fType(print, [_X, unit]). /* simple print */

/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-  % this doesnt account for the return type, gflet appends return type to args like in ocaml -Kyle
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
%gvar(_, _) :- false().

:- dynamic(gvar/2), dynamic(lvar/2).
