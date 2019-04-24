:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% tests for typeExp
test(typeExp_iplus) :- 
    typeExp(iplus(int,int), int).

% this test should fail
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

test(typeExp_iplus_T, [true(T == int)]) :-
    typeExp(iplus(int, int), T).

% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iplus(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct


/* -------------------------------------------------------------------------- */

test(addFloat) :-
    infer([fplus(float, float)], T),
    assertion(T==float).

test(minusInt) :-
    infer([iminus(X,Y)], int),
    assertion(X==int), assertion(Y==int).

test(ifStatement) :-
    infer([if(<(float, float), [int, float, int], [int, int])], T),
    assertion(T==int).

test(ifStatementFail, [fail]) :-
    infer([if(<(float, float), [int, float, float], [int, int])], _).

test(forStatement) :-
    infer([for(int, bool, [int, int, string, int])], T),
    assertion(T==unit).

test(forStatementFail, [fail]) :-
    infer([for(float, bool, [int, int, string, int])], _).

test(globalVariable) :-
    infer([gvLet(v, T, int)], _),
    assertion(T==int).

test(globalVariables) :-
    infer([gvLet(v,T1, int), gvLet(x, T2, float), gvLet(s, T3, string)], _),
    assertion(T1==int), assertion(T2==float), assertion(T3==string).

:-end_tests(typeInf).
