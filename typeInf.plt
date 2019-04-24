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

/* Test 1 */
test(addFloat) :-
    infer([fplus(float, float)], T),
    assertion(T==float).

/* Test 2 */
test(minusInt) :-
    infer([iminus(X,Y)], int),
    assertion(X==int), assertion(Y==int).

/* Test 3 */
test(nestedMath) :-
    infer([itimes(int, iplus(int, T))], T1),
    assertion(T==int), assertion(T1==int).

/* Test 4 */
test(moreNestedMath) :-
    infer([gvLet(x, T, float), gvar(x,T2), fplus(T4, fplus(T3, fplus(float, T2)))], _),
    assertion(T==float), assertion(T2==float), assertion(T3==float), assertion(T4==float),
    gvar(x,float).

/* Test 5 */
test(ifStatement) :-
    infer([if(<(float, float), [int, float, int], [int, int])], T),
    assertion(T==int).

/* Test 6 */
test(ifNestedStatement) :-
    infer([if(<(float, float), [int, float, int], [if(<(float, float), [int, float, int], [int, int])])], T),
    assertion(T==int).

/* Test 7 */
test(ifStatementFail, [fail]) :-
    infer([if(<(float, float), [int, float, float], [int, int])], _).

/* Test 8 */
test(forStatement) :-
    infer([for(int, bool, [int, int, string, int])], T),
    assertion(T==unit).

/* Test 9 */
test(forNestedStatement) :-
    infer([for(int, bool, [for(int, bool, [int, int, string, int])])], T),
    assertion(T==unit).

/* Test 10 */
test(forStatementFail, [fail]) :-
    infer([for(float, bool, [int, int, string, int])], _).

/* Test 11 */
test(globalVariable) :-
    infer([gvLet(v, T, int)], _),
    assertion(T==int),
    gvar(v, int).

/* Test 12 */
test(globalVariables) :-
    infer([gvLet(v,T1, int), gvLet(x, T2, float), gvLet(s, T3, string)], _),
    assertion(T1==int), assertion(T2==float), assertion(T3==string),
    gvar(v, int), gvar(x, float), gvar(s, string).

/* Test 13 */
test(function) :-
    infer([gvLet(x, T1, float), gfLet(add, [int, int], T, [int, gvar(x, T2)])], _),
    assertion(T1==float),
    assertion(T2==float),
    assertion(T==float),
    gvar(x,float),
    gvar(add, [int,int,float]).

/* Test 14 */
test(complexTest1Fail, [fail]) :-
    infer([gvLet(x, T1, float), gvar(x, C1), gvLet(y, T2, int), gvLet(d, T3, unit), gvar(d, C2), fdivide(C2,C1)], _).

/* Test 15 */
test(complexTest1) :-
    infer([gvLet(x, T1, float), gvLet(y, T2, int), gvLet(q, T3, bool), gvLet(z, T4, string), gvLet(d, T5, unit)], _),
    assertion(T1==float), assertion(T2==int), assertion(T3==bool), assertion(T4==string), assertion(T5==unit).
    
/* Test 16 */
test(printTest) :-
    infer([print(int),print(float),print(bool),print(unit),print(string)], _).
    
/* Test 17 */
test(letIn) :-
    infer([lvLetIn(x,T,float,[gvLet(y, T1, lvar(x,T2))])], unit),
    assertion(T==float), assertion(T1==float), assertion(T2==float),
    \+lvar(x,float), %make sure x is deleted when leaving scope
    gvar(y, float).

/* Test 18 */
test(moreComplexLetIn):-
    infer([lvLetIn(x,T,float,[lvLetIn(y,T1,int,[gvLet(gy, T2, lvar(y,T3)), gvLet(gx, T4, lvar(x,T5))])])], unit),
    assertion(T==float), assertion(T1==int), assertion(T2==int), assertion(T3==int), assertion(T4==float),assertion(T5==float),
    \+lvar(x,float),
    \+lvar(y,int),
    gvar(gy,int),
    gvar(gx,float).

/* Test 19 */
test(failLetIn, [fail]):-
    infer([lvLetIn(x,T,float,[lvLetIn(t,T2,int,[lvLetIn(z,T3,bool,[gvLet(gx,T4,lvar(x,T5))])],gvLet(gz,_,lvar(z,_)))])], unit),
    assertion(T==float), assertion(T2==int), assertion(T3==bool), assertion(T4==float), assertion(T5==float),
    gvar(gx, float).

/* Test 20 */
test(complexTest2Fail, [fail]) :-
    infer([if(==(float, float), [for(int, =(int,int), [for(int, bool, [int, int, string, int])])], [if(<(float, float), [int, float, unit], [lvLetIn(x,T,float,[lvLetIn(y,T1,int,[gvLet(gy, T2, lvar(y,T3)), gvLet(gx, T4, lvar(x,T5))])])])])], T),
    assertion(T==unit).

:-end_tests(typeInf).
