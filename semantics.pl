%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions: lookup and address allocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% σ[ρ[x]]
lookupValue(Var, Env, Store, Value) :- 
  lookupAddress(Var, Env, Address), member([Address, Value], Store), !.

%% ρ[x]
lookupAddress(Var, Env, Address) :- member([Var, Address], Env), !.

%% x ∉ ρ
undefined(Var, Env) :- \+lookupAddress(Var, Env, _).

%% σ[a ↦ v]
set(_, _, [], []) :- !.
set(Address, Value, [[Address, _]|Rest], [[Address, Value]|Rest]) :- !.
set(Address, Value, [[Address2, Value2]|Rest], [[Address2, Value2]|Result]) :-
  Address \== Address2, set(Address, Value, Rest, Result), !.

%% given an environment, allocate a new memory location
alloc([], 0) :- !. % ρ0
alloc([[_, Addr]|_], N) :- N is Addr+1, !.

%% define ⇓ as an operator (so we can use it in infix notation)
%%  954 means that it binds more tightly than `,`
%%  xfx means that each operand has the same precedence
:- op(954, xfx, ⇓).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% ------------------ [Num]
%%   <n, ρ, σ> ⇓ n

(N, _, _) ⇓ N :- 
  number(N), 
  !.


%% ------------------ [Var]
%%  <x, ρ, σ> ⇓ σ[ρ[x]]

(Var, Env, Store) ⇓ Value :- 
  lookupValue(Var, Env, Store, Value), 
  !.


%%         <e1, ρ, σ> ⇓ v1
%%         <e2, ρ, σ> ⇓ v2
%% -------------------------------- [Op]
%%   <e1 op e2, ρ, σ> ⇓ v1 op v2

(Left + Right, Env, Store) ⇓ Result :- 
  (Left,  Env, Store) ⇓ L, 
  (Right, Env, Store) ⇓ R, 
  Result is L+R, 
  !.
(Left - Right, Env, Store) ⇓ Result :- 
  (Left,  Env, Store) ⇓ L, 
  (Right, Env, Store) ⇓ R, 
  Result is L-R, 
  !.
(Left * Right, Env, Store) ⇓ Result :- 
  (Left,  Env, Store) ⇓ L, 
  (Right, Env, Store) ⇓ R, 
  Result is L*R, 
  !.
(Left / Right, Env, Store) ⇓ Result :- 
  (Left,  Env, Store) ⇓ L, 
  (Right, Env, Store) ⇓ R, 
  Result is L/R, 
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Statements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%                  x ∉ ρ
%%              <e, ρ, σ> ⇓ v
%%               a = alloc()
%%              ρ' = ρ[x ↦ a]
%%              σ' = σ[a ↦ v]
%% ------------------------------------ [VarDecl]
%%    <var x := e, ρ, σ> ⇓ <ρ', σ'>

 (var(X, Expr), Env, Store) ⇓ ([[X, Address]|Env], [[Address, Value]|Store]) :- 
   \+lookupAddress(X, Env, _),    % make sure x is not already defined
   (Expr, Env, Store) ⇓ Value,    % evaluate right-hand side
   alloc(Env, Address),           % allocate space for x
   !.

 
 %%                  x ∈ ρ
 %%                <e, σ> ⇓ v
 %%               a = ρ[a]
 %%              ρ' = ρ[x ↦ a]
 %%              σ' = σ[a ↦ v]
 %% ------------------------------------ [SetVar]
 %%    <set x := e, ρ, σ> ⇓ <ρ', σ'>

 (set(X, Expr), Env, Store) ⇓ (Env, Store1) :- 
   lookupAddress(X, Env, Address),      % make sure x is defined; get its addr
   (Expr, Env, Store) ⇓ Value,          % evaluate right-hand side
   set(Address, Value, Store, Store1),  % assign x to the result
   !.
 

 %%           <e, ρ, σ> ⇓ v
 %%            (print v)
 %% ----------------------------- [Print]
 %%   <print e, ρ, σ> ⇓ <ρ, σ>

 (print(Expr), Env, Store) ⇓ (Env, Store) :- 
   (Expr, Env, Store) ⇓ Result,  % evaluate the expression
   write(Result), nl,            % print the result (followed by newline)
   !.
 

 %%         <s1, ρ, σ> ⇓ σ'
 %%         <s2, ρ, σ'> ⇓ σ''
 %% ------------------------------ [Seq]
 %%   <s1 ; s2, ρ, σ> ⇓ <ρ, σ''>

 (seq(Stmt1, Stmt2), Env, Store) ⇓ (Env2, Store2) :- 
   (Stmt1, Env,  Store)  ⇓ (Env1, Store1),  % evaluate 1st stmt
   (Stmt2, Env1, Store1) ⇓ (Env2, Store2),  % chain-evaluate 2nd stmt
   !. 
 

 %%                  <e, ρ, σ> ⇓ 0
 %%                 <st, ρ, σ> ⇓ <ρ', σ'>
 %% -------------------------------------------------- [If0-True]
 %%   <if0 (e) then {st} else {sf}, ρ, σ> ⇓ <ρ', σ'>

 (if0(Expr, TrueBranch, _), Env, Store) ⇓ (Env1, Store1) :-
   (Expr, Env, Store) ⇓ 0,                     % evaluate condition
   (TrueBranch, Env, Store) ⇓ (Env1, Store1),  % evaluate true branch
   !.
 

 %%                <e, ρ, σ> ⇓ n    n ≠ 0
 %%                 <sf, ρ, σ> ⇓ <ρ', σ'>
 %% -------------------------------------------------- [If0-False]
 %%    <if0 (e) then {st} else {sf}, ρ, σ> ⇓ <ρ, σ'>

 (if0(Expr, _, FalseBranch), Env, Store) ⇓ (Env1, Store1) :-
   (Expr, Env, Store) ⇓ N,                     % evaluate condition
   N \== 0,                                    % if result is not 0...
   (FalseBranch, Env, Store) ⇓ (Env1, Store1), % evaluate false branch
   !.
 

 %%                    f ∉ ρ
 %%                  a = alloc()
 %%                 ρ' = ρ[f ↦ a]
 %%                  v = <f, x, s, ρ'>
 %%                 σ' = σ[a ↦ v]
 %% ----------------------------------------- [Def]
 %%    <def f(x) := {s}, ρ, σ> ⇓ <ρ', σ'>

 (def(F, X, S), Env, Store) ⇓ (Env1, [[Address, Value]|Store]) :-
     undefined(F, Env),           % make sure f is not already defined
     alloc(Env, Address),         % allocate space for f 
     Env1 = [[F, Address]|Env],   % bind f to address in env
     Value = clo(F, X, S, Env1),  % make a closure for f's value (recursion enabled!)
     !.
 

 %%             f ∈ ρ
 %%     <f, ρ, σ> ⇓ <f, x, s, ρ_c>
 %%         <e, ρ, σ> ⇓ v
 %%           a = alloc()
 %%          ρ_c' = ρc[x ↦ a]
 %%          σ' = σ[a ↦ v]
 %%    <s, ρ_c', σ'> ⇓ <ρ'', σ''>
 %% ------------------------------- [Call]
 %%    <f(e), ρ, σ> ⇓ <ρ, σ''>

 (call(F, Expr), Env, Store) ⇓ (Env, Store2) :-
     (F, Env, Store) ⇓ clo(F, X, Body, FEnv),  % lookup f to get closure
     (Expr, Env, Store) ⇓ Value,               % evaluate argument
     alloc(Env, Address),                      % allocate space for parameter
     FEnv1 = [[X, Address]|FEnv],              % bind param to address in env
     Store1 = [[Address,Value]|Store],         % bind address to arg value in store
     (Body, FEnv1, Store1) ⇓ (_, Store2),      % evaluate function body
     !.
 
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Tests
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
 %% helper function for evaluating statements and looking up a variable's value
 hasValue(Variable, Stmt, Value) :- 
   Stmt ⇓ (Env, Store), 
   lookupValue(Variable, Env, Store, Value).
 
 %% Numbers
 :- begin_tests(numbers).
 test(number_empty) :-
         (1, [], []) ⇓ 1.
 test(number_env_store) :-
         (1, [[x,0]], [[0,1]]) ⇓ 1.
 :- end_tests(numbers).
 
 
 % Variables
 :- begin_tests(variables).
 test(var_empty, [fail]) :-
         (x, [], []) ⇓ 1.
 test(var_env_store) :-
         (x, [[x,0]], [[0,1]]) ⇓ 1.
 test(var_more_env_store) :-
         (x, [[y,10],[x,0]], [[10,100],[0,1]]) ⇓ 1.
 test(var_bad_store, [fail]) :-
         (x, [[y,0]], [[0,1]]) ⇓ 1.
 :- end_tests(variables).
 
 % Math
 :- begin_tests(math).
 test(add) :-
         (1+1, [], []) ⇓ 2.
 test(subtract) :-
         (1-1, [], []) ⇓ 0.
 test(multiply) :-
         (3*3, [], []) ⇓ 9.
 test(divide) :-
         (30/3, [], []) ⇓ 10.
 test(precedence) :-
         (1+2*3, [], []) ⇓ 7.
 test(associativity) :-
         (1-2-3, [], []) ⇓ -4.
 :- end_tests(math).

 % Variable declarations
 :- begin_tests(var_decls).
 test(var_decl1) :-
         (var(x, 1), [], []) ⇓ ([[x, 0]], [[0, 1]]).
 test(var_redecl, [fail]) :-
         (var(x, 100), [[x, 0]], [[0, 1]]) ⇓ _.
 :- end_tests(var_decls).

 % Variable updates
 :- begin_tests(var_updates).
 test(var_update1) :-
         (set(x, 100), [[x, 0]], [[0, 1]]) ⇓ ([[x, 0]], [[0, 100]]).
 test(var_update2, [fail]) :-
         (set(x, 1), [], []) ⇓ _.
 :- end_tests(var_updates).

 % Sequencing
 :- begin_tests(seq).
 test(seq) :-
         (seq(var(x, 1), set(x, x+1)), [], []) ⇓ ([[x, 0]], [[0, 2]]).
 :- end_tests(seq).

 % If0 statements
 :- begin_tests(if0_stmts).
 test(if0_true) :-
         (if0(0, var(x, 1), var(x, 2)), [], []) ⇓ ([[x, 0]], [[0, 1]]).
 test(if0_false) :-
         (if0(1, var(x, 1), var(x, 2)), [], []) ⇓ ([[x, 0]], [[0, 2]]).
 :- end_tests(if0_stmts).

 % Function definitions
 :- begin_tests(func_defs).
 test(f0) :-
         (def(id, x, set(result, x)), [], []) ⇓ ([[id, 0]], [[0, clo(id, x, set(result, x), [[id, 0]])]]).
 :- end_tests(func_defs).

 % Function calls
 :- begin_tests(func_calls).
 s1(var(result, 0)).
 s2(def(plus2, x, set(result, x+2))).
 s3(call(plus2, 2)).
 s(seq(S1, seq(S2, S3))) :- s1(S1), s2(S2), s3(S3).
 
 test(f0) :-
         (seq(var(result, 0), seq(def(plus2, x, set(result, x+2)), call(plus2, 2))), [], []) ⇓ 
        ([[plus2, 1], [result, 0]],
         [[2, 2], [1, clo(plus2, x, set(result, x+2), [[plus2, 1], [result, 0]])], [0, 4]]).
 test(recursive) :-
  (seq(var(result, 0), 
       seq(def(fact, n, if0(n, 
                            set(result, 1), 
                            seq(call(fact, n-1), set(result, n*result)))), 
           call(fact, 3))), [], []) ⇓
  ([[fact, 1], [result, 0]],
    [[5, 0], 
     [4, 1], 
     [3, 2], 
     [2, 3],
     [1, clo(fact, 
             n, 
             if0(n, 
                 set(result, 1), 
                 seq(call(fact, n-1), set(result, n*result))), 
             [[fact, 1], [result, 0]])], 
     [0, 6]]).
 :- end_tests(func_calls).
