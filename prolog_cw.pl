/* ****************************** Start of Questions ********************************* */




/* ****************************** Start  of Question 1 ********************************* */

/* Question 1: functional vs. logic programming */

    /* consider the following implementation of 'fast' list-reverse in OCaml

    let rec helper xs ys = match xs with [] -> ys | x :: xs -> helper xs (x :: ys)

    let fast_reverse xs = helper xs []

    */

    /* Show how to take the above functional definition
       and translate it into prolog clauses.

    In your answer, pay careful attention to the arity/arities of the predicates involved,
    as well as explaining in your comments how you handle:
    
    a. pattern-matching
    b. recursion
    c. the role of types, if any
    
    */

    % Pattern matching is handled by defining the different patterns by separate rules
    helper([], Y) :-
        write(Y).
    helper([H|T], Y) :- % Recursion is handled by using the predicate within the rule itself
        helper(T, [H|Y]).

    fast_reverse(X) :-
        helper(X, []).
    % All arguments for all predicates are Lists. Even though not explicit, List type is implied within rule definition, if not implied in predicate parameters.

/* ****************************** End of Question 1 ********************************* */




/* ****************************** Start of Question 2 ********************************* */

/* Q2: triangular numbers */

    /* Q2a. write rules defining a predicate `ints` of arity 2, so that
    
    ints(n,ns) holds exactly when ns = [0, 1, 2, ..., (n-1)], provided n > 0,
                              and ns = [], otherwise

    make clear in your answer if you use any auxiliary, or pre-defined, predicates
    in your solution, and explain their usage
    */
    ints(N, Ns) :- % Case: N <= 0
        N =< 0,
        Ns = [].
    ints(N, Ns) :- % Case: N > 0
        DecN is N-1,
        numlist(0, DecN, Ns). % numlist generates a list from arg1 to arg2 and stores it in arg3

    /* Q2b. write rules defining a predicate `sum_of_ints` of arity 2, so that
    
    sum_of_ints(n,m) holds exactly when m = 0 + 1 + 2 + ... + (n-1), provided n > 0,
                                    and m = 0, otherwise

    make clear in your answer if you use any auxiliary, or pre-defined, predicates
    in your solution, and explain their usage
    */
    sum_of_ints(N, M) :- % Case: N <= 0
        N =< 0,
        M = 0.
    sum_of_ints(N, M) :- % Case: N > 0
        DecN is N - 1, % Decrement N so that below helper rule can start sum from (n-1) to 0
        helper(DecN, 0, M). % Created a helper rule (defined below) with an accumulator as arg2
    
    helper(N, Acc, M) :- % helper RECURSIVE CASE
        N > 0, % Checks that N > 0
        NewAcc is Acc + N, 
        DecN is N - 1, % Decrements N for next recursive run of helper rule
        helper(DecN, NewAcc, M).
    helper(0, Acc, Acc). % helper BASE CASE

/* ****************************** End of Question 2 ********************************* */




/* ****************************** Satrt of Question 3 ********************************* */

/* Question 3: unification */

    /*
      Consider the definitions of the following two predicates, un/2 and eq/2
    */

un(X,X) :- true.
un(X,Y) :- false.

eq(X,Y) :- X is Y.
eq(Y,X) :- Y is X. 

    /*

    by considering suitable example queries, involving constants,
    constant expressions, and also logic variables,
    and the output behaviour of the Prolog interpreter on such queries,
    which you should include as comments in your answer, explain
    the difference(s), if any, between the two predicates `un` and `eq`

    */

    /*  To fully understand the predicates 'un' and 'eq', I queried them on the Prolog terminal using constants, expressions and logic variables. The below are my findings:
            % Constants
            ?- un(3,3).
            true .
            ?- un(3,4).
            false .
            ?- eq(3,3).
            true .
            ?- eq(3,4).
            false .

            % Constant Expressions
            ?- un(2+1 , 5-2).
            false .
            ?- un(2+1 , 2+1).
            true .
            ?- eq(2+1 , 5-2).
            false .
            ?- eq(2+1 , 2+1).
            false .

            % Constants and Constant Expressions
            - un(3 , 5-2).
            false .
            ?- eq(3 , 5-2).
            true .

            % Logical Variables
            ?- un(N, N).
            true .
            ?- un(N, M).
            N = M ;
            false .
            ?- eq(N, N).
            ERROR: Arguments are not sufficiently instantiated
            ?- eq(N, M).
            ERROR: Arguments are not sufficiently instantiated
        Observing the outputs of above queries, we can say the following:
            When constants are inputted into the predicates, both predicates behave in the same manner
            When either constant expressions or logical variables are inputted into the predicates, the predicates behave differently.
        With the observations made, we can explain the difference between the two predicates.
        While both predicates compare their arguments, the 'un' emulates the usage of the '=' operator for comparison, while the 'eq' predicate implicitly uses the 'is' operator.
            '=' means unify in Prolog and knows nothing of arithmetic operations. It simply uses the idea of structure. An example code of ?- A + B = 5 + 2. would return A=5, B=2.
            'is' on the other hand does know of arihmetics. It unifies the left argument with the result of its right argument. That is why in the above 'Constants and Constant Expression'
            example, the 'eq' predicate returns true, as it evaluates the right argument. And the 'un' predicate returns false, since the right argument is not evaluated, it is just taken 
            as 5-2 rather than 3. This also explains the error in the 'Logical Variable' example for 'eq', as neither N nor M can be evaluated. 
    */

/* ****************************** End of Question 3 ********************************* */




/* ****************************** Start of Question 4 ********************************* */

/* Question 4: program comprehension */

    /* The following database of facts and rules defines a fragment of the ScotRail
    network of trainlines and towns between Edinburgh Waverley and Leuchars stations,
    together with the travel times between them in minutes
    (these travel times should be taken as specimens, and need not reflect real-world times!)

    Study the facts and rules carefully, then answer the questions below in comments,
    EXCEPT where you are asked to extend the database in Q4d. 
    */

/* some slow lines */

slow(waverley,10,haymarket).
slow(haymarket,17,inverkeithing).
slow(inverkeithing,12,kirkcaldy).
slow(kirkcaldy,19,cupar).
slow(cupar,13,leuchars).

/* some fast lines */

fast(waverley,20,inverkeithing).
fast(haymarket,20,kirkcaldy).
fast(kirkcaldy,20,leuchars).

/* a line is either slow or fast, but takes as long as it takes */

line(X,T,Y) :- slow(X,T,Y).
line(X,T,Y) :- fast(X,T,Y).

/* the longest journey begins with a single step */

journey(X,N,Z) :- line(X,L,Y), journey(Y,M,Z), N is L+M. 
journey(X,0,X) :- !.

long_journey(X,N,Z) :- long_journey(X,L,Y), long_journey(Y,M,Z), N is L+M. 
long_journey(X,0,X) :- !.

/* how long does it take? */

total_journey_time(X,Y) :- journey(X,N,Y), N > 0, write("Total journey time is: "), write(N), write(" minutes\n").
total_journey_time(X,X) :- write("No need to take the train! Just stay where you are: "), write(X), write("\n"). 

    /* Q4a: querying the database */

    /* write a query to show all the possible routes from waverley to leuchars */
            % journey(waverley , N , leuchars).

    /* which of these routes takes the shortest time? the longest? */
            % The shortest route would be the 4th route: N = 50
            % The longest route would be the first route: N = 71

    /* write a query to show all the possible routes from *any* station to leuchars */
            % journey(X , N , leuchars).

    /* which of these routes takes the shortest time? the longest? */
            % The shortest route would be: X = leuchars, N = 0; |OR|  X = cupar, N = 13;
            % The longest route would be: X = waverley, N = 71;

    /* explain why the total_journey_time/2 predicate does NOT
       necessarily write out journey times in ascending/descending order? */
            /*  Firstly, there is no comparison within the rules to compare one time with another, therefore, there is no way for the predicate to display the journey times in any 
                order. Secondly, the predicate uses another predicate called 'journey' within its rules. After running the 'journey' predicate through the trace feature in Prolog, 
                it was obvious that this predicate works on the basis of backtracking. First the predicate cycles through all the slow lines to get from X to Y. Once it reaches the 
                end it then backtracks to calculate the total time (N). After the time is calculated, the next cycle should start. The next cycle does not start from the beginning, 
                but backtracks from the last line. As it backtracks, it checks to see whether there is an alternate fast line that can be taken, and once it is found, then the 
                predicate cycles through as normal until it can reach Y, and so on. This is why there seems to be no discernable order or pattern at which the length of the journey 
                is displayed using the total_journey_time predicate.
            */

    /* Q4b: understanding the database */

    /* why would you avoid queries involving the `long_journey` predicate? */
            % The reason to avoid is that the querying of this predicate takes up too much memory, as it atempts to recursively implement the predicate twice, causing the stack limit to
            % be exceeded, no matter the routes. For example, 
                long_journey(waverley, haymarket).
            % Which should be as simple as 10 (as that is the only route), still gives a Stack limit exceeded ERROR.
  
    /* Q4c: modifying the database */

    /* what change could you make to the above program to try fast lines before slow ones? */
            % You can swap the line rules. Instead of:
                % line(X,T,Y) :- slow(X,T,Y).
                % line(X,T,Y) :- fast(X,T,Y).
            % you can do:
                % line(X,T,Y) :- fast(X,T,Y).
                % line(X,T,Y) :- slow(X,T,Y).
            % This would cause predicates like journey to cycle through the fast lines before looking at any slow lines.

    /* what change could you make to the above program to show the intermediate stations along any journey? */
            /*  To show the intermediate stations, a 4th argument will have to be added to both the 'journey' predicate. This argument will be a list that will contain all of the stations. 
                The recursive 'journey' predicate can be changed as follows:
                    journey(X,N,Z,[X|C]) :-
                        line(X,L,Y),
                        journey(Y,M,Z,C),
                        N is L+M.
                We also need to make changes to the base 'journey' predicate:
                    journey(X,T,Y,[X,Y]) :-
                        line(X,T,Y).
                Note the difference between the first and second pattern of list construction: [X, Y] vs [X|C]. This because C will be a 
                list, while Y will be an atom, when the rule will succeed.
            */

    /* BONUS Question Q4d: extending the database */

    /* how might you extend the database with a new predicate, shortest_journey/2

    which will compute the *shortest* journey time between any two towns in the database?

    Explain carefully how you arrived at your solution, and what additional predicates,
    if any, predefined or otherwise, you may need in your answer.*/

        shortest_journey(X, Y) :-
            setof(N , journey(X, N, Y), [N|_]),
            journey(X, N, Y),
            write("Shortest journey time is: "),
            write(N), write(' minutes').
    /*  The predicate journey/3 displays the times for all possible routes from X to Z. To find the
        shortest time, I had to find the smallest value of all the times. The easiest way to do this is 
        to create a list of all the values and find the minimum value. An easier way (in the sense of 
        less computation) would be to create an ordered list and return the first atom. The predicate
        setof/3 creates an ordered list. It works as such that arg1 is the value received from arg2 (the
        goal i.e. the predicate that produces the desired value) and is then stored in arg3. Since it is 
        an ordered list, I simply put [N|_] to output the first atom (N) and ignore the rest (_) rather
        than putting a variable for the whole list to be stored in.
    */

/* ****************************** End of Question 4 ********************************* */




/* ****************************** End of Questions ********************************* */


