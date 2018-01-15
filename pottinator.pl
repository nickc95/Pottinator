:- include('characters.pl').
:- include('factcategories.pl').
:- include('factkb.pl').

%% APP PREDICATES %%

% initialise_characters(R) is true if R contains a list of all characters defined in the application.
initialise_characters(R):-
    findall(X, character(X), R).
    
% fetch_facts(L, R) is true if R contains a list of all occurrences of facts from characters in list L.
fetch_facts([], []).
fetch_facts([L1|L], R):-
    findall(X, prop(L1, _, X), R1),
    fetch_facts(L, R2),
    append(R1, R2, R).
    
% element_count(X, N, L) is true if N is the number of instances of X in list L.
element_count(X, N, L):-
    aggregate(count, member(X, L), N).
    
% least_element_count(X, N, L) is true if N is the least number of instances of an X in list L.
least_element_count(X, N, L):-
    aggregate(min(N1, X1), element_count(X1, N1, L), min(N, X)).
    
% get_a_question(L, C, R) is true if R contains the least occurring statement question of category C based on characters in list L.
get_a_question(L, C, R):-
    fetch_facts(L, R1),
    least_element_count(R, _, R1),
    prop(_, C, R).
    
% get_a_random_question(L, C, R) is true if R contains a random statement question of category C based on characters in list L.
get_a_random_question(L, C, R):-
    fetch_facts(L, R1),
    random_member(R, R1),
    prop(_, C, R).

% fact_intersect(Q, L, R) is true if list R contains all the characters from list L that have the fact_query Q.
fact_intersect(Q, L, R):-
    findall(X, prop(X, _, Q), R1),
    intersection(L, R1, R).
    
% fetch_fact_categories(CF, R) is true if R contains a list of all fact categories from confirmed characters in list CF.
fetch_fact_categories([], []).
fetch_fact_categories([prop(_ ,FC ,_)|L], [FC|R]):-
    fetch_fact_categories(L, R).

% missing_categories(CF, R) is true if list R contains all the fact categories that are missing from confirmed facts CF.
missing_categories(CF, R):-
    findall(X, fact_category(X), R1),
    fetch_fact_categories(CF, R2),
    subtract(R1, R2, R).
    
% add_to_list(E, L, R) is true if R is a list L with element E added to it.
add_to_list(E, [], [E]).
add_to_list(E, L, [E|L]).

%% Game Implementation %%

% type go. to start the game
:- dynamic yes/1,no/1.

% go is always true.
go:-
    initialise_characters(IC),
    get_a_random_question(IC, C, Q),
    ask_and_process_question(IC, _, C, Q).
    
% ask_and_process_question(L, CF, C, Q) is true if L is a list of characters, CF is a list of previously confirmed facts,
% C is a fact category and Q is a fact.
ask_and_process_question(L, CF, C, Q):-
    length(L, N),
    ((N > 1) ->
        write(Q), write("? type yes/no."), nl,
        read(Response), nl,
        fact_intersect(Q, L, R),
        ((Response == yes ; Response == y) ->
            add_to_list(prop(new_character, C, Q), CF, NCF),
            get_a_question(R, NC, NQ),
            ask_and_process_question(R, NCF, NC, NQ)
            ;
            ((Response == no ; response == n) ->
                subtract(L, R, R1),
                get_a_question(R1, NC, NQ),
                ask_and_process_question(R1, CF, NC, NQ)
                ;
                write("invalid input, please try again."), nl,
                ask_and_process_question(L, CF, C, Q))
            )
        ;
        ((N == 1) ->
            write("is "), write(L), write(" who you were thinking of? type yes/no."),  nl,
            process_post_game(CF)
            ;
            write('NO SOLN'))). % should never get here

% process_post_game(CF) is true if CF is a list of confirmed facts.
process_post_game(CF):-
    read(Response), nl,
    ((Response == yes ; Response == y) ->
        write("game over"), nl
        ;
        ((Response == no ; Response == n) ->
			missing_categories(CF, MC),
            write('please type the name of the character:'), nl,
			read(Name), nl, 
			write_content_to_file(CF, MC, Name)
			;
            write("invalid input, please try again."), nl,
            process_post_game(CF))
        ).
		
write_content_to_file(CF, MC, N):-
	open('characters.pl', append, Stream1),
	open('factkb.pl', append, Stream2),
	add_name_to_characters(N, Stream1),
	add_confirmed_facts_to_kb(CF, N, Stream2),
	add_facts_to_kb(MC, N, Stream2),
	close(Stream1),
	close(Stream2).

% add_name_to_characters(N) is true if N is the name of the character to be added.
add_name_to_characters(N, S):-
	write(S, character(N)), write(S, "."), nl(S).

% add_confirmed_facts_to_kb(CF, N, S) is true if CF is a list of confirmed facts and N is the name of the character to be added,
% using stream S.
add_confirmed_facts_to_kb([], _, _).
add_confirmed_facts_to_kb([prop(_, C, F)|CF], N, S):-
	write(S, prop(N, C, F)), write(S, "."), nl(S),
	add_confirmed_facts_to_kb(CF, N, S).
	
% add_facts_to_kb(MC, N, S) is true if MC is the missing categories of facts needed and N is the name of the character to be added,
% using stream S.
add_facts_to_kb([], _, _).
add_facts_to_kb([C|MC], N, S):-
	write("please type the answer to "), write(C), nl,
	read(Answer), nl,
	write(S, prop(N, C, Answer)), write(S, "."), nl(S),
	add_facts_to_kb(MC, N, S).



