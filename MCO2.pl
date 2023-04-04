count_predicates(Count) :-
    findall(_, tb(_), Predicates),
    length(Predicates, Count).

demographics(Name, Age) :-
    write('What is your name? '),
    read(Name),
    write('What is your age? '),
    read(Age),
    askSex.

askSex :-
    write('What is your sex? (m/f) '),
    read(Input),
    (
        Input = 'm' -> assert(sex(male));
        Input = 'f' -> assert(sex(female));
        Input = X -> 
        (
            format('Invalid input. Please enter \'m\' or \'f\'.'), nl,
            askSex
        )
    ).


askSymptom(Question, Predicate) :-
    write(Question),
    read(Input),
    (
        (
            Input = 'y' -> 
            (
                assert(Predicate),
                count_predicates(Count),
                format('TB ~w', [Count])
            )
        );
        Input = 'n';
        (
            Input = Y ->
            (
                format('Invalid input. Please enter \'y\' or \'n\'.'), nl,
                askSymptom(Question, Predicate)
            )
        )
    ).

% Initialization of dyanmic predicates
:- dynamic(tb/1).

:- initialization(main).
main :- 
    % Ask for the patient's name, age, and sex
    demographics(Name, Age),
    askSymptom(('Do you have a cough? '), tb(1)).