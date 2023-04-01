count_predicates(Count) :-
    findall(_, tb(_), Predicates),
    length(Predicates, Count).

demographics(Name, Age, Sex) :-
    write('What is your name? '),
    read(Name),
    write('What is your age? '),
    read(Age),
    write('What is your sex? (m/f) '),
    read(Sex).

askSex(Input, Question) :-
    write(Question),
    read(X),
    (
        Input = 'm';
        Input = 'f';
        (
            Input = X ->
                askSex(Input, Question)
        )
    ).


askSymptom(Input, Question, Predicate) :-
    write(Question),
    read(X),
    (
        (
            X = 'y' -> 
            (
                assert(Predicate),
                count_predicates(Count),
                format('TB ~w', [Count])
            )
        );
        X = 'n';
        (
            X = Y ->
            (
                format('Invalid input. Please enter \'y\' or \'n\'.'), nl,
                askSymptom(Input, Question)
            )
        )
    ).

:- dynamic(tb/1).

:- initialization(main).
main :- 
    /*
        If you have tuberculosis, add 1 to TB score.
    */
    demographics(Name, Age, Sex),
    askSymptom(Input, ('Do you have a cough? '), tb(1)).

    /*
        testing commit
    */