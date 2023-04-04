:- dynamic diarrhea/1.

diarrhea(1).

scoreUp(Predicate) :-
    Predicate(X),
    Y is X + 1,
    assert(Predicate(Y)),
    retract(Predicate(X)).

:- initialization(main).
main :- 
    diarrhea(X),
    write(X), nl,
    Y is X + 1,
    assert(diarrhea(Y)),
    retract(diarrhea(X)),
    write(Y).