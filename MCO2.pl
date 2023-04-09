/**
*   MCO2 - ChatBot
*   Section: S15
*   Author: 
*       BACOSA, Gabriel
*       CUALES, Bianca 
*       LASALA, Kyle 
*       REYNADO, Alyza
*   
*   Description:
*       This is a chatbot that will ask the user about their symptoms and will
*       provide a possible diagnosis based on the answers given.
*/

/**
*   Parameters:
*       Name - The name of the patient
*       Age - The age of the patient
*   Description:
*       Asks the patient for their name, age, and sex
*/
demographics :-
    write('Name: '),
    read(Name),
    assert(name(Name)),
    write('Age: '),
    read(Age),
    assert(age(Age)),
    askSex.

/**
*   Description:
*       Helper function for knowing and checking the input for sex
*/
askSex :-
    write('What is your sex? (m/f) '),
    read(Input),
    (
        Input = 'm' -> assert(sex(male));
        Input = 'f' -> assert(sex(female));
        (
            format('Invalid input. Please enter \'m\' or \'f\'.'), nl,
            askSex
        )
    ).

/**
*   Description:
*       Introduces the chatbot and provides a brief description of its purpose
*/
introduction :-   
    nl,
    write('========================================================================'), nl,
    write('                                 MEDIBOT'), nl,
    write('========================================================================'), nl,
    write('Hello! I am MediBot! Your personal health care companion!'), nl,nl,

    write('I\'m here to help you find out what might be causing your symptoms'), nl,
    write('and provide you with some guidance on what steps to take next.'), nl,
    write('Simply tell me about your symptoms and answer any questions I may'), nl,
    write('have, and I\'ll use my knowledge to provide you with a possible '), nl,
    write('diagnosis. Please keep in mind that while I can provide some insights,'), nl,
    write('I am not a substitute for professional medical advice, and it\'s '), nl,
    write('important to always consult with a qualified healthcare provider '), nl,
    write('for proper diagnosis and treatment. Let\'s get started!'), nl,
    write('========================================================================'), nl, nl,

    write('If you have read and understood, please type anything to continue'), nl,
    write('followed by a period (.) and press enter.'), nl,
    read(_), nl, nl,

    write('========================================================================'), nl,
    write('                              INSTRUCTIONS'), nl,
    write('========================================================================'), nl,
    write('If asked about the NAME, kindly put your name inside a quotation (" ").'), nl,nl,
    write('If asked about the AGE, kindly input a whole number (e.g. 20).'), nl,nl,
    write('If asked about a YES/NO question, input "y" for YES and "n" for NO.'), nl,nl,
    write('Every after the INPUT, please put a period (.) before you enter.'), nl,
    write('========================================================================'), nl, nl,
    write('If you have read and understood, please type anything to continue'), nl,
    write('followed by a period (.) and press enter.'), nl,
    read(_), nl, nl.
    
/**
*   Parameters:
*       Question - The question to be asked
*       Predicate - The predicate to be asserted
*   Description:
*       Asks the patient for their symptoms
*/
askSymptom(Question, Predicate, Answer) :-
    write(Question),
    read(Input),
    (
        % if the input is y, assert the predicate and count the number of predicates
        Input = 'y' -> assert(Predicate), Answer = 'y';

        % if the input is n, do not assert the predicate
        Input = 'n' -> Answer = 'n';

        % if the input is not y or n, ask again
        (
            format('Invalid input. Please enter \'y\' or \'n\'.'), nl,
            askSymptom(Question, Predicate, Answer)
        )
    ).

/**
*   Parameters:
*       L - The list of values
*       X - The starting index
*       MaxIndex - The index of the maximum value
*   Description:
*       Helper function for finding the index of the maximum value
*       This function is the base case
*/
index_of_Max([L|List], X, MaxIndex) :- 
    length([L|List], Length), 
    X >= Length,
    retractall(max(_)),
    asserta(max(MaxIndex)).

/**
*   Parameters:
*       L - The list of values
*       X - The starting index
*       MaxIndex - The index of the maximum value
*   Description:
*       This function finds the index of the maximum value
*       This function is the recursive case
*/
index_of_Max([L|List], X, MaxIndex) :- 
    nth0(X, [L|List], Value),
    (
        X > 0 ->
            (
                (
                    nth0(MaxIndex, [L|List], Max),
                    Value > Max -> 
                        (NewMaxIndex is X)
                );
                (
                    nth0(MaxIndex, [L|List], Max),
                    Value =< Max -> 
                        (NewMaxIndex is MaxIndex)
                )
            );
        X = 0 -> NewMaxIndex is X
    ),
    Y is X + 1,
    index_of_Max([L|List], Y, NewMaxIndex).

/**
*   Parameters:
*       [Head|Tail] - The list of values
*       Index - The index of the value to be replaced
*       NewElement - The new value to be inserted
*       [Head|Rest] - The new list with the replaced value
*   Description:
*       This function replaces an element in a list at a given index
*       This function is the recursive case
*/
replace_list_element([Head|Tail], Index, NewElement, [Head|Rest]) :-
    Index > 0, % continue recursively until the index is 0
    NewIndex is Index - 1,
    replace_list_element(Tail, NewIndex, NewElement, Rest).

/**
*   Parameters:
*       [_|Tail] - The list of values
*       0 - The index of the value to be replaced
*       NewElement - The new value to be inserted
*       [NewElement|Tail] - The new list with the replaced value
*   Description:
*       This function replaces an element in a list at a given index
*       This function is the base case
*/
replace_list_element([_|Tail], 0, NewElement, [NewElement|Tail]) :- !. 

/**
*   Parameters:
*       X - The element to be deleted
*       [X|T] - The list to be processed
*       NewT - The new list with the deleted element
*   Description:
*       This function deletes all occurences of an element in a list
*       This function is the base case
*/
delete_all(_, [], []).

/**
*   Parameters:
*       X - The element to be deleted
*       [X|T] - The list to be processed
*       NewT - The new list with the deleted element
*   Description:
*       This function deletes all occurences of an element in a list
*       If X is the head of the list, skip and process the rest
*/
delete_all(X, [X|T], NewT) :- 
    !,
    delete_all(X, T, NewT).

/**
*   Parameters:
*       X - The element to be deleted
*       [H|T] - The list to be processed
*       [H|NewT] - The new list with the deleted element
*   Description:
*       This function deletes all occurences of an element in a list
*       If X is not the head of the list, process the rest
*/
delete_all(X, [H|T], [H|NewT]) :- 
    delete_all(X, T, NewT).

/**
*   Parameters:
*       Highest - disease with the highest value
*   Description:
*       This function finds the disease with the highest value
*/
getScore(Highest) :- 
    diarrhea(Diarrhea),
    bronchitis(Bronchitis),
    influenza(Influenza),
    tuberculosis(Tuberculosis),
    chicken_pox(ChickenPox),
    measles(Measles),
    malaria(Malaria),
    schistosomiasis(Schistosomiasis),
    dengue(Dengue),
    tetanus(Tetanus),
    [_|Diseases] = [_, Diarrhea, Bronchitis, Influenza, 
                    Tuberculosis, ChickenPox, Measles, 
                    Malaria, Schistosomiasis, Dengue, Tetanus],
    [_|DisWords] = [_, 'Diarrhea', 'Bronchitis', 'Influenza', 
                    'Tuberculosis', 'Chicken Pox', 'Measles', 
                    'Malaria', 'Schistosomiasis', 'Dengue','Tetanus'],
    (
        (
            checked('Diarrhea') -> 
                (
                    replace_list_element(Diseases, 0, -1, NewDiseases),
                    replace_list_element(DisWords, 0, -1, NewDisWords)
                );
            (
                NewDiseases = Diseases,
                NewDisWords = DisWords
            )
        ),
        (
            checked('Bronchitis') -> 
                (
                    replace_list_element(NewDiseases, 1, -1, NewDiseases1),
                    replace_list_element(NewDisWords, 1, -1, NewDisWords1)
                );
            (
                NewDiseases1 = NewDiseases,
                NewDisWords1 = NewDisWords
            )
        ),
        (
            checked('Influenza') ->
                (
                    replace_list_element(NewDiseases1, 2, -1, NewDiseases2),
                    replace_list_element(NewDisWords1, 2, -1, NewDisWords2)
                );
            (
                NewDiseases2 = NewDiseases1,
                NewDisWords2 = NewDisWords1
            )
        ),
        (
            checked('Tuberculosis') ->
                (
                    replace_list_element(NewDiseases2, 3, -1, NewDiseases3),
                    replace_list_element(NewDisWords2, 3, -1, NewDisWords3)
                );
            (
                NewDiseases3 = NewDiseases2,
                NewDisWords3 = NewDisWords2
            )
        ),
        (
            checked('Chicken Pox') ->
                (
                    replace_list_element(NewDiseases3, 4, -1, NewDiseases4),
                    replace_list_element(NewDisWords3, 4, -1, NewDisWords4)
                );
            (
                NewDiseases4 = NewDiseases3,
                NewDisWords4 = NewDisWords3
            )
        ),
        (
            checked('Measles') ->
                (
                    replace_list_element(NewDiseases4, 5, -1, NewDiseases5),
                    replace_list_element(NewDisWords4, 5, -1, NewDisWords5)
                );
            (
                NewDiseases5 = NewDiseases4,
                NewDisWords5 = NewDisWords4
            )
        ),
        (
            checked('Malaria') ->
                (
                    replace_list_element(NewDiseases5, 6, -1, NewDiseases6),
                    replace_list_element(NewDisWords5, 6, -1, NewDisWords6)
                );
            (
                NewDiseases6 = NewDiseases5,
                NewDisWords6 = NewDisWords5
            )
        ),
        (
            checked('Schistosomiasis') ->
                (
                    replace_list_element(NewDiseases6, 7, -1, NewDiseases7),
                    replace_list_element(NewDisWords6, 7, -1, NewDisWords7)
                );
            (
                NewDiseases7 = NewDiseases6,
                NewDisWords7 = NewDisWords6
            )
        ),
        (
            checked('Dengue') ->
                (
                    replace_list_element(NewDiseases7, 8, -1, NewDiseases8),
                    replace_list_element(NewDisWords7, 8, -1, NewDisWords8)
                );
            (
                NewDiseases8 = NewDiseases7,
                NewDisWords8 = NewDisWords7
            )
        ),
        (
            checked('Tetanus') ->
                (
                    replace_list_element(NewDiseases8, 9, -1, NewDiseases9),
                    replace_list_element(NewDisWords8, 9, -1, NewDisWords9)
                );
            (
                NewDiseases9 = NewDiseases8,
                NewDisWords9 = NewDisWords8
            )
        )
    ),
    % write(NewDisWords9), nl,
    % write(NewDiseases9), nl,
    delete_all(-1, NewDiseases9, ResultDiseases),
    delete_all(-1, NewDisWords9, ResultDisWords),
    length(ResultDiseases, ListLength), 
    % write(ResultDiseases), nl,
    (
        ListLength > 0,
            (
                index_of_Max(ResultDiseases, 0, _),
                max(Index),
                nth0(Index, ResultDisWords, Highest)
            );
        Highest = 'No Disease'
    ).

% Disease Scores
% 1 - Diarrhea
:- dynamic diarrhea/1.
diarrhea(0).
% 2 - Bronchitis
:- dynamic bronchitis/1.
bronchitis(0).
% 3 - Influenza
:- dynamic influenza/1.
influenza(0).
% 4 - Tuberculosis
:- dynamic tuberculosis/1.
tuberculosis(0).
% 5 - Chicken Pox
:- dynamic chicken_pox/1.
chicken_pox(0).
% 6 - Measles
:- dynamic measles/1.
measles(0).
% 7 - Malaria
:- dynamic malaria/1.
malaria(0).
% 8 - Schistosomiasis
:- dynamic schistosomiasis/1.
schistosomiasis(0).
% 9 - Dengue
:- dynamic dengue/1.
dengue(0).
% 10 - Tetanus
:- dynamic tetanus/1.
tetanus(0).

/**
*   The following functions will add score
*   to respective disease scores.
*/
add_diarrhea :- 
    diarrhea(X),
    Y is X + 1,
    retract(diarrhea(X)),
    assert(diarrhea(Y)).

add_bronchitis :-
    bronchitis(X),
    Y is X + 1,
    retract(bronchitis(X)),
    assert(bronchitis(Y)).

add_influenza :-
    influenza(X),
    Y is X + 1,
    retract(influenza(X)),
    assert(influenza(Y)).

add_tuberculosis :-
    tuberculosis(X),
    Y is X + 1,
    retract(tuberculosis(X)),
    assert(tuberculosis(Y)).

add_chicken_pox :-
    chicken_pox(X),
    Y is X + 1,
    retract(chicken_pox(X)),
    assert(chicken_pox(Y)).

add_measles :-
    measles(X),
    Y is X + 1,
    retract(measles(X)),
    assert(measles(Y)).

add_malaria :-
    malaria(X),
    Y is X + 1,
    retract(malaria(X)),
    assert(malaria(Y)).

add_schistosomiasis :-
    schistosomiasis(X),
    Y is X + 1,
    retract(schistosomiasis(X)),
    assert(schistosomiasis(Y)).

add_dengue :-
    dengue(X),
    Y is X + 1,
    retract(dengue(X)),
    assert(dengue(Y)).

add_tetanus :-
    tetanus(X),
    Y is X + 1,
    retract(tetanus(X)),
    assert(tetanus(Y)).

% Initialize Diseases
diarrhea_confirm :- 
    watery_stool(1),
    frequent_poop(1),
    (
        vomiting(1);
        fever(1);
        body_ache(1);
        head_ache(1);
        stomach_ache(1)
    ).

bronchitis_confirm :-
    cough(1),
    soreness_chest(1),
    (
        sore_throat(1);
        body_ache(1);
        head_ache(1)
    ).

influenza_confirm :-
    fever(1),
    head_ache(1),
    fatigue(1),
    runny_nose(1),
    (
        cough(1);
        sore_throat(1);
        vomiting(1);
        watery_stool(1)
    ).

tuberculosis_confirm :-
    cough(1),
    coughing_blood(1),
    night_sweats(1),
    (
        soreness_chest(1);
        weakness(1);
        fever(1);
        weight_loss(1);
        fatigue(1);
        body_ache(1);
        head_ache(1)
    ).

chicken_pox_confirm :-
    rashes(1),
    (
        head_ache(1);
        fever(1);
        fatigue(1);
        loss_of_appetite(1)
    ).

measles_confirm :-
    rashes(1),
    koplik_spots(1),
    (    
        fever(1);
        cough(1);
        runny_nose(1);
        red_watery_eyes(1)
    ).
        
malaria_confirm :-
    vomiting(1),
    shivering(1),
    sweating(1),
    mild_jaundice(1),
    increased_respiratory_rate(1),
    (
        fever(1);
        head_ache(1);
        body_ache(1);
        weakness(1)
    ).
    
schistosomiasis_confirm :-
    rashes(1),
    chills(1),
    (
        fever(1);
        body_ache(1);
        cough(1)
    ).

dengue_confirm :-
    increased_respiratory_rate(1),
    bleeding_from_gums_or_nose(1),
    blood_in_urine(1),
    bruises(1),
    eye_pain(1),
    (
        fatigue(1);
        rashes(1);
        vomiting(1);
        fever(1)
    ).

tetanus_confirm :-
    trouble_swallowing(1),
    muscle_spasm(1),
    seizures(1),
    (
        head_ache(1);
        fever(1);
        sweating(1);
        changes_in_blood_pressure(1)
    ).

% Symptoms of Diseases
diarrhea_specifics :-
    (
        % Similarity count: 1
        askSymptom('Do you have stomach ache? (y/n) ', stomach_ache(1), Answer8),
        (
            Answer8 = 'y' -> true;
            Answer8 = 'n' -> assert(stomach_ache(0))
        )
    ),
    (
        % Similarity count: 2
        (current_predicate(watery_stool/1), watery_stool(_)) -> true;
        askSymptom('Do you have watery stool? (y/n) ', watery_stool(1), Answer9),
        (
            Answer9 = 'y' -> 
                (
                    add_influenza
                );
            Answer9 = 'n' -> assert(watery_stool(0))
        )
    ),
    (
        % Similarity count: 1
        askSymptom('Do you experience bowel movements more frequently than usual? (y/n) ', frequent_poop(1), Answer10),
        (
            Answer10 = 'y' -> true;
            Answer10 = 'n' -> assert(frequent_poop(0))
        )
    ).

bronchitis_specifics :- 
    (
        % Similarity count: 2
        cough(1),
        (current_predicate(soreness_chest/1), soreness_chest(_)) -> true;
        askSymptom('Do you have any soreness or discomfort in your chest? (y/n) ', soreness_chest(1), Answer10),
        (
            Answer10 = 'y' -> 
                (
                    add_tuberculosis
                );
            Answer10 = 'n' -> assert(soreness_chest(0))
        )
    ),
    (
        % Similarity count: 2
        (current_predicate(sore_throat/1), sore_throat(_)) -> true;
        askSymptom('Do you have a sore throat? (y/n) ', sore_throat(1), Answer11),
        (
            Answer11 = 'y' ->
                (
                    add_influenza
                );
            Answer11 = 'n' -> assert(sore_throat(0))
        )
    ).

influenza_specifics :- 
    (
        % Similarity count: 2
        (current_predicate(sore_throat/1), sore_throat(_)) -> true;
        askSymptom('Do you have a sore throat? (y/n) ', sore_throat(1), Answer12),
        (
            Answer12 = 'y' ->
                (
                    add_bronchitis
                );
            Answer12 = 'n' -> assert(sore_throat(0))
        )
    ),
    (
        % Similarity count: 2
        (current_predicate(runny_nose/1), runny_nose(_)) -> true;
        askSymptom('Do you have a runny nose? (y/n) ', runny_nose(1), Answer13),
        (
            Answer13 = 'y' ->
                (
                    add_measles
                );
            Answer13 = 'n' -> assert(runny_nose(0))
        )
    ).

tuberculosis_specifics :- 
    (
        % Similarity count: 2
        (current_predicate(weakness/1), weakness(_)) -> true;
        askSymptom('Have you been experiencing weakness? (y/n) ', weakness(1), Answer14),
        (
            Answer14 = 'y' -> 
                (
                    add_malaria
                );
            Answer14 = 'n' -> assert(weakness(0))
        )
    ),
    (
        % Similarity count: 2
        (current_predicate(soreness_chest/1), soreness_chest(_)) -> true;
        askSymptom('Do you have any soreness or discomfort in your chest? (y/n) ', soreness_chest(1), Answer18),
        (
            Answer18 = 'y' -> 
                (
                    add_bronchitis
                );
            Answer18 = 'n' -> assert(soreness_chest(0))
        )
    ),
    (
        % Similarity count: 1
        askSymptom('Have you experienced any weight loss?', weight_loss(1), Answer15),
        (
            Answer15 = 'y' -> true;
            Answer15 = 'n' -> assert(weight_loss(0))
        )
    ),
    (
        % Similarity count: 1
        cough(1) ->
        (
            askSymptom('Have you coughed up any blood?', coughing_blood(1), Answer17),
            (
                Answer17 = 'y' -> true;
                Answer17 = 'n' -> assert(coughing_blood(0))
            )
        )
    ),
    (
        % Similarity count: 1
        (cough(1), coughing_blood(1)) ->
            (
                askSymptom('Have you been experiencing night sweats?', night_sweats(1), Answer16),
                (
                    Answer16 = 'y' -> true;
                    Answer16 = 'n' -> assert(night_sweats(0))
                )
            )
    ).

chicken_pox_specifics :-
    (
        % Similarity count: 1
        rashes(1) ->
        (
            askSymptom('Have you been experiencing loss of appetite? (y/n) ', loss_of_appetite(1), Answer19),
            (
                Answer19 = 'y' -> true;
                Answer19 = 'n' -> assert(loss_of_appetite(0))
            )
        );
        true
    ).

measles_specifics :-
    (
        % Similarity count: 2
        (current_predicate(runny_nose/1), runny_nose(_)) -> true;
        askSymptom('Do you have a runny nose? (y/n) ', runny_nose(1), Answer20),
        (
            Answer20 = 'y' -> 
                (
                    add_influenza
                );
            Answer20 = 'n' -> assert(runny_nose(0))
        )
    ),
    (
        % Similarity count: 1
        (rashes(1)) ->
            (
                askSymptom('Do you have koplik spots (white spots in mouth)? (y/n) ', koplik_spots(1), Answer21),
                (
                    Answer21 = 'y' -> true;
                    Answer21 = 'n' -> assert(koplik_spots(0))
                )
            )
    ),
    (
        % Similarity count: 1
        (rashes(1),koplik_spots(1)) ->
            (
                askSymptom('Have you been experiencing red watery eyes? (y/n) ', red_watery_eyes(1), Answer22),
                (
                    Answer22 = 'y' -> true;
                    Answer22 = 'n' -> assert(red_watery_eyes(0))
                )
            );
        true
    ).

malaria_specifics :-
    (
        % Similarity count: 2
        (current_predicate(sweating/1), sweating(_)) -> true;
        askSymptom('Have you been experiencing excessive sweating? (y/n)', sweating(1), Answer23),
        (
            Answer23 = 'y' -> 
                (
                    add_tetanus
                );
            Answer23 = 'n' -> assert(sweating(0))
        )
    ),
    (
        % Similarity count: 2
        (current_predicate(weakness/1), weakness(_)) -> true;
        askSymptom('Have you been experiencing weakness? (y/n) ', weakness(1), Answer24),
        (
            Answer24 = 'y' -> 
                (
                    add_tuberculosis
                );
            Answer24 = 'n' -> assert(weakness(0))
        )
    ),
    (
        % Similarity count: 1
        (vomiting(1))->
            (
                askSymptom('Have you been experiencing shivering? (y/n)', shivering(1), Answer25),
                    (
                        Answer25 = 'y' -> true;
                        Answer25 = 'n' -> assert(shivering(0))
                    )
            )
    ),
    (
        % Similarity count: 1
        (vomiting(1), shivering(1)) -> 
            (
                askSymptom('Have you been experiencing mild jaundice? (y/n)', mild_jaundice(1), Answer26),
                (
                    Answer26 = 'y' -> true;
                    Answer26 = 'n' -> assert(mild_jaundice(0))
                )
            )
    ),
    (
        % Similarity count: 1
        (vomiting(1), shivering(1),mild_jaundice(1))-> 
            (
                askSymptom('Have you been experiencing increased respiratory rate? (y/n)', increased_respiratory_rate(1), Answer27),
                (
                    Answer27 = 'y' -> true;
                    Answer27 = 'n' -> assert(increased_respiratory_rate(0))
                )
            )
    ).

schistosomiasis_specifics :-
    (
        % Similarity count: 1
        (rashes(1))-> 
            (
                askSymptom('Have you been experiencing chills? (y/n) ', chills(1), Answer28),
                (
                    Answer28 = 'y' -> true;
                    Answer28 = 'n' -> assert(chills(0))
                )
            );
        assert(chills(0))
    ).

dengue_specifics :-
    (
        % Similarity count: 1
        askSymptom('Have you been experiencing increased respiratory rate? (y/n) ', increased_respiratory_rate(1), Answer29),
        (
            Answer29 = 'y' -> true;
            Answer29 = 'n' -> assert(increased_respiratory_rate(0))
        )
    ),
    (
        % Similarity count: 1
        increased_respiratory_rate(1) ->
        (
            askSymptom('Have you been experiencing bleeding from gums or nose? (y/n) ', bleeding_from_gums_or_nose(1), Answer30),
            (
                Answer30 = 'y' -> true;
                Answer30 = 'n' -> assert(bleeding_from_gums_or_nose(0))
            )
        )
    ),
    (
        % Similarity count: 1
        (
            increased_respiratory_rate(1), bleeding_from_gums_or_nose(1),
            (
                askSymptom('Have you been experiencing blood in urine? (y/n) ', blood_in_urine(1), Answer31),
                (
                    Answer31 = 'y' -> true;
                    Answer31 = 'n' -> assert(blood_in_urine(0))
                )
            )
        )
    ),
    (
        (
            increased_respiratory_rate(1), 
            bleeding_from_gums_or_nose(1),
            blood_in_urine(1)
        ) ->
            (
                askSymptom('Do you have bruises? (y/n) ', bruises(1), Answer32),
                (
                    Answer32 = 'y' -> true;
                    Answer32 = 'n' -> assert(bruises(0))
                )
            )
    ),
    (
        (
            increased_respiratory_rate(1), 
            bleeding_from_gums_or_nose(1),
            blood_in_urine(1),
            bruises(1)
        ) ->
            (
                askSymptom('Have you been experiencing eye pain? (y/n) ', eye_pain(1), Answer33),
                (
                    Answer33 = 'y' -> true;
                    Answer33 = 'n' -> assert(eye_pain(0))
                )
            )
    ).

tetanus_specifics :-
    (
        % Similarity count: 2
        (current_predicate(sweating/1), sweating(_)) -> true;
        askSymptom('Have you been experiencing excessive sweating? (y/n)', sweating(1), Answer34),
        (
            Answer34 = 'y' -> 
                (
                    add_malaria
                );
            Answer34 = 'n' -> assert(sweating(0))
        )
    ),
    (
        % Similarity count: 1
        askSymptom('Have you been experiencing trouble in swallowing? (y/n)', trouble_swallowing(1), Answer35),
        (
            Answer35 = 'y' -> true;
            Answer35 = 'n' -> assert(trouble_swallowing(0))
        )
    ),
    (
        % Similarity count: 1
        trouble_swallowing(1) ->
            (
                askSymptom('Have you been experiencing muscle spasm? (y/n)', muscle_spasm(1), Answer36),
                (
                    Answer36 = 'y' -> true;
                    Answer36 = 'n' -> assert(muscle_spasm(0))
                )
            )
    ),
    (
        % Similarity count: 1
        (trouble_swallowing(1), muscle_spasm(1)) ->
            (
                askSymptom('Have you been experiencing seizures? (y/n)', seizures(1), Answer37),
                (
                    Answer37 = 'y' -> true;
                    Answer37 = 'n' -> assert(seizures(0))
                )
            )
    ),
    (
        % Similarity count: 1
        (trouble_swallowing(1), muscle_spasm(1), seizures(1)) ->
            (
                askSymptom('Have you been experiencing changes in blood pressure? (y/n)', changes_in_blood_pressure(1), Answer38),
                (
                    Answer38 = 'y' -> true;
                    Answer38 = 'n' -> assert(changes_in_blood_pressure(0))
                )
            )
    ).

% Treatment for the diseases
diarrhea_treatment :-
    write('\nYou may have DIARRHEA, the following are the possible treatments'),nl,
    write('for the disease:'), nl, nl,

    write('\t- Drink more fluids'), nl, 
    write('\t- Avoid fatty, high-fiber, or highly seasoned foods'), nl,
    write('\t- Avoid caffeine and alcohol'), nl,
    write('\t- Recommended for BRAT diet'), nl,
    write('\t\t- Bananas, Rice, Apple, Toast'), nl,
    write('\t- Ask for basic medications for diarrhea'), nl,nl,
    write('Note: You may have diarrhoeal disease.'),nl,
    write('It is a compilation of diseases that inhibits diarrhea as a symptom.'),nl,
    write('It is still best to consult a doctor to have a proper diagnosis.'),nl,nl
.

bronchitis_treatment :-
    write('\nYou may have BRONCHITIS, the following are the possible treatments for'),nl,
    write('the disease:'), nl,nl,

    write('\t- Get plenty of rest'),nl,
    write('\t- Drink plenty of fluids'),nl,
    write('\t- Use a clean humidifier or cool mist vaporizer'),nl,
    write('\t- Use saline nasal spray or drops'),nl,
    write('\t- Breathe in steam from a bowl of hot water or shower'),nl,
    write('\t- Use honey to further help ease the symptoms'),nl,
    write('\t- Ask for doctor or pharmacist about over-the-counter medicines'), nl,nl,
    write('Note: Immediately consult a doctor for a proper diagnosis and treatment'),nl,
    write('because bronchitis can be a serious illness. If left untreated, it can'),nl,
    write('further develop and maybe related to other respiratory diseases.'),nl,nl
.

influenza_treatment :-
    write('\nYou may have INFLUENZA (Flu), the following are the possible treatments'), nl,
    write('for the disease:'), nl, nl,

    write('\t- Ask for flu antiviral drug prescription'),nl,
    write('\t- Get a flu vaccine as advised by doctor'),nl,
    write('\t- Drink plenty of fluids'),nl,
    write('\t- Get plenty of rest'),nl,nl,
    write('Note: It is still best to consult a doctor for the best diagnosis'),nl,
    write('and treatment for the disease.'),nl,nl
.

tuberculosis_treatment :-
    write('\nYou may have TUBERCULOSIS (TB), the following are the possible'),nl,
    write('treatments for the disease:'), nl,nl,

    write('\t- The doctor may prescribe anti-TB drugs'),nl,
    write('\t- Ask for TB-DOT in local government units'),nl,
    write('\t\t- Tuberculosis Directly Observed therapy (DOT)'),nl,nl,

    write('Note: Please consult a doctor for proper diagnosis and instructions.'),nl,
    write('TB can be infectious and can be transmitted to others.'),nl,
    write('It is advised to practice proper hygiene and avoid contact with'),nl,
    write('other people.'),nl,nl
.

chicken_pox_treatment :-
    write('\nYou may have CHICKEN POX, the following are the possible treatments'),nl,
    write('for the disease:'), nl,nl,

    write('\t- Non-aspirin medications for fever relieve'), nl,
    write('\t- Calamine lotion, cool bath with baking soda,'),nl,
    write('\t  uncooked oatmeal to help relieve itching'),nl,
    write('\t- Get a chicken pox vaccine as advised by doctor'),nl,
    write('\t- Avoid scratching the blisters'),nl,
    write('\t- Avoid spicy, acidic, or salty foods'), nl,

    write('Note: It is advised to consult a doctor for proper diagnosis and'),nl,
    write('treatment. There are other common diseases that can be very similar'),
    write('with the symptoms of chicken pox.'),nl,nl
.

measles_treatment :-
    write('\nYou may have MEASLES, the following are the possible treatments for'), nl,
    write('the disease:'), nl,nl,

    write('\t- Take fever reducers if you have fever'), nl,
    write('\t- Ask for doctors if antibiotics are needed to ease symptoms'),nl,
    write('\t- Take Vitamin A supplements'), nl,nl,

    write('Note: There is no specific treatment for this disease. It is best to'),nl,
    write('consult a doctor for proper diagnosis and treatment.'),nl,nl
.

malaria_treatment :-
    write('\nYou may have MALARIA, the following are the possible treatments for'),nl,
    write('the disease:'), nl, nl,
    write('\t- Antimalarial medications as instructed by doctors'), nl, nl,
    write('Note: The treatment for malaria is only done by professionals. Please'), nl,
    write('consult a doctor for proper diagnosis and treatment.'), nl, nl
.

schistosomiasis_treatment :- 
    write('\nYou may have SCHISTOSOMIASIS, the following are the possible'),nl,
    write('treatments for the disease:'), nl,nl,
    write('\t- Medications such as Praziquantel as prescribed by doctors'), nl,
    write('\tNote: Symptoms in schistosomiasis are present in other diseases.'), nl,
    write('It is best to have further testing with doctor consultation to confirm'),nl,
    write('the schistosomiasis.'), nl, nl
.

dengue_treatment :-
    write('\nYou may have DENGUE, requires immediate medical care!'), nl,
    write('Please go to the nearest hospital for proper diagnosis and treatment.'), nl,
    write('Dengue is a serious disease that can be fatal.'), nl, nl
.

tetanus_treatment :-
    write('\nYou may have TETANUS, the following are the possible treatments'),nl,
    write('for the disease:'), nl, nl,
    write('\t- Aggressive wound care'), nl,
    write('\t- Ask the doctors if antibiotics are needed'), nl,
    write('\t- Tetanus vaccination as advised by doctors'), nl, nl,
    write('Note: If you have a wound exposed to contamination such as soil,'),nl,
    write('feces, dirt, water, or other contaminated materials, there is a high'),nl,
    write('possibility that you have this disease. Please consult your doctor'),nl,
    write('immediately.'), nl, nl
.

% Unclear Diagnosis
unclear_treatment :-
    retract(checked('Diarrhea')),
    retract(checked('Bronchitis')),
    retract(checked('Influenza')),
    retract(checked('Tuberculosis')),
    retract(checked('Chicken Pox')),
    retract(checked('Measles')),
    retract(checked('Malaria')),
    retract(checked('Schistosomiasis')),
    retract(checked('Dengue')),
    retract(checked('Tetanus')),
    getScore(Highest),
    write('\nDiagnosis is UNCLEAR, but based on your inputs the most'),nl,
    format('probable disease is ~w.', [Highest]), nl,nl,
    write('Please consult a doctor for in the nearest in the nearest'),nl,
    write('clinic or hospital for proper diagnosis and treatment.'),nl,nl
.

diagnostic(Diagnosis) :-
    Diagnosis = 'Diarrhea' -> diarrhea_treatment;
    Diagnosis = 'Bronchitis' -> bronchitis_treatment;
    Diagnosis = 'Influenza' -> influenza_treatment;
    Diagnosis = 'Tuberculosis' -> tuberculosis_treatment;
    Diagnosis = 'Chicken Pox' -> chicken_pox_treatment;
    Diagnosis = 'Measles' -> measles_treatment;
    Diagnosis = 'Malaria' -> malaria_treatment;
    Diagnosis = 'Schistosomiasis' -> schistosomiasis_treatment;
    Diagnosis = 'Dengue' -> dengue_treatment;
    Diagnosis = 'Tetanus' -> tetanus_treatment;
    Diagnosis = 'Unclear' -> unclear_treatment.

% Ask for the symptoms of the patient.
disease_specifics(Diagnosis) :-
    getScore(Highest),
    (
        Highest = 'Diarrhea' -> 
            (
                % Specific for diarrhea
                diarrhea_specifics,
                (
                    diarrhea_confirm -> Diagnosis = 'Diarrhea'
                ); 
                (
                    % write('you DO NOT have diarrhea'), nl,
                    assert(checked('Diarrhea')),
                    disease_specifics(Diagnosis)
                )
            );
        Highest = 'Bronchitis' ->
            (
                % Specific for bronchitis
                bronchitis_specifics,
                (
                    bronchitis_confirm -> Diagnosis = 'Bronchitis'
                );
                (
                    % write('you DO NOT have bronchitis'), nl,
                    assert(checked('Bronchitis')),
                    disease_specifics(Diagnosis)
                )
            );
        Highest = 'Influenza' ->
            (
                % Specific for influenza
                influenza_specifics,
                (
                    influenza_confirm -> Diagnosis = 'Influenza'
                );
                (
                    % write('you DO NOT have influenza'), nl,
                    assert(checked('Influenza')),
                    disease_specifics(Diagnosis)
                )
            );
        Highest = 'Tuberculosis' ->
            (
                % Specific for tuberculosis
                tuberculosis_specifics,
                (
                    tuberculosis_confirm -> Diagnosis = 'Tuberculosis'
                );
                (
                    % write('you DO NOT have tuberculosis'), nl,
                    assert(checked('Tuberculosis')),
                    disease_specifics(Diagnosis)
                )
            );
        Highest = 'Chicken Pox' ->
            (
                % Specific for Chicken Pox
                chicken_pox_specifics,
                (
                    chicken_pox_confirm -> Diagnosis = 'Chicken Pox'
                );
                (
                    % write('you DO NOT have chicken pox'), nl,
                    assert(checked('Chicken Pox')),
                    disease_specifics(Diagnosis)
                )
            );
        Highest = 'Measles' ->
            (
                % Specific for measles
                measles_specifics,
                (
                    measles_confirm -> Diagnosis = 'Measles'
                );
                (
                    % write('you DO NOT have measles'), nl,s
                    assert(checked('Measles')),
                    disease_specifics(Diagnosis)
                )
            );
        Highest = 'Malaria' ->
            (
                % Specific for Malaria
                malaria_specifics,
                (
                    malaria_confirm -> Diagnosis = 'Malaria'
                );
                (
                    % write('you DO NOT have malaria'), nl,
                    assert(checked('Malaria')),
                    disease_specifics(Diagnosis)
                )
            );
        Highest = 'Schistosomiasis' ->
            (
                % Specific for schistosomiasis
                schistosomiasis_specifics,
                (
                    schistosomiasis_confirm -> Diagnosis = 'Schistosomiasis'
                );
                (
                    % write('you DO NOT have schistosomiasis'), nl,
                    assert(checked('Schistosomiasis')),
                    disease_specifics(Diagnosis)
                )
            );
        Highest = 'Dengue' ->
            (
                % Specific for Dengue
                dengue_specifics,
                (
                    dengue_confirm -> Diagnosis = 'Dengue'
                );
                (
                    % write('you DO NOT have dengue'), nl,
                    assert(checked('Dengue')),
                    disease_specifics(Diagnosis)
                )
            );
        Highest = 'Tetanus' ->
            (
                % Specific for tetanus
                tetanus_specifics,
                (
                    tetanus_confirm -> Diagnosis = 'Tetanus'
                );
                (
                    write('you DO NOT have tetanus'), nl,
                    assert(checked('Tetanus')),
                    disease_specifics(Diagnosis)
                )
            );
        Highest = _ -> Diagnosis = 'Unclear'
    ).

:- dynamic checked/1.

% Main Function
:- initialization(main).
main :- 
    % Ask for the patient's name, age, and sex
    introduction,
    write('========================================================================'), nl,
    write('                          PATIENT INFORMATION'), nl,
    write('========================================================================'), nl,
    % Ask for the demographics of the patient.
    demographics,
    write('========================================================================'), nl,nl,nl,

    write('========================================================================'), nl,
    write('                               SYMPTOMS'), nl,
    write('========================================================================'), nl,

    % Ask for the symptoms with duplication.
    askSymptom('Do you have a fever? (y/n) ', fever(1), Answer1),                    % Similarity Count: 9
    (
        Answer1 = 'y' -> 
            (
                add_diarrhea, 
                add_influenza,
                add_tuberculosis,
                add_chicken_pox,
                add_measles,
                add_malaria,
                add_schistosomiasis,
                add_dengue,
                add_tetanus
            );
        Answer1 = 'n' -> assert(fever(0))
    ),
    askSymptom('Do you have a headache? (y/n) ', head_ache(1), Answer2),             % Similarity Count: 6
    (
        Answer2 = 'y' -> 
            (
                add_diarrhea,
                add_bronchitis,
                add_influenza,
                add_chicken_pox,
                add_malaria,
                add_tetanus
            );
        Answer2 = 'n' -> assert(head_ache(0))
    ),
    askSymptom('Are you experiencing coughing? (y/n) ', cough(1), Answer3),          % Similarity Count: 5
    (
        Answer3 = 'y' ->
            (
                add_bronchitis,
                add_influenza,
                add_tuberculosis,
                add_measles,
                add_schistosomiasis
            );
        Answer3 = 'n' -> assert(cough(0))
    ),
    askSymptom('Have you vomited recently? (y/n) ', vomiting(1), Answer4),           % Similarity Count: 4
    (
        Answer4 = 'y' ->
            (
                add_diarrhea,
                add_influenza,
                add_malaria,
                add_dengue
            );
        Answer4 = 'n' -> assert(vomiting(0))
    ),
    askSymptom('Are you experiencing body aches? (y/n) ', body_ache(1), Answer5),    % Similarity Count: 4
    (
        Answer5 = 'y' ->
            (
                add_diarrhea,
                add_bronchitis,
                add_malaria,
                add_schistosomiasis
            );
        Answer5 = 'n' -> assert(body_ache(0))
    ),
    askSymptom('Are you experiencing fatigue? (y/n) ', fatigue(1), Answer6),         % Similarity Count: 4
    (
        Answer6 = 'y' ->
            (
                add_bronchitis,
                add_influenza,
                add_chicken_pox,
                add_dengue
            );
        Answer6 = 'n' -> assert(fatigue(0))
    ),
    askSymptom('Are you experiencing rashes? (y/n) ', rashes(1), Answer7),           % Similarity Count: 4
    (
        Answer7 = 'y' ->
            (
                add_chicken_pox,
                add_measles,
                add_schistosomiasis,
                add_dengue
            );
        Answer7 = 'n' -> assert(rashes(0))
    ),

    disease_specifics(Diagnosis),
    write('========================================================================'), nl, nl, nl,

    
    write('========================================================================'), nl,
    write('                               DIAGNOSIS'), nl,
    write('========================================================================'), nl,
    diagnostic(Diagnosis),
    write('My diagnosis might be wrong, as I am limited to the diseases that I can'),nl,
    write('diagnose. I am only designed to assist you in knowing your possible'),nl,
    write('disease but will not become a substitute to any medical professionals.'), nl,nl,
    write('========================================================================'),nl,nl,
    write('Thank you for using MediBot!'),nl,nl.