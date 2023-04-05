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
    [_|Diseases] = [_, Diarrhea, Bronchitis, Influenza, 
                    Tuberculosis, ChickenPox, Measles, 
                    Malaria, Schistosomiasis, Dengue],
    [_|DisWords] = [_, 'Diarrhea', 'Bronchitis', 'Influenza', 
                    'Tuberculosis', 'Chicken Pox', 'Measles', 
                    'Malaria', 'Schistosomiasis', 'Dengue'],
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
    write(NewDisWords9), nl,
    write(NewDiseases9), nl,
    delete_all(-1, NewDiseases9, ResultDiseases),
    delete_all(-1, NewDisWords9, ResultDisWords),
    index_of_Max(ResultDiseases, 0, _),
    max(Index),
    nth0(Index, ResultDisWords, Highest).

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
    soreness_chest(1),
    cough(1),
    (
        sore_throat(1);
        body_ache(1);
        head_ache(1)
    ).

influenza_confirm :-
    fever(1),
    head_ache(1),
    fatigue(1),
    (
        cough(1);
        runny_nose(1);
        sore_throat(1);
        vomiting(1);
        watery_stool(1)
    ).

tuberculosis_confirm :-
    cough(1),
    coughing_blood(1),
    night_sweats(1),
    (
        fever(1);
        weight_loss(1);
        fatigue(1);
        body_ache(1);
        head_ache(1)
    ).

% Symptoms of Diseases
diarrhea_specifics :-
    (
        (current_predicate(stomach_ache/1), stomach_ache(_)) -> true;
        askSymptom('Do you have stomach ache? (y/n) ', stomach_ache(1), _)
    ),
    (
        (current_predicate(watery_stool/1), watery_stool(_)) -> true;
        askSymptom('Do you have watery stool? (y/n) ', watery_stool(1), Answer8),
        (
            Answer8 = 'y' -> 
                (
                    add_influenza
                );
            Answer8 = 'n' -> assert(watery_stool(0))
        )
    ),
    (
        (current_predicate(frequent_poop/1), frequent_poop(_)) -> true;
        askSymptom('Do you experience bowel movements more frequently than usual? (y/n) ', frequent_poop(1), Answer9),
        (
            Answer9 = 'y' -> 
                (
                    add_tuberculosis
                );
            Answer9 = 'n' -> assert(frequent_poop(0))
        )
    ).

bronchitis_specifics :- 
    (
        (current_predicate(soreness_chest/1), soreness_chest(_)) -> true;
        askSymptom('Do you have any soreness or discomfort in your chest? (y/n) ', soreness_chest(1), Answer8),
        (
            Answer8 = 'y' -> true;
            Answer8 = 'n' -> assert(soreness_chest(0))
        )
    ),
    (
        (current_predicate(sore_throat/1), sore_throat(_)) -> true;
        askSymptom('Do you have a sore throat? (y/n) ', sore_throat(1), Answer9),
        (
            Answer9 = 'y' ->
                (
                    add_influenza
                );
            Answer9 = 'n' -> assert(sore_throat(0))
        )
    ).

influenza_specifics :- 
    (
        (current_predicate(sore_throat/1), sore_throat(_)) -> true;
        askSymptom('Do you have a sore throat? (y/n) ', sore_throat(1), Answer8),
        (
            Answer8 = 'y' ->
                (
                    add_bronchitis
                );
            Answer8 = 'n' -> assert(sore_throat(0))
        )
    ),
    (
        (current_predicate(runny_nose/1), runny_nose(_)) -> true;
        askSymptom('Do you have a runny nose? (y/n) ', runny_nose(1), Answer9),
        (
            Answer9 = 'y' ->
                (
                    add_measles
                );
            Answer9 = 'n' -> assert(runny_nose(0))
        )
    ).

tuberculosis_specifics :- 
    (
        askSymptom('Have you been experiencing weakness? (y/n) ', weakness(1), Answer10),
        (
            Answer10 = 'y' -> true;
            Answer10 = 'n' -> assert(weakness(0))
        )
    ),
    (
        askSymptom('Have you experienced any unexplained weight loss?', weight_loss(1), Answer11),
        (
            Answer11 = 'y' -> true;
            Answer11 = 'n' -> assert(weight_loss(0))
        )
    ),
    (
        askSymptom('Have you been experiencing night sweats?', night_sweats(1), Answer12),
        (
            Answer12 = 'y' -> true;
            Answer12 = 'n' -> assert(night_sweats(0))
        )
    ),
    (
        askSymptom('Have you coughed up any blood?', coughing_blood(1), Answer13),
        (
            Answer13 = 'y' -> true;
            Answer13 = 'n' -> assert(coughing_blood(0))
        )
    ),
    (
        (current_predicate(soreness_chest/1), soreness_chest(_)) -> true;
        askSymptom('Have you been experienced chest pain', soreness_chest(1), Answer14),
        (
            Answer14 = 'y' -> true;
            Answer14 = 'n' -> assert(soreness_chest(0))
        )
    ).

% Ask for the symptoms of the patient.
symptom_specifics :-
    getScore(Highest),
    write(Highest), nl,
    (
        Highest = 'Diarrhea' -> 
            (
                % Specific for diarrhea
                diarrhea_specifics,
                (
                    diarrhea_confirm -> 
                        write('you have diarrhea'), nl
                ); 
                (
                    write('you DO NOT have diarrhea'), nl,
                    assert(checked('Diarrhea')),
                    symptom_specifics
                )
            );
        Highest = 'Bronchitis' ->
            (
                % Specific for bronchitis
                bronchitis_specifics,
                (
                    bronchitis_confirm -> 
                        write('you have bronchitis'), nl
                );
                (
                    write('you DO NOT have bronchitis'), nl,
                    assert(checked('Bronchitis')),
                    symptom_specifics
                )
            );
        Highest = 'Influenza' ->
            (
                % Specific for influenza
                influenza_specifics,
                (
                    influenza_confirm -> write('you have influenza')
                );
                (
                    write('you DO NOT have influenza'), nl,
                    assert(checked('Influenza')),
                    symptom_specifics
                )
            );
        Highest = 'Tuberculosis' ->
            (
                % Specific for tuberculosis
                tuberculosis_specifics,
                (
                    tuberculosis_confirm -> write('you have tuberculosis')
                );
                (
                    write('you DO NOT have tuberculosis'), nl,
                    assert(checked('Tuberculosis')),
                    symptom_specifics
                )
            );
        Highest = _ ->
            (
                write('No disease found'), nl
            )
        % Highest = 'Tuberculosis' ->
        %     (
        %         % Specific for tuberculosis
        %         tuberculosis_specifics,
        %         (tuberculosis_confirm -> write('you have tuberculosis'));
        %         write('you DO NOT have tuberculosis')
        %     );
        % Highest = 'Chicken Pox' ->
        %     (
        %         % Specific for chicken pox
        %         chicken_pox_specifics,
        %         (chicken_pox_confirm -> write('you have chicken pox'));
        %         write('you DO NOT have chicken pox')
        %     );
        % Highest = 'Measles' ->
        %     (
        %         % Specific for measles
        %         measles_specifics,
        %         (measles_confirm -> write('you have measles'));
        %         write('you DO NOT have measles')
        %     );
        % Highest = 'Malaria' ->
        %     (
        %         % Specific for malaria
        %         malaria_specifics,
        %         (malaria_confirm -> write('you have malaria'));
        %         write('you DO NOT have malaria')
        %     );
        % Highest = 'Schistosomiasis' ->
        %     (
        %         % Specific for schistosomiasis
        %         schistosomiasis_specifics,
        %         (schistosomiasis_confirm -> write('you have schistosomiasis'));
        %         write('you DO NOT have schistosomiasis')
        %     );
        % Highest = 'Dengue' ->
        %     (
        %         % Specific for dengue
        %         dengue_specifics,
        %         (dengue_confirm -> write('you have dengue'));
        %         write('you DO NOT have dengue')
        %     );
        % Highest = 'Tetanus' ->
        %     (
        %         % Specific for tetanus
        %         tetanus_specifics,
        %         (tetanus_confirm -> write('you have tetanus'));
        %         write('you DO NOT have tetanus')
        %     );
    ).

:- dynamic checked/1.

% Main Function
:- initialization(main).
main :- 
    % % Ask for the patient's name, age, and sex
    % introduction,
    % write('========================================================================'), nl,
    % write('                          PATIENT INFORMATION'), nl,
    % write('========================================================================'), nl,
    % % Ask for the demographics of the patient.
    % demographics,
    % write('========================================================================'), nl,nl,nl,

    % write('========================================================================'), nl,
    % write('                               SYMPTOMS'), nl,
    % write('========================================================================'), nl,

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
    
    symptom_specifics.

    % changes ----