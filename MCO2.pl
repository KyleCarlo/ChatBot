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
*   Parameters:
*       Name - The name of the patient
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
    index_of_Max(Diseases, 0, _),
    max(Index),
    nth0(Index, DisWords, Highest).

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
    frequent_poop(1);
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
    sore_throat(1);
    (
        body_ache(1);
        head_ache(1)
    ).

% Symptoms of Diseases
influenza_specifics :- 
    (
        current_predicate(sore_throat/1);
        askSymptom('Do you have a sore throat? (y/n) ', sore_throat(1), Answer8),
        (
            Answer8 = 'y' ->
                (
                    add_bronchitis
                );
            Answer8 = 'n' -> true
        )
    ),
    (
        runny_nose(1);
        askSymptom('Do you have a runny nose? (y/n) ', runny_nose(1), Answer9),
        (
            Answer9 = 'y' ->
                (
                    add_measles
                );
            Answer9 = 'n' -> true
        )
    ).

bronchitis_specifics :- 
    (
        (
            current_predicate(soreness_chest/1);
            askSymptom('Do you have any soreness or discomfort in your chest? (y/n) ', 
                        soreness_chest(1), Answer8),
            (
                Answer8 = 'y' ->
                    (
                        add_influenza,
                        add_bronchitis
                    );
                Answer8 = 'n' -> true
            )
        ),
        (
            current_predicate(runny_nose/1);
            askSymptom('Do you have a runny nose? (y/n) ', runny_nose(1), Answer9),
            (
                Answer9 = 'y' ->
                    (
                        add_influenza,
                        add_measles
                    );
                Answer9 = 'n' -> true
            )
        )
    )


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
        Answer1 = 'n' -> true
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
        Answer2 = 'n' -> true
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
        Answer3 = 'n' -> true
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
        Answer4 = 'n' -> true
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
        Answer5 = 'n' -> true
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
        Answer6 = 'n' -> true
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
        Answer7 = 'n' -> true
    ),

    % Score the diseases
    getScore(Highest),
    write(Highest), nl,
    (
        Highest = 'Influenza' ->
            (
                % Specific for influenza
                influenza_specifics
            );
        Highest = 'Bronchitis' ->
            (
                % Specific for bronchitis
                
            );
        Highest = _ -> true
    ).


    % % Specific for diarrhea
    % askSymptom('Do you have stomach pain? (y/n) ', stomach_ache(1), Answer8),
    % askSymptom('Are you experiencing watery stool? (y/n) ', watery_stool(1), Answer9),
    % askSymptom('Are you experiencing frequent pooping? (y/n) ', frequent_poop(1), Answer10);

    % (diarrhea_confirm ->
    %     write('you have diarrhea')).