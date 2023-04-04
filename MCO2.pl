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
*       Count - The number of predicates that are currently asserted
*   Description:
*       Counts the number of predicates that are currently asserted
*/
:- use_module(library(tty)).


count_predicates(Count, What) :-
    findall(_, What, Predicates),
    length(Predicates, Count).

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

% Initialize Diseases
diarrhea :- 
    watery_stool(1),
    frequent_poop(1);
    (
        vomiting(1);
        fever(1);
        body_ache(1);
        head_ache(1);
        stomach_ache(1)
    ).

bronchitis :-
    soreness_chest(1),
    cough(1),
    sore_throat(1);
    (
        body_ache(1);
        head_ache(1)
    ).

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
    askSymptom('Do you have a fever? (y/n) ', fever(1), Answer),                    % Similarity Count: 9
    askSymptom('Do you have a headache? (y/n) ', head_ache(1), Answer),             % Similarity Count: 6
    askSymptom('Are you experiencing coughing? (y/n) ', cough(1), Answer),          % Similarity Count: 5
    askSymptom('Have you vomited recently? (y/n) ', vomiting(1), Answer),           % Similarity Count: 4
    askSymptom('Are you experiencing body aches? (y/n) ', body_ache(1), Answer),    % Similarity Count: 4
    askSymptom('Are you experiencing fatigue? (y/n) ', fatigue(1), Answer),         % Similarity Count: 4
    askSymptom('Are you experiencing rashes? (y/n) ', rashes(1), Answer),           % Similarity Count: 4

    % Specific for diarrhea
    askSymptom('Do you have stomach pain? (y/n) ', stomach_ache(1), Answer),
    askSymptom('Are you experiencing watery stool? (y/n) ', watery_stool(1), Answer),
    askSymptom('Are you experiencing frequent pooping? (y/n) ', frequent_poop(1), Answer);

    diarrhea ->
        write('you have diarrhea').