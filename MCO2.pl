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
count_predicates(Count) :-
    findall(_, tb(_), Predicates),
    length(Predicates, Count).

/**
*   Parameters:
*       Name - The name of the patient
*       Age - The age of the patient
*   Description:
*       Asks the patient for their name, age, and sex
*/
demographics(Name, Age) :-
    write('What is your name? '),
    read(Name),
    write('What is your age? '),
    read(Age),
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
    write('').
    

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
            format('Invalid input. Please enter \'y\' or \'n\'.'), nl,
            askSymptom(Question, Predicate)
        )
    ).

% Initialization of dyanmic predicates
:- dynamic(tb/1).

:- initialization(main).
main :- 
    % Ask for the patient's name, age, and sex
    introduction.
    % demographics(Name, Age).
    demographics(Name, Age),
    write('========================================================================'), nl,
    write('                          PATIENT INFORMATION'), nl,
    write('========================================================================'), nl,
    format('Name: ~w', [Name]), nl,
    format('Age: ~w', [Age]), nl,
    format().
