% Main menu predicate
play :-
    write('Welcome to Replica!'), nl,
    write('1. Human vs Human'), nl,
    write('2. Human vs Computer'), nl,
    write('3. Computer vs Human'), nl,
    write('4. Computer vs Computer'), nl,
    write('Choose game mode (1-4): '),
    read(Choice),
    setup_game(Choice).