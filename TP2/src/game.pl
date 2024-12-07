
% Dynamic predicates for game configuration
:- dynamic game_config/2.

/*
    play/0
    Main predicate that initializes the game and displays the menu.
    Allows player to choose game mode and starts the game with selected configuration.
*/
play :-
    display_title,
    display_menu,
    read_menu_option(Choice),
    setup_game(Choice).

/*
    display_title/0
    Displays the game title in ASCII art for better presentation.
*/
display_title :-
    nl,
    write('===================================='), nl,
    write('              REPLICA               '), nl,
    write('       A Strategic Board Game       '), nl,
    write('===================================='), nl,
    nl.

/*
    display_menu/0
    Shows the main menu options to the user.
*/
display_menu :-
    write('Select game mode:'), nl,
    write('1. Human vs Human'), nl,
    write('2. Human vs Computer'), nl,
    write('3. Computer vs Human'), nl,
    write('4. Computer vs Computer'), nl,
    write('0. Exit'), nl,
    write('Choose an option (0-4): ').

/*
    read_menu_option(-Choice)
    Reads and validates user input for menu selection.
    @param Choice The validated menu choice
*/
read_menu_option(Choice) :-
    catch(read(Input), _, fail),
    (   number(Input),
        Input >= 0,
        Input =< 4
    ->  Choice = Input
    ;   write('Invalid option! Please choose a number between 0 and 4.'), nl,
        display_menu,
        read_menu_option(Choice)
    ).

/*
    setup_game(+Choice)
    Configures the game based on the selected menu option.
    @param Choice The menu option selected by the user
*/
setup_game(0) :- 
    write('Thanks for playing!'), nl,
    halt.
setup_game(Choice) :-
    Choice > 0,
    Choice =< 4,
    configure_game_mode(Choice, GameConfig),
    initial_state(GameConfig, InitialState),
    display_game(InitialState),
    game_loop(InitialState).

/*
    configure_game_mode(+Choice, -GameConfig)
    Sets up the game configuration based on menu choice.
    For computer players, also handles difficulty selection.
    @param Choice The selected menu option
    @param GameConfig The resulting game configuration
*/
configure_game_mode(Choice, GameConfig) :-
    (   Choice = 1 -> GameConfig = [human-human]
    ;   Choice = 2 -> configure_computer_difficulty(player2, Config), GameConfig = [human-Config]
    ;   Choice = 3 -> configure_computer_difficulty(player1, Config), GameConfig = [Config-human]
    ;   Choice = 4 -> configure_computer_difficulty(player1, Config1),
                     configure_computer_difficulty(player2, Config2),
                     GameConfig = [Config1-Config2]
    ).

/*
    configure_computer_difficulty(+Player, -Config)
    Handles computer difficulty selection for a specific player.
    @param Player The player being configured (player1 or player2)
    @param Config The resulting configuration including difficulty
*/
configure_computer_difficulty(Player, computer-Level) :-
    format('Select difficulty for ~w:', [Player]), nl,
    write('1. Easy (Random moves)'), nl,
    write('2. Hard (Strategic moves)'), nl,
    write('Choose difficulty (1-2): '),
    read(Choice),
    (   Choice = 1 -> Level = 1
    ;   Choice = 2 -> Level = 2
    ;   write('Invalid choice. Please select 1 or 2.'), nl,
        configure_computer_difficulty(Player, computer-Level)
    ).

