% Dynamic predicates for game configuration
:- dynamic game_config/2.
:- use_module(library(lists)).
:- use_module(library(random)).

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
                                MAIN GAME CONTROL
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

play :-
    display_title,
    display_menu,
    read_menu_option(Choice),
    setup_game(Choice).

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
                                MENU AND SETUP
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

display_title :-
    nl,
    write('===================================='), nl,
    write('              REPLICA               '), nl,
    write('       A Strategic Board Game       '), nl,
    write('===================================='), nl,
    nl.

display_menu :-
    write('Select game mode:'), nl,
    write('1. Human vs Human'), nl,
    write('2. Human vs Computer'), nl,
    write('3. Computer vs Human'), nl,
    write('4. Computer vs Computer'), nl,
    write('0. Exit'), nl,
    write('Choose an option (0-4): ').

read_menu_option(Choice) :-
    get_char(Input),
    get_char(_), % consume newline
    (  Input = '0' -> Choice = 0
    ;  Input = '1' -> Choice = 1
    ;  Input = '2' -> Choice = 2
    ;  Input = '3' -> Choice = 3
    ;  Input = '4' -> Choice = 4
    ;  write('Invalid option! Please choose a number between 0 and 4.'), nl,
       display_menu,
       read_menu_option(Choice)
    ).

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

configure_game_mode(Choice, GameConfig) :-
    (   Choice = 1 -> GameConfig = [human-human]
    ;   Choice = 2 -> configure_computer_difficulty(player2, Config), GameConfig = [human-Config]
    ;   Choice = 3 -> configure_computer_difficulty(player1, Config), GameConfig = [Config-human]
    ;   Choice = 4 -> configure_computer_difficulty(player1, Config1),
                     get_char(_),
                     configure_computer_difficulty(player2, Config2),
                     GameConfig = [Config1-Config2]
    ).

configure_computer_difficulty(Player, Config) :-
    format('Select difficulty for ~w:', [Player]), nl,
    write('1. Easy (Random moves)'), nl,
    write('2. Hard (Strategic moves)'), nl,
    write('Choose difficulty (1-2): '),
    get_char(Choice),
    (   Choice = '1' -> Config = level1
    ;   Choice = '2' -> Config = level2
    ;   write('Invalid choice. Please select 1 or 2.'), nl,
        configure_computer_difficulty(Player, Config)
    ).

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
                            BOARD REPRESENTATION
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

% Board representation:
% empty - empty square
% w - white piece
% b - black piece
% wk - white king
% bk - black king

initial_state(GameConfig, state(Board, white, GameConfig)) :-
    initial_board(Board).

initial_board(Board) :-
    empty_board(EmptyBoard),
    place_initial_pieces(EmptyBoard, Board).

empty_board([
    [empty,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty,empty]
]).

place_initial_pieces(Board, FinalBoard) :-
    % Place black pieces in top-left corner (0,0 to 3,3)
    place_black_pieces(Board, Board1),
    % Place white pieces in bottom-right corner (4,4 to 7,7)
    place_white_pieces(Board1, FinalBoard).

place_black_pieces(Board, NewBoard) :-
    % Place 2x2 square with king in corner
    replace_element(Board, 0, 0, bk, B1),   % King in corner
    replace_element(B1, 0, 1, b, B2),       % Regular piece right of king
    replace_element(B2, 1, 0, b, B3),       % Regular piece below king
    replace_element(B3, 1, 1, b, B4),       % Regular piece diagonal from king
    
    % Place right flanking pieces
    replace_element(B4, 0, 2, b, B5),      % Top piece
    replace_element(B5, 0, 3, b, B6),      % Top right piece
    replace_element(B6, 1, 2, b, B7),      % Bottom piece
    replace_element(B7, 1, 3, b, B8),      % Bottom right piece
    
    % Place bottom flanking pieces
    replace_element(B8, 2, 0, b, B9),      % Left piece
    replace_element(B9, 2, 1, b, B10),     % Right piece
    replace_element(B10, 3, 0, b, B11),    % Bottom left piece
    replace_element(B11, 3, 1, b, NewBoard). % Bottom right piece

place_white_pieces(Board, NewBoard) :-
    % Place 2x2 square with king in corner (bottom-right corner)
    replace_element(Board, 7, 7, wk, B1),       % King in corner
    replace_element(B1, 7, 6, w, B2),           % Left of king
    replace_element(B2, 6, 7, w, B3),           % Above king
    replace_element(B3, 6, 6, w, B4),           % Diagonal from king
    
    % Place "L" flanking pieces
    % First 2x2 square to the left
    replace_element(B4, 7, 4, w, B5),          % Bottom row
    replace_element(B5, 7, 5, w, B6),          % Bottom row
    replace_element(B6, 6, 4, w, B7),          % Top row
    replace_element(B7, 6, 5, w, B8),          % Top row
    
    % Second 2x2 square above
    replace_element(B8, 4, 7, w, B9),          % Bottom row
    replace_element(B9, 4, 6, w, B10),         % Bottom row
    replace_element(B10, 5, 7, w, B11),        % Top row
    replace_element(B11, 5, 6, w, NewBoard).   % Top row

% Helper predicate to place a piece on the board
place_piece(Board, Piece, Row, Col, NewBoard) :-
    replace_element(Board, Row, Col, Piece, NewBoard).

% Replace element in a list of lists
replace_element(Matrix, Row, Col, NewElement, NewMatrix) :-
    nth0(Row, Matrix, OldRow),
    replace_in_row(OldRow, Col, NewElement, NewRow),
    replace_in_row(Matrix, Row, NewRow, NewMatrix).

replace_in_row(List, Index, Element, NewList) :-
    nth0(Index, List, _, TempList),
    nth0(Index, NewList, Element, TempList).

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
                            DISPLAY FUNCTIONS
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

display_game(state(Board, Player, _)) :-
    nl,
    write('Current Player: '), write(Player), nl,
    write('   1  2  3  4  5  6  7  8'), nl,
    write('   -----------------------'), nl,
    display_board(Board, 1),
    nl.

display_board([], _) :- !.
display_board([Row|Rest], N) :-
    format('~w |', [N]),
    display_row(Row),
    nl,
    NextN is N + 1,
    display_board(Rest, NextN).

display_row([]) :- !.
display_row([Cell|Rest]) :-
    display_cell(Cell),
    display_row(Rest).

display_cell(empty) :- !, write(' . ').
display_cell(w) :- !, write(' W ').
display_cell(b) :- !, write(' B ').
display_cell(wk) :- !, write('(W)').
display_cell(bk) :- write('(B)').

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
                            GAME OVER CONDITIONS
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


% game_over(+GameState, -Winner)
game_over(state(Board, _, _), Winner) :-
    % Check if white has a king in black's starting corner (top-left)
    get_piece(Board, 1, 1, wk), !,
    Winner = white.

game_over(state(Board, _, _), Winner) :-
    % Check if black has a king in white's starting corner (bottom-right)
    get_piece(Board, 8, 8, bk), !,
    Winner = black.

game_over(state(Board, white, gameOver), Winner) :-
    % Check if gameConfig is gameOver after black play
    !,
    Winner = black.

game_over(state(Board, black, gameOver), Winner) :-
    % Check if gameConfig is gameOver after white play
    Winner = white.

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
                            GAME LOOP
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

game_loop(state(Board, Player, GameConfig)) :-
    (   game_over(state(Board, Player, GameConfig), Winner)
    ->  format('Game Over! Winner: ~w~n', [Winner])
    ;   get_move(state(Board, Player, GameConfig), newState(NewBoard, NewPlayer, NewGameConfig)),
        display_game(state(NewBoard, NewPlayer, NewGameConfig)),
        game_loop(state(NewBoard, NewPlayer, NewGameConfig))
    ).


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
                            TRANSFORM MOVES
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

% Handle transform move for white human player
move(state(Board, white, [human-Config]), (KingRow, KingCol, 'T'), newState(NewBoard, black, [human-Config])) :-
    !,
    get_piece(Board, KingRow, KingCol, wk),  % Verify selected piece is a white king
    write('Select the piece to transform (Row-Column): '), nl,
    read(TargetRow-TargetCol),
    get_char(_),
    get_piece(Board, TargetRow, TargetCol, w),  % Verify target is a white piece
    check_line_of_sight(Board, KingRow, KingCol, TargetRow, TargetCol, white),
    TR is TargetRow - 1,
    TC is TargetCol - 1,
    replace_element(Board, TR, TC, wk, NewBoard).

% Handle transform move for black human player
move(state(Board, black, [Config-human]), (KingRow, KingCol, 'T'), newState(NewBoard, white, [Config-human])) :-
    !,
    get_piece(Board, KingRow, KingCol, bk),  % Verify selected piece is a black king
    write('Select the piece to transform (Row-Column): '), nl,
    read(TargetRow-TargetCol),
    get_char(_),
    get_piece(Board, TargetRow, TargetCol, b),  % Verify target is a black piece
    check_line_of_sight(Board, KingRow, KingCol, TargetRow, TargetCol, black),
    TR is TargetRow - 1,
    TC is TargetCol - 1,
    replace_element(Board, TR, TC, bk, NewBoard).

% Handle transform move for white computer
move(state(Board, white, GameConfig), (KingRow, KingCol, 'T'), newState(NewBoard, black, GameConfig)) :-
    get_piece(Board, KingRow, KingCol, wk),  % Verify selected piece is a white king
    get_all_pieces(Board, white, AllPieces),
    random_member(TargetRow-TargetCol, AllPieces),
    get_piece(Board, TargetRow, TargetCol, w),  % Verify target is a white piece
    check_line_of_sight(Board, KingRow, KingCol, TargetRow, TargetCol, white),
    TR is TargetRow - 1,
    TC is TargetCol - 1,
    replace_element(Board, TR, TC, wk, NewBoard).

% Handle transform move for black computer
move(state(Board, black, GameConfig), (KingRow, KingCol, 'T'), newState(NewBoard, white, GameConfig)) :-
    get_piece(Board, KingRow, KingCol, bk),  % Verify selected piece is a black king
    get_all_pieces(Board, black, AllPieces),
    random_member(TargetRow-TargetCol, AllPieces),
    get_piece(Board, TargetRow, TargetCol, b),  % Verify target is a black piece
    check_line_of_sight(Board, KingRow, KingCol, TargetRow, TargetCol, black),
    TR is TargetRow - 1,
    TC is TargetCol - 1,
    replace_element(Board, TR, TC, bk, NewBoard).


% Check if there's clear line of sight between two positions
check_line_of_sight(Board, KingRow, KingCol, TargetRow, TargetCol, Player) :-
    (KingRow = TargetRow -> check_horizontal_sight(Board, KingRow, KingCol, TargetCol, Player)
    ; KingCol = TargetCol -> check_vertical_sight(Board, KingCol, KingRow, TargetRow, Player)
    ; abs(KingRow - TargetRow) =:= abs(KingCol - TargetCol) -> 
        check_diagonal_sight(Board, KingRow, KingCol, TargetRow, TargetCol, Player)
    ).

% Check horizontal line of sight
check_horizontal_sight(Board, Row, Col1, Col2, Player) :-
    MinCol is min(Col1, Col2) + 1,
    MaxCol is max(Col1, Col2) - 1,
    \+ has_enemy_between_horizontal(Board, Row, MinCol, MaxCol, Player).

% Check vertical line of sight
check_vertical_sight(Board, Col, Row1, Row2, Player) :-
    MinRow is min(Row1, Row2) + 1,
    MaxRow is max(Row1, Row2) - 1,
    \+ has_enemy_between_vertical(Board, Col, MinRow, MaxRow, Player).

% Check diagonal line of sight
check_diagonal_sight(Board, Row1, Col1, Row2, Col2, Player) :-
    DirRow is sign(Row2 - Row1),
    DirCol is sign(Col2 - Col1),
    \+ has_enemy_between_diagonal(Board, Row1, Col1, Row2, Col2, DirRow, DirCol, Player).

% Helper predicates to check for enemy pieces
has_enemy_between_horizontal(Board, Row, Col, MaxCol, white) :-
    Col =< MaxCol,
    get_piece(Board, Row, Col, Piece),
    (Piece = b ; Piece = bk), !.
has_enemy_between_horizontal(Board, Row, Col, MaxCol, black) :-
    Col =< MaxCol,
    get_piece(Board, Row, Col, Piece),
    (Piece = w ; Piece = wk), !.
has_enemy_between_horizontal(Board, Row, Col, MaxCol, Player) :-
    Col < MaxCol,
    NextCol is Col + 1,
    has_enemy_between_horizontal(Board, Row, NextCol, MaxCol, Player).

has_enemy_between_vertical(Board, Col, Row, MaxRow, white) :-
    Row =< MaxRow,
    get_piece(Board, Row, Col, Piece),
    (Piece = b ; Piece = bk), !.
has_enemy_between_vertical(Board, Col, Row, MaxRow, black) :-
    Row =< MaxRow,
    get_piece(Board, Row, Col, Piece),
    (Piece = w ; Piece = wk), !.
has_enemy_between_vertical(Board, Col, Row, MaxRow, Player) :-
    Row < MaxRow,
    NextRow is Row + 1,
    has_enemy_between_vertical(Board, Col, NextRow, MaxRow, Player).

has_enemy_between_diagonal(Board, Row, Col, TargetRow, TargetCol, DirRow, DirCol, white) :-
    NextRow is Row + DirRow,
    NextCol is Col + DirCol,
    (NextRow = TargetRow, NextCol = TargetCol -> false
    ; get_piece(Board, NextRow, NextCol, Piece),
      (Piece = b ; Piece = bk) -> true
    ; has_enemy_between_diagonal(Board, NextRow, NextCol, TargetRow, TargetCol, DirRow, DirCol, white)
    ).
has_enemy_between_diagonal(Board, Row, Col, TargetRow, TargetCol, DirRow, DirCol, black) :-
    NextRow is Row + DirRow,
    NextCol is Col + DirCol,
    (NextRow = TargetRow, NextCol = TargetCol -> false
    ; get_piece(Board, NextRow, NextCol, Piece),
      (Piece = w ; Piece = wk) -> true
    ; has_enemy_between_diagonal(Board, NextRow, NextCol, TargetRow, TargetCol, DirRow, DirCol, black)
    ).

sign(N, 1) :- N > 0, !.
sign(N, -1) :- N < 0, !.
sign(0, 0).

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
                            MOVES
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

get_move(state(Board, white, [human-Config]), NewState) :-
    repeat,
    write('Select the piece you want to move by writing Row-Column.'), nl,
    read(PieceRow-PieceColumn),
    get_char(_), % consume newline
    (   PieceRow > 0,
        PieceRow =< 8,
        PieceColumn > 0,
        PieceColumn =< 8
    ->  write('Select the action you want to perform'), nl,
        write('1. Horizontal'), nl,
        write('2. Vertical'), nl,
        write('3. Diagonal'), nl,
        write('4. Transform'), nl,  
        write('Choose an option: '), nl,
        get_char(Input),
        get_char(_), % consume newline
        (Input = '1' -> Direction = 'H'
        ;  Input = '2' -> Direction = 'V'
        ;  Input = '3' -> Direction = 'D'
        ;  Input = '4' -> Direction = 'T'),
        (choose_move(state(Board, white, _), human, (PieceRow, PieceColumn, Direction)),
        move(state(Board, white, [human-Config]), (PieceRow, PieceColumn, Direction), NewState)
        -> true
        ; (Direction = 'T' 
          -> write('Transform not possible - no line of sight or invalid target. Try another move.'), nl, fail
          ; write('Invalid move. Try again.'), nl, fail
        ))
    ;   write('Invalid coordinates. Try again.'), nl,
        fail
    ).

get_move(state(Board, black, [Config-human]), NewState) :-
    repeat,
    write('Select the piece you want to move by writing Row-Column.'), nl,
    read(PieceRow-PieceColumn),
    get_char(_), % consume newline
    (   PieceRow > 0,
        PieceRow =< 8,
        PieceColumn > 0,
        PieceColumn =< 8
    ->  write('Select the action you want to perform'), nl,
        write('1. Horizontal'), nl,
        write('2. Vertical'), nl,
        write('3. Diagonal'), nl,
        write('4. Transform'), nl,  
        write('Choose an option: '), nl,
        get_char(Input),
        get_char(_), % consume newline
        (Input = '1' -> Direction = 'H'
        ;  Input = '2' -> Direction = 'V'
        ;  Input = '3' -> Direction = 'D'
        ;  Input = '4' -> Direction = 'T'),
        (choose_move(state(Board, black, _), human, (PieceRow, PieceColumn, Direction)),
        move(state(Board, black, [Config-human]), (PieceRow, PieceColumn, Direction), NewState)
        -> true
        ; (Direction = 'T' 
          -> write('Transform not possible - no line of sight or invalid target. Try another move.'), nl, fail
          ; write('Invalid move. Try again.'), nl, fail
        ))
    ;   write('Invalid coordinates. Try again.'), nl,
        fail
    ).

get_move(state(Board, white, [Level-Config]), NewState) :-
    repeat,
    choose_move(state(Board, white, _), Level, (PieceRow, PieceColumn, Direction)),
    move(state(Board, white, [Level-Config]), (PieceRow, PieceColumn, Direction), NewState).

get_move(state(Board, black, [Config-Level]), NewState) :-
    repeat,
    choose_move(state(Board, black, _), Level, (PieceRow, PieceColumn, Direction)),
    move(state(Board, black, [Config-Level]), (PieceRow, PieceColumn, Direction), NewState).


valid_moves(state(Board, Player, _), ListOfMoves) :-
    get_all_pieces(Board, Player, AllPieces),
    valid_moves(Board, Player, AllPieces, [], ListOfMoves).

valid_moves(Board, Player, [], ListOfMoves, ListOfMoves) :- !.

valid_moves(Board, white, [(PieceRow-PieceColumn)|Pieces], Aux, ListOfMoves) :- 
    test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'H'), PieceColumn, MoveH),
    test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'V'), PieceRow, MoveV),
    test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'D'), (PieceRow, PieceColumn), MoveD),
    append(MoveH, Aux, M1),
    append(MoveV, M1, M2),
    append(MoveD, M2, Moves),
    valid_moves(Board, white, Pieces, Moves, ListOfMoves).

valid_moves(Board, black, [(PieceRow-PieceColumn)|Pieces], Aux, ListOfMoves) :- 
    test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'H'), PieceColumn, MoveH),
    test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'V'), PieceRow, MoveV),
    test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'D'), (PieceRow, PieceColumn), MoveD),
    append(MoveH, Aux, M1),
    append(MoveV, M1, M2),
    append(MoveD, M2, Moves),
    valid_moves(Board, black, Pieces, Moves, ListOfMoves).


test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    PC2 is PC - 1,
    PC2 > 0,
    get_piece(Board, PieceRow, PC2, P),
    (P = w ; P = wk),
    !,
    test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'H'), PC2, Move).

test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    PC2 is PC - 1,
    PC2 > 0,
    get_piece(Board, PieceRow, PC2, P),
    (P = b ; P = bk),
    !,
    Move = [(PieceRow, PieceColumn, 'H')].

test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    PC2 is PC - 1,
    PC2 > 0,
    get_piece(Board, PieceRow, PC2, P),
    P = empty,
    !,
    Move = [(PieceRow, PieceColumn, 'H')].

test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    Move = [].

test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    PR2 is PR - 1,
    PR2 > 0,
    get_piece(Board, PR2, PieceColumn, P),
    (P = w ; P = wk),
    !,
    test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'V'), PR2, Move).

test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    PR2 is PR - 1,
    PR2 > 0,
    get_piece(Board, PR2, PieceColumn, P),
    (P = b ; P = bk),
    !,
    Move = [(PieceRow, PieceColumn, 'V')].

test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    PR2 is PR - 1,
    PR2 > 0,
    get_piece(Board, PR2, PieceColumn, P),
    P = empty,
    !,
    Move = [(PieceRow, PieceColumn, 'V')].

test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    Move = [].

test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    PR2 is PR - 1,
    PR2 > 0,
    PC2 is PC - 1,
    PC2 > 0,
    get_piece(Board, PR2, PC2, P),
    (P = w ; P = wk),
    !,
    test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'D'), (PR2, PC2), Move).

test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    PR2 is PR - 1,
    PR2 > 0,
    PC2 is PC - 1,
    PC2 > 0,
    get_piece(Board, PR2, PC2, P),
    (P = b ; P = bk),
    !,
    Move = [(PieceRow, PieceColumn, 'D')].

test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    PR2 is PR - 1,
    PR2 > 0,
    PC2 is PC - 1,
    PC2 > 0,
    get_piece(Board, PR2, PC2, P),
    P = empty,
    !,
    Move = [(PieceRow, PieceColumn, 'D')].

test_valid_move(state(Board, white, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    Move = [].

test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    PC2 is PC + 1,
    PC2 < 9,
    get_piece(Board, PieceRow, PC2, P),
    (P = b ; P = bk),
    !,
    test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'H'), PC2, Move).

test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    PC2 is PC + 1,
    PC2 < 9,
    get_piece(Board, PieceRow, PC2, P),
    (P = w ; P = wk),
    !,
    Move = [(PieceRow, PieceColumn, 'H')].

test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    PC2 is PC + 1,
    PC2 < 9,
    get_piece(Board, PieceRow, PC2, P),
    P = empty,
    !,
    Move = [(PieceRow, PieceColumn, 'H')].

test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    Move = [].

test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    PR2 is PR + 1,
    PR2 < 9,
    get_piece(Board, PR2, PieceColumn, P),
    (P = b ; P = bk),
    !,
    test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'V'), PR2, Move).

test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    PR2 is PR + 1,
    PR2 < 9,
    get_piece(Board, PR2, PieceColumn, P),
    (P = w ; P = wk),
    !,
    Move = [(PieceRow, PieceColumn, 'V')].

test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    PR2 is PR + 1,
    PR2 < 9,
    get_piece(Board, PR2, PieceColumn, P),
    P = empty,
    !,
    Move = [(PieceRow, PieceColumn, 'V')].

test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    Move = [].

test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    PR2 is PR + 1,
    PR2 < 9,
    PC2 is PC + 1,
    PC2 < 9,
    get_piece(Board, PR2, PC2, P),
    (P = b ; P = bk),
    !,
    test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'D'), (PR2, PC2), Move).

test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    PR2 is PR + 1,
    PR2 < 9,
    PC2 is PC + 1,
    PC2 < 9,
    get_piece(Board, PR2, PC2, P),
    (P = w ; P = wk),
    !,
    Move = [(PieceRow, PieceColumn, 'D')].

test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    PR2 is PR + 1,
    PR2 < 9,
    PC2 is PC + 1,
    PC2 < 9,
    get_piece(Board, PR2, PC2, P),
    P = empty,
    !,
    Move = [(PieceRow, PieceColumn, 'D')].

test_valid_move(state(Board, black, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    Move = [].


choose_move(_, human, (_, _, Direction)) :-
    Direction = 'T',
    !.

choose_move(state(Board, white, _), human, (PieceRow, PieceColumn, Direction)) :-
    valid_moves(state(Board, white, _), ListOfMoves),
    member((PieceRow, PieceColumn, Direction), ListOfMoves).

choose_move(state(Board, black, _), human, (PieceRow, PieceColumn, Direction)) :-
    valid_moves(state(Board, black, _), ListOfMoves),
    member((PieceRow, PieceColumn, Direction), ListOfMoves).

choose_move(state(Board, white, _), level1, (PieceRow, PieceColumn, Direction)) :-
    random_member(Direction, ['H', 'V', 'D', 'T']),
    select_random_move(state(Board, white, _), (PieceRow, PieceColumn, Direction)).

choose_move(state(Board, black, _), level1, (PieceRow, PieceColumn, Direction)) :-
    random_member(Direction, ['H', 'V', 'D', 'T']),
    select_random_move(state(Board, white, _), (PieceRow, PieceColumn, Direction)).

choose_move(state(Board, white, _), level2, (PieceRow, PieceColumn, Direction)) :-
    get_all_pieces(Board, white, AllPieces),
    get_best_moves(Board, white, AllPieces, BestMoves),
    select_best_move(state(Board, white, _), BestMoves, white, (PieceRow, PieceColumn, Direction)).

choose_move(state(Board, black, _), level2, (PieceRow, PieceColumn, Direction)) :-
    get_all_pieces(Board, black, AllPieces),
    get_best_moves(Board, black, AllPieces, BestMoves),
    select_best_move(state(Board, black, _), BestMoves, black, (PieceRow, PieceColumn, Direction)).


select_random_move(state(Board, white, _), (PieceRow, PieceColumn, 'T')) :-
    !,
    get_all_pieces(Board, white, AllPieces),
    random_member(PieceRow-PieceColumn, AllPieces).

select_random_move(state(Board, white, _), (PieceRow, PieceColumn, Direction)) :-
    !,
    valid_moves(state(Board, white, _), ListOfMoves),
    random_member((PieceRow, PieceColumn, Direction), ListOfMoves).

select_random_move(state(Board, black, _), (PieceRow, PieceColumn, 'T')) :-
    !,
    get_all_pieces(Board, black, AllPieces),
    random_member(PieceRow-PieceColumn, AllPieces).

select_random_move(state(Board, black, _), (PieceRow, PieceColumn, Direction)) :-
    !,
    valid_moves(state(Board, black, _), ListOfMoves),
    random_member((PieceRow, PieceColumn, Direction), ListOfMoves).


select_best_move(state(Board, white, _), [], white, (PieceRow, PieceColumn, Direction)) :-
    !,
    random_member(Direction, ['H', 'V', 'D', 'T']),
    select_random_move(state(Board, white, _), (PieceRow, PieceColumn, Direction)).

select_best_move(state(Board, black, _), [], black, (PieceRow, PieceColumn, Direction)) :-
    !,
    random_member(Direction, ['H', 'V', 'D', 'T']),
    select_random_move(state(Board, black, _), (PieceRow, PieceColumn, Direction)).

select_best_move(_, BestMoves, _, Move) :-
    separate_moves(BestMoves, KingMoves, OtherMoves),
    select_king_other(KingMoves, OtherMoves, Move).


select_king_other([], OtherMoves, Move) :-
    !,
    random_member(Move, OtherMoves).

select_king_other(KingMoves, _, Move) :-
    random_member(Move, KingMoves).


separate_moves(BestMoves, KingMoves, OtherMoves) :-
    separate_moves(BestMoves, [], [], KingMoves, OtherMoves).

separate_moves([], KingMoves, OtherMoves, KingMoves, OtherMoves) :- !.

separate_moves([(PieceRow, PieceColumn, Direction, Piece)|BestMoves], KingAux, OtherAux, KingMoves, OtherMoves) :-
    Piece = w,
    !,
    separate_moves(BestMoves, KingAux, [(PieceRow, PieceColumn, Direction)|OtherAux], KingMoves, OtherMoves).

separate_moves([(PieceRow, PieceColumn, Direction, Piece)|BestMoves], KingAux, OtherAux, KingMoves, OtherMoves) :-
    Piece = b,
    !,
    separate_moves(BestMoves, KingAux, [(PieceRow, PieceColumn, Direction)|OtherAux], KingMoves, OtherMoves).

separate_moves([(PieceRow, PieceColumn, Direction, Piece)|BestMoves], KingAux, OtherAux, KingMoves, OtherMoves) :-
    Piece = wk,
    !,
    separate_moves(BestMoves, [(PieceRow, PieceColumn, Direction)|KingAux], OtherAux, KingMoves, OtherMoves).

separate_moves([(PieceRow, PieceColumn, Direction, Piece)|BestMoves], KingAux, OtherAux, KingMoves, OtherMoves) :-
    Piece = bk,
    separate_moves(BestMoves, [(PieceRow, PieceColumn, Direction)|KingAux], OtherAux, KingMoves, OtherMoves).


move(state(Board, white, GameConfig), (PieceRow, PieceColumn, Direction), newState(NewBoard, NewPlayer, NewGameConfig)) :-
    get_piece(Board, PieceRow, PieceColumn, P),
    (P = w ; P = wk),
    validate_move(state(Board, white, GameConfig), (PieceRow, PieceColumn, Direction), newState(NewBoard, NewPlayer, NewGameConfig)).

move(state(Board, black, GameConfig), (PieceRow, PieceColumn, Direction), newState(NewBoard, NewPlayer, NewGameConfig)) :-
    get_piece(Board, PieceRow, PieceColumn, P),
    (P = b ; P = bk),
    validate_move(state(Board, black, GameConfig), (PieceRow, PieceColumn, Direction), newState(NewBoard, NewPlayer, NewGameConfig)).

get_piece_in_row([P|_], 1, P) :- !.

get_piece_in_row([Piece|Pieces], PieceColumn, P) :- 
    N is PieceColumn - 1,
    get_piece_in_row(Pieces, N, P).

get_piece([Row|_], 1, PieceColumn, P) :- 
    !,
    get_piece_in_row(Row, PieceColumn, P).

get_piece([Row|Rows], PieceRow, PieceColumn, P) :- 
    N is PieceRow - 1,
    get_piece(Rows, N, PieceColumn, P).


get_all_pieces(Board, Player, AllPieces) :-
    get_all_pieces(Board, Player, 1, [], AllPieces).

get_all_pieces([], _, _, AllPieces, AllPieces) :- !.

get_all_pieces([Row|Rows], white, RowNumber, Aux, AllPieces) :-
    get_all_pieces_row(Row, white, RowNumber, 1, [], AllPiecesRow),
    N is RowNumber + 1,
    append(Aux, AllPiecesRow, Aux2),
    get_all_pieces(Rows, white, N, Aux2, AllPieces).


get_all_pieces([Row|Rows], black, RowNumber, Aux, AllPieces) :-
    get_all_pieces_row(Row, black, RowNumber, 1, [], AllPiecesRow),
    N is RowNumber + 1,
    append(Aux, AllPiecesRow, Aux2),
    get_all_pieces(Rows, black, N, Aux2, AllPieces).


get_all_pieces_row([], _, _, _, AllPiecesRow, AllPiecesRow) :- !.

get_all_pieces_row([Piece|Pieces], white, RowNumber, ColumnNumber, Aux, AllPiecesRow) :-
    Piece = w,
    !,
    N is ColumnNumber + 1,
    get_all_pieces_row(Pieces, white, RowNumber, N, [(RowNumber-ColumnNumber)|Aux], AllPiecesRow).

get_all_pieces_row([Piece|Pieces], white, RowNumber, ColumnNumber, Aux, AllPiecesRow) :-
    Piece = wk,
    !,
    N is ColumnNumber + 1,
    get_all_pieces_row(Pieces, white, RowNumber, N, [(RowNumber-ColumnNumber)|Aux], AllPiecesRow).

get_all_pieces_row([Piece|Pieces], white, RowNumber, ColumnNumber, Aux, AllPiecesRow) :-
    !,
    N is ColumnNumber + 1,
    get_all_pieces_row(Pieces, white, RowNumber, N, Aux, AllPiecesRow).

get_all_pieces_row([Piece|Pieces], black, RowNumber, ColumnNumber, Aux, AllPiecesRow) :-
    Piece = b,
    !,
    N is ColumnNumber + 1,
    get_all_pieces_row(Pieces, black, RowNumber, N, [(RowNumber-ColumnNumber)|Aux], AllPiecesRow).

get_all_pieces_row([Piece|Pieces], black, RowNumber, ColumnNumber, Aux, AllPiecesRow) :-
    Piece = bk,
    !,
    N is ColumnNumber + 1,
    get_all_pieces_row(Pieces, black, RowNumber, N, [(RowNumber-ColumnNumber)|Aux], AllPiecesRow).

get_all_pieces_row([Piece|Pieces], black, RowNumber, ColumnNumber, Aux, AllPiecesRow) :-
    N is ColumnNumber + 1,
    get_all_pieces_row(Pieces, black, RowNumber, N, Aux, AllPiecesRow).


get_best_moves(Board, Player, AllPieces, BestMoves) :-
    get_best_moves(Board, Player, AllPieces, [], BestMoves).

get_best_moves(Board, white, [], BestMoves, BestMoves) :- !.

get_best_moves(Board, white, [(PieceRow-PieceColumn)|Pieces], Aux, BestMoves) :- 
    test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'H'), PieceColumn, MoveH),
    test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'V'), PieceRow, MoveV),
    test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'D'), (PieceRow, PieceColumn), MoveD),
    append(MoveH, Aux, M1),
    append(MoveV, M1, M2),
    append(MoveD, M2, Moves),
    get_best_moves(Board, white, Pieces, Moves, BestMoves).

get_best_moves(Board, black, [], BestMoves, BestMoves) :- !.

get_best_moves(Board, black, [(PieceRow-PieceColumn)|Pieces], Aux, BestMoves) :- 
    test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'H'), PieceColumn, MoveH),
    test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'V'), PieceRow, MoveV),
    test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'D'), (PieceRow, PieceColumn), MoveD),
    append(MoveH, Aux, M1),
    append(MoveV, M1, M2),
    append(MoveD, M2, Moves),
    get_best_moves(Board, black, Pieces, Moves, BestMoves).


test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    PC2 is PC - 1,
    PC2 > 0,
    get_piece(Board, PieceRow, PC2, P),
    (P = w ; P = wk),
    !,
    test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'H'), PC2, Move).

test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    PC2 is PC - 1,
    PC2 > 0,
    get_piece(Board, PieceRow, PC2, P),
    (P = b ; P = bk),
    !,
    Move = [(PieceRow, PieceColumn, 'H', P)].

test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    Move = [].

test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    PR2 is PR - 1,
    PR2 > 0,
    get_piece(Board, PR2, PieceColumn, P),
    (P = w ; P = wk),
    !,
    test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'V'), PR2, Move).

test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    PR2 is PR - 1,
    PR2 > 0,
    get_piece(Board, PR2, PieceColumn, P),
    (P = b ; P = bk),
    !,
    Move = [(PieceRow, PieceColumn, 'V', P)].

test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    Move = [].

test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    PR2 is PR - 1,
    PR2 > 0,
    PC2 is PC - 1,
    PC2 > 0,
    get_piece(Board, PR2, PC2, P),
    (P = w ; P = wk),
    !,
    test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'D'), (PR2, PC2), Move).

test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    PR2 is PR - 1,
    PR2 > 0,
    PC2 is PC - 1,
    PC2 > 0,
    get_piece(Board, PR2, PC2, P),
    (P = b ; P = bk),
    !,
    Move = [(PieceRow, PieceColumn, 'D', P)].

test_destination(state(Board, white, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    Move = [].

test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    PC2 is PC + 1,
    PC2 < 9,
    get_piece(Board, PieceRow, PC2, P),
    (P = b ; P = bk),
    !,
    test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'H'), PC2, Move).

test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    PC2 is PC + 1,
    PC2 < 9,
    get_piece(Board, PieceRow, PC2, P),
    (P = w ; P = wk),
    !,
    Move = [(PieceRow, PieceColumn, 'H', P)].

test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'H'), PC, Move) :-
    Move = [].

test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    PR2 is PR + 1,
    PR2 < 9,
    get_piece(Board, PR2, PieceColumn, P),
    (P = b ; P = bk),
    !,
    test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'V'), PR2, Move).

test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    PR2 is PR + 1,
    PR2 < 9,
    get_piece(Board, PR2, PieceColumn, P),
    (P = w ; P = wk),
    !,
    Move = [(PieceRow, PieceColumn, 'V', P)].

test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'V'), PR, Move) :-
    Move = [].

test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    PR2 is PR + 1,
    PR2 < 9,
    PC2 is PC + 1,
    PC2 < 9,
    get_piece(Board, PR2, PC2, P),
    (P = b ; P = bk),
    !,
    test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'D'), (PR2, PC2), Move).

test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    PR2 is PR + 1,
    PR2 < 9,
    PC2 is PC + 1,
    PC2 < 9,
    get_piece(Board, PR2, PC2, P),
    (P = w ; P = wk),
    !,
    Move = [(PieceRow, PieceColumn, 'D', P)].

test_destination(state(Board, black, _), (PieceRow, PieceColumn, 'D'), (PR, PC), Move) :-
    Move = [].



/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
                            MOVE VALIDATION
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

calculate_destination(state(Board, white, _), (PieceRow, PieceColumn, 'H'), Piece, NewBoard) :-
    PC is PieceColumn - 1,
    PC > 0,
    get_piece(Board, PieceRow, PC, P),
    (P = w ; P = wk),
    !,
    calculate_destination(state(Board, white, _), (PieceRow, PC, 'H'), Piece, NewBoard).

calculate_destination(state(Board, white, _), (PieceRow, PieceColumn, 'H'), Piece, NewBoard) :-
    PC is PieceColumn - 1,
    PC > 0,
    get_piece(Board, PieceRow, PC, P),
    P = empty,
    !,
    PR is PieceRow - 1,
    PC2 is PC - 1,
    replace_element(Board, PR, PC2, Piece, NewBoard).

calculate_destination(state(Board, white, _), (PieceRow, PieceColumn, 'H'), Piece, NewBoard) :-
    PC is PieceColumn - 1,
    PC > 0,
    get_piece(Board, PieceRow, PC, P),
    (P = b ; P = bk),
    PR is PieceRow - 1,
    PC2 is PC - 1,
    replace_element(Board, PR, PC2, Piece, NewBoard).


calculate_destination(state(Board, white, _), (PieceRow, PieceColumn, 'V'), Piece, NewBoard) :-
    PR is PieceRow - 1,
    PR > 0,
    get_piece(Board, PR, PieceColumn, P),
    (P = w ; P = wk),
    !,
    calculate_destination(state(Board, white, _), (PR, PieceColumn, 'V'), Piece, NewBoard).

calculate_destination(state(Board, white, _), (PieceRow, PieceColumn, 'V'), Piece, NewBoard) :-
    PR is PieceRow - 1,
    PR > 0,
    get_piece(Board, PR, PieceColumn, P),
    P = empty,
    !,
    PR2 is PR - 1,
    PC is PieceColumn - 1,
    replace_element(Board, PR2, PC, Piece, NewBoard).

calculate_destination(state(Board, white, _), (PieceRow, PieceColumn, 'V'), Piece, NewBoard) :-
    PR is PieceRow - 1,
    PR > 0,
    get_piece(Board, PR, PieceColumn, P),
    (P = b ; P = bk),
    PR2 is PR - 1,
    PC is PieceColumn - 1,
    replace_element(Board, PR2, PC, Piece, NewBoard).


calculate_destination(state(Board, white, _), (PieceRow, PieceColumn, 'D'), Piece, NewBoard) :-
    PR is PieceRow - 1,
    PR > 0,
    PC is PieceColumn - 1,
    PC > 0,
    get_piece(Board, PR, PC, P),
    (P = w ; P = wk),
    !,
    calculate_destination(state(Board, white, _), (PR, PC, 'D'), Piece, NewBoard).

calculate_destination(state(Board, white, _), (PieceRow, PieceColumn, 'D'), Piece, NewBoard) :-
    PR is PieceRow - 1,
    PR > 0,
    PC is PieceColumn - 1,
    PC > 0,
    get_piece(Board, PR, PC, P),
    P = empty,
    !,
    PR2 is PR - 1,
    PC2 is PC - 1,
    replace_element(Board, PR2, PC2, Piece, NewBoard).

calculate_destination(state(Board, white, _), (PieceRow, PieceColumn, 'D'), Piece, NewBoard) :-
    PR is PieceRow - 1,
    PR > 0,
    PC is PieceColumn - 1,
    PC > 0,
    get_piece(Board, PR, PC, P),
    (P = b ; P = bk),
    PR2 is PR - 1,
    PC2 is PC - 1,
    replace_element(Board, PR2, PC2, Piece, NewBoard).

calculate_destination(state(Board, black, _), (PieceRow, PieceColumn, 'H'), Piece, NewBoard) :-
    PC is PieceColumn + 1,
    PC < 9,
    get_piece(Board, PieceRow, PC, P),
    (P = b ; P = bk),
    !,
    calculate_destination(state(Board, black, _), (PieceRow, PC, 'H'), Piece, NewBoard).

calculate_destination(state(Board, black, _), (PieceRow, PieceColumn, 'H'), Piece, NewBoard) :-
    PC is PieceColumn + 1,
    PC < 9,
    get_piece(Board, PieceRow, PC, P),
    P = empty,
    !,
    PR is PieceRow - 1, 
    PC2 is PC - 1,       
    replace_element(Board, PR, PC2, Piece, NewBoard).

calculate_destination(state(Board, black, _), (PieceRow, PieceColumn, 'H'), Piece, NewBoard) :-
    PC is PieceColumn + 1,
    PC < 9,
    get_piece(Board, PieceRow, PC, P),
    (P = w ; P = wk),
    PR is PieceRow - 1,  
    PC2 is PC - 1,       
    replace_element(Board, PR, PC2, Piece, NewBoard).

calculate_destination(state(Board, black, _), (PieceRow, PieceColumn, 'V'), Piece, NewBoard) :-
    PR is PieceRow + 1,
    PR < 9,
    get_piece(Board, PR, PieceColumn, P),
    (P = b ; P = bk),
    !,
    calculate_destination(state(Board, black, _), (PR, PieceColumn, 'V'), Piece, NewBoard).

calculate_destination(state(Board, black, _), (PieceRow, PieceColumn, 'V'), Piece, NewBoard) :-
    PR is PieceRow + 1,
    PR < 9,
    get_piece(Board, PR, PieceColumn, P),
    P = empty,
    !,
    PR2 is PR - 1,      
    PC is PieceColumn - 1,  
    replace_element(Board, PR2, PC, Piece, NewBoard).

calculate_destination(state(Board, black, _), (PieceRow, PieceColumn, 'V'), Piece, NewBoard) :-
    PR is PieceRow + 1,
    PR < 9,
    get_piece(Board, PR, PieceColumn, P),
    (P = w ; P = wk),
    PR2 is PR - 1,      
    PC is PieceColumn - 1,  
    replace_element(Board, PR2, PC, Piece, NewBoard).

calculate_destination(state(Board, black, _), (PieceRow, PieceColumn, 'D'), Piece, NewBoard) :-
    PR is PieceRow + 1,
    PR < 9,
    PC is PieceColumn + 1,
    PC < 9,
    get_piece(Board, PR, PC, P),
    (P = b ; P = bk),
    !,
    calculate_destination(state(Board, black, _), (PR, PC, 'D'), Piece, NewBoard).

calculate_destination(state(Board, black, _), (PieceRow, PieceColumn, 'D'), Piece, NewBoard) :-
    PR is PieceRow + 1,
    PR < 9,
    PC is PieceColumn + 1,
    PC < 9,
    get_piece(Board, PR, PC, P),
    P = empty,
    !,
    PR2 is PR - 1,    
    PC2 is PC - 1,    
    replace_element(Board, PR2, PC2, Piece, NewBoard).

calculate_destination(state(Board, black, _), (PieceRow, PieceColumn, 'D'), Piece, NewBoard) :-
    PR is PieceRow + 1,
    PR < 9,
    PC is PieceColumn + 1,
    PC < 9,
    get_piece(Board, PR, PC, P),
    (P = w ; P = wk),
    PR2 is PR - 1,    
    PC2 is PC - 1,    
    replace_element(Board, PR2, PC2, Piece, NewBoard).

validate_move(state(Board, white, GameConfig), (PieceRow, PieceColumn, Direction), newState(NewBoard, black, NewGameConfig)) :-
    get_piece(Board, PieceRow, PieceColumn, Piece),
    (Piece = w ; Piece = wk),
    count_kings(Board, black, CountBefore),
    calculate_destination(state(Board, white, _), (PieceRow, PieceColumn, Direction), Piece, NB),
    PR is PieceRow - 1,
    PC is PieceColumn - 1,
    replace_element(NB, PR, PC, empty, NewBoard),
    count_kings(NewBoard, black, CountAfter),
    verify_king_eaten(CountBefore, CountAfter, GameConfig, NewGameConfig).

validate_move(state(Board, black, GameConfig), (PieceRow, PieceColumn, Direction), newState(NewBoard, white, NewGameConfig)) :-
    get_piece(Board, PieceRow, PieceColumn, Piece),
    (Piece = b ; Piece = bk),
    count_kings(Board, white, CountBefore),
    calculate_destination(state(Board, black, _), (PieceRow, PieceColumn, Direction), Piece, NB),
    PR is PieceRow - 1,
    PC is PieceColumn - 1,
    replace_element(NB, PR, PC, empty, NewBoard),
    count_kings(NewBoard, white, CountAfter),
    verify_king_eaten(CountBefore, CountAfter, GameConfig, NewGameConfig).


count_kings(Board, Player, Count) :-
    count_kings(Board, Player, 1, 0, Count).

count_kings([], _, _, Count, Count) :- !.

count_kings([Row|Rows], white, RowNumber, Aux, Count) :-
    count_kings_row(Row, white, RowNumber, 1, 0, CountRow),
    N is RowNumber + 1,
    Aux2 is Aux + CountRow,
    count_kings(Rows, white, N, Aux2, Count).


count_kings([Row|Rows], black, RowNumber, Aux, Count) :-
    count_kings_row(Row, black, RowNumber, 1, 0, CountRow),
    N is RowNumber + 1,
    Aux2 is Aux + CountRow,
    count_kings(Rows, black, N, Aux2, Count).


count_kings_row([], _, _, _, CountRow, CountRow) :- !.

count_kings_row([Piece|Pieces], white, RowNumber, ColumnNumber, Aux, CountRow) :-
    Piece = wk,
    !,
    N is ColumnNumber + 1,
    Aux2 is Aux + 1,
    count_kings_row(Pieces, white, RowNumber, N, Aux2, CountRow).

count_kings_row([Piece|Pieces], white, RowNumber, ColumnNumber, Aux, CountRow) :-
    !,
    N is ColumnNumber + 1,
    count_kings_row(Pieces, white, RowNumber, N, Aux, CountRow).

count_kings_row([Piece|Pieces], black, RowNumber, ColumnNumber, Aux, CountRow) :-
    Piece = bk,
    !,
    N is ColumnNumber + 1,
    Aux2 is Aux + 1,
    count_kings_row(Pieces, black, RowNumber, N, Aux2, CountRow).

count_kings_row([Piece|Pieces], black, RowNumber, ColumnNumber, Aux, CountRow) :-
    N is ColumnNumber + 1,
    count_kings_row(Pieces, black, RowNumber, N, Aux, CountRow).


verify_king_eaten(CountBefore, CountAfter, GameConfig, NewGameConfig) :-
    CountBefore > CountAfter,
    !,
    NewGameConfig = gameOver.

verify_king_eaten(CountBefore, CountAfter, GameConfig, GameConfig).