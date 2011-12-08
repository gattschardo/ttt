:- module ttt.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module int, char, string, list.

:- type position ---> {int,int}.

:- type player ---> 'X' ; 'O'.

:- type board ---> board( x_stones  :: list(position),
                          o_stones  :: list(position),
                          count     :: int ).

:- type input ---> move(position) ; err ; exit.

:- func board_to_string(board) = string.
board_to_string(board(Xs,Os,_)) = S :-
    SepLine = "-+-+-\n",
    S = line(1,Xs,Os) ++ SepLine ++ line(2,Xs,Os) ++ SepLine ++ line(3,Xs,Os).

:- func line(int,list(position),list(position)) = string.
line(Line,Xs,Os) = S :-
    Sep = "|",
    S = findchar({Line,1},Xs,Os) ++ Sep
     ++ findchar({Line,2},Xs,Os) ++ Sep
     ++ findchar({Line,3},Xs,Os) ++ "\n".

:- func findchar(position,list(position),list(position)) = string.
findchar(I,[X|Xs],Os) = (if X = I then "X" else findchar(I,Xs,Os)).
findchar(I,[],[O|Os]) = (if O = I then "O" else findchar(I,[],Os)).
findchar(_,[],[]) = " ".

:- func ptc(player) = char.
ptc('X') = 'X'.
ptc('O') = 'O'.

:- func toggle(player) = player.
toggle('X') = 'O'.
toggle('O') = 'X'.

:- pred insert(position::in, player::in, board::in, board::out) is semidet.
insert(Pos,Player,Bi,Bo) :-
    Bi = board(Xs,Os,C),
    not list.member(Pos,Xs),
    not list.member(Pos,Os),
    (
      Player = 'X',
      Bn = Bi^x_stones := [Pos|Bi^x_stones]
    ;
      Player = 'O',
      Bn = Bi^o_stones := [Pos|Bi^o_stones]
    ),
    Bo = Bn^count := C + 1.

:- pred input(player::in, input::out, io::di, io::uo) is cc_multi.
input(P,M,!IO) :-
    io.format("%c> ",[c(ptc(P))],!IO),
    io.read_line_as_string(R,!IO),
    (
        R = ok(S),
        string.to_int(string.strip(S), I),
        I > 0, I < 10,
      	Y = 1 + (I - 1) mod 3, X = 1 + (I - 1) / 3,
        M = move({X,Y})
    ;
        R = eof,
        M = exit
    ;
        M = err
    ).

:- pred won(board::in, player::out) is nondet.
won(board(Xs,Os,_),P) :-
    (
      won(Xs), P = 'X'
    ;
      won(Os), P = 'O'
    ).

:- pred won(list(position)::in) is semidet.
won(L) :-
    (
      list.member({K,1},L),
      list.member({K,2},L),
      list.member({K,3},L)
    ;
      list.member({1,K},L),
      list.member({2,K},L),
      list.member({3,K},L)
    ;
      list.member({1,1},L),
      list.member({2,2},L),
      list.member({3,3},L)
    ;
      list.member({1,3},L),
      list.member({2,2},L),
      list.member({3,1},L)
    ).

:- pred play(position::in, player::in, board::in, io::di, io::uo) is cc_multi.
play(P,Pi,Bi,!IO) :-
    ( if
      insert(P,Pi,Bi,Bn)
    then
      ( if
        won(Bn,Pw)
      then
        win(Pw,!IO)
      else
      ( if
          board(_,_,9) = Bn
        then
          lose(!IO)
        else
          Pn = toggle(Pi),
          output(Pn,Bn,!IO)
        )
      )
    else
      used(P,Pi,Bi,!IO)
    ).

:- pred game(player::in, board::in, io::di, io::uo) is cc_multi.
game(Pi,Bi,!IO) :-
    input(Pi,M,!IO),
    (
      M = move(P),
      play(P,Pi,Bi,!IO)
    ;
      M = err,
      err(Pi,Bi,!IO)
    ;
      M = exit,
      quit(!IO)
    ).

:- pred lose(io::di, io::uo) is det.
lose(!IO) :-
    io.write_string("game is a draw\n",!IO).

:- pred win(player::in, io::di, io::uo) is det.
win(P,!IO) :-
    io.format("player %c wins!\n",[c(ptc(P))],!IO).

:- pred output(player::in, board::in, io::di, io::uo) is cc_multi.
output(P,Bi,!IO) :-
  S = board_to_string(Bi),
  io.write_string(S ++ "\n",!IO),
  game(P,Bi,!IO).

:- pred err(player::in, board::in, io::di, io::uo) is cc_multi.
err(P,B,!IO) :-
    io.write_string("enter number between 1 and 9\n",!IO),
    game(P,B,!IO).

:- pred used(position::in, player::in, board::in, io::di, io::uo) is cc_multi.
used({X,Y},P,B,!IO) :-
    io.format("field %d/%d is used!\n",[i(X),i(Y)],!IO),
    game(P,B,!IO).

:- pred quit(io::di, io::uo) is det.
quit(!IO) :-
    io.write_string("\n",!IO).

main(!IO) :-
    B0 = board([],[],0),
    output('X',B0,!IO).
