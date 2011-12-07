:- module ttt.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module int, char, string, list.

:- type position ---> {int,int}.

:- type player ---> 'X' ; 'O'.

:- type ttt_board ---> board( x_stones  :: list(position),
                              o_stones  :: list(position),
                              count     :: int ).

:- type input ---> move(position) ; err ; exit.

:- func board_to_string(ttt_board) = string.
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

:- pred game(player::in, player::out, ttt_board::in, ttt_board::out, io::di, io::uo) is cc_multi.
game(Pi,Po,Bi,Bo,!IO) :-
    input(Pi,M,!IO),
    (
      M = move(P),
      (
      	Pi = 'X',
      	Bn = Bi^x_stones := [P|Bi^x_stones],
      	output('O',Po,Bn,Bo,!IO)
      ;
      	Pi = 'O',
      	Bn = Bi^o_stones := [P|Bi^o_stones],
      	output('X',Po,Bn,Bo,!IO)
      )
    ;
      M = err,
      err(Pi,Po,Bi,Bo,!IO)
    ;
      M = exit,
      quit(Pi,Po,Bi,Bo,!IO)
    ).

:- pred output(player::in, player::out, ttt_board::in, ttt_board::out, io::di, io::uo) is cc_multi.
output(!P,Bi,Bo,!IO) :-
  S = board_to_string(Bi),
  io.write_string(S ++ "\n",!IO),
  game(!P,Bi,Bo,!IO).

:- pred err(player::in, player::out, ttt_board::in, ttt_board::out, io::di, io::uo) is cc_multi.
err(!P,!B,!IO) :-
    io.write_string("enter number between 1 and 9\n",!IO),
    game(!P,!B,!IO).

:- pred quit(player::in, player::out, ttt_board::in, ttt_board::out, io::di, io::uo) is det.
quit(Pi,Po,Bi,Bo,!IO) :-
    io.write_string("\n",!IO),
    Po = Pi, Bo = Bi.

main(!IO) :-
    B0 = board([],[],0),
    output('X',_,B0,_,!IO).
