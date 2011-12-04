:- module ttt.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module int, string, list.

:- type ttt_board ---> board( x_stones  :: list(int),
                              o_stones  :: list(int),
                              count     :: int ).

:- type input ---> move(int) ; err ; exit.

:- func board_to_string(ttt_board) = string.
board_to_string(board(Xs,Os,_)) = S :-
    SepLine = "-+-+-\n",
    S = line(0,Xs,Os) ++ SepLine ++ line(3,Xs,Os) ++ SepLine ++ line(6,Xs,Os).

:- func line(int,list(int),list(int)) = string.
line(Offset,Xs,Os) = S :-
    Sep = "|",
    S = findchar(Offset+1,Xs,Os) ++ Sep ++ findchar(Offset+2,Xs,Os) ++ Sep ++ findchar(Offset+3,Xs,Os) ++ "\n".

:- func findchar(int,list(int),list(int)) = string.
findchar(I,[X|Xs],Os) = (if X = I then "X" else findchar(I,Xs,Os)).
findchar(I,[],[O|Os]) = (if O = I then "O" else findchar(I,[],Os)).
findchar(_,[],[]) = " ".

:- pred input(input::out, io::di, io::uo) is cc_multi.
input(X,IOin,IOout) :-
    io.write_string("X> ",IOin,IO1),
    io.read_line_as_string(R,IO1,IOout),
    (
        R = ok(S),
        string.to_int(string.strip(S), Y),
        Y > 0, Y < 10,
        X = move(Y)%,
        %IO2 = IOout
    ;
        R = eof,
        %IO2 = IOout,%io.write_string("\n",IO2,IOout),
        X = exit
    ;
        %R = error(_),
        %IO2 = IOout,%io.write_string("Error occured!\n",IO2,IOout),
        X = err
    %;
        %io.write_string("enter number between 1 and 9\n",IO2,IO3),
        %IO2 = IOout,
        %X = err.
        %input(X,IO3,IOout)
    ).

:- pred game(ttt_board::in, ttt_board::out, io::di, io::uo) is cc_multi.
game(Bi,Bo,!IO) :-
    input(I,!IO),
    (
      I = move(X),
      Bn = Bi^x_stones := [X|Bi^x_stones],
      output(Bn,Bo,!IO)
    ;
      I = err,
      err(Bi,Bo,!IO)
    ;
      I = exit,
      quit(Bi,Bo,!IO)
    ).

:- pred output(ttt_board::in, ttt_board::out, io::di, io::uo) is cc_multi.
output(Bi,Bo,!IO) :-
  S = board_to_string(Bi),
  io.write_string(S ++ "\n",!IO),
  game(Bi,Bo,!IO).

:- pred err(ttt_board::in, ttt_board::out, io::di, io::uo) is cc_multi.
err(!B,!IO) :-
    io.write_string("enter number between 1 and 9\n",!IO),
    game(!B,!IO).

:- pred quit(ttt_board::in, ttt_board::out, io::di, io::uo) is det.
quit(Bi,Bo,!IO) :-
    io.write_string("\n",!IO),
    Bo = Bi.

main(!IO) :-
    B0 = board([],[],0),
    output(B0,_,!IO).
