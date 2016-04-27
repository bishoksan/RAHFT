new13(A,B,C,D,E,F):-A=0.
new8(A,B,C,D,E):-F=1, D=0, new7(A,B,C,D,F).
new8(A,B,C,D,E):-F=0, D=< -1, new7(A,B,C,D,F).
new8(A,B,C,D,E):-F=0, D>=1, new7(A,B,C,D,F).
new7(A,B,C,D,E):-F=:=E, new13(F,A,B,C,D,E).
new6(A,B,C,D,E):-F=1, 2*B=:=D, new7(A,B,C,D,F).
new6(A,B,C,D,E):-2*B-D>=1, new8(A,B,C,D,E).
new6(A,B,C,D,E):-2*B-1*D=< -1, new8(A,B,C,D,E).
new4(A,B,C,D,E):-F=1+A, G=2+D, A-B=<0, new4(F,B,C,G,E).
new4(A,B,C,D,E):-A-B>=1, new6(A,B,C,D,E).
new3(A,B,C,D,E):-F=1, G=:=H, I=0, new4(F,H,G,I,E).
new2:-new3(A,B,C,D,E).
new1:-new2.
false:-new1.
