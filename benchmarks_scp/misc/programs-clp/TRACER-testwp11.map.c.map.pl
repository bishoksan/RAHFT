new14(A,B,C,D,E,F,G,H) :- A=0.
new11(A,B,C,D,E,F,G) :- H=0, B>=6, new10(A,B,C,D,E,F,H).
new11(A,B,C,D,E,F,G) :- H=1, B=<5, new10(A,B,C,D,E,F,H).
new10(A,B,C,D,E,F,G) :- new14(G,A,B,C,D,E,F,G).
new8(A,B,C,D,E,F,G) :- H=0, A>=11, new10(A,B,C,D,E,F,H).
new8(A,B,C,D,E,F,G) :- A=<10, new11(A,B,C,D,E,F,G).
new6(A,B,C,D,E,F,G) :- H=5, D>=1, new8(A,B,H,D,E,F,G).
new6(A,B,C,D,E,F,G) :- H=6, D=<0, new8(A,B,H,D,E,F,G).
new4(A,B,C,D,E,F,G) :- H=4, F>=1, new6(H,B,C,D,E,F,G).
new4(A,B,C,D,E,F,G) :- H=5, F=<0, new6(H,B,C,D,E,F,G).
new3(A,B,C,D,E,F,G) :- H=2, E>=1, new4(A,H,C,D,E,F,G).
new3(A,B,C,D,E,F,G) :- H=3, E=<0, new4(A,H,C,D,E,F,G).
new2 :- new3(A,B,C,D,E,F,G).
new1 :- new2.
false :- new1.
