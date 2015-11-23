false :- r7(A,B,C,D),C < (D + 1).
false :- r7(A,B,C,D),C > (D + 1).
r8 :- r7(A,B,C,D),C = (D + 1), C = E, D = F.
r6(A,B,C,D) :- r5(A,B,E,F),(D >= 0), E = C.
r5(A,B,C,D) :- A = C, B = D.
r4(A,B,C,D) :- r2(A,B,E,F,G,H),C = 1, G = D, H = I.
r4(A,B,C,D) :- r3(A,B,E,F,G,H),C = (H + 1), G = D, H = I.
r2(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),H =< 0, G = D, H = E, I = F.
r1(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),(H > 0), G = D, H = E, I = F.
r3(A,B,C,D,E,F) :- r1(A,B,C,G,H,I),J = (H - 1),r4(K,J,L,M),L = F,G = D, H = E.
r0(A,B,C,D,E,F) :- r1(G,H,I,J,K,L),B = (K - 1),A = D, B = E.
r7(A,B,C,D) :- r6(A,B,E,F),G = F,r4(H,G,I,J),I = C,F = D.
r0(A,B,C,D,E,F) :- r6(G,H,I,J),B = J,A = D, B = E.
