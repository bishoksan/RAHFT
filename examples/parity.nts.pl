r11 :- r10(A,B,C,D),C > 0.
r11 :- r10(A,B,C,D),C < 0.
false :- r10(A,B,C,D),C = 0.
r9(A,B,C,D) :- r8(A,B,E,F),(D >= 0).
r8(A,B,C,D) :- A = C, B = D.
r7(A,B,C,D) :- r4(A,B,E,F),(F > 0), E = C, F = D.
r6(A,B,C,D) :- r5(A,B,E,F),C = 1, F = D.
r5(A,B,C,D) :- r4(A,B,E,F),F = 0, E = C, F = D.
r3(A,B,C,D) :- r0(A,B,E,F),(F > 0), E = C, F = D.
r2(A,B,C,D) :- r1(A,B,E,F),C = 0, F = D.
r1(A,B,C,D) :- r0(A,B,E,F),F = 0, E = C, F = D.
r2(A,B,C,D) :- r3(A,B,E,F),G = (F - 1),r6(H,G,I,J),I = C,F = D.
r4(A,B,C,D) :- r3(E,F,G,H),B = (H - 1),A = C, B = D.
r6(A,B,C,D) :- r7(A,B,E,F),G = (F - 1),r2(H,G,I,J),I = C,F = D.
r0(A,B,C,D) :- r7(E,F,G,H),B = (H - 1),A = C, B = D.
r10(A,B,C,D) :- r9(A,B,E,F),G = F,r6(H,G,I,J),I = C,F = D.
r4(A,B,C,D) :- r9(E,F,G,H),B = H,A = C, B = D.
