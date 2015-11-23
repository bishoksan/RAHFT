r9 :- r8(A,B,C,D),C = 3*D.
false :- r8(A,B,C,D),C < 3*D.
false :- r8(A,B,C,D),C > 3*D.
r7(A,B,C,D) :- r6(A,B,E,F),(D >= 0).
r6(A,B,C,D) :- A = C, B = D.
r5(A,B,C,D) :- r4(A,E,B,F,G,H),C = (2 * G), G = I, H = D.
r5(A,B,C,D) :- r3(A,E,B,F,G,H),C = (3 + (2 * G)), G = I, H = D.
r5(A,B,C,D) :- r1(A,E,B,F,G,H),C = 0, G = I, H = D.
r4(A,B,C,D,E,F) :- r2(A,B,C,G,H,I), G = D, H = E, I = F.
r3(A,B,C,D,E,F) :- r2(A,B,C,G,H,I), G = D, H = E, I = F.
r1(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),I = 0, G = D, H = E, I = F.
r2(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),2*J = I,r5(K,J,L,M),L = E,G = D, I = F.
r0(A,B,C,D,E,F) :- r0(G,H,I,J,K,L),2*C = L,A = D, C = F.
r8(A,B,C,D) :- r7(A,B,E,F),G = F,r5(H,G,I,J),I = C,F = D.
r0(A,B,C,D,E,F) :- r7(G,H,I,J),C = J,A = D, C = F.
