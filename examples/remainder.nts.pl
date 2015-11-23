r7 :- r6(A,B,C,D),C < 1.
r7 :- r6(A,B,C,D),C > 1.
false :- r6(A,B,C,D),C = 1.
r5(A,B,C,D) :- r4(A,B,E,F),(D >= 0).
r4(A,B,C,D) :- A = C, B = D.
r3(A,B,C,D,E,F) :- r2(A,B,C,G,H,I),D = H, H = E, I = F.
r2(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),(H < I), G = D, H = E, I = F.
r1(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),(H >= I), G = D, H = E, I = F.
r3(A,B,C,D,E,F) :- r1(A,B,C,G,H,I),J = (H - I), K = I,r3(L,J,K,M,N,O),M = D,H = E, I = F.
r0(A,B,C,D,E,F) :- r1(G,H,I,J,K,L),B = (K - L), C = L,A = D, B = E, C = F.
r6(A,B,C,D) :- r5(A,B,E,F),G = F, H = 2,r3(I,G,H,J,K,L),J = C,F = D.
r0(A,B,C,D,E,F) :- r5(G,H,I,J),B = J, C = 2,A = D, B = E, C = F.
