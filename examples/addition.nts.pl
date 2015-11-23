r7 :- r6(A,B,C,D,E,F),D = (F + E).
false :- r6(A,B,C,D,E,F),D > (F + E).
false :- r6(A,B,C,D,E,F),D < (F + E).
r5(A,B,C,D,E,F) :- r4(A,B,C,G,H,I),(F >= 0), (E >= 0).
r4(A,B,C,D,E,F) :- A = D, B = E, C = F.
r3(A,B,C,D,E,F) :- r2(A,C,B,G,H,I),D = I, H = F, I = E.
r2(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),H = 0, G = D, H = E, I = F.
r1(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),H > 0, G = D, H = E, I = F.
r1(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),H < 0, G = D, H = E, I = F.
r3(A,B,C,D,E,F) :- r1(A,C,B,G,H,I),J = (I + 1), K = (H - 1),r3(L,J,K,M,N,O),M = D,H = F, I = E.
r0(A,B,C,D,E,F) :- r1(G,H,I,J,K,L),C = (L + 1), B = (K - 1),A = D, C = F, B = E.
r6(A,B,C,D,E,F) :- r5(A,B,C,G,H,I),J = I, K = H,r3(L,J,K,M,N,O),M = D,H = E, I = F.
r0(A,B,C,D,E,F) :- r5(G,H,I,J,K,L),C = L, B = K,A = D, C = F, B = E.
