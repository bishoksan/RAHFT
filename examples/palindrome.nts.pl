r8 :- r7(A,B,C,D,E,F),2*D =< 4 + (E - F).
false :- r7(A,B,C,D,E,F),2*D > 4 + (E - F).
r6(A,B,C,D,E,F) :- r5(A,B,C,G,H,I),(F >= 0), (E >= 0), (E >= F).
r5(A,B,C,D,E,F) :- A = D, B = E, C = F.
r4(A,B,C,D,E,F) :- r2(A,C,B,G,H,I,J,K),D = 1, I = F, J = E, K = L.
r4(A,B,C,D,E,F) :- r3(A,C,B,G,H,I,J,K),D = (1 + K), I = F, J = E, K = L.
r2(A,B,C,D,E,F,G,H) :- r0(A,B,C,D,I,J,K,L),(K >= J), I = E, J = F, K = G, L = H.
r1(A,B,C,D,E,F,G,H) :- r0(A,B,C,D,I,J,K,L),(K < J), I = E, J = F, K = G, L = H.
r3(A,B,C,D,E,F,G,H) :- r1(A,B,C,D,I,J,K,L),M = (K + 1), N = (J - 1),r4(O,M,N,P,Q,R),P = H,I = E, J = F, K = G.
r0(A,B,C,D,E,F,G,H) :- r1(I,J,K,L,M,N,O,P),C = (O + 1), B = (N - 1),A = E, C = G, B = F.
r7(A,B,C,D,E,F) :- r6(A,B,C,G,H,I),J = I, K = H,r4(L,J,K,M,N,O),M = D,H = E, I = F.
r0(A,B,C,D,E,F,G,H) :- r6(I,J,K,L,M,N),C = N, B = M,A = E, C = G, B = F.
