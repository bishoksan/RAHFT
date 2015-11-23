r10 :- r9(A,B,C,D),D = C.
false :- r9(A,B,C,D),D > C.
false :- r9(A,B,C,D),D < C.
r8(A,B,C,D) :- r7(A,B,E,F),(D >= 0).
r7(A,B,C,D) :- A = C, B = D.
r2(A,B,C,D) :- r6(A,E,B,F,G,H,I,J),C = (J + H), H = K, I = D, J = L.
r4(A,B,C,D,E,F,G,H) :- r0(A,B,C,D,I,J,K,L),(K > 1), I = E, J = F, K = G, L = H.
r2(A,B,C,D) :- r3(A,E,B,F,G,H,I,J),C = 1, H = K, I = D, J = L.
r3(A,B,C,D,E,F,G,H) :- r0(A,B,C,D,I,J,K,L),K = 1, I = E, J = F, K = G, L = H.
r2(A,B,C,D) :- r1(A,E,B,F,G,H,I,J),C = 0, H = K, I = D, J = L.
r1(A,B,C,D,E,F,G,H) :- r0(A,B,C,D,I,J,K,L),K = 0, I = E, J = F, K = G, L = H.
r5(A,B,C,D,E,F,G,H) :- r4(A,B,C,D,I,J,K,L),2*M = K  + 2,r2(N,M,O,P),O = H,I = E, J = F, K = G.
r0(A,B,C,D,E,F,G,H) :- r4(I,J,K,L,M,N,O,P),2*C = O  + 2,A = E, C = G.
r6(A,B,C,D,E,F,G,H) :- r5(A,B,C,D,I,J,K,L),2*M = K,r2(N,M,O,P),O = F,I = E, K = G, L = H.
r0(A,B,C,D,E,F,G,H) :- r5(I,J,K,L,M,N,O,P),2*C = O ,A = E, C = G.
r9(A,B,C,D) :- r8(A,B,E,F),G = F,r2(H,G,I,J),I = C,F = D.
r0(A,B,C,D,E,F,G,H) :- r8(I,J,K,L),C = L,A = E, C = G.
