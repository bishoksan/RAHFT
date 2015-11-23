r12 :- r11(A,B,C,D),C < 0.
r12 :- r11(A,B,C,D),C > 0.
false :- r11(A,B,C,D),C = 0.
r10(A,B,C,D) :- r9(A,B,E,F),(D > 0).
r9(A,B,C,D) :- A = C, B = D.
r8(A,B,C,D) :- r2(A,B,E,F,G,H,I,J),C = 1, H = D, I = K, J = L.
r8(A,B,C,D) :- r7(A,B,E,F,G,H,I,J),C = (G + I), H = D, I = K, J = L.
r6(A,B,C,D,E,F,G,H) :- r5(A,B,C,D,I,J,K,L),(H > 0), (H < J), I = E, J = F, K = G.
r5(A,B,C,D,E,F,G,H) :- r4(A,B,C,D,I,J,K,L),E = (I + K), J = F, K = G, L = H.
r3(A,B,C,D,E,F,G,H) :- r1(A,B,C,D,I,J,K,L),E = 0, J = F, K = G, L = H.
r2(A,B,C,D,E,F,G,H) :- r0(A,B,C,D,I,J,K,L),(J =< 5), I = E, J = F, K = G, L = H.
r1(A,B,C,D,E,F,G,H) :- r0(A,B,C,D,I,J,K,L),(J > 5), I = E, J = F, K = G, L = H.
r4(A,B,C,D,E,F,G,H) :- r3(A,B,C,D,I,J,K,L),10*M = J,r8(N,M,O,P),O = G,I = E, J = F, L = H.
r0(A,B,C,D,E,F,G,H) :- r3(I,J,K,L,M,N,O,P),10*B = N ,A = E, B = F.
r7(A,B,C,D,E,F,G,H) :- r6(A,B,C,D,I,J,K,L),M = L,r8(N,M,O,P),O = G,I = E, J = F, L = H.
r0(A,B,C,D,E,F,G,H) :- r6(I,J,K,L,M,N,O,P),B = P,A = E, B = F.
r11(A,B,C,D) :- r10(A,B,E,F),G = F,r8(H,G,I,J),I = C,F = D.
r0(A,B,C,D,E,F,G,H) :- r10(I,J,K,L),B = L,A = E, B = F.
