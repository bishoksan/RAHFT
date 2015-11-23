r11 :- r10(A,B,C,D,E,F),D = 1.
false :- r10(A,B,C,D,E,F),D > 1.
false :- r10(A,B,C,D,E,F),D < 1.
r9(A,B,C,D,E,F) :- r8(A,B,C,G,H,I),(F >= 0), (E >= 0), G = D.
r8(A,B,C,D,E,F) :- A = D, B = E, C = F.
r7(A,B,C,D,E,F,G,H,I,J) :- r5(A,B,C,D,E,K,L,M,N,O),(N < 0), K = F, L = G, M = H, N = I, O = J.
r6(A,B,C,D,E,F,G,H,I,J) :- r5(A,B,C,D,E,K,L,M,N,O),(N >= 0), K = F, L = G, M = H, N = I, O = J.
r5(A,B,C,D,E,F,G,H,I,J) :- r3(A,B,C,D,E,K,L,M,N,O),K = F, L = G, M = H, O = J.
r4(A,B,C,D,E,F) :- r2(B,A,C,G,H,I,J,K,L,M),D = 1, I = E, K = F, L = N, M = O.
r3(A,B,C,D,E,F,G,H,I,J) :- r1(A,B,C,D,E,K,L,M,N,O),2*J = K + M, K = F, L = G, M = H, N = I.
r2(A,B,C,D,E,F,G,H,I,J) :- r0(A,B,C,D,E,K,L,M,N,O),(M < K), K = F, M = H, N = I, O = J.
r1(A,B,C,D,E,F,G,H,I,J) :- r0(A,B,C,D,E,K,L,M,N,O),(M >= K), K = F, M = H, N = I, O = J.
r4(A,B,C,D,E,F) :- r6(B,A,C,G,H,I,J,K,L,M),N = I, O = (M - 1),r4(P,N,O,Q,R,S),Q = D,I = E, K = F, L = T, M = U.
r0(A,B,C,D,E,F,G,H,I,J) :- r6(K,L,M,N,O,P,Q,R,S,T),A = P, C = (T - 1),B = G, A = F, C = H.
r4(A,B,C,D,E,F) :- r7(B,A,C,G,H,I,J,K,L,M),N = (M + 1), O = K,r4(P,N,O,Q,R,S),Q = D,I = E, K = F, L = T, M = U.
r0(A,B,C,D,E,F,G,H,I,J) :- r7(K,L,M,N,O,P,Q,R,S,T),A = (T + 1), C = R,B = G, A = F, C = H.
r10(A,B,C,D,E,F) :- r9(A,B,C,G,H,I),J = I, K = H,r4(L,J,K,M,N,O),M = D,H = E, I = F.
r0(A,B,C,D,E,F,G,H,I,J) :- r9(K,L,M,N,O,P),A = P, C = O,B = G, A = F, C = H.
