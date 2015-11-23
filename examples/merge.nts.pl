r12 :- r11(A,B,C,D,E,F),D = (F + E).
false :- r11(A,B,C,D,E,F),D > (F + E).
false :- r11(A,B,C,D,E,F),D < (F + E).
r10(A,B,C,D,E,F) :- r9(A,B,C,G,H,I),(F >= 0), (E >= 0), G = D.
r9(A,B,C,D,E,F) :- A = D, B = E, C = F.
r4(A,B,C,D,E,F) :- r8(A,G,C,H,B,I,J,K,L,M),D = (1 + J), J = N, K = F, L = O, M = E.
r4(A,B,C,D,E,F) :- r7(A,G,C,H,B,I,J,K,L,M),D = (1 + J), J = N, K = F, L = O, M = E.
r6(A,B,C,D,E,F,G,H,I,J) :- r3(A,B,C,D,E,K,L,M,N,O),(N < 0), K = F, L = G, M = H, N = I, O = J.
r5(A,B,C,D,E,F,G,H,I,J) :- r3(A,B,C,D,E,K,L,M,N,O),(N >= 0), K = F, L = G, M = H, N = I, O = J.
r4(A,B,C,D,E,F) :- r2(A,G,C,H,B,I,J,K,L,M),D = M, J = N, K = F, L = O, M = E.
r4(A,B,C,D,E,F) :- r1(A,G,C,H,B,I,J,K,L,M),D = K, J = N, K = F, L = O, M = E.

r3(A,B,C,D,E,F,G,H,I,J) :- r0(A,B,C,D,E,K,L,M,N,O),O < 0, M > 0, K = F, L = G, M = H, O = J.
r3(A,B,C,D,E,F,G,H,I,J) :- r0(A,B,C,D,E,K,L,M,N,O),O > 0, M < 0, K = F, L = G, M = H, O = J.
r3(A,B,C,D,E,F,G,H,I,J) :- r0(A,B,C,D,E,K,L,M,N,O),O < 0, M < 0, K = F, L = G, M = H, O = J.
r3(A,B,C,D,E,F,G,H,I,J) :- r0(A,B,C,D,E,K,L,M,N,O),O > 0, M > 0, K = F, L = G, M = H, O = J.

r2(A,B,C,D,E,F,G,H,I,J) :- r0(A,B,C,D,E,K,L,M,N,O),M = 0, K = F, L = G, M = H, N = I, O = J.
r1(A,B,C,D,E,F,G,H,I,J) :- r0(A,B,C,D,E,K,L,M,N,O),O = 0, K = F, L = G, M = H, N = I, O = J.
r7(A,B,C,D,E,F,G,H,I,J) :- r5(A,B,C,D,E,K,L,M,N,O),P = (O - 1), Q = M,r4(R,P,Q,S,T,U),S = G,K = F, M = H, N = I, O = J.
r0(A,B,C,D,E,F,G,H,I,J) :- r5(K,L,M,N,O,P,Q,R,S,T),E = (T - 1), C = R,A = F, E = J, C = H.
r8(A,B,C,D,E,F,G,H,I,J) :- r6(A,B,C,D,E,K,L,M,N,O),P = O, Q = (M - 1),r4(R,P,Q,S,T,U),S = G,K = F, M = H, N = I, O = J.
r0(A,B,C,D,E,F,G,H,I,J) :- r6(K,L,M,N,O,P,Q,R,S,T),E = T, C = (R - 1),A = F, E = J, C = H.
r11(A,B,C,D,E,F) :- r10(A,B,C,G,H,I),J = I, K = H,r4(L,J,K,M,N,O),M = D,H = E, I = F.
r0(A,B,C,D,E,F,G,H,I,J) :- r10(K,L,M,N,O,P),E = P, C = O,A = F, E = J, C = H.
