r9 :- r8(A,B,C,D,E,F),(D =< F).
false :- r8(A,B,C,D,E,F),(D > F).
r7(A,B,C,D,E,F) :- r6(A,B,C,G,H,I),(F >= 0), (E >= 0), (E =< F).
r6(A,B,C,D,E,F) :- A = D, B = E, C = F.
r5(A,B,C,D,E,F) :- r2(A,B,G,H,C,I,J,K,L,M),D = 0, J = E, K = N, L = O, M = F.
r5(A,B,C,D,E,F) :- r4(A,B,G,H,C,I,J,K,L,M),D = ((K + L) + 1), J = E, K = N, L = O, M = F.
r2(A,B,C,D,E,F,G,H,I,J) :- r0(A,B,C,D,E,K,L,M,N,O),(2*O > L ), K = F, L = G, M = H, N = I, O = J.
r1(A,B,C,D,E,F,G,H,I,J) :- r0(A,B,C,D,E,K,L,M,N,O),(2*O =< L ), K = F, L = G, M = H, N = I, O = J.
r3(A,B,C,D,E,F,G,H,I,J) :- r1(A,B,C,D,E,K,L,M,N,O),P = L, Q = (2 * O),r5(R,P,Q,S,T,U),S = H,K = F, L = G, N = I, O = J.
r0(A,B,C,D,E,F,G,H,I,J) :- r1(K,L,M,N,O,P,Q,R,S,T),B = Q, E = (2 * T),A = F, B = G, E = J.
r4(A,B,C,D,E,F,G,H,I,J) :- r3(A,B,C,D,E,K,L,M,N,O),P = L, Q = ((2 * O) + 1),r5(R,P,Q,S,T,U),S = I,K = F, L = G, M = H, O = J.
r0(A,B,C,D,E,F,G,H,I,J) :- r3(K,L,M,N,O,P,Q,R,S,T),B = Q, E = ((2 * T) + 1),A = F, B = G, E = J.
r8(A,B,C,D,E,F) :- r7(A,B,C,G,H,I),J = I, K = H,r5(L,J,K,M,N,O),M = D,H = E, I = F.
r0(A,B,C,D,E,F,G,H,I,J) :- r7(K,L,M,N,O,P),B = P, E = O,A = F, B = G, E = J.
