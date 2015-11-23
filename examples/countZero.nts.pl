r11 :- r10(A,B,C,D),(C =< D).
false :- r10(A,B,C,D),(C > D).
r9(A,B,C,D) :- r8(A,B,E,F),(D > 0).
r8(A,B,C,D) :- A = C, B = D.
r4(A,B,C,D) :- r3(A,E,B,F,G,H),C = 1, G = I, H = D.
r4(A,B,C,D) :- r7(A,E,B,F,G,H),C = (G + 1), G = I, H = D.
r6(A,B,C,D,E,F) :- r2(A,B,C,G,H,I), H = E, I = F.
r5(A,B,C,D,E,F) :- r2(A,B,C,G,H,I), G = D, H = E, I = F.
r4(A,B,C,D) :- r1(A,E,B,F,G,H),C = 0, G = I, H = D.
r3(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),I = 0, G = D, H = E, I = F.
r3(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),I = 10, G = D, H = E, I = F.
r2(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),(I > 10), G = D, H = E, I = F.
r1(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),(I < 10), I > 0, G = D, H = E, I = F.
r1(A,B,C,D,E,F) :- r0(A,B,C,G,H,I),(I < 10), I < 0, G = D, H = E, I = F.
r7(A,B,C,D,E,F) :- r5(A,B,C,G,H,I),10*J = I ,r4(K,J,L,M),L = E,G = D, I = F.
r0(A,B,C,D,E,F) :- r5(G,H,I,J,K,L),10*C = L ,A = D, C = F.
r4(A,B,C,D) :- r6(A,E,B,F,G,H),10*I = H ,r4(J,I,K,L),K = C,G = M, H = D.
r0(A,B,C,D,E,F) :- r6(G,H,I,J,K,L),10*C = L,A = D, C = F.
r10(A,B,C,D) :- r9(A,B,E,F),G = F,r4(H,G,I,J),I = C,F = D.
r0(A,B,C,D,E,F) :- r9(G,H,I,J),C = J,A = D, C = F.
