% {i>=0, n>=1} // initial state
% 
% i = 1;
% while (i<n) { 
%     a[i] = a[i−1] + 1; 
%     i = i + 1; 
% }
% 
% { exists j. j>=0, j=<n-2, a[j] >= a[j+1]} //error states or the property to be verified

% Example Prolog obtained using MAP tool

false :- K=J+1, J>=0, J=<N−2, U>=V, N=<I, read(A, J, U), read(A, K, V), p(I, N, A).
p(I, N, A) :- 1=<H, H=<N−1, G=H−1, I=H+1, Z=W+1, read(B,G,W), write(B,H,Z,A), p(H,N,B).
p(I, N, A) :- I=1, N>=1.
