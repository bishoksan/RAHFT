fib(A, A, K):- K=0, A>=0,  A=<1.
fib(A, B, K) :- A >= 2, A2 = A - 2, fib(A2, B2, K1),
           A1 = A - 1, fib(A1, B1, K2), B = B1 + B2, dim(K1, K2, K).
 dim(K1, K2, K1):-K1>=K2+1.  
 dim(K1, K2, K2):-K1=<K2-1.    
 dim(K1, K1, K3):-K3 = K1+1.    
 
 false :- 
 	2*K >= A+1,
 	fib(A,B,K).    