new104(A,B,C,D,E,F,G) :- B=0.
new104(A,B,C,D,E,F,G) :- H=1+F, B=< -1, new23(A,C,D,E,H,G).
new104(A,B,C,D,E,F,G) :- H=1+F, B>=1, new23(A,C,D,E,H,G).
new102(A,B,C,D,E,F) :- G=1, B-D>=0, new104(A,G,B,C,D,E,F).
new102(A,B,C,D,E,F) :- G=0, B-D=< -1, new104(A,G,B,C,D,E,F).
new100(A,B,C,D,E,F,G) :- B=0.
new100(A,B,C,D,E,F,G) :- B=< -1, new102(A,C,D,E,F,G).
new100(A,B,C,D,E,F,G) :- B>=1, new102(A,C,D,E,F,G).
new98(A,B,C,D,E,F) :- G=1, D>=1, new100(A,G,B,C,D,E,F).
new98(A,B,C,D,E,F) :- G=0, D=<0, new100(A,G,B,C,D,E,F).
new96(A,B,C,D,E,F,G) :- B=0.
new96(A,B,C,D,E,F,G) :- B=< -1, new98(A,C,D,E,F,G).
new96(A,B,C,D,E,F,G) :- B>=1, new98(A,C,D,E,F,G).
new94(A,B,C,D,E,F) :- G=1, B-E>=0, new96(A,G,B,C,D,E,F).
new94(A,B,C,D,E,F) :- G=0, B-E=< -1, new96(A,G,B,C,D,E,F).
new92(A,B,C,D,E,F,G) :- B=0.
new92(A,B,C,D,E,F,G) :- B=< -1, new94(A,C,D,E,F,G).
new92(A,B,C,D,E,F,G) :- B>=1, new94(A,C,D,E,F,G).
new88(A,B,C,D,E,F,G) :- B=0.
new88(A,B,C,D,E,F,G) :- H=1+G, B=< -1, new46(A,C,D,E,F,H).
new88(A,B,C,D,E,F,G) :- H=1+G, B>=1, new46(A,C,D,E,F,H).
new86(A,B,C,D,E,F) :- G=1, B-E>=0, new88(A,G,B,C,D,E,F).
new86(A,B,C,D,E,F) :- G=0, B-E=< -1, new88(A,G,B,C,D,E,F).
new84(A,B,C,D,E,F,G) :- B=0.
new84(A,B,C,D,E,F,G) :- B=< -1, new86(A,C,D,E,F,G).
new84(A,B,C,D,E,F,G) :- B>=1, new86(A,C,D,E,F,G).
new82(A,B,C,D,E,F) :- G=1, E>=1, new84(A,G,B,C,D,E,F).
new82(A,B,C,D,E,F) :- G=0, E=<0, new84(A,G,B,C,D,E,F).
new80(A,B,C,D,E,F,G) :- B=0.
new80(A,B,C,D,E,F,G) :- B=< -1, new82(A,C,D,E,F,G).
new80(A,B,C,D,E,F,G) :- B>=1, new82(A,C,D,E,F,G).
new78(A,B,C,D,E,F) :- G=1, B-F>=0, new80(A,G,B,C,D,E,F).
new78(A,B,C,D,E,F) :- G=0, B-F=< -1, new80(A,G,B,C,D,E,F).
new76(A,B,C,D,E,F,G) :- B=0.
new76(A,B,C,D,E,F,G) :- B=< -1, new78(A,C,D,E,F,G).
new76(A,B,C,D,E,F,G) :- B>=1, new78(A,C,D,E,F,G).
new72(A,B,C,D,E,F,G) :- B=0.
new72(A,B,C,D,E,F,G) :- H=1+G, B=< -1, new49(A,C,D,E,F,H).
new72(A,B,C,D,E,F,G) :- H=1+G, B>=1, new49(A,C,D,E,F,H).
new70(A,B,C,D,E,F) :- G=1, B-D>=0, new72(A,G,B,C,D,E,F).
new70(A,B,C,D,E,F) :- G=0, B-D=< -1, new72(A,G,B,C,D,E,F).
new68(A,B,C,D,E,F,G) :- B=0.
new68(A,B,C,D,E,F,G) :- B=< -1, new70(A,C,D,E,F,G).
new68(A,B,C,D,E,F,G) :- B>=1, new70(A,C,D,E,F,G).
new66(A,B,C,D,E,F) :- G=1, D>=1, new68(A,G,B,C,D,E,F).
new66(A,B,C,D,E,F) :- G=0, D=<0, new68(A,G,B,C,D,E,F).
new64(A,B,C,D,E,F,G) :- B=0.
new64(A,B,C,D,E,F,G) :- B=< -1, new66(A,C,D,E,F,G).
new64(A,B,C,D,E,F,G) :- B>=1, new66(A,C,D,E,F,G).
new62(A,B,C,D,E,F) :- G=1, B-E>=0, new64(A,G,B,C,D,E,F).
new62(A,B,C,D,E,F) :- G=0, B-E=< -1, new64(A,G,B,C,D,E,F).
new60(A,B,C,D,E,F,G) :- B=0.
new60(A,B,C,D,E,F,G) :- B=< -1, new62(A,C,D,E,F,G).
new60(A,B,C,D,E,F,G) :- B>=1, new62(A,C,D,E,F,G).
new58(A,B,C,D,E,F) :- G=1, E>=1, new60(A,G,B,C,D,E,F).
new58(A,B,C,D,E,F) :- G=0, E=<0, new60(A,G,B,C,D,E,F).
new56(A,B,C,D,E,F,G) :- B=0.
new56(A,B,C,D,E,F,G) :- B=< -1, new58(A,C,D,E,F,G).
new56(A,B,C,D,E,F,G) :- B>=1, new58(A,C,D,E,F,G).
new54(A,B,C,D,E,F) :- G=1, B-F>=0, new56(A,G,B,C,D,E,F).
new54(A,B,C,D,E,F) :- G=0, B-F=< -1, new56(A,G,B,C,D,E,F).
new52(A,B,C,D,E,F,G) :- B=0.
new52(A,B,C,D,E,F,G) :- B=< -1, new54(A,C,D,E,F,G).
new52(A,B,C,D,E,F,G) :- B>=1, new54(A,C,D,E,F,G).
new50(A,B,C,D,E,F) :- G=1, F>=1, new52(A,G,B,C,D,E,F).
new50(A,B,C,D,E,F) :- G=0, F=<0, new52(A,G,B,C,D,E,F).
new49(A,B,C,D,E,F) :- B-F>=0, new50(A,B,C,D,E,F).
new49(A,B,C,D,E,F) :- G=1+E, B-F=< -1, new45(A,B,C,D,G,F).
new48(A,B,C,D,E,F) :- G=1, F>=1, new76(A,G,B,C,D,E,F).
new48(A,B,C,D,E,F) :- G=0, F=<0, new76(A,G,B,C,D,E,F).
new46(A,B,C,D,E,F) :- B-F>=0, new48(A,B,C,D,E,F).
new46(A,B,C,D,E,F) :- B-F=< -1, new49(A,B,C,D,E,C).
new45(A,B,C,D,E,F) :- B-E>=0, new46(A,B,C,D,E,C).
new45(A,B,C,D,E,F) :- B-E=< -1, new25(A,B,C,D,C,F).
new44(A,B,C,D,E,F) :- G=1, E>=1, new92(A,G,B,C,D,E,F).
new44(A,B,C,D,E,F) :- G=0, E=<0, new92(A,G,B,C,D,E,F).
new40(A,B,C,D,E,F,G) :- B=0.
new40(A,B,C,D,E,F,G) :- H=1+F, B=< -1, new25(A,C,D,E,H,G).
new40(A,B,C,D,E,F,G) :- H=1+F, B>=1, new25(A,C,D,E,H,G).
new38(A,B,C,D,E,F) :- G=1, B-D>=0, new40(A,G,B,C,D,E,F).
new38(A,B,C,D,E,F) :- G=0, B-D=< -1, new40(A,G,B,C,D,E,F).
new36(A,B,C,D,E,F,G) :- B=0.
new36(A,B,C,D,E,F,G) :- B=< -1, new38(A,C,D,E,F,G).
new36(A,B,C,D,E,F,G) :- B>=1, new38(A,C,D,E,F,G).
new34(A,B,C,D,E,F) :- G=1, D>=1, new36(A,G,B,C,D,E,F).
new34(A,B,C,D,E,F) :- G=0, D=<0, new36(A,G,B,C,D,E,F).
new32(A,B,C,D,E,F,G) :- B=0.
new32(A,B,C,D,E,F,G) :- B=< -1, new34(A,C,D,E,F,G).
new32(A,B,C,D,E,F,G) :- B>=1, new34(A,C,D,E,F,G).
new30(A,B,C,D,E,F) :- G=1, B-E>=0, new32(A,G,B,C,D,E,F).
new30(A,B,C,D,E,F) :- G=0, B-E=< -1, new32(A,G,B,C,D,E,F).
new28(A,B,C,D,E,F,G) :- B=0.
new28(A,B,C,D,E,F,G) :- B=< -1, new30(A,C,D,E,F,G).
new28(A,B,C,D,E,F,G) :- B>=1, new30(A,C,D,E,F,G).
new26(A,B,C,D,E,F) :- G=1, E>=1, new28(A,G,B,C,D,E,F).
new26(A,B,C,D,E,F) :- G=0, E=<0, new28(A,G,B,C,D,E,F).
new25(A,B,C,D,E,F) :- B-E>=0, new26(A,B,C,D,E,F).
new25(A,B,C,D,E,F) :- B-E=< -1, new6(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- B-E>=0, new44(A,B,C,D,E,F).
new23(A,B,C,D,E,F) :- B-E=< -1, new45(A,B,C,D,C,F).
new19(A,B,C,D,E,F,G) :- B=0.
new19(A,B,C,D,E,F,G) :- H= -1+E, B=< -1, new3(A,C,E,H,F,G).
new19(A,B,C,D,E,F,G) :- H= -1+E, B>=1, new3(A,C,E,H,F,G).
new17(A,B,C,D,E,F) :- G=1, B-D>=0, new19(A,G,B,C,D,E,F).
new17(A,B,C,D,E,F) :- G=0, B-D=< -1, new19(A,G,B,C,D,E,F).
new15(A,B,C,D,E,F,G) :- B=0.
new15(A,B,C,D,E,F,G) :- B=< -1, new17(A,C,D,E,F,G).
new15(A,B,C,D,E,F,G) :- B>=1, new17(A,C,D,E,F,G).
new13(A,B,C,D,E,F) :- G=1, D>=1, new15(A,G,B,C,D,E,F).
new13(A,B,C,D,E,F) :- G=0, D=<0, new15(A,G,B,C,D,E,F).
new11(A,B,C,D,E,F,G) :- B=0.
new11(A,B,C,D,E,F,G) :- B=< -1, new13(A,C,D,E,F,G).
new11(A,B,C,D,E,F,G) :- B>=1, new13(A,C,D,E,F,G).
new9(A,B,C,D,E,F) :- G=1, B-D>=0, new11(A,G,B,C,D,E,F).
new9(A,B,C,D,E,F) :- G=0, B-D=< -1, new11(A,G,B,C,D,E,F).
new7(A,B,C,D,E,F,G) :- B=0.
new7(A,B,C,D,E,F,G) :- B=< -1, new9(A,C,D,E,F,G).
new7(A,B,C,D,E,F,G) :- B>=1, new9(A,C,D,E,F,G).
new6(A,B,C,D,E,F) :- G=1, D>=1, new7(A,G,B,C,D,E,F).
new6(A,B,C,D,E,F) :- G=0, D=<0, new7(A,G,B,C,D,E,F).
new5(A,B,C,D,E,F) :- A=< -1, new23(A,B,C,D,C,F).
new5(A,B,C,D,E,F) :- A>=1, new23(A,B,C,D,C,F).
new5(A,B,C,D,E,F) :- A=0, new25(A,B,C,D,C,F).
new4(A,B,C,D,E,F) :- B-D>=1, new5(A,B,C,D,E,F).
new4(A,B,C,D,E,F) :- B-D=<0, new6(A,B,C,D,E,F).
new3(A,B,C,D,E,F) :- G=1+D, D>=1, new4(A,B,G,D,E,F).
new2(A) :- new3(A,B,C,B,D,E).
new1 :- new2(A).
false :- new1.
