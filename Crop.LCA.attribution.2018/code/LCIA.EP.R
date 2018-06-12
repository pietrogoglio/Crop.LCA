LCIA.EP=function(LCA.env, i){


 input=LCA.env$input

#print(input[i,])



NO3=input[i, 16]
#print("NO3")
#print(NO3)

PO4=input[i,17]
#print("PO4")
#print(PO4)

P=input[i,18]
#print("P")
#print(P)
P=P+0.326*PO4
#print(P)


NH3=input[i,10]
#print("NH3")
#print(NH3)


LCA.env$EP[i]=P*3.06+0.1*NO3+NH3*0.35

#print(LCA.env$EP)


# browser()























































































}