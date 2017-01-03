LCIA.AP=function(LCA.env, i){


input=LCA.env$input

#print(input[i,])


NO=input[i,11]
#print("NO")
#print(NO)
 
SO2=input[i,12]
#print("SO2")
#print(SO2)

NH3=input[i, 10]
#print("NH3")
#print(NH3)





LCA.env$AP[i]=1.6*NH3+0.5*NO+1.2*SO2
#print(LCA.env$AP)




# browser()























































































}