LCIA.GWP=function(LCA.env, i){

 input=LCA.env$input

 #print((input[i,])

CO2=sum(input[i,5:6])
#print(("CO2")
#print((CO2)

#print((input[i,5:6])
CH4=input[i,7]
#print(("CH4")
#print((input[i,7])
#print((CH4)
N2O=input[i,8]
#print(("N2O")
#print((N2O)
CO=input[i,9]
#print(("CO")
#print((CO)
NO=input[i,11]
#print(("NO")
#print((NO)
SF6=input[i,15]
#print(("SF6")
#print((SF6)
NMVOC=input[i,13]
#print(("NMVOC")
#print((NMVOC)









LCA.env$GWP[i]=CO2+28*CH4+265*N2O+23500*SF6-11*NO+3.3*CO+NMVOC*4.5




























































































}