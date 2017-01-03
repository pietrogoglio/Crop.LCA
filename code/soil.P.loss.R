

 soil.P.loss=function(LCA.env, P.data, N.P.input, climate, yi){


 
 
##This function computes the P loss integrating USLE equation in agreement with Nemecek et al. 2014, Faist Emmeneger et al. 2009, Stone and Hilborn 2012
##WARNING this function does not distinguish among particular anti-erosion practices
#  N.P.input=get("year.results", envir=LCA.env)

# 
tab=get("tab", envir=LCA.env)
year=get("year", envir=LCA.env)





#calculating the erosivitiy factor necessary for the USLE
for (clim in (1:nrow(climate))) {
                                 if (N.P.input[yi,1]==climate[clim,1]){
					    rainfall=climate[clim, 2]
					    snowfall=climate[clim,3]
					    }
                                 }
precipitation.factor=rainfall-0.1*snowfall
#print(precipitation.factor)
#print(rainfall)
#print(snowfall)
if (precipitation.factor<=850) {
	erosivity.factor=0.0483*precipitation.factor^1.61
}
else{
      erosivity.factor=587.8-1.219*precipitation.factor+0.004105*precipitation.factor^2
}
#print(erosivity.factor)
                                 

#reading data for the tillage factor by reading the tab table and interpreting
#the function read whether or not tillage has been done or not, if it is done with a harrow mulch tillage value will be used, if there is a plough the date of ploughing is interpreted according to the LCA.Input.Table, in order to be read the month should be expressed with the full word

l=1
fl=1
counter=0
for (y in 1:length(year)){
		      repeat{
		              #print(l)
			      if ((as.character(tab[l,1])>as.character(N.P.input[yi,1])|(l==nrow(tab)))){
			                             #print("N.P.input")
			                             #print(N.P.input[yi,1])
			                             #print(tab[l,1])
			                             #print(l)
			                             l=fl
			                             break()
                                                       
			      }
                     
#         ##print(N.P.input)
#           ##print(yi)
#                               ##print(N.P.input[yi,3])
#                               ##print(tab[l,3])
			     
# 					browser()
			     


                              
			      if (as.character(N.P.input[yi,3])==as.character(tab[l,3])){
					if ((grepl("harrow", tab[l,4])==TRUE)|(grepl("Harrow", tab[l,4])==TRUE)|(counter==0)){
					c2=0.6
				      
				      
						counter=1
					}
					else{
						if ((grepl("plough", tab[l,4])==TRUE)|(grepl("Plough", tab[l,4])==TRUE)){
														    c2=0.9
														    if ((grepl("August", tab[l,15])==TRUE)|(grepl("September", tab[l,15])==TRUE)|((grepl("October", tab[l,15]))==TRUE)){
																c2=1
														    }
														    counter=1
														    }
						else{
							if (counter==0) {
									  c2=0.25
									  }
						  }
					}
					
			    

				}

			
			l=l+1
			#print(l)
                       }
# 	##print(y)
# 	##print(yi)
#  	##print(c2)
                       					   
			
	 }
#print(yi)
			      #print("c2")
			      
		      #print(c2)
		      
#retrieving the value for different crops,

if ((grepl("Maize",as.character(N.P.input[yi,3]))==TRUE)|(grepl("maize",as.character(N.P.input[yi,3]))==TRUE)){
      c1=0.4
}
if ((grepl("Wheat",as.character(N.P.input[yi,3]))==TRUE)|(grepl("wheat",as.character(N.P.input[yi,3]))==TRUE)|(grepl("Barley",as.character(N.P.input[yi,3]))==TRUE)|(grepl("barley",as.character(N.P.input[yi,3]))==TRUE)|(grepl("Triticale",as.character(N.P.input[yi,3]))==TRUE)|(grepl("triticale",as.character(N.P.input[yi,3]))==TRUE)|(grepl("Oat",as.character(N.P.input[yi,3]))==TRUE)|(grepl("oat",as.character(N.P.input[yi,3]))==TRUE)|(grepl("Rye",as.character(N.P.input[yi,3]))==TRUE)|(grepl("rye",as.character(N.P.input[yi,3]))==TRUE)){
      c1=0.4
}
if ((grepl("canola",as.character(N.P.input[yi,3]))==TRUE)|(grepl("Canola",as.character(N.P.input[yi,3]))==TRUE)|(grepl("rapeseed",as.character(N.P.input[yi,3]))==TRUE)|(grepl("Rapeseed",as.character(N.P.input[yi,3]))==TRUE)|(grepl("Faba beans",as.character(N.P.input[yi,3]))==TRUE)|(grepl("faba beans",as.character(N.P.input[yi,3]))==TRUE)){
      c1=0.5
}
if ((grepl("Alfalfa",as.character(N.P.input[yi,3]))==TRUE)|(grepl("alfalfa",as.character(N.P.input[yi,3]))==TRUE)|(grepl("Clover",as.character(N.P.input[yi,3]))==TRUE)|(grepl("clover",as.character(N.P.input[yi,3]))==TRUE)){
	      c1=0.02
}


#print("c1")
#print(c1)
#print(as.character(N.P.input[yi, 3]))


#retrieving input coefficient for the slope factor, calculating the slope factor
slope=as.numeric(P.data["slope",])

if (slope>1){
             slope=slope*0.01
}

if (slope<0.01){
      expLS=0.2
}
if ((slope>=0.01)&(slope<0.035)){
      expLS=0.3
}
if ((slope>=0.035)&(slope<=0.05)){
      expLS=0.4
}
if (slope>0.05){
      expLS=0.5
}


#calculatint the slope factor
LS=(as.numeric(P.data["slope.length",])*3.281/72.6)^expLS*(65.41*(sin(slope))^2+4.56*(sin(slope))+0.065)

#print(expLS)
#print(LS)
#print(P.data["erodibility",])
#print(P.data["practice.factor",])


# calculating the soil erosion rate in agreement with Nemecek et al., 2014

soil.erosion..kg.ha.y=1000*erosivity.factor*as.numeric(P.data["erodibility",])*LS*c1*c2*as.numeric(P.data["practice.factor",])


# 

# Calculating soil P related to soil erosion in agreement with Nemecek et al., 2014
LCA.env$soil.erosion[yi]=soil.erosion..kg.ha.y

LCA.env$soil.P.erosion.to.rivers[yi]=soil.erosion..kg.ha.y*as.numeric(P.data["P.content.topsoil",])*as.numeric(P.data["P.enrichment.factor",])*as.numeric(P.data["eroded.soil.reaching.river",])
# print(LCA.env$soil.erosion[yi])
# print(P.data)
#print(yi)
#  print(LCA.env$soil.P.erosion.to.rivers[yi])
# 
# print(N.P.input[yi,])

# Calculating soil P due to run off in water in agreement with Nemecek et al., 2014
LCA.env$P.run.off.water[yi]=0.175*(1+0.2/80*as.numeric(as.character(N.P.input[yi,6])))

# print(LCA.env$P.run.off.water[yi])

LCA.env$P.leaching.ground.water[yi]=0.175
#print(LCA.env$P.leaching.ground.water[yi])

# Calculating soil P due to run off to surface water in agreement with Nemecek et al., 2014
LCA.env$P.loss[yi]=sum(LCA.env$soil.P.erosion.to.rivers[yi], LCA.env$P.run.off.water[yi], LCA.env$P.leaching.ground.water[yi])
#  print(LCA.env$P.loss)
#  browser()



















































}