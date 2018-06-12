farm.transport=function(LCA.env) {



# print("This function calculates farm transport impact distiguishing self propelled disk broadcaster as a truck and the petrol truck used to collect grains during harvest")
farm.transport.assumptions=LCA.env$farm.transport.assumptions
md=LCA.env$md

PC=rep(0, nrow(farm.transport.assumptions))
OC=rep(0, nrow(farm.transport.assumptions))
DC=rep(0, nrow(farm.transport.assumptions))
EC=rep(0, nrow(farm.transport.assumptions))
CO2=rep(0, nrow(farm.transport.assumptions))
CO2b=rep(0, nrow(farm.transport.assumptions))
CH4=rep(0, nrow(farm.transport.assumptions))
N2O=rep(0, nrow(farm.transport.assumptions))
CO=rep(0, nrow(farm.transport.assumptions))
NO=rep(0, nrow(farm.transport.assumptions))
NMVOC=rep(0, nrow(farm.transport.assumptions))
SO2=rep(0, nrow(farm.transport.assumptions))
PM=rep(0, nrow(farm.transport.assumptions))
NH3=rep(0, nrow(farm.transport.assumptions))
SF6=rep(0, nrow(farm.transport.assumptions))
NO3w=rep(0, nrow(farm.transport.assumptions))
PO4=rep(0, nrow(farm.transport.assumptions))
P=rep(0, nrow(farm.transport.assumptions))

# print("setting a counter to avoid to eliminate harvester consumption in harvest in the case of the petrol truck")
	
# 	print("calculating energy consumption for farm transport, data taken from Brentrup et al. 2004")
# 	print("All impacts are calculated for GJ and kg of diesel or petrol")
# 
# 		  
# 	
# 	print("correcting for the petrol truck, running the tf (tranport function) function")
# 	print("")
# 	print("")
# 	print("")
counter=0
for (i in 1:nrow(farm.transport.assumptions)) {


	#_________________________________________________________________________________________________________________________________________________________________________________________________
#accorging to Dyer and Desjardins the working day for farmers during summer season can be easily established as 10 h per day
	
# 		print(i)
		
		if ((grepl("Petrol", farm.transport.assumptions[i, 5])==TRUE)| (grepl("petrol", farm.transport.assumptions[i, 5])==TRUE)){
			    if (counter==0) {
				counter=1
# 				print("calculating impact for the harvester")
				DC[i]=farm.transport.assumptions[i,6]*0.9*2*farm.transport.assumptions[i,7]/10
				
				
				if (grepl("self propelled disk broadcaster", farm.transport.assumptions[i, 5])==FALSE){
				    
				    
				    OC[i]=(((0.00059*md[i,6]*0.735+0.02169)*0.91)*farm.transport.assumptions[i,6]*2*farm.transport.assumptions[i,7]/10)/25

				    }
				
				else{
# 				     print("distinguishing for the self propelled disk broadcaster for oil consumption estimation")
				}
				    
# 				print(farm.transport.assumptions[i,6], farm.transport.assumptions[i,7])
# 				print(EC[i])
# 				browser()
# HHV according to the GREET model

				EC[i]=DC[i]*45.77e-3+OC[i]*45.54e-3
				

# 				print("calculating field emissions, according to Brentrup et al., 2004, therefore on the basis of the LHV")
				CO2[i]=DC[i]*42.7*74.4e-3
				CO2b[i]=DC[i]*0
				CH4[i]=DC[i]*42.7*4.8e-3*1e-3
				CO[i]=DC[i]*42.7*0.4*1e-3
			
				N2O[i]=DC[i]*42.7*3.4e-3*1e-3
				NH3[i]=DC[i]*42.7*2.7e-3*1e-3
				NO[i]=DC[i]*42.7*0.84*1e-3
				SO2[i]=DC[i]*42.7*0.023*1e-3
				NMVOC[i]=DC[i]*42.7*0.2*1e-3
				PM[i]=DC[i]*42.7*8.9e-2*1e-3
				SF6[i]=DC[i]*0
# 				print("setting arrays for arrays for petrol consumption and HCC134 emissions")
				NO3w[i]=DC[i]*0
				PO4[i]=DC[i]*0
				P[i]=DC[i]*0
				
				PC[i]=0	
				
				}
			    else{
				    DC[i]=0
# 				    print("accounting for petrol consumption considering the 400 bushels truck")
				    amount=as.numeric(as.character(LCA.env$truck.harvest.transport.amount))
# 				    print("distance has been adapted in order to take into account the number of trips per day")
				    distance=as.numeric(as.character(LCA.env$truck.harvest.field.farm.centre.distance))*farm.transport.assumptions[i,7]/as.numeric(as.character(LCA.env$truck.harvest.field.farm.centre.maximum.trips.per.day))

				    transport.mean=as.character(LCA.env$truck.harvest.transport.mean)
# # 				    print(amount)
# # 				    print(distance)
# # 				    print(transport.mean)
#   				    browser()
				    output=tf(LCA.env, distance, amount, transport.mean)
#   				    print(output)
#  				    browser()
				    DC[i]=0
				    OC[i]=0
				    PC[i]=as.numeric(as.character(output[2]))
				    EC[i]=as.numeric(as.character(output[3]))
				    CO2[i]=as.numeric(as.character(output[4]))
				    CO2b[i]=as.numeric(as.character(output[5]))
				    CH4[i]=as.numeric(as.character(output[6]))
				      N2O[i]=as.numeric(as.character(output[7]))
				      CO[i]=as.numeric(as.character(output[8]))
				    NH3[i]=as.numeric(as.character(output[9]))
				    NO[i]=as.numeric(as.character(output[10]))
				    SO2[i]=as.numeric(as.character(output[11]))
				      NMVOC[i]=as.numeric(as.character(output[12]))
				      
# 				      print(DC[i])
# 				      print(CO2[i])
# 				      browser()
				      PM[i]=as.numeric(as.character(output[13]))
				      SF6[i]=as.numeric(as.character(output[14]))
				      NO3w[i]=as.numeric(as.character(output[15]))
				      PO4[i]=as.numeric(as.character(output[16]))
				      P[i]=as.numeric(as.character(output[17]))
# 				      print(1131)
# 				    browser()
#  				    print("petrol")
#  				    print(PC[i])
				    
				    counter=0
#   				      browser()
				}
		      }
		else{
# 	          print("eureka")
	#______________________________________________________________________________________________________________________________________________________________________________________	      
		  DC[i]=farm.transport.assumptions[i,6]*1*2*farm.transport.assumptions[i,7]/10*0.9
		  

		  
		  if (grepl("self propelled disk broadcaster", farm.transport.assumptions[i, 5])==FALSE){
				    
				    OC[i]=(((0.00059*md[i,6]*0.735+0.02169)*0.91)*farm.transport.assumptions[i,6]*2*farm.transport.assumptions[i,7]/10)/25
# 				    browser()

				# 		  browser()
	# 		  print(farm.transport.assumptions[i,6]) 
	# 		  print(farm.transport.assumptions[i,7])
	# 		  print(EC[i])
	# 		  browser()
			  EC[i]=DC[i]*45.77e-3+OC[i]*45.54e-3
			  

# 			  print("calculating emissions, according to Brentrup et al., 2004")
			  CO2[i]=DC[i]*42.7*74.4e-3
			  CO2b[i]=DC[i]*0
			  CH4[i]=DC[i]*42.7*4.8e-3*1e-3
			  CO[i]=DC[i]*42.7*0.4*1e-3
			  N2O[i]=DC[i]*42.7*3.4e-3*1e-3
			  NH3[i]=DC[i]*42.7*2.7e-3*1e-3
			  NO[i]=DC[i]*42.7*0.84*1e-3
			  SO2[i]=DC[i]*42.7*0.023*1e-3
			  NMVOC[i]=DC[i]*42.7*0.2*1e-3
			  PM[i]=DC[i]*42.7*8.9e-2*1e-3
			  SF6[i]=DC[i]*0
	# 		  print("setting arrays for arrays for petrol consumption and emissions")
			  NO3w[i]=DC[i]*0
			  PO4[i]=DC[i]*0
			  P[i]=DC[i]*0
			  PC[i]=0
# 			  print(1571)
# 			  browser()
			}
		      else{
# 		      print("distinguishing for the self propelled disk broadcaster for oil and diesel consumption estimation")
		      
	
		  
		 
		      transport.mean=as.character(LCA.env$self.propelled.disk.broadcaster.mean.transport)
		     
		      distance=farm.transport.assumptions[i,6]*farm.transport.assumptions[i,7]/as.numeric(as.character(LCA.env$self.propelled.disk.broadcaster.max.number.per.day))
# 		      print("considering the maximum load capacity as 5 t")
		      amount=as.numeric(as.character(LCA.env$self.propelled.disk.broadcaster.capacity))

		      output=tf(LCA.env, distance, amount, transport.mean)

		      EC[i]=as.numeric(as.character(output[3]))
		      DC[i]=as.numeric(as.character(output[1]))

		    
		    CO2[i]=as.numeric(as.character(output[4]))
		    CO2b[i]=as.numeric(as.character(output[5]))
		    CH4[i]=as.numeric(as.character(output[6]))
		    N2O[i]=as.numeric(as.character(output[7]))
		    CO[i]=as.numeric(as.character(output[8]))

		    NH3[i]=as.numeric(as.character(output[9]))
		    NO[i]=as.numeric(as.character(output[10]))
		    SO2[i]=as.numeric(as.character(output[11]))
		    NMVOC[i]=as.numeric(as.character(output[12]))
		    PM[i]=as.numeric(as.character(output[13]))
  		    SF6[i]=as.numeric(as.character(output[14]))
  		    NO3w[i]=as.numeric(as.character(output[15]))
  		    PO4[i]=as.numeric(as.character(output[16]))
  		    P[i]=as.numeric(as.character(output[17]))
#   		    print(1901)
# 		    browser()
		    }
		  }
		
			


	
	}

	
	  
	  
	  
	  

	  farmti=data.frame(farm.transport.assumptions[,1:5], DC, PC, OC, EC, CO2, CO2b, CH4, N2O, CO, NH3, NO, SO2, NMVOC, PM, SF6, NO3w, PO4, P)

# 	  print(head(farmti))
	 
names(farmti)[names(farmti)=="linea...7."]="operating machinery"
names(farmti)[names(farmti) == 'DC'] <-"farm transport diesel consumption (kg ha-1)"
names(farmti)[names(farmti) == 'PC'] <-"farm transport petrol consumption (kg ha-1)"
names(farmti)[names(farmti) == 'OC'] <-"farm transport oil consumption (kg ha-1)"
names(farmti)[names(farmti) == 'EC'] <-"farm transport energy consumption (GJ ha-1)"
names(farmti)[names(farmti) == 'CO2'] <-"farm transport CO2 emissions (kg ha-1)"
names(farmti)[names(farmti) == 'CO2b'] <-"farm transport biogenic CO2 emissions (kg ha-1)"
names(farmti)[names(farmti) == 'CH4'] <-"farm transport CH4 emissions (kg ha-1)"
names(farmti)[names(farmti) == 'N2O'] <-"farm transport N2O emissions (kg ha-1)"
names(farmti)[names(farmti) == 'CO'] <-"farm transport CO emissions (kg ha-1)"
names(farmti)[names(farmti) == 'NH3'] <-"farm transport NH3 emissions (kg ha-1)"
names(farmti)[names(farmti) == 'NO'] <-"farm transport NO emissions (kg ha-1)"
names(farmti)[names(farmti) == 'SO2'] <-"farm transport SO2 emissions (kg ha-1)"
names(farmti)[names(farmti) == 'NMVOC'] <-"farm transport NMVOC emissions (kg ha-1)"
names(farmti)[names(farmti) == 'PM'] <-"farm transport PM emissions (kg ha-1)"

names(farmti)[names(farmti) == 'SF6'] <-"farm transport SF6 emissions (kg ha-1)"
names(farmti)[names(farmti) == 'NO3w'] <-"farm transport NO3 leached to water (kg ha-1)"
names(farmti)[names(farmti) == 'PO4'] <-"farm transport PO4 emissions (kg ha-1)"
names(farmti)[names(farmti) == 'P'] <-"farm transport P emissions (kg ha-1)"
# print(2281)
# browser()
	   assign("farmti", farmti, envir=LCA.env)
	  write.table(farmti, file="farmti.csv", sep=",", row.names=FALSE) 
# browser()







































}