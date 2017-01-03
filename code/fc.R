
fc=function(LCA.env){
  
#                  print(head(md))
#                  browser()
# 		 Sys.sleep(0.5)
                md=get("md", envir=LCA.env)
                transportmeanslist=get("transportmeanslist", envir=LCA.env)

                 hp=md[,6]

		 ewc=md[,7]
#                 print(hp)
#                 print(ewc)


# 	    print("DC diesel consumption (kg ha-1), building the vector")
# 	    ______________________________________________________________________________________________________________________________________________________________________________________


		DC=hp*0.2/ewc

# 		if 


	

	
#              print("calculating oil consumption according to the ASABE standard 497.4 2003")
# ______________________________________________________________________________________________________________________________________________________________________________________


	  OC=((0.00059*hp*0.735+0.02169)*0.91)/ewc
	  
	  
# 	  print("calculating energy consumption for field operation, data taken from USDOE 2014")
# 	  print("All impacts are calculated for GJ and kg of diesel or petrol")
#______________________________________________________________________________________________________________________________________________________________________________________	      
          
          EC=DC*45.77e-3+OC*45.54e-3
# 	  print(EC)

# 	  print("calculating field emissions, according to Brentrup et al., 2004, therefore using the LHV")
	  CO2=DC*42.7*74.4e-3
	  CO2b=DC*0
	  CH4=DC*42.7*4.8e-3*1e-3
	  CO=DC*42.7*0.4*1e-3
	  N2O=DC*42.7*3.4e-3*1e-3
	  NH3=DC*42.7*2.7e-3*1e-3
	  NO=DC*42.7*0.84*1e-3
	  SO2=DC*42.7*0.023*1e-3
	  NMVOC=DC*42.7*0.2*1e-3
	  PM=DC*42.7*8.9e-2*1e-3
	  SF6=DC*0
	  NO3w=DC*0
	  PO4=DC*0
	  P=DC*0
	  
# 	  print("setting arrays for arrays for petrol consumption and HCC134 emissions")
	  
	  PC=rep(0, nrow(md))	
	 
	 
	  
# 	  print("correcting for the petrol truck, running the tf (tranport function) function")
	  #_________________________________________________________________________________________________________________________________________________________________________________________________
# 	   print("setting a counter to avoid to eliminate harvester consumption")
          counter=0
          for (i in 1:nrow(md)) {
# 		  print(i)
		  
	          if ((grepl("Petrol", md[i, 5])==TRUE)| (grepl("petrol", md[i, 5])==TRUE)){
			      if (counter==0) {
				  counter=1}
			      else{
				      DC[i]=0
# 				      print("accounting for petrol consumption considering that the 400 bushels truck just covers a 1000 m distance to collect the grains from the harvester")
				      amount=as.numeric(as.character(LCA.env$truck.harvest.transport.amount))

				      distance=as.numeric(as.character(LCA.env$truck.harvest.transport.distance))

				      transport.mean=as.character(LCA.env$truck.harvest.transport.mean)
				      
#  				      assign("transport.mean", transport.mean, envir=LCA.env) 


# 				      print(distance)
# 				      print(amount)

				      tf(LCA.env, distance, amount, transport.mean)
 				      output=LCA.env$output
#  				      browser()
# 				      browser()
				      DC[i]=0
				      OC[i]=0
				      PC[i]=as.numeric(as.character(output[2]))
				      EC[i]=as.numeric(as.character(output[3]))
				      CO2[i]=as.numeric(as.character(output[4]))
				      CO2b[i]=as.numeric(as.character(output[5]))
					CH4[i]=as.numeric(as.character(output[6]))
					N2O[i]=as.numeric(as.character(output[7]))
					CO[i]=as.numeric(as.character(output[8]))
					NO[i]=as.numeric(as.character(output[10]))
					NMVOC[i]=as.numeric(as.character(output[12]))
					SO2[i]=as.numeric(as.character(output[11]))
					
					PM[i]=as.numeric(as.character(output[13]))
# 					SF6[i]=as.numeric(as.character
					
				      NH3[i]=0
				      SF6[i]=as.numeric(as.character(output[14]))
				      NO3w[i]=0
				      PO4[i]=0
				      P[i]=0
# 				      print("petrol")
# 				      print(PC[i])
#   				      browser()
				      counter=0
 				      
				  }
			}
		  else{
		       PC[i]=0
		       
		  }
	
		}
		PC=as.numeric(as.character(PC))
		
# 		print(DC)
# 	  browser()
	  

	  
	  
	  
	  
 	  
	  
	  

		    fico=data.frame(md[,1:5], DC, PC, OC, EC, CO2, CO2b, CH4, N2O, CO, NH3, NO, SO2, NMVOC, PM, SF6, NO3w, PO4, P)
# 		    browser()


# 	print("building the fico dataframe containing the effect of just field consumption")

# 	print(head(fico))
# 	Sys.sleep(0.01)




names(fico)[names(fico)=="linea...7."]="operating machinery"
names(fico)[names(fico) == 'DC'] <-"field diesel consumption (kg ha-1)"
names(fico)[names(fico) == 'PC'] <-"field petrol consumption (kg ha-1)"
names(fico)[names(fico) == 'OC'] <-"field oil consumption (kg ha-1)"
names(fico)[names(fico) == 'EC'] <-"field energy consumption (GJ ha-1)"
names(fico)[names(fico) == 'CO2'] <-"Field CO2 emissions (kg ha-1)"
names(fico)[names(fico) == 'CO2b'] <-"Field operation CO2 biogenic emissions (kg ha-1)"

names(fico)[names(fico) == 'CH4'] <-"Field CH4 emissions (kg ha-1)"
names(fico)[names(fico) == 'N2O'] <-"Field N2O emissions (kg ha-1)"
names(fico)[names(fico) == 'CO'] <-"Field CO emissions (kg ha-1)"
names(fico)[names(fico) == 'NH3'] <-"Field NH3 emissions (kg ha-1)"
names(fico)[names(fico) == 'NO'] <-"Field NO emissions (kg ha-1)"
names(fico)[names(fico) == 'SO2'] <-"Field SO2 emissions (kg ha-1)"
names(fico)[names(fico) == 'NMVOC'] <-"Field NMVOC emissions (kg ha-1)"
names(fico)[names(fico) == 'PM'] <-"Field PM emissions (kg ha-1)"
names(fico)[names(fico) == 'SF6'] <-"Field SF6 emissions (kg ha-1)"

names(fico)[names(fico) == 'NO3w'] <-"Machinery NO3 leaching (kg ha-1)"

names(fico)[names(fico) == 'PO4'] <-"Machinery PO4 emissions (kg ha-1)"

names(fico)[names(fico) == 'P'] <-"Machinery P emissions (kg ha-1)"


write.table(fico, file="fico.csv", sep=",", row.names=FALSE)	
# print("fico (field consumption)")
# print(head(fico))
# Sys.sleep(1)
assign("fico", fico, envir=LCA.env)


# browser()






















  
 }