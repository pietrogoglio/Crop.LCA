fp=function(LCA.env) {
# browser()
PC=as.numeric(as.character(LCA.env$PC))
DC=as.numeric(as.character(LCA.env$DC))
LOC=as.numeric(as.character(LCA.env$LOC)) 
GC=as.numeric(as.character(LCA.env$GC))
FOC=as.numeric(as.character(LCA.env$FOC))
CC=as.numeric(as.character(LCA.env$CC))
# print(PC)
# browser()
fuel=get("fuel", envir=LCA.env)
location=as.character(LCA.env$location)


##print("This function computes invenotry data knowing the type of fuel (Petrol, PC; Diesel, DC; Oil consumption, LOC, CC coal consumption, GC gas consumption), fuel and location)")
if (((length(PC)==length(DC))==TRUE)&((length(PC)==length(LOC))==TRUE)&((length(PC)==length(CC))==TRUE)&((length(PC)==length(GC))==TRUE)&((length(PC)==length(FOC))==TRUE)) {
  }
  else{
      print("make sure all your fuel data arguments have the same number of elements")
  }


for (n in 1:length(PC)) {
	for (f in (1:6)) {
		      fuelchecklist=c("diesel", "lubricating oil", "natural gas", "coal", "petrol", "fuel oil")
		      if ((n==1)& (f==1)){
# 			    print("checking for the right fuel")
# 			    print("The variable fuelchecklist and fuelcheck have been introduced to select the right fuel data")
			    }
		      for (m in 1:nrow(fuel)) {
			      fuelcheck=fuelchecklist[f]
			      
			    if (((fuelcheck==fuel[m, 2])==TRUE)&((location==fuel[m,1])==TRUE)) {
								  fuel.data=fuel[m,1:ncol(fuel)]
								 
												      
				}
			    
		      }
		      
		      
# 		print("setting up a common variable to compute the impacts")
		#@______________________________________________________________________________________________________________________________________________________________________________________
							      
		    if (f==1) {
				input=DC
			}
		    if (f==2) {
			      input=LOC
		      }
		    if (f==3) {
			      input=GC
			}
		    if (f==4) {
			      input=CC
			}
		    if (f==5) {
				input=PC  
		    }
		    if (f==6) {
		               input=FOC
			}
      # 	      print(input)
      # 	      print(is(input))
      # 	      browser()
				    

		#                         browser()
		#                         The input data are read line by line, adding column to column
		# ________________________________________________mfp=_____________________________________________________________________________________________

			    
		  ee=input[n]*fuel.data[6:ncol(fuel.data)]
# 		  browser()
		  ee=as.numeric(as.character(ee))
		  if (f==1) {
			    outputi=ee                                                                   
			    }
		  else{
#       	         browser()
		      for (i in 1:length(ee)) {
			      outputi[i]=outputi[i]+ee[i]    
			      }
		      }
		  
		  # print(ee) 
		  # print(outputi)
#        	    browser()
		  
		  }
    # 				print(outputi)
														
		
	  if (n==1) {
		      output=outputi
	    }
	  else{
		output=rbind(output, outputi)
		}
    # 	                if (y==0){
    # 	                
    # 			print(output)
    # 			browser()
    #                        }
	  

	  
# 			    print(output)
# 			    browser()                        


	  }
                        
 # print(head(output))
 # to correct in case of vectors
  check=is(output)
if ((check[1]=="vector")|(check[2]=="vector")) {
			  output=data.frame(output[1],output[2], output[3],output[4], output[5],output[6], output[7],output[8], output[9],output[10], output[11], output[12], output[13], output[14], output[15])         
#                           print(nrow(output))  
			  }
else {
      }

  colnames(output)=c("Energy consumption GJ", "CO2 kg", "CO2 biogenic kg", "CH4 kg", "N2O kg", "CO kg", "NH3 kg", "NO2 kg", "SO2 kg", "NMVOC kg",  "PM kg", "SF6 kg", "NO3 to water kg kg-1", "PO4 kg", "P kg")
# assign("output", output, envir=LCA.env)
LCA.env$output=output
# sink("NUL")
#  print(head(output))
# print(output)
#  Sys.sleep(0.5)

#  browser()







}

      


