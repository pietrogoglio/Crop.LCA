ep=function(LCA.env, EC, location, electricity) {


##print("This function computes invenotry data knowing the electricity consumption, EC in GJ and location)")

        for (n in (1:length(EC))){
                 for (m in 1:nrow(electricity)){
			    if ((location==electricity[m,1])==TRUE) {
								  electricity.data=electricity[m,1:ncol(electricity)]
												      
				}

			    }
		      
		      
		      
# 		print("setting up a common variable to compute the impacts")
		#@______________________________________________________________________________________________________________________________________________________________________________________
							      
		    
				input=EC

      # 	      print(input)
      # 	      print(is(input))
      # 	      browser()
				    

		#                         browser()
		#                         The input data are read line by line, adding column to column
		# ________________________________________________mfp=_____________________________________________________________________________________________

			    
		  ee=input[n]*electricity.data[5:ncol(electricity.data)]
# 		  browser()
		  ee=as.numeric(as.character(ee))

		  
# 		   print(ee) 
# 		 

    # 				print(outputi)
														
		
	  if (n==1) {
		      output=ee
# 		      print(output)
# 		      print("output")
# 		      browser()
	    }
	  else{
		output=rbind(output, ee)
		}
    # 	                if (y==0){
    # 	                
    # 			print(output)
    # 			browser()
    #                        }
	  
#         	  browser()
		  
		  }
	  
# 			    print(ncol(output))
#  			    browser()                        


	 
                        
 # print(head(output))
     check=is(output)
#  print(length(output))
if ((check[1]=="vector")|(check[2]=="vector")) {
			  output=data.frame(output[1],output[2], output[3],output[4], output[5],output[6], output[7],output[8], output[9],output[10], output[11], output[12], output[13], output[14], output[15])         
#                           print(nrow(output))  
			  }
else {
      } 
 

             colnames(output)=c("Energy consumption GJ", "CO2 kg", "CO2 biogenic kg", "CH4 kg", "N2O kg", "CO kg", "NH3 kg", "NO2 kg", "SO2 kg", "NMVOC kg",  "PM kg", "SF6 kg", "NO3 to water kg kg-1", "PO4 kg", "P kg") 
assign("output", output, envir=LCA.env)
# print(output)
# Sys.sleep(0.5)

#  browser()





}


      


