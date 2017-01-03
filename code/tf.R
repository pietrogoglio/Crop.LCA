tf=function(LCA.env, distance, amount, transport.mean) {

# print("This function computes invenotry data knowing the mean of tranport (transport.mean), mean of transport inventory (transportmeanslist), distance and amount of goods (amount); the data should be listed line by line with in column values")
# print("Depending on the information taken into account in the transport file, this function is able to distinguish among different fuel used for transport")
# print("The overall output is called output (global environment variable), a table that contains inventory data")
# browser()
                               transportmeanslist=get("transportmeanslist", envir=LCA.env)
                               for (m in 1:nrow(transportmeanslist)) {
                                                             meancheck=as.character(unlist(transportmeanslist[m,1]))
                                                 
							    if ((meancheck==transport.mean)==TRUE) {
												  mean.transport.data=transportmeanslist[m,1:ncol(transportmeanslist)]
# 												  browser()
								}
							    else {
							    
							    }
							  }
							 
							  
                               

                               trip=distance*2

                               
E=rep(0, length(amount))
# y=scan()
output=matrix(ncol=(length(mean.transport.data)-4+3), nrow=length(amount))
# browser()
for (i in 1:length(amount)) {
                                 
				if ((transport.mean=="truck")|(transport.mean=="marine cargo freight")|(transport.mean=="rail")) {
# 				    print("assign the specific energy consumption for diesel per GJ kg-1 of fuel, on the basis of the GREET model values)")
				    SEC=0.04577
				    E=SEC*amount[i]*trip*mean.transport.data[3]
				    
# 				    if (y==0){
	                
# 			print(E)
# 			browser()
#                        }
# 				    print(E)
# 				    browser()
				    
				    }
# 				    print(transport.mean)
# 				    browser()
				if (transport.mean=="petrol truck") {
# 				    print("assign the specific energy consumption for diesel per GJ kg-1 of fuel)")
				    SEC=0.04654
				    
				    E=SEC*amount[i]*trip*mean.transport.data[4]
				    }
				     
				if (transport.mean=="marine bulk freight") {
# 				    print("assign the specific energy consumption for heavy fuel oil per GJ kg-1 of fuel on the basis of the HHV)")
				    SEC=0.04554
				    
				    E=SEC*amount[i]*trip*mean.transport.data[3]
				    }
				
                        
                  
                        
                        diesel.consumption=amount[i]*trip*mean.transport.data[3]
                        petrol.consumption=amount[i]*trip*mean.transport.data[4]
#                         print(E)
                       
                        output[i,1]=as.numeric(diesel.consumption)
                        output[i,2]=as.numeric(petrol.consumption)
                        output[i,3]=as.numeric(E)

                         
#                         The input data are read line by line, adding column to column
# _____________________________________________________________________________________________________________________________________________

                        for (n in 5:length(mean.transport.data)) {
                                                                   
                                                                   nemission=mean.transport.data[n]*amount[i]*trip

#                                                                    print(outputi)
# 								   col=n-1
                                                                   output[i,n-1]=as.numeric(nemission)
#                                                                    print(output)
#                                                                    browser()
                                                                   }

# 	                if (y==0){
# 	                
# 			print(output)
# 			browser()
#                        }
                        

                        
#                         print(output)
#                         browser()                        


                        }

#                         print(output)
# print(head(output))
output=data.frame(output)
#correcting for vectors
check=is(output)
# if (check==FALSE)  {

colnames(output)=c("Diesel consumption kg", "Petrol consumption", "Energy consumption GJ", "CO2 kg", "CO2 biogenic kg", "CH4 kg", "N2O kg", "CO kg", "NH3", "NO2 kg", "SO2 kg", "NMVOC kg", "PM kg", "SF6 kg", "NO3 to water kg kg-1", "PO4 kg", "P kg") 

assign("output", output, envir=LCA.env)
# 
#  print(output)
# 
# browser()











}