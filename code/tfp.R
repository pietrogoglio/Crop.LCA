tfp=function(LCA.env) {

transportmeanslist=get("transportmeanslist", envir=LCA.env)
distance=get("distance", envir=LCA.env)
amount=get("amount", envir=LCA.env)
transport.mean=get("transport.mean", envir=LCA.env)
# print(amount)
#  print(transport.mean)
# print(distance)
# print("This function computes invenotry data knowing the mean of tranport (transport.mean), mean of transport inventory (transportmeanslist), distance and amount of goods (amount); the data should be listed line by line with in column values")
# print("Depending on the information taken into account in the transport file, this function is able to distinguish among different fuel used for transport")
# print("The overall output is called output (global environment variable), a table that contains inventory data")

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
foutput=matrix(ncol=(length(mean.transport.data)-3), nrow=length(amount))

# print(transport)
#  print(foutput)
#  browser()
for (i in 1:length(amount)) {
                             
				if ((transport.mean=="truck")|(transport.mean=="marine cargo freight")|(transport.mean=="rail")) {
# 				    print("assign the specific energy consumption for diesel per GJ kg-1 of fuel, on the basis of the GREET model values)")
				    SEC=0.04577
				    E=SEC*amount[i]*trip*mean.transport.data[3]
				    DC=amount[i]*trip*mean.transport.data[3]
# 				    print("DC")
# 				    print(DC)
# 				    print(distance)
# 				    print(trip)
				    
				   PC=0
				    FOC=0
# 				    if (y==0){
	                
# 			print(E)
# 			browser()
#                        }
# 				    print(E)
				    
				    
				    }
				if (transport.mean=="petrol truck") {
# 				    print("assign the specific energy consumption for diesel per GJ kg-1 of fuel)")
				    SEC=0.04654
				    
				    E=SEC*amount[i]*trip*mean.transport.data[4]
				    PC=amount[i]*trip*mean.transport.data[4]
				    DC=0
				    FOC=0
				    
				    }
				if (transport.mean=="marine bulk freight") {
# 				    print("assign the specific energy consumption for heavy fuel oil per GJ kg-1 of fuel on the basis of the HHV)")
				    SEC=0.04554
				    
				    E=SEC*amount[i]*trip*mean.transport.data[3]
				    FOC=amount[i]*trip*mean.transport.data[3]
				    PC=0
				    DC=0
				    }
				
				    
# 				    print(E)
# 				    print(transport)
# 				    browser()
                        
                        
                        diesel.consumption=amount[i]*trip*mean.transport.data[3]
                        petrol.consumption=amount[i]*trip*mean.transport.data[4]

#                         print(foutput)
#                         browser()
                
#                         print(E)
#                         browser()
			#calculating the impact related to production
			location=get("transport.fuel.production.location", envir=LCA.env)
# 			print(location)
			GC=0
			CC=0
			LOC=0
			PC=as.numeric(as.vector(PC))
			DC=as.numeric(as.vector(DC))
			FOC=as.numeric(as.vector(FOC))
# 			 print(PC)
# 			 print(DC)
# 			 print(LOC)
# 			 print(GC)
# 			 print(CC)
# 			 print(FOC)
# 			 print(location)
# 			print("This is the output of the fuction fp")
                        assign("GC", GC, envir=LCA.env)
                        assign("CC", CC, envir=LCA.env)
                        assign("LOC", LOC, envir=LCA.env)
                        assign("FOC", FOC, envir=LCA.env)
                        assign("PC", PC, envir=LCA.env)
                        assign("DC", DC, envir=LCA.env)
                        assign("location", location, envir=LCA.env)
#                         print(PC)
#                         browser()
 
			fp(LCA.env) 
			  
			production=get("output", envir=LCA.env)
#                          print(production)
#                         browser()
#                         The input data are read line by line, adding column to column
# _____________________________________________________________________________________________________________________________________________
                     
                        for (n in 2:length(production)) {
                                                                   
                                                                   nemission=mean.transport.data[n+3]*amount[i]*trip
#                                                                    print(mean.transport.data[1,])
#                                                                      print(nemission)
#                                                                     print(n+3)
#                                                                     print(i)
#                                                                     browser()
#                                                                    
                                                                              
#                                                                     print(production)
								    foutput[i,n]=as.numeric(sum(nemission, production[n]))
								    foutput[i,1]=as.numeric(E[i]+production[1])
#                                                                         print(foutput)
#                                                                         browser()
#                                                                    
                                                                   
                                                                    }
#   				print(output)  
	
		    
# 		    colnames(transportd)=c(label, "Energy GJ kg-1 km-1", colnames(mean.transport.data[5:length(mean.transport.data)]))
#                     print("transport")
        
                    
#                     print(production)
 	        
# 		   print(foutput) 
# #                   print(outputi)
#                  browser()

# 	                if (y==0){
# 	                
# 			print(output)
# 			browser()
#                        }
                        

                        
#                         print(output)
#                         browser()                        


                        }
 output=foutput                       
# print(head(output))
#correcting for vectors
check=is(output)
output=data.frame(output)
# if ((check[1]=="vector")|(check[2]=="vector"))   {
colnames(output)=c("Energy consumption GJ", "CO2 kg", "CO2 biogenic kg", "CH4 kg", "N2O kg", "CO kg", "NH3", "NO2 kg", "SO2 kg", "NMVOC kg", "PM kg", "SF6 kg", "NO3 to water kg kg-1", "PO4 kg", "P kg") 

assign("output", output, envir=LCA.env)


#    print(output)
 











}