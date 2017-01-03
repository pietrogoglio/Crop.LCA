fpt=function(LCA.env){
tab=get("tab", envir=LCA.env)
materials=get("materials", envir=LCA.env)
fico=get("fico", envir=LCA.env)
# #print(fico)
total.fertiliser.production.transport=matrix(nrow=nrow(fico), ncol=ncol(materials)-4)
fertiliser.pollution=matrix(nrow=nrow(fico), ncol=3)
# #print(fertiliser.pollution)

for (i in 1:nrow(tab)){

			if (i==1){
				  r=1
				  }
			      
              
	if  ((grepl("fertiliser", tab[i, 4])==TRUE)|(grepl("Fertiliser", tab[i, 4])==TRUE)){
#  
# 		  #print(i)
# 		  #print(tab[i,14])
# # 		  #print(tab[i,13])
# retrieving data for fertiliser component
		  fertiliser.data=as.character(tab[i,14])
	          fertiliser.data=str_replace_all(fertiliser.data, ",", "")
		  fertiliser.data=(strsplit(fertiliser.data, " "))
		 
		fertiliser.data=data.frame(fertiliser.data)
		fertiliser.data=(as.vector(as.matrix(fertiliser.data)))
# 		  #print(fertiliser.data)
		  fertiliser.number=length(fertiliser.data)/2
# 		 #browser() 
##		# separating each component of the compound fertiliser retrieving its name and NPKS component
# the fertiliser should be put in this order for instance NPKS composed of NH4H2PO4, KCl, (NH4)2SO4
# NPKS 35-25-10-10, NH4H2PO4 11-52-0-0, KCl 0-0-60-0, (NH4)2SO4 20.5-0-0-24
# for nutrient present there should be a zero the only acceptable format are 0-0-0-0 for NPKS or 0-0-0 NPK
		  for (f in (1:(fertiliser.number))){
						      
							if (f==1) {
								    fertiliser.type=fertiliser.data[f]
# 								    #browser()
# s								    #print(fertiliser.type)
								    
							}
							else {
							
							      fertiliser.type.i=fertiliser.data[2*f-1]
							      fertiliser.type=c(fertiliser.type, fertiliser.type.i)
							    
							}
							if (f==1) {
								    fertiliser.concentration=fertiliser.data[2*f]
							}
							else{
								fertiliser.concentration.i=fertiliser.data[2*f]
								fertiliser.concentration=c(fertiliser.concentration, fertiliser.concentration.i)
							
							}
		  
		  }


		  if ((grepl("NPKS", fertiliser.type[1])==TRUE)|(grepl("NP", fertiliser.type[1])==TRUE)){
			  for (f in 1:length(fertiliser.concentration)) {
									if (f==1) {
									fertiliser.concentration.c=str_replace_all(fertiliser.concentration[f], "-"," ")
									 fertiliser.concentration.c=(strsplit(fertiliser.concentration.c, " "))
									fertiliser.concentration.c=data.frame(fertiliser.concentration.c)
									fertiliser.concentration.c=(as.vector(as.matrix(fertiliser.concentration.c)))
									
									}
									else{
									      fertiliser.concentration.c.i=str_replace_all(fertiliser.concentration[f], "-"," ")
									      fertiliser.concentration.c.i=(strsplit(fertiliser.concentration.c.i, " "))
									      fertiliser.concentration.c.i=data.frame(fertiliser.concentration.c.i)
									      fertiliser.concentration.c.i=(as.vector(as.matrix(fertiliser.concentration.c.i)))
									      fertiliser.concentration.c=cbind(fertiliser.concentration.c, fertiliser.concentration.c.i)
									
									}
			  
# 			  #print(fertiliser.total.concentration)
			  
	# 		  #print(i)
	# 		  #print(fertiliser.type)
# 			  #print(fertiliser.concentration.c)
# 			  #browser()
			  
			  
			  
			  }
			 
			 col=ncol(fertiliser.concentration.c)
			 row=nrow(fertiliser.concentration.c)
			 
			  fertiliser.concentration.c=as.numeric(fertiliser.concentration.c)
# 			   #print(fertiliser.concentration.c)
			   
			 
			  
			  fertiliser.concentration=matrix(fertiliser.concentration.c, ncol=col, nrow=row)*0.01
			  #convert the matrix to be coherent to be solved erasing empty lines
			  fertiliser.matrix=fertiliser.concentration
			 for (l in (1: nrow(fertiliser.matrix))) {
			                                      column=rep(0, ncol(fertiliser.matrix))
			                                      if (fertiliser.concentration[l,]==column){
			                                      fertiliser.matrix=fertiliser.concentration[-l, ]
			                                      }
			                                     
			 }
# 			 #browser()
			  while (ncol(fertiliser.matrix)!=(nrow(fertiliser.matrix)+1)){
					column=rep(0, nrow(fertiliser.matrix))
					fertiliser.matrix=cbind(fertiliser.matrix, column)
				
			  
			  }
# 			  #print(fertiliser.concentration)
# 			  #browser()
			  ##calculating the percentage for each nutrient supplied by a certain fertiliser component
			  component.amount=solve(fertiliser.matrix[,2:ncol(fertiliser.matrix)], fertiliser.matrix[,1])
# 			  #print(fertiliser.concentration)
# 			  #print(component.amount)
#   		          #browser()







			  }

			  fertiliser.amount=tab[i,13]
			  
			  if (grepl("\\+", fertiliser.amount)==TRUE){
			  fertiliser.amount=str_replace_all(fertiliser.amount, "\\+"," ")
			  fertiliser.amount=(strsplit(fertiliser.amount, " "))
# 			   #print(i)
			  
# 			   fertiliser.amount=data.frame(fertiliser.amount)
# 			   fertiliser.amount=(as.vector(as.matrix(fertiliser.amount)))
# 			   #print(fertiliser.amount)
# 			  #browser()
			  }
# 			  #print(fertiliser.amount)
			  fertiliser.amount=data.frame(fertiliser.amount)
			  fertiliser.amount=(as.vector(as.matrix(fertiliser.amount)))
			  fertiliser.amount=as.numeric(fertiliser.amount)
#                           #print(fertiliser.amount)

			  #print component amount
# 		  #print(fertiliser.amount)

		  for (f in (1:length(fertiliser.type))){
			
		  
		  ##calculating the impact of production
		  
                                                               if ((fertiliser.type[1]=="NPKS")|(fertiliser.type[1]=="NP")){
									if (f>1) {
									
										  if (xor((fertiliser.type[f]=="urea"),(fertiliser.type[f]=="Urea"))){
										  #calculating urea fertiliser tranport considering the distances by truck and by rail established in the assumptions file and read as global variable urea.transport.rail and urea.transport.truck
											amount=component.amount[f-1]*fertiliser.amount
# 											#print(amount)
											location=get("transport.fuel.production.location", envir=LCA.env)

										        transport.mean=get("urea.transport.mean.2", envir=LCA.env)

										        distance=as.numeric(get("urea.transport.2.distance", envir=LCA.env))
#  										        #print(distance)
#  										        #print(transport.mean)
											assign("distance", distance, envir=LCA.env)
											
										        assign("transport.mean", transport.mean, envir=LCA.env)
										        assign("amount", amount, envir=LCA.env)
#    										        #print(amount)
 										       
										        tfp(LCA.env)
										        transport.urea=get("output", envir=LCA.env)
										        #print(transport.urea)
										        transport.mean=get("urea.transport.mean.1", envir=LCA.env)
										        assign("transport.mean", transport.mean, envir=LCA.env)
										        distance=as.numeric(get("urea.transport.1.distance", envir=LCA.env))
										        assign("distance", distance, envir=LCA.env)
#  										        #print(distance)
#  										        #print(transport.mean)
#  										        #print(amount)
# 										        #browser()
											assign("location", location, envir=LCA.env)
										        tfp(LCA.env)
											output=get("output", envir=LCA.env)
#    										        #print(output)
#   										        
# 										        
										        transport.urea=transport.urea+output
#  										        #print(output)
#    										        #print(transport.urea)
#     										        #browser()
										        material="CO(NH2)2"
#  										        #browser()
## calculating impact of urea in relation to inputs of N in the environment
											amountNNH3=component.amount[f-1]*fertiliser.amount*0.46
 											amountNNO3=0
 											amountPPO4=0
# 											#print(amountNNH3)
# 											#print(170)
# 											#browser()
											location=as.character(get("urea.production.location", envir=LCA.env))
                                                                                        assign("location", location, envir=LCA.env)
                                                                                        assign("material", material, envir=LCA.env)

                                                                                        
										  }
										  
                                                                                  else{
										   #the number in fertiliser.concentration depends on the type of fertiliser. For different fertilisers, different FU units have been used (N, P2O5,K2O)
											for (l in (1:nrow(materials))){
												if ((grepl("as N", materials[l, 4]))&(fertiliser.type[f]==materials[l,2])){
												 fertiliser.basis=1
												  }
												if ((grepl("as P2O5", materials[l, 4]))&(fertiliser.type[f]==materials[l,2])){
													fertiliser.basis=2
												  }
												if ((grepl("as K2O", materials[l, 4]))&(fertiliser.type[f]==str_replace_all((as.character(materials[l,2])), " ", ""))) {
													fertiliser.basis=3
												}
									
# 												#print("fertiliser.basis")
												
											}
# 											#print("fertiliser.basis")
# 												#print(fertiliser.basis)
# 										   #print(component.amount[f-1])
# 										   #print(fertiliser.concentration[fertiliser.basis,f])
										   amount=component.amount[f-1]*fertiliser.amount*fertiliser.concentration[fertiliser.basis,f]
										  
										   location=as.character(get("fertiliser.inventory.location", envir=LCA.env))
										   material=fertiliser.type[f]
# 										   #print(material)
# 										   #browser()
# # 										  to adjust for KCl
#                                                                                    if (grepl("KCl", material)==TRUE){
# 											 material=str_replace_all(material, "Cl", "Cl ")
# 											 #print(material)
#                                                                                    }
# calculating the amount of N-NO3 and P-PO4 introduced in the system through fertiliser

                                                                                  amountNNH3=component.amount[f-1]*fertiliser.amount*fertiliser.concentration[1,f]
                                                                                  amountPPO4=component.amount[f-1]*fertiliser.amount*fertiliser.concentration[2,f]*0.44
#                                                                                   #print(amountNNH3)
#                                                                                   #print(amountPPO4)
#                                                                                   #browser()
                                                                                  assign("location", location, envir=LCA.env)
                                                                                  assign("material", material, envir=LCA.env)
                                                                                  assign("amount", amount, envir=LCA.env)
                                                                                  }
                                                
                                                                                  
										   #print(LCA.env$amount)
                                                                                   #print(amount)
                                                                                   #print(component.amount)
#                                                                                   #print(location)
										   #print(f)
                                                                                    #print(LCA.env$material)
#                                                                                   #print(output)
#                                                                                   #browser()

                                                                                    materials.production(LCA.env)
                                                                                   output=get("output", envir=LCA.env)
                                                                                   #print(output)
									           #print(270)
#                                                                                    #browser()


                                                                                   #f==2 because if f==1 we considering the data in the matrix regarding the general composition of the fertiliser
                                                                                   ##summing up all the impacts for the fertiliser component and adding the transport 
                                                                                   #this NPKS and NP fertiliser were used during seeding therefore the assumption is that the fertiliser is transport by the fertiliser supplier with diesel truck with the same size as the one use to transport fertiliser from the fertiliser manufacturer to the local storehouse
                                                                                   if (f==2){
												  fertiliser.transport.production=output
# 											  #print(fertiliser.transport.production)  
									
											  if (xor((fertiliser.type[f]=="urea"),(fertiliser.type[f]=="Urea"))){
#  											  #print(transport.urea)
#  											  #print(fertiliser.transport.production)
											  fertiliser.transport.production=fertiliser.transport.production+transport.urea
# 											  #print(fertiliser.transport.production)
# 											  #browser()
											  }
# 											  #print(fertiliser.transport.production)
                                                                                   }
                                                                                  else{
											if (xor((fertiliser.type[f]=="urea"),(fertiliser.type[f]=="Urea"))){
											    fertiliser.transport.production=fertiliser.transport.production+transport.urea
											    }
 											#print(fertiliser.transport.production)
											fertiliser.transport.production=fertiliser.transport.production+output
 											#print(output)
 											#print(fertiliser.transport.production)
# 											#browser()
											if (f==length(fertiliser.type)){
												 transport.mean=as.character(get("fertiliser.local.transport.mean", envir=LCA.env))
												 distance=as.numeric(as.character((get("fertiliser.local.transport.distance", envir=LCA.env))))
												 assign("distance", distance, envir=LCA.env)
												 assign("transport.mean", transport.mean, envir=LCA.env)
												 location=as.character(get("transport.fuel.production.location", envir=LCA.env))
# 												 #print(location)
# 												 #print(transport.mean)
# 												 #print(distance)
												 assign("amount", fertiliser.amount, envir=LCA.env)
#  												 print(LCA.env$amount)
# 												 #print(distance)
# 												 #print("distance")
											         assign("location", location, envir=LCA.env)
 											         #print(312)
        												 #print(LCA.env$location)
 # 												 browser()

												 tfp(LCA.env)
												 output=get("output", envir=LCA.env)
# # 												 #browser()
 												 #print(fertiliser.transport.production)
    												 #print(output)
 												 fertiliser.transport.production=fertiliser.transport.production+output
    												 #print(fertiliser.transport.production)
    												 #print(246)
    												 ##browser()
												 }
											}
												  ##summing up the impact related to fertiliser application due to nutrient supply, the data is stored in variable fertiliser.pollution.i which has the first 4 columns from the tab table
										
										lineapollution=cbind(amountNNH3, amountNNO3, amountPPO4)
										if (f==2){
											  fertiliser.pollution.i=lineapollution
										}
										else{
										      fertiliser.pollution.i=fertiliser.pollution.i+lineapollution

# 										 #print(fertiliser.pollution.i)
# 										 #print(280)
# 										 #browser()
										}
#                                                                                  #print(output)
                                                               
                                                               
                                                               }
                                                               
                                                               }

                                                               else{
#                                                                #print(i)
#                                                                #print("i")                                                               
#                                                                #print(fertiliser.type)
#                                                                #print(fertiliser.concentration)
                                                               for (a in (1:length(fertiliser.amount))){
	##                                                               # computing the impact related to single fertilisers, or separate fertilisers, the transport distance is established depending on a survey of fertiliser manufacture and the boundary of LCI inventory
									if ((fertiliser.type[f]=="Urea")|(fertiliser.type[f]=="urea")|(fertiliser.type[f]=="NH3")&((f==a)==TRUE)){
	#  									#print(fertiliser.amount)
	#  									#print(fertiliser.type[f])
										amount=fertiliser.amount[a]
										assign("amount", amount, envir=LCA.env)
										
# 										#print(amount)
#  										#print(324)
# 										Sys.sleep(5)
# 									        #browser()
	#									    computing the impact related to transport of urea accorgind to the urea.transport.rail and urea.tranport.truck variable the distance have been set as an average between thre fertiliser manufacture facilities close to Glenlea farm experiments and read in the assumptions file
										  if ((fertiliser.type[f]=="Urea")|(fertiliser.type[f]=="urea")){
											      distance=as.numeric(as.character(get("urea.transport.2.distance", envir=LCA.env)))
											      transport.mean=as.character(get("urea.transport.mean.2", envir=LCA.env))
											      location=as.character(get("transport.fuel.production.location", envir=LCA.env))
# 											      #print(location)
# 											      #print(distance)
# 											      #print(transport.mean)
# 											      #print(369)
# 											      #browser()
											      }
											      
	#									  computing the impact related to transport of urea accorgind to the NH3.transport.rail and NH3.tranport.truck variable the distance according to a survey of manufacturers for Glenlea farm experiments and read in the assumptions file
										  if (fertiliser.type[f]=="NH3"){
											      distance=as.numeric(as.character(get("ammonia.transport.2.distance", envir=LCA.env)))
											      transport.mean=as.character(get("ammonia.transport.mean.2", envir=LCA.env))
											      location=as.character(get("transport.fuel.production.location", envir=LCA.env))
											      #print(location)
											      }
										  assign("distance", distance, envir=LCA.env)
										  assign("location", location, envir=LCA.env)
										  
	#  	    										        #print(distance)
	#  	    										        #print(transport.mean)
										  assign("transport.mean", transport.mean, envir=LCA.env)
#   	    										        #print(LCA.env$amount)
#   	    										        #print(LCA.env$location)
#   	    										        #print(LCA.env$distance)
#    	  	   										        #browser()
										  tfp(LCA.env)
										  fertiliser.transport=get("output", envir=LCA.env)
# 										  #print(fertiliser.transport)
# 										  #browser()
										  
	#									  computing the impact related to transport of urea accorgind to the urea.transport.rail and urea.tranport.truck variable the distance have been set as an average between thre fertiliser manufacture facilities close to Glenlea farm experiments and read in the assumptions file
										  if ((fertiliser.type[f]=="Urea")|(fertiliser.type[f]=="urea")){
											      distance=as.numeric(as.character(get("urea.transport.1.distance", envir=LCA.env)))
											      transport.mean=as.character(get("urea.transport.mean.1", envir=LCA.env))
											      material="CO(NH2)2"
											      location=as.vector(as.character(get("transport.fuel.production.location", envir=LCA.env)))
											      }
											      
	# 									  computing the impact related to transport of urea accorgind to the NH3.transport.rail and NH3.tranport.truck variable the distance according to a survey of manufacturer for Glenlea farm experiments and read in the assumptions file
										  if (fertiliser.type[f]=="NH3"){
											      distance=as.numeric(as.character(get("ammonia.transport.1.distance", envir=LCA.env)))+as.numeric(get("f.t.d", envir=LCA.env))
											      transport.mean=as.character(get("ammonia.transport.mean.1", envir=LCA.env))
											      location=as.character(get("transport.fuel.production.location", envir=LCA.env))
											      
											  material="NH3"
											      }
										  
										  assign("distance", distance, envir=LCA.env)
										  assign("material", material, envir=LCA.env)
										  assign("transport.mean", transport.mean, envir=LCA.env)
										  assign("location", location, envir=LCA.env)
										  
#  	 												    #print(LCA.env$distance)
#  	 												    #print(LCA.env$transport.mean)
#  	 												    #print(LCA.env$amount)
#   	 											    #browser()
										  tfp(LCA.env)
	                                                                          output=get("output", envir=LCA.env)	
# 	                                                                          #print(output)
# 									          #browser()				
	#  		       							  #print("fertiliser.transport")
# 	  		       							  #print(fertiliser.transport)
										  fertiliser.transport=fertiliser.transport+output
	# #  	    										        #print(output)
# 	  	     										        #print(fertiliser.transport)
	#  									  #print(material)
	#  									  #print(fertiliser.amount)
	                                                                          if ((fertiliser.type[f]=="Urea")|(fertiliser.type[f]=="urea")){
											  location=as.vector(as.character(get("urea.production.location", envir=LCA.env)))
	                                                                          }
	                                                                          
	                                                                          if (fertiliser.type[f]=="NH3"){
											  location=as.vector(as.character(get("ammonia.production.location", envir=LCA.env)))
	                                                                          }
	                                                                          
 										  assign("location", location, envir=LCA.env)
#  										  #print(430)
#   	 									  #browser()
#  										  
										#calculating the impact of material production
										
										  materials.production(LCA.env)
										  output=get("output", envir=LCA.env)
										  # 									  #print(i)
#  	 									  #print(output)
#  	 									  #print(fertiliser.transport)
										  fertiliser.transport.production=fertiliser.transport+output
										  
#  	 									 #print(fertiliser.transport.production)
# 										#print(443)
#  	 									  #browser()
										##computingp the impac of supply fertiser in onthe agroecosyste
										  if ((fertiliser.type[f]=="Urea")|(fertiliser.type[f]=="urea")){
											      concentration=0.46
											      }
										  if (fertiliser.type[f]=="NH3"){
											      concentration=0.824
											      }
											      
										amountNNH3=fertiliser.amount[a]*concentration 
										amountNNO3=0
										amountPPO4=0
										##summing up the impact related to fertiliser application due to nutrient supply, the data is stored in variable fertiliser.pollution.i which has the first 4 columns from the tab 
										fertiliser.pollution.i=cbind(amountNNH3, amountNNO3, amountPPO4)
										if (length(fertiliser.amount)>1){
														  
														  fertiliser.transport.production.ua=fertiliser.transport.production
														  fertiliser.pollution.i.ua=fertiliser.pollution.i
# 														  #print(concentration)
# 														  
# 														  #print(fertiliser.transport.production)
# 														  #print(fertiliser.transport.production.ua)
# 														  #print(fertiliser.pollution.i)
# 														  #print(fertiliser.pollution.i.ua)
# 														  #browser()
														
														 
										  }
	# 									#print(fertiliser.pollution.i)
	# 									#browser()
# 									        #print(a)
# 									        #print(f)
										  }


									if ((fertiliser.type[f]=="[NH4PO4]n")&(f==a)) {
										
										
		# 								#print(i)
										fertiliser.concentration=str_replace_all(fertiliser.concentration[f], "-"," ")
										fertiliser.concentration=(strsplit(fertiliser.concentration, " "))
										fertiliser.concentration=data.frame(fertiliser.concentration)
										fertiliser.concentration=as.numeric(as.vector(as.matrix(fertiliser.concentration)))
										fertiliser.concentration=fertiliser.concentration*0.01
										#computing transport impact
										distance=as.numeric(as.character(get("fertiliser.local.transport.distance", envir=LCA.env)))
		# 						                The impact of this fertiliser for transport has been calculated considering that the density of the fertiliser is 1 kg dm-3, despite the fertiliser solution might have a higher density

										amount=fertiliser.amount[a]
# 										#print(amount)
# 										#print(435)
# 										Sys.sleep(5)
# 										#browser()
										location=as.character(get("transport.fuel.production.location", envir=LCA.env))

										transport.mean=as.character(get("fertiliser.local.transport.mean", envir=LCA.env))
										assign("location", location, envir=LCA.env)
										assign("transport.mean", transport.mean, envir=LCA.env)
										assign("amount", amount, envir=LCA.env)
										assign("distance", distance, envir=LCA.env)
										#print(515)
 										#browser()
										
										tfp(LCA.env)
																														
										

										fertiliser.transport=get("output", envir=LCA.env)

										material="(NH4)H2PO4"

		# 								#print(amount)
		# 								#print(material)
# 										#print(fertiliser.concentration)
										amount=amount*as.numeric(fertiliser.concentration[1])
										location=as.character(get("fertiliser.inventory.location", envir=LCA.env))
										assign("amount", amount, envir=LCA.env)
										assign("location", location, envir=LCA.env)
										assign("material", material, envir=LCA.env)
										
# 										#print(amount)
# 										#print(445)
# 										Sys.sleep(5)
# 										#browser()
								
										# in this case it is considered to be monoammonium phosphate 
# 										#browser()
										materials.production(LCA.env)
										output=get("output", envir=LCA.env)
# 										#browser()
# 		 								#print(fertiliser.transport)
# 		 								#print(output)
										fertiliser.transport.production=fertiliser.transport+output
# 		 								#print(fertiliser.transport.production)
# 										#browser()
										amountNNO3=0
										amountNNH3=amount
										amountPPO4=fertiliser.amount[a]*as.numeric(fertiliser.concentration[2])*0.44
										fertiliser.pollution.i=cbind(amountNNH3, amountNNO3, amountPPO4)
		# 						                #print(fertiliser.pollution.i)
										if (length(fertiliser.amount)>1){
															  fertiliser.transport.production.asp=fertiliser.transport.production
 															  fertiliser.pollution.i.asp=fertiliser.pollution.i
# 															  #print(fertiliser.transport.production.asp)
# 															  #print(fertiliser.transport.production)
# 															  #print(fertiliser.pollution.i.asp)
# 															  #print(fertiliser.pollution.i)
# 															  
															  
											  }
		## 								#browser() #########
										}
		  
# 									#print(a)
# 									#print(fertiliser.amount)
# 									#print(f)
									if ((length(fertiliser.amount)==a)&(length(fertiliser.amount)>1)&((f==length(fertiliser.type))==TRUE)){
  										    fertiliser.transport.production=fertiliser.transport.production.ua+fertiliser.transport.production.asp
  										    fertiliser.pollution.i=fertiliser.pollution.i.ua+fertiliser.pollution.i.asp
#  										    #print("eureka")
#  										    #print(fertiliser.transport.production)
#  										    #print(fertiliser.pollution.i)
#  										    #browser()
 										    }
									
		  
# 		  #browser()
  
									}
# 									

								}
			
		  }







         














 	  
         total.fertiliser.production.transport[r,]=as.numeric(fertiliser.transport.production)
         fertiliser.pollution[r,]=as.numeric(fertiliser.pollution.i)
         
#          #print(total.fertiliser.production.transport)
#          #print(fertiliser.transport.production)
#          if (i==82){
#                    #browser()
#          }
	}
	
 	else{
             if (((grepl("Petrol", tab[i,7])==TRUE)|(grepl("petrol", tab[i,7])==TRUE))&((grepl("harvest", tab[i,4])==TRUE)|(grepl("harvesting", tab[i,4])==TRUE)|(grepl("Harvest", tab[i,4])==TRUE))){
		    total.fertiliser.production.transport[r,]=matrix(rep(0, ncol(total.fertiliser.production.transport)))
		    fertiliser.pollution[r,]=matrix(rep(0,3))
		    r=r+1
# 		    #print(r)
		    
	      }
	      total.fertiliser.production.transport[r,]=matrix(rep(0, ncol(total.fertiliser.production.transport)))
	      fertiliser.pollution[r,]=matrix(rep(0,3))
# 	      #print(fertiliser.pollution)
 	      

	
	      }
	r=r+1
# 	#print(r)
# 	#print(i)
	
# 	#print(i)
	}
	

nutrient.inputs=data.frame(fertiliser.pollution)
colnames(nutrient.inputs)=c("amount N-NH3 kg ha-1", "amount N-NO3 kg ha-1", "amount P-PO4 kg ha-1")
nutrient.inputs=cbind(fico[,1:5], nutrient.inputs)
# #print(head(nutrient.inputs))
assign("nin", nutrient.inputs, envir=LCA.env)
write.table(LCA.env$nin, file="nutrient.inputs.csv", sep=",", row.names=FALSE)
	

total.fertiliser.production.transport=data.frame(total.fertiliser.production.transport)
colnames(total.fertiliser.production.transport)=colnames(output)
total.fertiliser.production.transport=cbind(fico[,1:5], total.fertiliser.production.transport)


assign("tfpt", total.fertiliser.production.transport, envir=LCA.env)
write.table(LCA.env$tfpt, file="total.fertiliser.production.transport.csv", sep=",", row.names=FALSE)

# #print(head(total.fertiliser.production.transport))





























}