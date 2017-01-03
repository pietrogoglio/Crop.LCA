ppt=function(LCA.env){
tab=get("tab", envir=LCA.env)
materials=get("materials", envir=LCA.env)
output=get("output", envir=LCA.env)
fico=get("fico", envir=LCA.env)

#print("This function computes the impact of production, transport and retailing according to the Ecoinvent 2.0 data, while the impact related to transport, considering a petrol truck and a distance from St Agathe to Glenlea according to GHGenious")
#print("The output of this function has the same structure of the fico variable and it is saved in a global table called tppt and in the file total.pesticide.production.transport.csv")

counter=0
 for (i in 1:nrow(tab)) {
# 			  #print(i)
# 			  #print(tab[i,4])
# 			  #print(tab[i,12])
#filling the firsty empty line
			  if (i==1) {
			                  total.pesticide.production.transport=rep(0, ncol(output))         
			      }	
			  else{
				  
				    if ((grepl("Chemical weeding", tab[i,4])==TRUE)|(grepl("Pesticide", tab[i,4])==TRUE)){
	  # 									    #print(tab[i,])
	  # 									    #print(i)
	  # 									    #print(tab[i, 11])
	  # 									 
	  # 									    #print(tab[i, 12])
										  
										  # retrieving the active ingredient name for each active ingredient used in the experiment and making a data frame 
										  #____________________________________________________________________________________________________________________________________________________________________________________________________________________________
										      active.ingredient=tab[i,11]
										      #print(tab[,11])
										      #browser()
										      if (grepl("\\+", tab[i,11])==TRUE) {
															active.ingredient=str_replace_all(tab[i,11], "\\+", " ")			
															active.ingredient=data.frame(strsplit(active.ingredient, " "))
															
															}
										  active.ingredient=as.vector(as.matrix(active.ingredient))
# 										  
# 										  #print(active.ingredient)
# 										  #browser()


	  # 	#retrieving from the tab variable the conccentraion (% or g l-1) and the amount of product used in (l ha-1/g ha -1)
								  #____________________________________________________________________________________________________________________________________________________________________________________________________________________________
													  
										      if ((grepl("\\+", tab[i,12])==TRUE)|(grepl("\\*", tab[i,12])==TRUE)){
															amount.data=str_replace_all(tab[i,12], "\\+", " ")
															amount.data=str_replace_all(amount.data, "\\*", " ")
															amount.data=data.frame(strsplit(amount.data, " "))
															amount.data=as.numeric(as.vector(as.matrix(amount.data)))
# 															#print(amount.data)
															}
	  # 													      index=length(amount.data)/2
	  # 													      #print(index)

										      #calculating the impact of production of the active ingredient, transport of pesticide and fuel production for fuel used of transport of pesticide
				      #____________________________________________________________________________________________________________________________________________________________________________________________________________________________
										  
										      
															for (ind in (1:length(active.ingredient))){
																	      
																	      e=2*(ind)
																	      une=2*(ind-1)+1
																	      concentration=amount.data[e]
																	      pest=amount.data[une]
# 																	      #print(une)
# 																		#print(pest)
# 																		  #print(e)
# 																	      #print(concentration)
																	      LCA.env$material=as.character(active.ingredient[ind])
																	      LCA.env$amount=concentration*pest
																	      LCA.env$location=as.character(LCA.env$pesticide.production.location)
	 																     #print(LCA.env$amount)
# 	   																     #print(LCA.env$material)
																     #print(LCA.env$location)
# 																     #browser()
																		
# 																	      #print(output)
																	      pesticide.production=materials.production(LCA.env)
 																	      #print(pesticide.production)
# 																	      #print(pesticide.production)
  																	      #browser()
																	      
																	      LCA.env$amount=concentration*pest+pest
																	      LCA.env$distance=as.numeric(as.character(LCA.env$pesticide.transport.distance))
																	      LCA.env$transport.mean=as.character(LCA.env$pesticide.transport.mean)
	  # 																     #print(amount)
	  # 																     #print(distance)
																	      
																	      tfp(LCA.env)
																	     
																	      pesticide.transport=LCA.env$output
#  																	      #print(pesticide.transport)
#  																	      #browser()
																	      pesticide.production.transport.i=pesticide.production+pesticide.transport
# 	   																     #print(pesticide.production)
# 	   																     #print(pesticide.transport)
# 	   																     #print(pesticide.production.transport.i)
# 	   																     #browser()
																	      if (ind==1){
																			  pesticide.production.transport.l=pesticide.production.transport.i																                 
																			  }
																	      else{
# 																		    #print("pesticide.production.transport.l")
	  # 																	  #print(pesticide.production.transport.l)
																		    pesticide.production.transport.l= pesticide.production.transport.l+pesticide.production.transport.i
# 																		    #print("pesticide.production.transport.i")
	  # 																	  #print(pesticide.production.transport.i)
	  # 																	  #print(pesticide.production.transport.l)
	  # 																	  #browser()
																		    }
															}
															
				 
# 				 colnames(total.pesticide.production.transport)=colnames(pesticide.production.transport.l)
                                 label=colnames(pesticide.production.transport.l)
 				 pesticide.production.transport.l=as.vector(as.numeric(pesticide.production.transport.l))
#  				 print(total.pesticide.production.transport)
				  total.pesticide.production.transport=rbind(total.pesticide.production.transport, pesticide.production.transport.l)										      
# 				  #print(total.pesticide.production.transport)
# 				  #browser()
				  }
				    #filling up empty lines
				  else{
				      emptyline=rep(0, ncol(output))
				      total.pesticide.production.transport=rbind(total.pesticide.production.transport, emptyline)
					# to double the empty line)
					if ((grepl("Petrol", tab[i, 7])==TRUE)| (grepl("petrol", tab[i, 7])==TRUE)){
						  total.pesticide.production.transport=rbind(total.pesticide.production.transport, emptyline)
					}
 					
				  }
			 }
			
		































































	}

	colnames(total.pesticide.production.transport)=label
total.pesticide.production.transport=cbind(fico[,1:5], total.pesticide.production.transport)


assign("tppt", total.pesticide.production.transport, envir=LCA.env)
write.table(total.pesticide.production.transport, file="total.pesticide.production.transport.csv", sep=",", row.names=FALSE)



























}