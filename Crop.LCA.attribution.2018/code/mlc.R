mlc=function(LCA.env) {

malc=get("malc", envir=LCA.env)
fuel=get("fuel", envir=LCA.env)
machinery=get("machinery", envir=LCA.env)
tab=get("tab", envir=LCA.env)
materials=get("materials", envir=LCA.env)
farm.transport.assumptions=get("farm.transport.assumptions", envir=LCA.env)
md=get("md", envir=LCA.env)
fico=get("fico", envir=LCA.env)

 #print("This function computes impact related to the machinery production including machinery manufacture, raw material extraction, transport of raw materials according to GHG genious for self propelled machinery")
 #print(" ")
 #print("impact related to maintenance and repairs are computed according to Audsley et al 1997")
 #print(" ")
 #print("transport distance for raw materiasls are included in GHG genius for the distance covered up to the factory gate, while from the factory gate to the end user the impact is estimated on the basis of the average freight ")
 #print(" ")
 #print("this function consider as the first operating machinery made totally of steel, while the second one as trailer therefore composed of 95% steel and 5 % rubber")
#setting a counter in order to distinguish for double lines for harvest                           
  linea=0
for (i in 1:nrow(tab)){

		    tractor.weight=machinery[i, 6]
		    operating.machinery.weight=machinery[i,8]

	#correcting for the presence of plus and bracket signs
#____________________________________________________________________________________________________________________________________________________________________________________________________________________________	
	if (grepl("\\(|\\)", tractor.weight)==TRUE) {
# # 		#print("reading  and correcting in case there are two machines like during harvest")
# # 		#print("get rid of the brackets  
# # 		340 (189)")
		
		tractor.weight=str_replace_all(tractor.weight, "\\(|\\)", "")
		}
		
	if (grepl("\\+", tractor.weight)==TRUE) {
# 	      #print(tractor.weight)
	# 		        browser()
	      tractor.weight=str_replace_all(tractor.weight, "\\+", " ")			
# 	      #print("transform the string in a dataframe in order to extract the elements present")
		tractor.weight=data.frame(strsplit(tractor.weight, " "))
		
# 		  #print(tractor.weight)

	      
		# #print(hp)
	# 						      hp1=hp[2,1]
	# 						      hp=hp[1,1]
	# 							#print("transform everything as a vector")											
	# 						      
		# #print(hp)
		# #print(hp1)
		
		    
		}
	tractor.weight=as.numeric(as.vector(as.matrix(tractor.weight)))


		#taking out brackets and plus signs
	if (grepl("\\(|\\)", operating.machinery.weight)==TRUE) {
# 		#print("reading  and correcting in case there are two machines like during harvest")
# 		#print("get rid of the brackets  
# 		340 (189)")
		
		operating.machinery.weight=str_replace_all(operating.machinery.weight, "\\(|\\)", "")
		}
		
	if (grepl("\\+", operating.machinery.weight)==TRUE) {
# 	      #print(operating.machinery.weight)
	# 		        browser()
	      operating.machinery.weight=str_replace_all(operating.machinery.weight, "\\+", " ")			
# 	      #print("transform the string in a dataframe in order to extract the elements present")
		operating.machinery.weight=data.frame(strsplit(operating.machinery.weight, " "))
		
		  			  
		    
		}
	#correcting for NA presence
# 	#print(operating.machinery.weight)
# 	browser()
	if (operating.machinery.weight=="") {
	operating.machinery.weight=0
	}
	operating.machinery.weight=as.numeric(as.vector(as.matrix(operating.machinery.weight)))

		

# 	#print(operating.machinery.weight)	
# 	#print("eureka")
# 	browser()	    
# 	  
# 	#print("relating the weight of the tractor to the total number of hours")


	counter=0

	  for (col in 1:length(tractor.weight)) {
	      
                  for (mn in 1:length(operating.machinery.weight)) {
#                   #print(length(tractor.weight))
#                   #print(length(operating.machinery.weight))
#                   browser()

	#retrieving data for tractor, self propelled machinery weight, operating machinery weight and adjusting it in relation to h of time and ha of land
	#____________________________________________________________________________________________________________________________________________________________________________________________________________________________
		  
# 			  #print("distinguishing ofr petrol cars, indeed in the tab variable there is just one line for harvest while for malc there are 2 lines") 
			  if (((grepl("Petrol", tab[i,7])==TRUE)|(grepl("petrol", tab[i,7])==TRUE))&((grepl("harvest", tab[i,4])==TRUE)|(grepl("harvesting", tab[i,4])==TRUE)|(grepl("Harvest", tab[i,4])==TRUE))){
			    if (counter==0)   {
					      tractor.weight.h=rep(0, length(tractor.weight))
					      tractor.weight.h[col]=tractor.weight[col]/malc[i+linea,5+col]
 					      counter=1
# 					      #print("panta rei")
					      
					      # calculating the value for  ha in weight terms including field operation and farm transport
					      tractor.weight.ha[col]=tractor.weight.h[col]/((md[i+linea,7])+1/(((farm.transport.assumptions[i+linea,6]*2*farm.transport.assumptions[i+linea,7]/10)/25)))
					      
		      
		      
		      
					      operating.machinery.weight.h[col]=operating.machinery.weight/malc[i+linea,6]

					      # calculating the value for  ha in weight terms including field operation and farm transporttractor.weight.ha[mn]=tractor.weight.h[mn]/(md[i+linea,7])*((farm.transport.assumptions[i+linea,6]*2*farm.transport.assumptions[i+linea,7]/10)/25)
					   operating.machinery.weight.ha[col]=operating.machinery.weight.h[col]/((md[i+linea,7])+1/(((farm.transport.assumptions[i+linea,6]*2*farm.transport.assumptions[i+linea,7]/10)/25)))
					      linea=linea+1
# 					      #print("panta rei")
#        					  browser()
			    }
			    else{
			    
				tractor.weight.h[col]=tractor.weight[col]/malc[i+linea,5+col-1]
		      # 			     calculating the value for  ha in weight terms including field operation and farm transport
				tractor.weight.ha[col]=tractor.weight.h[col]/((md[i+linea,7])+1/((farm.transport.assumptions[i+linea,6]*2*farm.transport.assumptions[i+linea,7]/10)/25))
				operating.machinery.weight.h[col]=0
				operating.machinery.weight.ha[col]=operating.machinery.weight.h[col]
# 				#print("panta rei 1")
# 				browser()
				}
# 				#print("panta rei 1b")
# 			    browser()
			    }
			  else{
				if (col==1) {
				    tractor.weight.h=rep(0, length(tractor.weight))
				    tractor.weight.ha=rep(0, length(tractor.weight))
				   
				    }
				if (mn==1) {
				             operating.machinery.weight.h=rep(0, length(operating.machinery.weight))
				             operating.machinery.weight.ha=rep(0, length(operating.machinery.weight))
				}
				
				tractor.weight.h[col]=tractor.weight[col]/malc[i+linea,5+col]
				operating.machinery.weight.h[mn]=operating.machinery.weight[mn]/malc[i+linea,6+mn]
				 

				
			
				
				  
				if ((grepl("self propelled disk broadcaster", farm.transport.assumptions[i+linea, 5])==TRUE)|(grepl("combined harvester", farm.transport.assumptions[i+linea, 5])==TRUE)|(grepl("Windrower", farm.transport.assumptions[i+linea, 5])==TRUE)|(grepl("windrower", farm.transport.assumptions[i+linea, 5])==TRUE)){ 
# 				  #print("panta rei 3")
				  operating.machinery.weight.h[mn]=operating.machinery.weight/malc[i+linea,6]
				  }
				  
				#calculating the value for  ha in weight terms including field operation and farm transport
				tractor.weight.ha[col]=tractor.weight.h[col]/((md[i+linea,7])+1/((farm.transport.assumptions[i+linea,6]*2*farm.transport.assumptions[i+linea,7]/10)/25))
				operating.machinery.weight.ha[mn]=operating.machinery.weight.h[mn]/((md[i+linea,7])+1/((farm.transport.assumptions[i+linea,6]*2*farm.transport.assumptions[i+linea,7]/10)/25))
				  
				  
				  if (grepl("self propelled disk broadcaster", farm.transport.assumptions[i+linea, 5])==TRUE) {
# 				  #print("#considering that most of the way covered by the selfpropelled diskbroadcaster is a normal road and not a footpath")
				  tractor.weight.ha[col]=tractor.weight.h[col]/((md[i+linea,7])+1/((farm.transport.assumptions[i+linea,6]*2*farm.transport.assumptions[i+linea,7]/10)/50))
				  operating.machinery.weight.ha[col]=operating.machinery.weight.h[col]/((md[i+linea,7])+1/((farm.transport.assumptions[i+linea,6]*2*farm.transport.assumptions[i+linea,7]/10)/50))
# 				  #print("panta rei 2")
# 				  browser()
				  }
				  
				  if (operating.machinery.weight[col]==0){
									      operating.machinery.weight.ha[col]=0
				  }
				  
# 				#print(col)
			    }
 	       # #print(mn)
#  	        if (i==52){
# 			    browser()
# 			    }
# 		if (i==2){
# 			    browser()
# 			  }
		  }
	
        }
	




# browser()



# 	                 #print(tractor.weight.h)
# 	                 #print(length(tractor.weight.h))
# 	                 #print(tractor.weight.ha)
# 	                 #print(length(tractor.weight.ha))

#   	#print(operating.machinery.weight)
#   	#print(operating.machinery.weight.h)
#   	#print(operating.machinery.weight.ha)



# calculating the impact of material production for tractors and selfpropelled machinery, accordign to GHG genious, stored in a variable tractor.production
#____________________________________________________________________________________________________________________________________________________________________________________________________________________________
        for (l in (1:length(tractor.weight.ha))) {
                 for (ro in (1:4)){
			  amount=tractor.weight.ha[l]
			  if (ro==2)  {
				amount=tractor.weight.ha[l]*0.999
			  }
			  if (ro==3){
				    amount=tractor.weight.ha[l]*0.001
			  }
			  location=as.character(LCA.env$tractor.production.location)
			  material=materials[ro, 2]
# 			  #print(material)
                          assign("amount", amount, envir=LCA.env)
                          assign("location", location, envir=LCA.env)
                          assign("material", material, envir=LCA.env)
                          #print(LCA.env$amount)
                          #print(amount)
                          #print(LCA.env$location)
                          #print(location)
                          #print(LCA.env$material)
                          #print(material)
			  materials.production(LCA.env)
			  output=get("output", envir=LCA.env)
 			  #print(output)
 			  #print(222)
# 			  browser()
			  tractor.productioni=output
# 			  #print(tractor.productioni)
# 			  			  browser()
			  if (ro==1) {
			   tractor.production=tractor.productioni
			  }
			  else{
# 			      #print(tractor.production)
			       for (col in 1:length(tractor.production)) {					
					tractor.production[col]=tractor.production[col]+ tractor.productioni[col]
					}
						
 			       }
#  			       #print(tractor.productioni)
#     	        #print(tractor.production)
#     	        #print(237)
# 		browser()
#conputing energy for manufacture needed for maintenanace and reparis on ha basis 
		  if (ro==4){
                                                                   
		             EMi=tractor.production[1]
			    
		  }
                 
 	        }
 	        
		if (l==1) {
		      tractor.production1=tractor.production
		      EM=EMi
		      }
		if (l==2) {
		      tractor.production=rbind(tractor.production1, tractor.production)
		      EM=c(EM, EMi)
# 		      browser()
		      }
		EM=as.numeric(as.vector(EM))
		
#   	  #print(EM)
# 	  
# 	 
# 		
# 		
#  	  #print(tractor.production)
 	  
 	  


	               
 
# transport of machine of tractors, self-propelled machinery from the factory to the farm and related impact due to the production and transport of fuel
#____________________________________________________________________________________________________________________________________________________________________________________________________________________________

# 		#print("calculating the impact of tractor transport from the machinery factory to the end user")
		amount=tractor.weight.ha[l]
# 		if (length(tractor.weight.ha)>1){
# 		          #print(amount)
# 		          browser()
# 		}
		distance=as.numeric(as.character(LCA.env$tractor.transport.distance.1))
		transport.mean=as.character(LCA.env$tractor.transport.mean.1)
		location=as.character(LCA.env$transport.fuel.production.location)
		assign("transport.mean", transport.mean, envir=LCA.env)
		assign("amount", amount, envir=LCA.env)
		assign("distance", distance, envir=LCA.env)
		assign("location", location, envir=LCA.env)
		#print(amount)
		#print(LCA.env$amount)
		#print(distance)
		#print(LCA.env$distance)
		#print(amount)
		#print(LCA.env$amount)
		#print(location)
		#print(LCA.env$location)
		tfp(LCA.env)
		
		tractor.transport.rail=get("output", envir=LCA.env)
# 		#print(tractor.transport.rail)
		distance=as.numeric(as.character(LCA.env$tractor.transport.distance.2))
		transport.mean=as.character(LCA.env$tractor.transport.mean.2)
		location=as.character(LCA.env$transport.fuel.production.location)
		assign("transport.mean", transport.mean, envir=LCA.env)
		

		assign("distance", distance, envir=LCA.env)
		assign("location", location, envir=LCA.env)

		#print(amount)
		#print(LCA.env$amount)
		#print(distance)
		#print(LCA.env$distance)
		#print(amount)
		#print(LCA.env$amount)
		#print(location)
		#print(LCA.env$location)
		tfp(LCA.env)
		tractor.transport.truck=get("output", envir=LCA.env)
 		#print(tractor.transport.truck)
 		#print(288)
#       	  browser()
# 		
		tractor.transport=rep(0,length(tractor.transport.truck))
		for (coltrans in 1:length(tractor.transport.rail)){
		  tractor.transport[coltrans]=tractor.transport.truck[coltrans]+tractor.transport.rail[coltrans]
		  
		}
   
		tractor.transport=unlist(tractor.transport)
		tractor.transport=data.frame(tractor.transport[1],tractor.transport[2], tractor.transport[3],tractor.transport[4], tractor.transport[5],tractor.transport[6], tractor.transport[7],tractor.transport[8], tractor.transport[9],tractor.transport[10], tractor.transport[11], tractor.transport[12], tractor.transport[13], tractor.transport[14], tractor.transport[15])
#  		#print(2991)
#  		browser()
		if (l==1) {
		      tractor.transport1=tractor.transport          
		      }
		if (l==2) {
		      tractor.transport=rbind(tractor.transport1, tractor.transport)
		      }
		      colnames(tractor.transport)=colnames(output)
# 		      #print("tractor.transport")
# 	      #print(tractor.transport)
# 	      browser()
	      
	      	  
#	  calculating energy and emissions due to maintenance and repairs, the value are stored in the MandR variables in case of harvest the variable is a dataframe with two lines.
#____________________________________________________________________________________________________________________________________________________________________________________________________________________________
		# computing  energy related to maintenance and repairs
		if (l==1) {
			EMR=0
			}
# 			browser()
		EMR[l]=0.26*EM[l]
      # 	  #print(EMR)
		
		if (((grepl("combined harvester", machinery[i, 7])==TRUE)|(grepl("combined harvester", machinery[i, 7])==TRUE))&(l==1)){
			EMRi=0.23*EM[1]
# 			#print("eureka")
# 			browser()
			}
		
		
		if (l==2) {
			    EMR=c(EMRi, EMR[l])
      # 		      #print(EMRi)
			    
			  

			  }
		}
#         if ((i==10)|(i==55)|(i==64)){ 
# 	  #print(EM)
#  	  #print(EMR)
#  	  browser()
# 	  }
# 	  browser()
#computing emissions related to maintenance and repairs
          
			  if (length(EMR)==1){
			                        EMR=as.numeric(as.vector(EMR))
			                        EC=0.62*EMR
# 			                        #print("eureka")
			                        PC=0
			                        DC=0.03*EMR
			                        LOC=0
			                        GC=0
			                        
			                        
			                        CC=0
			                        FOC=0.35*EMR
			                        LCA.env$location=as.character(LCA.env$maintenance.repairs.location)
			                        
			                        distance=as.numeric(as.vector(LCA.env$f.t.d))
						transport.mean=as.character(LCA.env$f.t.m)
						amount=DC+FOC
 						#print(amount)
 						#print(transport.mean)
 						#print(distance)
						#print(416)
# 						###print(is(distance))
 						#print(LCA.env$location)
						#print(LCA.env$distance)

						
						tf(LCA.env, distance, amount, transport.mean)
						output=get("output",output, envir=LCA.env)
						#print(output)
						#print(425)
						DC=DC+output[1]
						tr=output
						##print(length(tr))
						DC=as.numeric(as.vector(DC))
						
						assign("DC", DC, envir=LCA.env)
						assign("PC", PC, envir=LCA.env)
						assign("LOC", LOC, envir=LCA.env)
						assign("CC", CC, envir=LCA.env)
						assign("GC", GC, envir=LCA.env)
						assign("FOC", FOC, envir=LCA.env)
						
			                        
			                        fp(LCA.env)
			                        fc=get("output", output, envir=LCA.env)
			                        ##print(length(fc))
			                        #print(443)
                                                #print(fc)
                                                
                                                electricity=LCA.env$electricity
                                                location=as.character(LCA.env$electricity.production.maintenance.repairs.location)
                                                #print(electricity)
                                                #print(location)
                                                
			                        ep(LCA.env, EC, location, electricity)
			                        ep=get("output", output, envir=LCA.env)
			                        #print(453)
			                        #print(ep)
			                        
			                        ##print(length(ep))
			                        MandR=rep(0, ncol(ep))
#  			                        browser()
			                        for (n in (1:ncol(ep))) {
			                                           
                                                                   
									     
									     ##print(tr[n+2])
							           if (n==1) {
							                     MandR[n]=EMR+tr[n+2]+fc[n]+ep[n]
							                     }
							           
                                                                   else {
									MandR[n]=tr[n+2]+fc[n]+ep[n]
									 
                                                                   }
			                                           
# 		                                           #print(fc)
# 			                                   #print(ep)
			                                           
# 			                                   #print(MandR)
# 			                        #print(406)
# 			                        browser()
			                        
			                        }
# 			                        #print(ncol(fc))
# 			                        #print(ncol(tr))
# 			                        #print(ncol(ep))
# 			     
			                        
						MandR=data.frame(MandR[1], MandR[2], MandR[3], MandR[4], MandR[5], MandR[6], MandR[7], MandR[8], MandR[9], MandR[10], MandR[11], MandR[12], MandR[13], MandR[14], MandR[15])
						colnames(MandR)=c("Energy consumption GJ", "CO2 kg", "CO2 biogenic kg", "CH4 kg", "N2O kg", "CO kg", "NH3 kg", "NO2 kg", "SO2 kg", "NMVOC kg",  "PM kg", "SF6 kg", "NO3 to water kg kg-1", "PO4 kg", "P kg")
#  				#print(MandR)
#  				#print(4151)
#  				browser()
			                        }
			                        
			    else {
			       for (l in (1:2)) {
			       			EC=0.62*EMR[l]
			                        LCA.env$PC=0
			                        DC=0.03*EMR[l]
			                        LCA.env$LOC=0
			                        LCA.env$GC=0
			                        
			                        
			                        LCA.env$CC=0
			                        LCA.env$FOC=0.35*EMR[l]
			                        LCA.env$location=as.character(LCA.env$maintenance.repairs.location)   
			                        distance=as.numeric(as.vector(LCA.env$f.t.d))
						transport.mean=as.character(LCA.env$f.t.m)
# 						#print(amount)
 						#print(transport.mean)
 						#print(distance)
						#print(510)
# 						###print(is(distance))
#  						#print(LCA.env$location)
# 						#print(LCA.env$distance)
# 						
			                        location=LCA.env$location
						amount=DC+FOC
						#print(amount)
					
# 						###print(transport.mean)
# 						distance=as.numeric(as.vector(distance))
# 						###print(is(distance))
# 						###print(transportmeanslist)
 						##print(DC)
# 						assign("transport.mean", transport.mean, envir=LCA.env)
										
						
						tf(LCA.env, distance, amount, transport.mean)
                                                output=get("output", envir=LCA.env)
						#print(output)
						#print(528)
# 						browser()
					
						LCA.env$DC=DC+output[1]
						tr=output
						##print(length(tr))
						LCA.env$location=as.character(LCA.env$fuel.production.location)
						#print("DC")
						#print(LCA.env$DC)
						#print(LCA.env$PC)
						#print(LCA.env$FOC)
						#print("FOC")
						#print(LCA.env$LOC)
						#print(LCA.env$GC)
						#print(LCA.env$CC)
						#print(LCA.env$location)
						
			                        fp(LCA.env)
			                        fc=LCA.env$output
# 			                        ##print(length(fc))
						#print(549)
						location=as.character(LCA.env$electricity.production.maintenance.repairs.location)
						#print(location)
			                        ep(LCA.env, EC, location, electricity)
			                        #print(LCA.env$output)
			                        ep=LCA.env$output
			                        #print(ep)
			                        #print(556)
			                        
			                        ##print(length(ep))
			                        MandR=rep(0, 11)
# 			                        browser()
			                        for (n in (1:ncol(ep))) {
																		      
	  #  									     ##print(tr[n+2])
									if (n==1) {
									MandR[n]=EMR[l]+tr[n+2]+fc[n]+ep[n]
									}
									    
									    else {
										  
										    MandR[n]=tr[n+2]+fc[n]+ep[n]
									    }
			                                           
#                                  ##print(fc)
#  			         				##print(ep)
#  			                                           
# 								      ##print(MandR)
			                        
			                        
			                        }
			                        MandR=data.frame(MandR[1], MandR[2], MandR[3], MandR[4], MandR[5], MandR[6], MandR[7], MandR[8], MandR[9], MandR[10], MandR[11], MandR[12], MandR[13], MandR[14], MandR [15])
			                        colnames(MandR)=c("Energy consumption GJ", "CO2 kg", "CO2biogenic kg", "CH4 kg", "N2O kg", "CO kg", "NH3 kg", "NO2 kg", "SO2 kg", "NMVOC kg",  "PM kg", "SF6 kg", "NO3 to water kg kg-1", "PO4 kg", "P kg")
#  						#print(4801)
#  			                        browser()
			                        if (l==1){
							  MandRd=MandR
# 							  browser()
			                        }
			                        if (l==2){
							  MandRd=rbind(MandRd, MandR)
							  MandR=MandRd
			                        }
			       
# 			    #print(MandR)
#  			       #print(497)
# 			       browser()
			       
			       }
			     
			       

							
			    }

 			   

# 			   if (nrow(MandR)==2) {
#  						browser()
#  			   }
#   			   if (i==81) {
#  					browser()				    
#  			  }
			  
			  
			  
			  
          
           	 # computing for material production for operating machinery, stored in a variable called operating.machinery.raw.materials.production which include all the component (rubber, steel) for all the operating machinery involved in the the crop management operation
#_____________________________________________________________________________________________________________________________________________________________________________________________________________________________
 
	
		    for (ind in (1:length(operating.machinery.weight.ha))) {
					  if (ind==1) {
					             steel=operating.machinery.weight.ha[ind]   
					             if (length(operating.machinery.weight.ha)==1){
												    steel=operating.machinery.weight.ha
# 												    #print("panta rei")
												   
					             }
					            
					             rubber=0
					  }
					  if (ind>1) {
					            steel=0.95*operating.machinery.weight.ha[ind]
					            rubber=0.05*operating.machinery.weight.ha[ind]

					  }
                                          LCA.env$location=as.character(LCA.env$operating.machinery.production.location)
	                                  LCA.env$material="steel"
	                                  LCA.env$amount=steel
	                                  #print(LCA.env$location)
	                                  #print(LCA.env$amount)
	                                  #print(LCA.env$material)
	                                  
	                                  operating.machinery.raw.materials.productions=materials.production(LCA.env)
					  #print(627)
					  #print(operating.machinery.raw.materials.productions)
					  #print(LCA.env$output)
	                                  
	                             
	                                  LCA.env$location=as.character(LCA.env$operating.machinery.production.location)
	                                  LCA.env$material="rubber"
	                                  LCA.env$amount=rubber
	                                  #print(LCA.env$location)
	                                  #print(LCA.env$amount)
	                                  #print(LCA.env$material)
	                                  
	                                  operating.machinery.raw.materials.productionr=materials.production(LCA.env)
	                                  #print(operating.machinery.raw.materials.productionr)
	                                  #print(LCA.env$output)
	                                  #print(645)
	                                  
	                                  operating.machinery.raw.materials.productioni=operating.machinery.raw.materials.productions+operating.machinery.raw.materials.productionr
	                                 
	                                  if (ind==1){
						    operating.machinery.raw.materials.production=operating.machinery.raw.materials.productioni
	                                  }
	                                  else{
					       if (((grepl("Petrol", tab[i,7])==TRUE)|(grepl("petrol", tab[i,7])==TRUE))&((grepl("harvest", tab[i,4])==TRUE)|(grepl("harvesting", tab[i,4])==TRUE)|(grepl("Harvest", tab[i,4])==TRUE))){
	                                       
							  operating.machinery.raw.materials.production=rbind(operating.machinery.raw.materials.production,operating.machinery.raw.materials.productioni)
							
							}
						else{
						     operating.machinery.raw.materials.production=operating.machinery.raw.materials.production+operating.machinery.raw.materials.productioni
						}
	                                       }
# 	                                       #print(operating.machinery.raw.materials.production)
# 	                                       #print(operating.machinery.raw.materials.productioni)
# 	                                       #print(569)
# 	                                       browser()
#  	                                       	if (i==53) {
#  	                                       	          
#  							  browser()
#  							  }
# 	                                       
 	                                  }
# 	if (length(operating.machinery.weight)>1) {
# 	
# 		browser()
# 		}
 	 
# calculating energy and emissions needed for transport of materials from the material factory to the manufacture factory, the assumption is that all the raw materials are transported by rail from Sarnia for rubber to Toronto and from Hamilton for steel to Toronto. The overall emissions and energy consumption is stored in a variable called operating.machinery.raw.materials.transport
##The impact related to fuel production and transport are already included in the assessment
#_________________________________________________________________________________________________________________________________________________________________________________________________________________________ 	  
# 	  
		    for (ind in (1:length(operating.machinery.weight.ha))) {
					  if (ind==1) {
					             steel=operating.machinery.weight.ha[ind]   
					             if (length(operating.machinery.weight.ha)==1){
												    steel=operating.machinery.weight.ha
# 												    #print("panta rei")
												   
					             }
					            
					             rubber=0
					  }
					  if (ind>1) {
							  steel=0.95*operating.machinery.weight.ha[ind]
							  rubber=0.05*operating.machinery.weight.ha[ind]

						
					      
						LCA.env$distance=as.numeric(as.character(LCA.env$steel.factory.to.operating.machinery.manufacture.factory.distance))
						LCA.env$amount=steel
						LCA.env$transport.mean=as.character(LCA.env$steel.factory.to.operating.machinery.manufacture.factory.mean)
						LCA.env$location=as.character(LCA.env$transport.fuel.production.location)

						
						#print(LCA.env$distance)
						#print(LCA.env$amount)
						#print(steel)
						#print(LCA.env$transport.mean)
						#print(LCA.env$location)
						#print(706)
						#print(i)
						tfp(LCA.env)
						
						operating.machinery.transports=LCA.env$output
						#print(operating.machinery.transports)
						#print(LCA.env$output)
						#print(709)
      # 				          browser()
						
						
						LCA.env$distance=as.numeric(as.character(LCA.env$rubber.factory.to.operating.machinery.manufacture.factory.distance))
						LCA.env$transport.mean=as.character(LCA.env$rubber.factory.to.operating.machinery.manufacture.factory.mean)
						#print(LCA.env$distance)
						#print(LCA.env$transport.mean)
						#print(rubber)
						LCA.env$amount=rubber
						#print(LCA.env$amount)
						tfp(LCA.env)
						operating.machinery.transportr=LCA.env$output
						#print(LCA.env$output)
						#print(operating.machinery.transportr)
						operating.machinery.transporti=operating.machinery.transports+operating.machinery.transportr
						
# 						 #print(operating.machinery.transports)
# 						  #print(operating.machinery.transportr)
						}
	                                  else{
						     
						LCA.env$amount=steel
						
						LCA.env$distance=as.numeric(as.character(LCA.env$steel.factory.to.operating.machinery.manufacture.factory.distance))
						LCA.env$transport.mean=as.character(LCA.env$steel.factory.to.operating.machinery.manufacture.factory.mean)
						#print(steel)
						#print(LCA.env$amount)
						#print(LCA.env$distance)
						#print(LCA.env$transport.mean)
						#print(i)
						tfp(LCA.env)
						#print(output)
						operating.machinery.transporti=LCA.env$output
						
						#print(operating.machinery.transporti)
						#print(LCA.env$output)
 						#print(733)
					    
					    }
# 	                                  #print(steel)
# 	                                  #print(rubber)
# 	                                 
# 	                                 
# # 	                                  browser()
# 	                                  #print(operating.machinery.transporti)
# 	                                  #print(i)
# 	                                  if (length(operating.machinery.weight.ha)>1){
# 						browser()
# 	                                                        
# 	                                  }
	                                  
	                                  
	                                  if (ind==1){
								operating.machinery.transport=operating.machinery.transporti
						     }
                                          else {
	                                  
					       if (((grepl("Petrol", tab[i,7])==TRUE)|(grepl("petrol", tab[i,7])==TRUE))&((grepl("harvest", tab[i,4])==TRUE)|(grepl("harvesting", tab[i,4])==TRUE)|(grepl("Harvest", tab[i,4])==TRUE))){
	                                       
							  operating.machinery.transport=rbind(operating.machinery.transport,operating.machinery.transporti)
# 							  #print(operating.machinery.transport)
# # 							  browser()	 
							
							}
						else{
						     
						
# 								#print(operating.machinery.transport)
								operating.machinery.transport=operating.machinery.transport+operating.machinery.transporti
# 								#print(operating.machinery.transport)
# 								#print(operating.machinery.transporti)
						     }
						   
						}
	                                  operating.machinery.raw.materials.transport=operating.machinery.transport
# 	                                  #print(operating.machinery.raw.materials.transport)
# 	                                  browser()	                        
	                                  
	                        }
	                        
	                        
	                        
	                        
	                        
	                        #calculating the energy required for operating machinery manufacture

	  #_____________________________________________________________________________________________________________________________________________________________________________________________________________________________                    
			       location=as.character(LCA.env$electricity.production.operating.machinery.manufacture.location)
			       #print(788)
			       electricity=data.frame(get("electricity", envir=LCA.env))
# 			       #print(electricity)
                               if (((grepl("Petrol", tab[i,7])==TRUE)|(grepl("petrol", tab[i,7])==TRUE))&((grepl("harvest", tab[i,4])==TRUE)|(grepl("harvesting", tab[i,4])==TRUE)|(grepl("Harvest", tab[i,4])==TRUE))){
					  for (ind in (1:length(operating.machinery.weight.ha))) {
					  # in this case the two different impact were not separated because in the second line the operating machinery weight is 0 kg ha-1
						    ECi=operating.machinery.weight.ha[ind]*0.0129
# 						   browser()
						    #print(location)
						    #print(ECi)
						    ep(LCA.env, ECi, location, electricity)
						    #print(LCA.env$output)
						    output=LCA.env$output
						    #print(output)
						    
						    if (ind==1){
								  ep=output    
								  EC=ECi
						    }
						    
						    else{
							    ep=rbind(ep, output)
							    EC=cbind(EC,ECi)
#  							    #print(i)
# #  							    print (ep)
#   							    #print(EC)
#  							    browser()
						    }
						    
					  }
                               }
				else{
				      total.operating.machinery.weight.ha=sum(operating.machinery.weight.ha)

				      if ((grepl("harrow", tab[i,7])==TRUE)|(grepl("harrow", tab[i,7])==TRUE)){
				      		EC=0.0086*total.operating.machinery.weight.ha
# 				      		#print(i)
# 				      		browser()
				      		}
				      else{
				      		EC=0.0074*total.operating.machinery.weight.ha
				      	
				      }
# 				      electricity=LCA.env$electricity
                                      #print(828)
#                                       #print(electricity)
				      ep(LCA.env, EC, location, electricity)
				      ep=LCA.env$output
				      #print(LCA.env$output)
				      #print(ep)
# 				      #print(828)
# 				      if(ind>1){
# # 						#print(i)
# 						
# 						}
				      }	
	  
		
	                               
# 	  				if ((i==40)&(i==76)){
# 	  							#print(i)
# 	  				                      browser()
# 	  				}
	  
	  
	  
	# transport of operating machinery from the factory to the farm and related impact due to the production and transport of fuel
#____________________________________________________________________________________________________________________________________________________________________________________________________________________________

# 		#print("calculating the impact of tractor transport from the machinery factory to the end user")
		LCA.env$amount=sum(operating.machinery.weight.ha)
		for (l in (1:length(operating.machinery.weight.ha))) {
			if (((grepl("Petrol", tab[i,7])==TRUE)|(grepl("petrol", tab[i,7])==TRUE))&((grepl("harvest", tab[i,4])==TRUE)|(grepl("harvesting", tab[i,4])==TRUE)|(grepl("Harvest", tab[i,4])==TRUE))){			
				LCA.env$amount=operating.machinery.weight.ha[l]
				}
#  		if (length(operating.machinery.weight.ha)>1){
#  		          #print(amount)
#  		          #print("718")
#  		          browser()
#  		}
		LCA.env$distance=as.numeric(as.character(LCA.env$operating.machinery.manufacture.transport.distance.1))
	
		LCA.env$transport.mean=as.character(LCA.env$operating.machinery.manufacture.transport.mean.1)

		# 		#print(amount)
		
		tfp(LCA.env)
		operating.machinery.transport.1=get("output", envir=LCA.env)
		#print(872)
		#print(operating.machinery.transport.1)
# 		#print(output)
		
# 		#print(operating.machinery.transport.rail)
		


		LCA.env$distance=as.numeric(as.character(LCA.env$operating.machinery.manufacture.transport.distance.2))
	
		LCA.env$transport.mean=as.character(LCA.env$operating.machinery.manufacture.transport.mean.2)
		#print(LCA.env$amount)
		#print(LCA.env$distance)
		#print(LCA.env$transport.mean)
		
		tfp(LCA.env)
		operating.machinery.transport.2=LCA.env$output
 		#print(operating.machinery.transport.2)
 		#print(operating.machinery.transport.1)
 		
		
		operating.machinery.transport=rep(0,length(operating.machinery.transport.1))
		for (coltrans in 1:length(operating.machinery.transport.1)){
		  operating.machinery.transport[coltrans]=operating.machinery.transport.2[coltrans]+operating.machinery.transport.1[coltrans]
		  
		}
		
      # 	  browser()
		operating.machinery.transport=unlist(operating.machinery.transport)
		operating.machinery.transport=data.frame(operating.machinery.transport[1],operating.machinery.transport[2], operating.machinery.transport[3],operating.machinery.transport[4], operating.machinery.transport[5],operating.machinery.transport[6], operating.machinery.transport[7],operating.machinery.transport[8], operating.machinery.transport[9],operating.machinery.transport[10], operating.machinery.transport[11], operating.machinery.transport[12], operating.machinery.transport[13], operating.machinery.transport[14], operating.machinery.transport[15])
		#print(operating.machinery.transport)
		#print(897)		
		colnames(operating.machinery.transport)=colnames(LCA.env$output)
 		#print(7681)
#  		browser()
#  		browser()
		if (((grepl("Petrol", tab[i,7])==TRUE)|(grepl("petrol", tab[i,7])==TRUE))&((grepl("harvest", tab[i,4])==TRUE)|(grepl("harvesting", tab[i,4])==TRUE)|(grepl("Harvest", tab[i,4])==TRUE))){			
			if (l==1) {
				   operating.machinery.transport1=operating.machinery.transport          
				  }
			if (l==2) {
				   operating.machinery.transport=rbind(operating.machinery.transport1, operating.machinery.transport)
				  }
	         
			}
# 		if (i==37){
# 				      #print("757")
# 				      #print("operating.machinery.transport.truck")
# 				      
# 				      #print(operating.machinery.transport.truck)
# 				      #print("operating.machinery.transport.rail")
# 				      #print(operating.machinery.transport.rail)
# 				      #print(operating.machinery.transport)
# 				      #print(i)
# 				      #print(l)
# 				      browser()
# 				      }
# 		if (i==37) {
# 		            #print(l)
# 		            #print(i)
# 		            browser()
# 		}
	         
		 }
	        

	  
	  
	  #check
		      #computing the total energy, store in the OEM to compute the amount of energy necessary for  maintenance and repairs for different  operating machinery according tio Brentrup et al. 2004
		#____________________________________________________________________________________________________________________________________________________________________________________________________________________________
# 		#print(EC)
# 		#print(operating.machinery.raw.materials.production)
# 		browser()
		if (nrow(tractor.production)==2) {
			OEM=rep(0, nrow(tractor.production))
			    for (row in 1:2){
				  OEMi=operating.machinery.raw.materials.production[row,1]+EC[row]
#  				  #print(operating.machinery.raw.materials.production[row,1])
#  				  #print(ep[row, 1])
				  OEM[row]=OEMi
#  				  #print(OEM)
#  				  #print(EC)
#  				  #print(operating.machinery.raw.materials.production)
#  				  #print(OEM)
#  				  #print(i)
#  				  browser()
# 				 
				  }
		}
		else {
			OEM=operating.machinery.raw.materials.production[1,1]+EC
#  			#print(EC)
# 			#print(operating.machinery.raw.materials.production)
#  			#print(OEM)
#  			browser()
			}
	#	  calculating energy and emissions due to maintenance and repairs, the value are stored in the OMMandR variables in case of harvest the variable is a dataframe with two lines.
#____________________________________________________________________________________________________________________________________________________________________________________________________________________________
		OEMR=rep(0, length(OEM))
		# computing  energy related to maintenance and repairs
		for (row in (1:nrow(operating.machinery.raw.materials.production))) {
			if ((grepl("harrow", tab[i,7])==TRUE)|(grepl("Harrow", tab[i,7])==TRUE)){
				OEMR[row]=0.3*OEM[row]
# 				#print(i)
# 				#print("panta rei")
# 				browser()
			
			}
			else{
				OEMR[row]=0.26*OEM[row]		           
			}
		}
# 		if (row=1) {
# 			OEMR=0
# 			}
# # 			browser()
# 		
#       # 	  #print(OEMR)
# 		
# 		if (((grepl("combined harvester", machinery[i, 7])==TRUE)|(grepl("combined harvester", machinery[i, 7])==TRUE))&(row=1)){
# 			OEMRi=0.23*OEM[1]
# # 			#print("eureka")
# # 			browser()
# 			}
# 		
# 		
# 		if (row=2) {
# 			    OEMR=c(OEMRi, OEMR[l])
#       # 		      #print(OEMRi)
# 			    
# 			  
# 
# 			  }
# 		}
#           if ((i==10)|(i==55)|(i==64)){ 
#   	  #print(OEM)
# #    	  #print(OEMR)
#    	  browser()
#   	  }

# #computing  energy and emissions related to maintenance and repairs stored in variable called OMMandR
#           
 			  if (length(OEMR)==1){
 			                        OEMR=as.numeric(as.vector(OEMR))
 			                        EC=0.62*OEMR
#   			                        #print("eureka")
 			                        LCA.env$PC=0
 			                        DC=0.03*OEMR
 			                        LCA.env$LOC=0
 			                        LCA.env$GC=0
 			                        
 			                        
 			                        LCA.env$CC=0
 			                        LCA.env$FOC=0.35*OEMR
 			                        LCA.env$location=as.character(LCA.env$fuel.production.location)
 			                        distance=as.numeric(as.character(LCA.env$f.t.d))
 						transport.mean=as.character(LCA.env$f.t.m)
 						amount=DC+FOC
    						#print(amount)
   						#print(transport.mean)
#  						distance=as.numeric(as.vector(distance))
   						#print(distance)
#   						#print(transportmeanslist)
#   						#print(DC)
#  						assign("transport.mean", transport.mean, envir=LCA.env)
#  						browser()


 						tf(LCA.env, distance, amount, transport.mean)
						output=LCA.env$output
  						#print(output)
  						#print(1044)
#  					        #print(DC)
 						DC=DC+output[1]
#  						#print(DC)
#  					         browser()
 						tr=output
#  						#print(length(tr))
 						LCA.env$DC=as.numeric(as.vector(DC))
 						
 						
 						#print(LCA.env$PC)
 						#print("DC")
 						#print(LCA.env$DC)
 						#print(LCA.env$LOC)
 						#print("FOC")
 						#print(LCA.env$FOC)
 						#print(LCA.env$GC)
 						#print(LCA.env$CC)
 						#print(LCA.env$location)
 			                        
 			                        fp(LCA.env)
 			                        
 			                        fc=LCA.env$output
 			                        #print(fc)
 			                        #print(LCA.env$output)
#  			                        #print(length(fc))
						location=as.character(LCA.env$electricity.production.maintenance.repairs.location)
						#print(location)
						#print(EC)
						#print(1071)
  			                        ep(LCA.env, EC, location, electricity)
 			                        #print(LCA.env$output)
 			                        ep=LCA.env$output
 			                        #print(ep)
#  			                        #print(length(ep))
#                                                    #print(928)
                                                   
 						

 			                        OMMandR=rep(0, ncol(ep))
 			                        for (n in (1:ncol(ep))) {
 			                                           
                                                                   if (n==1) {
 							                     OMMandR[n]=OEMR+tr[n+2]+fc[n]+ep[n]
 							                     }
 							           
                                                                    else {
 									 
 									 OMMandR[n]=tr[n+2]+fc[n]+ep[n]
                                                                     }
#                                                                     #print("fc")
#  			                                 			                         #print(fc)
#  			                                 			                         #print("ep")
#  			                         #print(ep)
#  			                                           
#  			                                         #print("tr")
#  			                        #print(tr)
#  			                       
#  			                         #print(OMMandR)
#      			                        # # # 	 
#  			                        browser()           
                                                

							}
						OMMandR=data.frame(OMMandR[1], OMMandR[2], OMMandR[3], OMMandR[4], OMMandR[5], OMMandR[6], OMMandR[7], OMMandR[8], OMMandR[9], OMMandR[10], OMMandR[11], OMMandR[12], OMMandR[13], OMMandR[14], OMMandR[15])
						colnames(OMMandR)=c("Energy consumption GJ", "CO2 kg", "CO2 biogenic kg", "CH4 kg", "N2O kg", "CO kg", "NH3 kg", "NO2 kg", "SO2 kg", "NMVOC kg",  "PM kg", "SF6 kg", "NO3 to water kg kg-1", "PO4 kg", "P kg")
# 						#print(9501)
# 						browser()
 			                        }
 			                        
 			    else {
 			       for (l in (1:2)) {
 			       			EC=0.62*OEMR[l]
 			                        LCA.env$PC=0
 			                        DC=0.03*OEMR[l]
 			                        LCA.env$LOC=0
 			                        LCA.env$GC=0
 			                        
 			                        
 			                        LCA.env$CC=0
 			                        LCA.env$FOC=0.35*OEMR[l]
 			                        
 			                         						
						LCA.env$location=as.character(LCA.env$fuel.production.location)
 			                        distance=as.numeric(as.character(LCA.env$f.t.d))
 						transport.mean=as.character(LCA.env$f.t.m)
 						amount=DC+FOC
    						#print(amount)
   						#print(transport.mean)
 						
   						#print(distance)
#   						#print(transportmeanslist)
#   						#print(DC)
  				
 			        
 						
 						tf(LCA.env, distance, amount, transport.mean)
  						#print(LCA.env$output)
  						output=LCA.env$output
  						#print(output)
  						#print(1184)
# 						#print(DC)
 						DC=DC+output[1]
#  						#print(DC)

 						tr=output
#  						#print(length(tr))
 						LCA.env$DC=as.numeric(as.vector(DC))
 						
 						#print(LCA.env$DC)
 						#print(LCA.env$FOC)
 						#print(LCA.env$LOC)
 						#print(LCA.env$GC)
 						#print(LCA.env$CC)
 						#print(LCA.env$location)
 			                        
 			                        fp(LCA.env)
 			                        #print(LCA.env$output)
 			                        fc=LCA.env$output
 			                        #print(fc)
 			                        #print(1202)
#   			                        #print(length(fc))
			                        location=as.character(LCA.env$electricity.production.maintenance.repairs.location)
			                        #print(EC)
			                        #print(location)
			                        
  			                        
 			                        ep(LCA.env, EC, location, electricity)
 			                        output=LCA.env$output
 			                        
 			                        ep=output
 			                        #print(output)
 			                        #print(ep)
 			                        #print(1215)
#  			                        browser()
#  			                        #print(length(ep))
#  			                        #print("OMMandR")
#  			                        #print(OMMandR)
 			                        OMMandR=rep(0, ncol(ep))
 			                        
 			                        for (n in (1:ncol(ep))) {
                        						     if (n==1) {
 							                     OMMandR[n]=OEMR[l]+tr[n+2]+fc[n]+ep[n]
 							                     }
                        
                                                                    else {
  			
  									 OMMandR[n]=tr[n+2]+fc[n]+ep[n]
                                                                    }
#  			                                           
#                                    #print(fc)
#    			         				#print(ep)
#    			                                           
#   								      #print(OMMandR)
#  			  
 			                        }
 			                       
#  			                        #print(OMMandR)
#  			                        browser()
 			                        OMMandR=data.frame(OMMandR[1], OMMandR[2], OMMandR[3], OMMandR[4], OMMandR[5], OMMandR[6], OMMandR[7], OMMandR[8], OMMandR[9], OMMandR[10], OMMandR[11], OMMandR[12], OMMandR[13], OMMandR[14], OMMandR[15])
 			                        colnames(OMMandR)=c("Energy consumption GJ", "CO2 kg", "CO2 biogenic kg", "CH4 kg", "N2O kg", "CO kg", "NH3 kg", "NO2 kg", "SO2 kg", "NMVOC kg",  "PM kg", "SF6 kg", "NO3 to water kg kg-1", "PO4 kg", "P kg")
#   			                        #print(10211)
#   			                        #print(OMMandR)
#   			                        browser()
 			                        if (l==1){
 							  OMMandRd=OMMandR
#   							  browser()
 			                        }
 			                        if (l==2){
 							  OMMandRd=rbind(OMMandRd, OMMandR)
 							  OMMandR=OMMandRd
 			                        }
 			                        
#  			                          	
#                                                                      #print("fc")
#   			                                 			                         #print(fc)
#   			                                 			                         #print("ep")
#   			                         #print(ep)
#   			                                           
#   			                                         #print("tr")
#   			                        #print(tr)
#   			                       
#   			                         #print(OMMandR)
# 						#print(l)
#   			                          	browser()     
#  			       
#  			       
#  			       #print(OMMandR)
#  			       browser()
 			       }
 			       
			       
 
 				 
 			    }
 			    
 			   
#  			   #print(OMMandR)	
#  	  		browser()
# 	  
# 	   if ((i==37)|(i==68)) {
#  	   	browser()
# 	   }
	   
	   
	   
	   
	   ##summing up all the impact
	   #creating the matrix to be filled to sum up all the impacts
# 	   #print("NA are made at the beginning")
	   total.machinery.production.transport.repair.i=data.frame(matrix(nrow=nrow(tractor.production), ncol =ncol(tractor.production)))
# 	   if (nrow(tractor.production)==2) {
#  			  #print(total.machinery.production.transport.repair.i)
#  			  browser()
# 			  }
           OEC=rep(0, nrow(tractor.production))
			
		for (row in (1:nrow(tractor.production))){
				    
				    
				    for (n in (1:ncol(tractor.production))) {
# 				                      if (i==53) {
# 									#print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
#  								      #print("tractor.production")
#  								      #print(tractor.production)
# # 								      #print(length(tractor.production))
# # 								      #print("tractor.transport")
#  								      #print(tractor.transport)
#  								      browser()
# 								      #print(length(tractor.transport))
#  		 						      #print("EM")
#  		 						      #print(EM)
# 								      #print("EMR")
# 								      #print(EMR)
#  								      #print("MandR")
#  								      #print(MandR)
# # 
#  								      #print("operating.machinery.raw.materials.transport")
#  								      #print(operating.machinery.raw.materials.transport)
#  								      #print("operating.machinery.raw.materials.production")
#  								      #print(operating.machinery.raw.materials.production)
# # 								
# # 								      #print(length(operating.machinery.raw.materials.production))
#  								      #print("OEM-operating.machinery.raw.materials.production[1,1]")
#  								      #print(OEM[row]-operating.machinery.raw.materials.production[row,1])
#  								      #print("operating.machinery.transport")
#  								      #print(operating.machinery.transport)
#  								      #print(length(operating.machinery.transport))
#  								      #print("OMMandR")
#  								      #print(OMMandR)
#  								      #print(length(OMMandR))
#  								      #print(row)
# # 								      #print(n)
# # 								      browser()
# 						      }
							 
						      total.machinery.production.transport.repair.i[row,n]=tractor.production[row,n]+tractor.transport[row,n]+operating.machinery.raw.materials.production[row,n]+operating.machinery.raw.materials.transport[row,n]+operating.machinery.transport[row,n]+MandR[row,n]+OMMandR[row,n]
							#    									     
							if (n==1) {
# 							                    if (nrow(tractor.production)==2) {
# 										      browser()
# 							                    }
								      OEC[row]=OEM[row]-operating.machinery.raw.materials.production[row,1]
								      total.machinery.production.transport.repair.i[row,n]=OEC[row]+total.machinery.production.transport.repair.i[row,n]
								    }
								    
				      total.machinery.production.transport.repair.i[row,]=cbind(total.machinery.production.transport.repair.i[row,1], total.machinery.production.transport.repair.i[row,2], total.machinery.production.transport.repair.i[row,3], total.machinery.production.transport.repair.i[row,4], total.machinery.production.transport.repair.i[row,5], total.machinery.production.transport.repair.i[row, 6], total.machinery.production.transport.repair.i[row, 7], total.machinery.production.transport.repair.i[row, 8], total.machinery.production.transport.repair.i[row, 9], total.machinery.production.transport.repair.i[row, 10], total.machinery.production.transport.repair.i[row,11], total.machinery.production.transport.repair.i[row,12], total.machinery.production.transport.repair.i[row,13], total.machinery.production.transport.repair.i[row,14], total.machinery.production.transport.repair.i[row,15])	                    
 				    colnames(total.machinery.production.transport.repair.i)=c("Energy consumption GJ", "CO2 kg", "CO2 biogenic kg", "CH4 kg", "N2O kg", "CO kg", "NH3 kg", "NO2 kg", "SO2 kg", "NMVOC kg",  "PM kg", "SF6 kg", "NO3 to water kg kg-1", "PO4 kg", "P kg")
				      
#  				      #print(total.machinery.production.transport.repair.i)
# 				      if (i==53){
				    
# 				      }
						      
				    }
#  				    #print("Eureka")

#  			  #print(11311)
#   				  if (row==2)	      {
#   				  #print(11391)
#   	   			  browser()
#   				  }
			}
	  
	  
	  
	  
	  

		  if (i==1) {
				total.machinery.production.transport.repair=total.machinery.production.transport.repair.i
			    }
		  else{
			    total.machinery.production.transport.repair=rbind(total.machinery.production.transport.repair, total.machinery.production.transport.repair.i)
		      }
 		      #print(total.machinery.production.transport.repair)
# 		      if (i==18){
# 		                 #print(tractor.weight.ha)
# 		                
#  		                 browser()
# 		      }
# 	    browser()   
	  }
           total.machinery.production.transport.repair=cbind(fico[,1:5], total.machinery.production.transport.repair)
# 	   #print(total.machinery.production.transport.repair)
	   
	   ## the impacts related to machinery production, transport, maintenance and repairs are stored in the variable tmptr and in the file total.machinery.production.transport.repair.csv, the variable has the same layout of the fico variable
	  #____________________________________________________________________________________________________________________________________________________________________________________________________________________________
	  assign("tmptr", total.machinery.production.transport.repair, envir=LCA.env)
	   write.table(total.machinery.production.transport.repair, file="total.machinery.production.transport.repair.csv", sep=",", row.names=FALSE)
	   

          



















          
          
          
          
          
          
          
          
          
          
          

#   	browser()











	}










































