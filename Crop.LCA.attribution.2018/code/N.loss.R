N.loss=function(LCA.env, N.assumptions, N.P.input, climate, crop.uptake, rooting.depth, root.to.shoot.ratio, yi){


#print("the impact of N leaching is accounted according to Nemecek et al. 2014, on the basis of the SQBC regression model")

tab=get("tab", envir=LCA.env)
year=get("year", envir=LCA.env)




#calculating the term for mineral fertiliser S
#_________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
S=sum(as.numeric(as.vector(unlist(N.P.input[yi, 4:5]))))
#print(S)

 
# calculating the term for crop uptake U
#___________________________________________________________________________________________________________________________________________________________________________________________________________________
# #print(root.to.shoot.ratio)
# #print(yi)
# browser()_
 residues.yield=0
 uptake.counter=0
 for (cu in (1:nrow(crop.uptake))) {
#                _                      #print(crop.uptake[cu,])
#                                      browser()
#                                    
#                                    
                                    if ((as.character(N.P.input[yi,3])==as.character(crop.uptake[cu,1]))==TRUE){
                                                counter=0
#                                                 #print(LCA.env$yield[yie, ])
#                                                 #print(N.P.input[yi,])
#                 _                                browser()
 						for (yie in (1:nrow(LCA.env$yield))) {
 							    
#   							    #print(crop.uptake[cu,])
#    						            #print(LCA.env$yield[yie,])
#    						            N.P.input[yi,]

 						            
							    if (((as.character(LCA.env$yield[yie,3])==as.character(N.P.input[yi,3]))==TRUE)&((as.character(LCA.env$yield[yie, 1])==as.character(N.P.input[yi,1]))==TRUE)&((as.character(LCA.env$yield[yie, 2])==as.character(N.P.input[yi,2]))==TRUE)){
								  grain.yield=LCA.env$yield[yie,10]
# 								  print(44)
# 							  print("grain.yield")
# 							print(grain.yield)
# 							print(yi)
# 								  browser()
								  
								  if (grain.yield!=0){            

      #the function read whether or not has been done or not, if it is done with a harrow mulch tillage value will be used, if there is a plough the date of ploughing is interpreted according to the Glenlea.input.table, in order to be read the month should be expressed with the full word

									    l=1
									    fl=1
									    counter=0
									    for (y in 1:length(year)){
												  repeat{
													  #print(l)
													  if ((as.character(tab[l,1])>as.character(N.P.input[yi,1])|(l==nrow(tab)))){
									    # 			                             ##print("N.P.input")
									    # 			                             ##print(N.P.input[yi,1])
									    # 			                             ##print(tab[l,1])
									    # 			                             ##print(l)
																l=fl
																break()
																  
													  }
												
									    #         ###print(N.P.input)
									    #           ###print(yi)
									    #                               ###print(N.P.input[yi,3])
									    #                               ###print(tab[l,3])
													
									    # 					browser()
													


													  
													  if (as.character(N.P.input[yi,3])==as.character(tab[l,3])){
														    if (((grepl("baling", tab[l,4])==TRUE)|(grepl("Baling", tab[l,4])==TRUE))&(counter==0)&(tab[l,1]==N.P.input[yi,1])){
													    
# 		_												              #print(77)
															    residues.yield=LCA.env$yield[yie,8]
															    counter=1
# 															    #print("eureka")
# 															  browser()
														    }
														    else{
															    if (grepl("flail chopper", tab[l,7])==TRUE){
																					residues.yield=LCA.env$yield[yie,8]
																					#print(85)
																					counter=1
																					}
															    else{
																  if (counter==0){  
																	residues.yield=0
																	}
																	
# 												#print(92)					
																  
																	  
																  }
# 											#print(96)
															      }
														    }
# 												            ##print(counter)		    
# 											                    ##print(residues.yield)
# 											                    browser()
													    l=l+1
# 													    ##print(l)
													    }

												    
											
										        
# 										        browser()
										      }
									  
															    
									#print(117)		  
									  }						            
 						            
 						            # considering that for alfalfa there is more than 1 year harvest

 						            
 						            
								  else{
                                                                       if (counter==0) {
										residues.yield=LCA.env$yield[yie,8]
										counter=1
									#print(124)
                                                                       }
                                                                       else{
									    counter=1
# 									    ##print(residues.yield)
									    residues.yield=sum(residues.yield, LCA.env$yield[yie,8])
									#print(130)
                                                                       }
												  
                                                                  #print(counter)  
        						                 
								  
								  #print(117)
								  #print("residues.yield")
								  #print(residues.yield)
								  #print("counter")
								  #print(counter)
								  #print("yie")
								  #print(yie)
								  }
							    }
							    else{
							         grain.yield=0
#  							         print(yi)
#  							         browser()
# 							         ##print(yie)
# 							         if (yi>=13){
# 									    browser()
# 						          }
							    }
 						}
 	
 	
 	# considering the right amount of N uptake for residues
				    if (residues.yield>0){
				                          if (((grepl("straw", crop.uptake[cu,2]))==TRUE)|((grepl("aboveground biomass", crop.uptake[cu,2]))==TRUE)|((grepl("silage", crop.uptake[cu,2]))==TRUE)) {
								  residues.N.content=crop.uptake[cu,4]
								  #print(crop.uptake[cu,])
# 								  browser()
								  #print(156)
								uptake.counter=1
				                          }
							  if (uptake.counter==0) {
								  residues.N.content=0
							   }
					    }
				    else{
				         residues.N.content=0
				    }
				    if (grain.yield>0){
				                        if ((grepl("grain", crop.uptake[cu,2]))==TRUE){
								 grain.N.content=crop.uptake[cu,4]
								 uptake.counter=1
								 #print(170)
				                        }
				                        else{
							      if (uptake.counter==0) {
									      grain.N.content=0
									      #print(175)
									      }
							}
				    }
				    else{
					  grain.N.content=0 
					  #print(181)
					 }
				    
                                    }
#   browser()                                    
 }
 
#  computing organic N
 

#print("grain.N.content")
#print(grain.N.content)

#print(N.P.input[yi,])
#print("residues.N.content")
#print(residues.N.content)
 #print("residues.yield") 
#print(residues.yield)
#print(yi)
# 				    #print(yie)
#print(grain.yield)

if (((grepl("Alfalfa", N.P.input[yi,3]))==TRUE)|((grepl("Faba beans", N.P.input[yi,3]))==TRUE)|(grepl("faba beans", N.P.input))|((grepl("faba bean", N.P.input[yi,3]))==TRUE)|((grepl("Faba bean", N.P.input[yi,3]))==TRUE)|((grepl("alfalfa", N.P.input[yi,3]))==TRUE)){
	    U=0.4*(grain.yield*grain.N.content+residues.yield*residues.N.content)			
}
else{
	    U=(grain.yield*grain.N.content+residues.yield*residues.N.content)			
}

#print(U)
# browser()
  				        
#calculating organic nitrogen content N.org in kg ha-1
#_________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
soil.volume=10000*N.assumptions["soil.profile", 4]
soil.mass=soil.volume*N.assumptions["bulk.density", 4]
c.content=soil.mass*N.assumptions["carbon.content", 4]*0.01
#print(c.content)
N.org=c.content*0.1*1000
#print(N.org)







  
  



#extracting rainfall factor P
#______________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
for (y in 1:length(year)){
			 if ((N.P.input[yi,1]==climate[y,1])==TRUE){
								     P=climate[y,3]+climate[y,2]
			 }
}

#print(P)






  
# extracting root.depth (L) considering soil depth
#____________________________________________________________________________________________________________________________________________________________________________
for (rd in (1:nrow(rooting.depth))){
# _				   #print(rooting.depth[rd,])
#                                    #print(rooting.depth[rd,3])
				    if (((N.P.input[yi,3]==rooting.depth[rd,1])==TRUE)){
					      L=rooting.depth[rd,3]
#  					      browser()
					      for (nas in (1:nrow(N.assumptions))){
									           if (N.assumptions[nas, 2]=="soil.depth"){
											  soil.depth=as.numeric(N.assumptions[nas,4])
									           }
# 									           browser()
					      }
					      if (L>soil.depth){
							L=soil.depth
					      }
# 				    #print(L)
				    
				    }
				    
# 				    browser()

}
#print(L)
# browser()



#computing nitrate leaching
#________________________________________________________________________________________________________________________________________________________________________________________

LCA.env$nitrate.leaching[yi]=21.37+(P/(N.assumptions["clay.content", 4]*L))*(0.0037*S+0.0000601*N.org-0.00362*U)

#print(LCA.env$nitrate.leaching)
# browser()








































































































}