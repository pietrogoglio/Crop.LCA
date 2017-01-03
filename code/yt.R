yt=function(LCA.env){



## This function compute the year value for any given table present in the local environment (LCA.env$input.table)
##WARNING: this function consider the annual value independently from annual or perennial values
##WARNING: the crop name for crops with double name should have the first capitalised
#the input.table has to have a similar lay out as the fico table

fico=LCA.env$fico

input.table=LCA.env$input.table

year=sort(unique(fico[,1]))

#establishing the number of plot evaluated + is a special character
ploti=unique(fico[,2])
ploti=as.character(ploti)
ploti=str_replace_all(ploti, " ", "")
ploti=as.character(unique(ploti))
for (pi in 1:length(ploti)){
if (grepl("\\+", ploti[pi])==TRUE) {
			  ploti[pi]=str_replace_all(ploti[pi], "\\+", " ")
			  ###print("panta rei")
			  }
			  }
			  ###print(ploti)
# #browser()
for (pi in 1:length(ploti)){
			    if (grepl("All", ploti[pi])==TRUE) {
										  
						      ploti=c(ploti[-pi])
						      
			    }
}


ploti=unlist(strsplit(ploti, " "))
ploti=unique(ploti)
LCA.env$ploti=sort(ploti)
ploti=LCA.env$ploti

# establishing the number of year
LCA.env$year=unique(fico[,1])
year=LCA.env$year
#print(year)


#establishing the number of numerical columns to be summed up
numcol=0
counter=1
for (col in (1:ncol(input.table))){
#                                   #browser()
				 if (((is(input.table[,col])=="numeric"))&(col>1)){
				                                                    if (counter==1) {
												    startcol=col
												    counter=0
												    }

				                                   numcol=numcol+1
				 }
}

#allocating the matrix
results=matrix(0, nrow=(length(ploti)*length(year)), ncol=(3+numcol))
# #browser()
#in order to have each column not as factor
results=as.data.frame(results)
results[,2]=rep(ploti, length(year))


#the function runs year by year, crop by crop, and then operation by operation
r=1
for (y in (1:length(year))){
			  results[r:(r+(length(ploti)-1)),1]=rep(year[y], length(ploti)) 
			  r=r+length(ploti)

		  }


##print(results)


r=1
# total=0
yamount=rep(0, numcol)

counterline=1
# #browser()
fl=1
counter=0
for (y in (1:length(year))){
			    #print(counter)
			    #print(94)
			 			       l=counterline
			 			       #print(l)
			 			       #print(r)
			 			       #print(98)
			 			       
      
			                               for (pi in 1:length(ploti)){
										   #print(100)
										   #print(l)
										    repeat{
																						    
													  
																	    
# 																		  #browser()  
										
																	    if (((grepl(results[r,2], input.table[l,2])==TRUE)|(grepl("All", input.table[l,2])==TRUE))&(results[r,1]==input.table[l,1])){ 
																	    #computing the amount of inventory output to be summed up
												    #  			                               total=sum(total,input.table[l,8])
												    # 			                               ##print(total)
												    # 			                               #browser()
																			  ##print("r")  
																			  ##print(r)
																			  ##print(l)
# 																					    l=l+1
# 																					    #browser()
																			  ##print(yamount)
																			  ##print(startcol)
																			  ##print(startcol+numcol-1)
																			  line=unlist(input.table[l,startcol:(startcol+numcol-1)])
																			  #print(input.table[l,])
																			  #print(line)
																			  #print(yamount)
																			    
																			  yamount=yamount+line
																			  
																			  #print(yamount)
																			  #browser()
																
																			  #print(input.table[l,3])
																			  #print(129)
																#accounting for the label 
																
																	      
							                                                                                                  if (counter==0){
																					  crop=as.character(input.table[l, 3])
																					  if (grepl(" ", crop)==TRUE){
																											       crop=as.character(input.table[l,3])
																					  }
																					  else{
																					  ##print("panta rei")
																						  
																					         #browser()
																						crop<- strsplit(as.character(input.table[l,3]), " ")
																						##print(crop)
																						
																						
																						crop=paste(toupper(substring(crop, 1, 1)), substring(crop, 2), sep = "", collapse = " ")
																						##print(crop)
																					   }
																					  
																					  counter=1
# 																					  #print(crop)
# 																					  #print(l)
# 																					  #print(r)
# 																					  #print(147)
# 																					  #browser()
																					  
																					 
							                                                                                                  
							                                                                                                  }
							                                                                                                  
							                                                                                                  else{
																				  if (grepl(" ", as.character(input.table[counterline,3])==TRUE)){
																											       crop1=as.character(input.table[counterline,3])
																											       #print(164)
																											       #print(counterline)
																											       #print(l)
																											       #browser()
																					  }
																					  else{
																						crop1<- strsplit(as.character(input.table[counterline,3]), " ")
																						##print(crop1)
																						
																						
																						crop1=paste(toupper(substring(crop1, 1, 1)), substring(crop1, 2), sep = "", collapse = " ")
																						#print(crop1)
																					   }															
																					   
																				  if (grepl(" ", as.character(input.table[l,3])==TRUE)){
																											       crop2=as.character(input.table[l,3])
																											       
																					  }
																					  else{
																						crop2<- strsplit(as.character(input.table[l,3]), " ")
																						##print(crop2)
																						
																						
																						crop2=paste(toupper(substring(crop2, 1, 1)), substring(crop2, 2), sep = "", collapse = " ")
																						#print(crop2)
																					   }
							                                                                                                          
	
 																				  if (((crop2!=crop1)==TRUE)&((input.table[l,2]==input.table[counterline, 2])==TRUE)&(input.table[l,1]==results[r,1])|(counterline>nrow(input.table))){
																				#print("eureka")
																				#print(l)
 																				#print(input.table[l,3])
 																				#print(input.table[counterline,3])
 																				#print(input.table[l,2])
 																				#print(input.table[counterline,2])
 																				#print(input.table[l,1])
 																				#print(input.table[counterline,1])
# 																				##print(l)
# 																				##print(counterline)
 																			       #print(195)
#  																			       #browser()
#  																				crop=paste(crop, as.character(input.table[l,3]), sep=" ")
# 
 																				
  																				}
																			  }
								
# 																									  ##print(crop)
#  																									  #browser()
																			  }
																									  
				      
																	    
			      # 													      ##print("l")
			      # 													      ##print(l)
			      # 													      l=l+1								        
			      # 													      if (results[r,1]==input.table[l,1]){
			      # 													                                  ##print("eureka")
			      # 																	  break()
			      # 																	  fl=l
			      # 													  
			      # 										                              }
																	    
																	  
														  
										if ((results[r,1]!=input.table[l,1])|(l>nrow(input.table))){		  
													counterline=l
													#print(r)
													#print(counter)
													#print("counterline")
													#print(counterline)
													#print("l")
													#print(l)
													#print(crop)
													#print(results)
 													
													break()
											      }							    
										      l=l+1						      
										    #print(227)
										    #print(l)
# 										    #browser()

										    }
				                        counter=0
				                        ##print(crop)
				                        
				                        #filling column 3 with the label
						        results[r,3]=crop
						        #print(crop)
						        #print(l)
						        #print(236)
						        #print(r)
#  						        #browser()
						        
						        # filling column 4 with different pollutant amount
						        #print(r)
						        #print(yamount)
						        #print(results)
						        
						        results[r,4:(4+numcol-1)]=yamount
						        #print(results)
						        #browser()
						        yamount=rep(0, numcol)
						        ##print(results)
						        
						        #fixing variable to start the computation for a different crop
							if (pi==(length(ploti))){
								    fl=l
								   }
							  else{
							       l=fl
							      }
							 ##print(r)
							##print(fl)
							##print(l)
# 							#browser()
							#print(256)
						        r=r+1
						        #print(r)
  			                               }
                               


# 		      
























































						   
			
}












                        




#                                 for (col in (1:ncol(results))) {
#  								if (col<=3){
#  									    results[,col]=data.frame(as.character(results[,col]))
#  								}
#  								else{
#  								      results[,col]=data.frame(as.numeric(as.character(results[,col])))
#  								}
# # 				                          				
# #                                
# #                                }
# 				labels=as.character(results[,1:3])
# 				figures=as.numeric(results[,4:ncol(results)])
#                                 results=data.frame(labels, figures)
#                                #browser()
                                colnames(results)=c(colnames(fico[1:3]), colnames(input.table[startcol:(startcol+numcol-1)]))

                               LCA.env$year.results=results
#                                 assign("year.results", results, envir=LCA.env)
#                                   year.results=results
#                                   return(year.results)
# #                                #print(results)
#                                #print(LCA.env$year.results)
				









































}