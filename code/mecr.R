mecr=function(LCA.env){
#                #print("This function read the table format contained in this working directory and store in the tab variable, cut it and make a specific table with just the machinery impact")
               
               
               #print(head(machinery))
               

               
               tab=get("tab", envir=LCA.env)
               yield=get("yield", envir=LCA.env)
               machinery=data.frame(tab[, 1:10])
         
               assign("machinery", machinery, envir=LCA.env)
               write.table(machinery, file="machinery.csv", sep=",", row.names=FALSE, col.names=TRUE)
               
               baling.per.year=0
               year.of.baling=tab[1,1]
               crop.baled=tab[1,3]
              

for (i in 1:nrow(machinery)){
                               

	  #print("reading the line and print it")
	  linea=machinery[i,]
# 	  str(linea)
# 	  ##print(linea)
# 	 # #browser()


          #print(" reading working speed and working front")
# _______________________________________________________________________________________________________________________________________________________________
          #print("working speed (km h-1) ws, ww worling width (m) to convert the factor into a vector")
			  
	  ww=as.numeric(as.character(unlist(linea[(10)])))
# 	  if (i==18) {
# 	  print(ww)
# 	  #browser()
# 	  }
# 	  reading working speed as it is in the linea table
	  ws=unlist(linea[9])
# 	   correcting in case the working capacity is already given
	  if (grepl(" ha h-1", ws)==TRUE) {
 	                            ws=str_replace_all(ws, " ha h-1", "")
 	                            wc=as.numeric(as.character(ws))
#                                     #print(wc)
# 	                           # #browser()
#                                     
	                            }
 	                            
	                            
# 	                            converting the factor into a real numeric value for a vector

	  ws=as.numeric(as.character(ws))
# 	  #print(ws)
# 	  #print(ww)
# 	#browser()  4 4machinery

          
        

	  #print("calculating effective working capacity")
# ___________________________________________________________________________________________________________________________________________________________

	  #print("effective working capacity in ha h-1 Peruzzi & Sartori. 1997 ewc(ha h-1)")
          
	  ewc=ws*ww*0.7/10
	  
#print("	  correcting effective  when the data is already given as ha h-1")
	  check=as.character(unlist(linea[9]))
# 	  #print(check)
	  if (grepl(" ha h-1", check)==TRUE) {
												ewc=wc
												#print("ewc=wc")
                                                                                              #print(ewc)
# 												#browser()
												
												}
# 	  #print(ewc)

         if (i==1){
		   
		    year=tab[i,1]
		   
		   
                   }
                   #considering multiple baling operations, indexbaling indicate the line within the tab table, while nbaling is the number of baling operations per year
#                    #print(tab)
#                   # #browser()
         if ((tab[i,4]=="baling")|(tab[i,4]=="Baling")){
							 y.p.c.t=paste(tab[i,1], tab[i,2], tab[i,3])
# 							computing hte number of baling per crop per year
							year.plot.crop=rep(NA, nrow(yield))
						
							for (n in 1:nrow(yield)){
										  
							
							                         year.plot.crop[n]=paste(yield[n,1], yield[n,2],yield[n,3])
							                         if (year.plot.crop[n]==y.p.c.t) {
											if ((year.of.baling)==tab[i,1]){
	#print(103)
											      
										  
												year.of.baling=tab[i,1]
												if (crop.baled==tab[i,3]){
													crop.baled=tab[i,3]
													baling.per.year=baling.per.year+1
													straw=yield[n,8]
													#print(108)
												}
												else{
													crop.baled=tab[i,3]
													baling.per.year=1
													straw=yield[n,8]
													#print(113)
													break()
												}
											}
										
											else{
												baling.per.year=1
												year.of.baling=tab[i,1]
												straw=yield[n,8]
												crop.baled=tab[i,3]
												#print(122)
												break()
											}
										
										  }
										
							
							
							
							
							
							
							
							}
							duplicated(year.plot.crop)
							
# 							cat("\n baling.per.year\n", baling.per.year)
							#cat("\n year.of.baling\n", year.of.baling)
							#cat("\n y.p.c.t\n", y.p.c.t)
# 							#print(yield[,8])
							#print(yield[,-4:-7])
							#cat("\n straw \n", straw, "\n")
							
										#browser()
							
							
							
							

 										
									
																				
																					           
																					           
																					           
																						     
																						    
	
						    #considering 15% moisture
						    straw=straw/0.85
						    
# 																					      #print(balingcheck)
# 																					      balingcheck=balingcheck+1
						    
						    ewc=9/straw
# 																					      #print(balingcheck)
# 																					      #print(balingcounter)
						    
						    #print(i)
						   # cat("\n", " ewc ", ewc, "\n")
						    #print(n)
						    #print(yield[n,])
						    #print("Changing the effective working capacity on the basis of the amount of straw or hay collected, considering 15% moisture")
						 # #browser()
						  
																					      
						  }
												
						
                                  
          

	  
	  
          #print("retrieving tractor power or machinery power")
# _____________________________________________________________________________________________________________________________
	  hp=unlist(linea[5]) 
	  if (i==47){
	  	 # #browser()
	  	  }
#           #print(" reading the string and correcting for KW and converting them in hp")
	  if (grepl(" KW", hp)==TRUE) {
					
				      #print(" reading the string and correcting for KW and converting them in hp")
					hp=str_replace_all(hp, " KW", "")
					#print(hp)
					hp=as.numeric(as.character(hp))
					  hp=hp/0.735
					  }
	    

	    #print(hp)
# 	   # #browser()
#  	#print("reading  and correcting in case there are two machines like during harvest")

	 if (grepl("\\(|\\)", hp)==TRUE) {
					      #print("reading  and correcting in case there are two machines like during harvest")
					      #print("get rid of the 340 (189)")
					      
					      hp=str_replace_all(hp, "\\(|\\)", "")
# 					
                                             #print("transform the string in a dataframe in order to extract the two element")
					      hp=data.frame(strsplit(hp, " "))
					      
						
					      
					      
					    
					      # #print(hp)
					      hp1=hp[2,1]
					      hp=hp[1,1]
               					#print("transform everything as a vector")											
					      
					      # #print(hp)
					      # #print(hp1)
# 					     # #browser()
						  
					      }
					  
	    
	    hp=as.numeric(as.character(hp))



	

	  
	  


	  if (i==1) {
		    md=data.frame(linea[1:4], linea[,7], hp, ewc)
	  }
	  else{
		mdl=data.frame(linea[1:4], linea[,7], hp, ewc)
		md=rbind(md, mdl)
		
		
# 		#print("Adding an extra line to include the two machines utilised during some field operation (e.g. harvest)")
		
		check=unlist(linea[5])
		if (grepl("\\(|\\)", check)==TRUE) {
					        #print("Adding an extra line to include the two machines utilised during some field operation (e.g. harvest)")
						hp=as.numeric(as.character(hp1))

						if (grepl("P", check)==TRUE) {
							#print("correcting for petrol truck, the horse power was corrected to 0 in order to avoid any further accounting")
							
							
						
						                              
						}
						mdl=data.frame(linea[1:4], linea[,7], hp, ewc)
						  md=rbind(md, mdl)    
						#print(md)
						}
						}

	#print("building the line of the fc dataframe")

# 	print(md)
	

	}



names(md)[names(md)=="linea...7."]="operating machinery"
names(md)[names(md)=="hp"]="Power (HP)"
names(md)[names(md)=="ewc"]="Effective field capacity (ha h-1)"

# names(fc)[names(fc) == 'DC'] <-"field diesel consumption (kg ha-1)"
# names(fc)[names(fc) == 'OC'] <-"field oil consumption (kg ha-1)"

write.table(md, file="md.csv", sep=",", row.names=FALSE)	
#browser()
#print(head(md))
assign("md", md, envir=LCA.env)               
               


}