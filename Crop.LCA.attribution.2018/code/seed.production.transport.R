seed.production.transport=function(LCA.env){


#this function computes the impact of seed production and seed transport on the basis of the amount of seed in the Glenlea.input.table either in kg ha-1 or seed ha-1
tab=get("tab", envir=LCA.env)



seed.production.transport=matrix(0, nrow=nrow(LCA.env$fico), ncol=(ncol(LCA.env$fico)-8))
#print(head(seed.production.transport))
l=0
for (i in 1:(nrow(tab))) {
                         #print(tab[i,1:4])
			  if (((grepl("Seeding", tab[i,4]))==TRUE)|((grepl("seeding", tab[i,4])==TRUE))){
					seed.amount=as.character(tab[i,17])
					  if (((grepl("seed", tab[i,17]))==TRUE)|(grepl("Seed", tab[i,17]))){
						  seed.amount=unlist(strsplit(as.character(tab[i,17]), "[^[:digit:]]"))
						  seed.amount=as.numeric(seed.amount[1])
						  #print(seed.amount)
						  #converting seed amount to kg of production
							if (((grepl("Maize", tab[i,3]))==TRUE)|(grepl("maize", tab[i,3 ]))) {
							#computing the weight of maize
								seed.amount=seed.amount*0.001*0.38
						  
		
					  


						  
						}
					}
					seed.amount=as.numeric(seed.amount)
					#print(seed.amount)
					crop=tab[i,3]
					
					if ((tab[i,3])!="Faba beans") {
					crop<- strsplit(as.character(tab[i,3]), " ")
					#print(crop)					
					crop=paste(toupper(substring(crop, 1, 1)), substring(crop, 2), sep = "", collapse = " ")
					
					
					}
					
					LCA.env$material=paste(crop, "seed")
					#print(LCA.env$material)
					
					LCA.env$location=as.character(LCA.env$seed.production.location)
					LCA.env$amount=seed.amount
					
					#print(LCA.env$location)
					#print(LCA.env$amount)
					#print(LCA.env$material)
						  
		#computing the impact of seed.production				  
					seed.production=materials.production(LCA.env)
					#print(seed.production)
					#print(l)
					LCA.env$amount=seed.amount
					LCA.env$location="Manitoba"
					LCA.env$distance=as.numeric(as.character(LCA.env$seed.transport.distance))
					LCA.env$transport.mean=as.character(LCA.env$seed.transport.transport.mean)
					#print(LCA.env$amount)
					#print(LCA.env$location)
					#print(LCA.env$seed.transport.distance)
					#print(LCA.env$seed.transport.transport.mean)
	#computing the impact of seed transport				
					seed.transport=tfp(LCA.env)
					#print(seed.transport)
					#print(seed.production)
					seed.production.transport[l,]=as.numeric(seed.production+seed.transport)
					#print(seed.production.transport[l,])
# 					#print(seed.production.transport)
                                        if (i>80){
						    #print(l)
						    #print(i)
						    #browser()
						    }
				
			  }
			  if (i==1){
				    l=1
			  }
			  #print("l")
			  #print(l)
			if (((grepl("Petrol", tab[i,7]))==TRUE)){
							    l=l+1
							    #print("panta rei")
							    #if(l==86){
							    #browser()
							   # }
							    }
                        

                        #print(tab[i,1:6])
                        #print(LCA.env$fico[l,1:6])
                        
				
                        #print(l)
			l=l+1 
                        #print("l")
                        #print(l)


























}

seed.production.transport=cbind(LCA.env$fico[,1:5], seed.production.transport)
#print(head(seed.production.transport))

colnames(seed.production.transport)=c(colnames(LCA.env$fico[,1:5]), colnames(LCA.env$output))


#print(head(seed.production.transport))

LCA.env$spt=seed.production.transport

write.table(seed.production.transport, file="seed.production.transport.csv", row.names=FALSE, sep=",")





#browser()










































































































}