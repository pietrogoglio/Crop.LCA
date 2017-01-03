LCA.ini=function(){
# 		      reading from different files data necessary to carry out the LCA
	print("This function starts the program, loads libraries, reads input files and initializes the variables necessary to run the LCA")
	library(stringr)

# 	  print("reading the grain straw hay yield.csv file and initialising the yield variable which are two numerical vectors representing grain and above biomass")
# 	  print("all the input files should be contained in the input directory")
# 	print("be careful in interpreting them, sometimes grain represent also hay as it is the main product output")
# 	print("all data are corrected for the mechanical harvest and they are expressed as DM")
	yield=read.table("input/grain straw hay yield.csv", sep=",", header=TRUE)
	
# 	Establishing the temporal environment
	LCA.env=new.env()
# 	print(str(yield))
	

	assign("yield", yield, envir=LCA.env)

# 	print("Yield table has been transformed as global variable")
#       
# 	
# 	
# 	print("read the input table")
	
	tab=read.table("input/LCA.Input.Table.csv", sep=",", header=TRUE)
# 	print("WARNING: () these symbols in the power column indicate that two machines with different power are used for the same operation")
# 		Sys.sleep(4)
# 	print("WARNING: x KW are automatically corrected to hp, as the calculation has been carried out on per hp basis")
#                 Sys.sleep(4)
# 	print("WARNING: in the working speed column the expression ha h-1 in quotations mark in the working speed column is considered to assign the input value for ewc efficient working capacity")
#                 Sys.sleep(7)
# 	print(head(tab))
# 	browser()
 	Sys.sleep(2)
# 	print("the input table is cut and only the mechanical inputs part are considered to evaluate the impacts of machinery")
	assign("tab", tab, envir=LCA.env)
	
	mecr(LCA.env)
#  	browser()
# 	print("reading the input regarding farm assumptions and save them in the variable farm.transport.assumptions variable, the farm.transport.assumptions table has the same lay-out of the machinery variable")
# 	print("according to local condition, the farm transport involve for fertiliser the transport from the local depot to the field; istead for pesticide it included the transport of the machinery from a surrounding farm, located 6.43 km away and to the field")
	farm.transport.assumptions=read.table("input/farm.transport.assumptions.csv", sep=",", header=TRUE)
	assign("farm.transport.assumptions", farm.transport.assumptions, envir=LCA.env)
#  	Sys.sleep(1)
 	
	
	fuel=read.table("input/fuel.csv", sep=",", header=TRUE)
# 	print(fuel)
# 	browser()
	assign("fuel", fuel, envir=LCA.env)
	
	electricity=read.table("input/electricity.csv", sep=",", header=TRUE)
# 	print(electricity)
# 	browser()
	assign("electricity", electricity, envir=LCA.env)
	
	
	transportmeanslist=read.table("input/transport.csv", sep=",", header=TRUE)

	
	  assign("transportmeanslist", transportmeanslist, envir=LCA.env)
# 	browser()
#  	Sys.sleep(0.5)
	
	assumptions=read.table("input/assumptions.csv", sep=",", header=TRUE)
# 	print(assumptions)
	assign("assumptions", assumptions, envir=LCA.env)
#  	browser()
         
	
	malc=read.table("input/life.cycle.machinery.csv", sep=",", header=TRUE)
# 	print(head(malc))
	assign("malc", malc, envir=LCA.env)
	
#          Sys.sleep(0.5)
#         print("reading data contained in the materials and save them in a global variable called materials")
	materials=read.table("input/materials.csv", sep=",", header=TRUE)
# 	print(head(materials))
	assign("materials", materials, envir=LCA.env)
	
	cat("\n", "\n", "\n")
        print("Retrieving data to calculate soil GHG emissions")
        cat("\n", "\n", "\n") 
      
        soil.emissions=read.table(file="input/soil.emissions.csv", header=TRUE, sep=",")
	
	
	#_____________________________________________________________________________________________________________________________________________________________________________________________________________________
	
	#________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
	
# 	print("Reading crop uptake and climate for nitrate leaching and P loss procedure according to Nemecek et al. 2014")
	climate=read.table("input/climate.csv", sep=",", header=TRUE)
# 	print(climate)
	assign("climate", climate, envir=LCA.env)
	
	crop.uptake=read.table("input/crop uptake.csv", sep=",", header=TRUE)
#  	print(crop.uptake)
 	

	
	rooting.depth=read.table("input/rooting.depth.csv", sep=",", header=TRUE)
# 	print(rooting.depth)
	
	N.assumptions=read.table("input/N.assumptions.csv", sep=",", header=TRUE)
	rownames(N.assumptions)=N.assumptions[,2]
	
# 	print(N.assumptions)
	
	
 	
	
	
	
	
	
	
	


	
	
	
	Sys.sleep(0.5)
	
	assumptionsP=read.table("input/assumptions P.csv", sep=",", header=TRUE)
	

	#_____________________________________________________________________________________________________________________________________________________________________________________________________________________
	#______________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
	
	
	
	
	
        
	rass(LCA.env)
	
# 	substituting assumptions with data for P assumptions
	
# 	converting P assumptions into variable
	
	P.data=data.frame(assumptionsP[,4])
# 	print(P.data)
#  	print(as.vector(as.character(assumptionsP[,2])))
	rownames(P.data)=as.vector(as.character(assumptionsP[,2]))
	
	assign("assumptions", N.assumptions, envir=LCA.env)
	rass(LCA.env)
# 	print(LCA.env$clay.content)
	


	 #_______________________________________________________________________________________________________________________________________________________________________________________________________________________	
 	#_____________________________________________________________________________________________________________________________________________________________________________________________________
        cat("\n")
	print("Calculating . . .")
	cat("\n", "\n", "\n")

#_____________________________________________________________________________________________________________________________________________________________________________________________________________________	

#_____________________________________________________________________________________________________________________________________________________________________________________________________________________
       
	fc(LCA.env)
#  	browser()
	Sys.sleep(0.5)
	fico=data.frame(LCA.env$fico)
	if (((nrow(fico)!=nrow(malc))==TRUE)|((nrow(fico)!=nrow(farm.transport.assumptions))==TRUE)|((nrow(malc)!=nrow(farm.transport.assumptions))==TRUE)) {
	       print("countercheck that the number of operation containted in the Glenlea.R.input.table.csv including petrol harvest operation are the same number in the farm.tranport.assumptions and machinery.life.cycle.inventory.csv file")
	       }
# 	browser()
#         print("")
# 	print("calculating impact of fuel transport from the depot to the farm for machinery operation, stored in a temporary variable and in a global variable ftf with the similar lay out of the fico variable ")
# 	print("")
# 	print("")
	#______________________________________________________________________________________________________________________________________________________________________________________

	  amount=as.numeric(as.character(fico[,6]+fico[,7]+fico[,8]))
	  distance=as.numeric(as.character(LCA.env$f.t.d))
	  transport.mean=as.character(LCA.env$f.t.m)
	  
	  assign("transport.mean", transport.mean, envir=LCA.env)
#           print(transport.mean)
#           print(amount)
          
          tf(LCA.env, distance, amount, transport.mean)
	  fuel.transport.fico=LCA.env$output	 
	  
	  Sys.sleep(0.5)
	  
	  fuel.transport.fico=data.frame(fico[,1:5], fuel.transport.fico)		  
	  assign("ftf", fuel.transport.fico, envir=LCA.env)
# 	  print(head(ftf))
	  
	  
# 	  Sys.sleep(0.5)
# 	  browser()
	  
	 
# 	 print("calculating impact for farm trasnport for each machinery operation, stored in a global variable called farmti.csv")
	  #______________________________________________________________________________________________________________________________________________________________________________________
# 	  browser()
          Sys.sleep(0.5)
	  farm.transport(LCA.env)
	  
# 	  browser()
	 
	 
	  
# 	  print("calculating the impact for the transport of fuel needed for farm transport")
# 	  #____________________________________________________________________________________________________________________________________________________________________________________________
	  for (i in (1:nrow(LCA.env$farmti))) {
		  
		     amount[i]=as.numeric(as.character(LCA.env$farmti[i,6]))+as.numeric(as.character(LCA.env$farmti[i,7]))+as.numeric(as.character(LCA.env$farmti[i,8]))
		     
		}
# 		print(amount)
# 		Sys.sleep(1)
	
	  
# 	  print(amount)
	  transport.mean=as.character(LCA.env$f.t.m)
	 
#	  browser()
        Sys.sleep(0.5)
        distance=as.numeric(as.character(LCA.env$f.t.d))
	
	farm.transport.fuel.transport=tf(LCA.env, distance, amount, transport.mean)
	
	  
		
#  	print(amount)
 	fatfti=data.frame(fico[,1:5],farm.transport.fuel.transport)
#  	print(head(fatfti))
 	write.table(fatfti, file="farm.transport.fuel.transport.csv", sep=",", row.names=FALSE)
 	assign("fatfti", fatfti, envir=LCA.env)
 	
 	
 	
 	
#  	print("")
# #         print(" building up the inventory file for machinery fuel consumption during field cultivation, transport of fuel needed for cultivation, farm transport, tranport of fuel for farm transport and field cultivation")
#         print("")
#         print("")
 #_____________________________________________________________________________________________________________________________________________________________________________________________________________________
#         Sys.sleep(0.5)
        
	LCA.env$PC=as.numeric(as.character(LCA.env$fico[,7]))+as.numeric(as.character(LCA.env$ftf[,7]))+as.numeric(as.character(LCA.env$farmti[,7]))+as.numeric(as.character(LCA.env$fatfti[,7]))
	
 	LCA.env$DC=as.numeric(as.character(LCA.env$fico[,6]))+as.numeric(as.character(LCA.env$ftf[,6]))+as.numeric(as.character(LCA.env$farmti[,6]))+as.numeric(as.character(LCA.env$fatfti[,6]))
        DC=LCA.env$DC
	
 	LCA.env$CC=rep(0, length(DC))
 	LCA.env$LOC=as.numeric(as.character(LCA.env$fico[,8]))+as.numeric(as.character(LCA.env$farmti[,8]))
#  	        print(LOC)

 	LCA.env$GC=rep(0, length(DC))
 	LCA.env$FOC=rep(0, length(DC))
 	LCA.env$location=as.character(LCA.env$fuel.production.location)
 	ffci=cbind(fico[,1:5], LCA.env$PC, LCA.env$DC, LCA.env$LOC, LCA.env$CC, LCA.env$GC, LCA.env$FOC)
 	
 	write.table(ffci, file="cultivation.fuel.transport.fuel.consumption.inventory.csv", sep=",", row.names=FALSE)
 	
	
	
# 	cat("\n", "\n", "\n")
# 	browser()
#         print("calculating the impact of of fuel production for the fuel necessary for field cultivation, farm transport, transport of fuel needed for field cultivation and farm transport")
#         print("")
 #_____________________________________________________________________________________________________________________________________________________________________________________________________________________
# 	Sys.sleep(0.5)
#  browser()

 	fp(LCA.env)
	mfp=get("output", envir=LCA.env)
#   	print(head(mfp))
#   	browser()
 	mfp=cbind(LCA.env$fico[,1:5], mfp)
 	
 	write.table(mfp, file="machinery.fuel.production.csv", sep=",", row.names=FALSE)
 	assign("mfp", mfp, envir=LCA.env)
#  	print("the overall output, including fuel production for: field consumption, farm transport, fuel needed to transport the fuel used in field cultivation, fuel needed to transport the fuel used in farm transport, is saved as a variable called mfp, machienry fuel production and in a file called machinery.fuel.prodcution.csv")
 	
#  	Sys.sleep(0.5)
 	
 	
 cat("\n")
 print("Summing inventory for machinery use")
  cat("\n", "\n", "\n")

  #______________________________________________________________________________________________________________________________________________________________________________________
 	fico=get("fico", envir=LCA.env)
 	farmti=get("farmti", envir=LCA.env)
 	ftf=get("ftf", envir=LCA.env)
 	
 	for (i in (1:nrow(fico))) {
 	                         lin=rep(0, ncol(fico)-9)
 	                         for (col in (9:ncol(fico))) {
 	                                                     ee=fico[i,col]+mfp[i,col-3]+farmti[i,col]+ftf[i,col-1]+fatfti[i,col-1]
 	                                                     if (col==9) {
 	                                                      lin=ee
 	                                                      }
							      else{
							           lin=cbind(lin, ee)
# 							           print("panta rei")
							      }
#   							      print(col)
#   							      print(lin)
  							     
 	                              }
 	                        if (i==1) {
					    tot=lin
				    }
 	                         else{
				    tot=rbind(tot, lin)
#  				    print(tot)
# 				    print("fico")
#   				    print(fico[1,])
#   				   print("mfp")
#   				    print(mfp[1,])
#   				    print("farmti")
#   				    print(farmti[1,])
#   				    print("ftf")
#   				    print(ftf[1,])
#   				    print("fatfti")
#   				    print(fatfti[1,])
#   				  
#    				    browser()

  				    }
				  

 	                         }
 	                         machinery.use.inventory=cbind(fico[,1:5],tot)
 	                         colnames(machinery.use.inventory)=c(colnames(fico[,1:5]),"Energy consumption GJ", "CO2 kg", "CO2 biogenic kg", "CH4 kg", "N2O kg", "CO kg", "NH3 kg", "NO2 kg", "SO2 kg", "NMVOC kg",  "PM kg", "SF6 kg", "NO3 to water kg", "PO4 kg", "P kg") 
#  	                         print(tot)
 	                         write.table(machinery.use.inventory, file="machinery.use.inventory.csv", sep=",", row.names=FALSE)

 	
 	
 	print("Calculating the impact of pesticide production and transport")
 	cat("\n", "\n", "\n")
#  	Sys.sleep(2)
 #___________________________________________________________________________________________________________________________________________________________________________________________________________________________________
	ppt(LCA.env)
	
#_______________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________	
	
	print("Calculating the impact of fertiliser production and transport, together with nutrient inputs")
	cat("\n", "\n", "\n")
# 	Sys.sleep(2)
        fpt(LCA.env)	

 	

	      print("Calculating impact of emissions and energy consumed for machinery production, tranport, maintenance and repairs, integrating GHGenius with Audsley et al. 1997 and Brentrup et al. 2004")
	      cat("\n", "please wait... it may take some time")
#       browser()
      #___________________________________________________________________________________________________________________________________________________________________________________________________________________________________
      #############################################################################################################################################################################################################V
  	mlc(LCA.env)
###############################################################################################################A
#         Sys.sleep(0.5)
cat("\n","\n")
print("Calculating the impact of seed production and transport")
cat("\n","\n")
seed.production.transport(LCA.env)

#_________________________________________________________________________________________________________________________________________________________________________________________________________________________






#         print("")
#         print("preparing the year value data frame ordered by increasing plot label and year for phosphate, NH4-N, NO3-N table")
#         print("")
#         print("")
        
          
	  #print(LCA.env$ploti)



	  #print(LCA.env$year)



 	
  	LCA.env$input.table=LCA.env$nin
 	
 	
 	yt(LCA.env)
 	N.P.input=LCA.env$year.results
#  	browser()
 	print("Calculating the quantity of NO3-N for the cropping systems")
 	cat("\n","\n", "\n")
 
 
 
  	sapply(1:nrow(N.P.input), function(yi) N.loss(LCA.env, N.assumptions, N.P.input, climate, crop.uptake, rooting.depth, root.to.shoot.ratio, yi))
  	LCA.env$nitrate.leaching=data.frame(N.P.input[,1:3], LCA.env$nitrate.leaching)
  	colnames(LCA.env$nitrate.leaching)=c(colnames(N.P.input[,1:3]), "soil nitrate kg N ha-1 y-1")
	write.table(LCA.env$nitrate.leaching, file="output/nitrate.leaching.csv", sep=",", row.names=FALSE)
# 	print(LCA.env$nitrate.leaching)
# 	browser()
 	#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
 	
 	#_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
 	print("Calculating the quantity of Phosphate-P loss and soil erosion")
 	
 	cat("\n","\n", "\n")
#  	Sys.sleep(1)
#  	P.loss(LCA.env)
        
 	year=LCA.env$year        
  	sapply(1:nrow(N.P.input), function(yi) soil.P.loss(LCA.env, P.data, N.P.input, climate, yi))
  	
  	LCA.env$soil.erosion=data.frame(N.P.input[,1:3], LCA.env$soil.erosion)
  	colnames(LCA.env$soil.erosion)=c(colnames(N.P.input[,1:3]), "soil erosion kg ha-1 y-1")
#   	print(LCA.env$soil.erosion)
  	write.table(LCA.env$soil.erosion, file="output/soil.erosion.csv", sep=",", row.names=FALSE)
  	
  	
  	LCA.env$P.loss=data.frame(N.P.input[,1:3],round(LCA.env$soil.P.erosion.to.rivers, 3), round(LCA.env$P.run.off.water, 3), round(LCA.env$P.leaching.ground.water,3), round(LCA.env$P.loss,3))
  	colnames(LCA.env$P.loss)=c(colnames(N.P.input[,1:3]), "Phosphorus emissions through water erosion to surface water (kg P ha-1 y-1)", "Phosphate run ­off to surface water (kg P ha-1 y-1)", "Phosphate leaching to ground water (kg P ha-1 y-1)", "Phosphate P loss to water  (kg P ha-1 y-1)")
#   	print(LCA.env$P.loss)
  	write.table(LCA.env$P.loss, file="output/P.loss.csv", sep=",", row.names=FALSE)
  	
  	#_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
 	
 	#____________________________________________________________________________________________________________________________________________________________________________________________
 	
#  	system("mkdir output")

  	#print("formatting all the tables to compute the LCI inventory")
  
#   	print("formatting machinery use inventory")
  	machinery.use.inventory=machinery.use.inventory[,-4:-5]
  	LCA.env$input.table=machinery.use.inventory
  	yt(LCA.env)
#   	browser()
  	year.machinery.use.inventory=LCA.env$year.results
  	#print(year.machinery.use.inventory)
  	write.table(year.machinery.use.inventory, file="output/year.machinery.use.inventory.csv", sep=",", row.names=FALSE)
#   	print(str(year.machinery.use.inventory))
  	
#   	print("formatting pesticide use inventory")
  	pesticides.inventory=LCA.env$tppt[,-4:-5]
  	LCA.env$input.table=pesticides.inventory
  	yt(LCA.env)
  	year.pesticides.inventory=LCA.env$year.results
  	#print(year.pesticides.inventory)
  	write.table(year.pesticides.inventory, file="output/year.pesticides.csv", sep=",", row.names=FALSE)

#   	print("formatting fertiliser use inventory")
  	fertilisers.inventory=LCA.env$tfpt[,-4:-5]
  	LCA.env$input.table=fertilisers.inventory
  	yt(LCA.env)
  	year.fertilisers.inventory=LCA.env$year.results
  	#print(year.fertilisers.inventory)
  	write.table(year.fertilisers.inventory, file="output/year.fertilisers.inventory.csv", sep=",", row.names=FALSE)
  	
#  	print("formatting machinery production maintenance repairs inventory")
  	
  	########################################################################################################################################################################################################################V
 	machinery.production.maintenance.repairs.inventory=LCA.env$tmptr[,-4:-5]
	
#	year.machinery.production.maintenance.repairs.inventory=read.table(file="output/year.machinery.production.maintenance.repairs.inventory.csv", sep=",",header=TRUE)
	
   	LCA.env$input.table=machinery.production.maintenance.repairs.inventory
   	yt(LCA.env)
   	year.machinery.production.maintenance.repairs.inventory=LCA.env$year.results

   	#print(year.machinery.production.maintenance.repairs.inventory)
   	write.table(year.machinery.production.maintenance.repairs.inventory, file="output/year.machinery.production.maintenance.repairs.inventory.csv", sep=",", row.names=FALSE)
   	################################################################################################################################################################################################################################A

#   	print("formatting seed production inventory")
  	seed.inventory=LCA.env$spt[,-4:-5]
  	LCA.env$input.table=seed.inventory
  	yt(LCA.env)
  	year.seed.inventory=LCA.env$year.results
  	#print(year.seed.inventory)
 	write.table(year.seed.inventory, file="output/year.seed.inventory.csv", sep=",", row.names=FALSE)
 	
 	year.inventory=matrix(0, nrow=nrow(year.machinery.use.inventory), ncol=ncol(year.machinery.use.inventory))
 
#  	print(year.inventory)
#  	browser()
 	
 	for (r in (1:nrow(year.inventory))) {
 	                         
 	                         for (col in (4:ncol(year.machinery.use.inventory))) {
					year.inventory[r,col]=as.numeric(as.character(year.machinery.use.inventory[r,col]))+as.numeric(as.character(year.pesticides.inventory[r,col]))+as.numeric(as.character(year.fertilisers.inventory[r,col]))+as.numeric(as.character(year.machinery.production.maintenance.repairs.inventory[r,col]))+as.numeric(as.character(year.seed.inventory[r,col]))



 	                                                      if ((col>=6)&(col<=8)){
# 									  print(soil.emissions[r,col-2])
#                 							      print(year.inventory[r,col])
									  year.inventory[r,col]=as.numeric(as.character(year.inventory[r,col]))+as.numeric(as.character(soil.emissions[r,col-2]))
#   									  print(year.inventory[r,col])
#   									  browser()
 	                                                      }
 	                                                      #considering ammonia volatilisation from soils
 	                                                      if (col==10){
# 									  print(year.inventory[r, 6:col])
									  year.inventory[r,col]=as.numeric(as.character(year.inventory[r,col]))+as.numeric(as.character(soil.emissions[r,7]))
# 									  print(year.inventory[r,col])
# 									  browser()
 	                                                      
 	                                                      }
 	                                                      
  	                                                      if (col==16){
#   	                                                                   print(year.inventory[r,col])
 									   year.inventory[r,col]=as.numeric(as.character(year.inventory[r,col]))+as.numeric(as.character(LCA.env$nitrate.leaching[r,4]))/0.226
#  									   print(year.inventory[r,col])
#  									   print(LCA.env$nitrate.leaching[r,4])
#   									   browser()
  	                                                      }
  	                                                      if (col==17){
 									   #print(year.inventory[r,col])
  	                                                                   year.inventory[r,col]=as.numeric(as.character(year.inventory[r,col]))+as.numeric(as.character(LCA.env$P.loss[r,7]))/0.326
  	                                                                   #print(year.inventory[r,col])
  	                                                                   }
							     
#  							     print("machinery.use")
#  					    print(year.machinery.use.inventory[r,col])
#  					    print("pesticides")
#  						    print(year.pesticides.inventory[r,col])
#  						    print("fertilisers")
#  						    print(year.fertilisers.inventory[r,col])
#  						    print("machinery.MandR")
#  						    print(year.machinery.production.maintenance.repairs.inventory[r,col])
#  						    print("seed")
#  						    print(year.seed.inventory[r,col])
#  						    print("year.inventory")
#  						    print(year.inventory[r,])
#  						    print("soil.emissions")
#  						    print(soil.emissions[r,col-1])
#  						    print("nitrate.loss")
#  						    print(LCA.env$nitrate.leaching[r,4])
#  						    print("p.loss")
#  						    print(LCA.env$P.loss[r,7])
# # 						    
# # 						    
#  						print(col)
# 
#  						print(year.inventory[r,])
#  						    print(r)
#  					        if (col==18){
#  						    browser()
#  					           }

 	                              }
#  	                              print(year.inventory)
#  	                              browser()

 	                         }
#         print(year.inventory)
#  	browser()
#  	
 	year.inventory=data.frame(year.machinery.use.inventory[,1:3], year.inventory[,4:ncol(year.inventory)])
 	
 	# a different table year.inventory.output is built with all the rounded values
 	year.inventory.output=data.frame(year.inventory[,1:3], round(year.inventory[,4],1), round(year.inventory[,5],0), round(year.inventory[,6],3), round(year.inventory[,7:9],2), round(year.inventory[,10],3), round(year.inventory[,11],2), round(year.inventory[,12:14],3), signif(year.inventory[,15],3), round(year.inventory[,16],0), round(year.inventory[,17],2), signif(year.inventory[,18],3))
#         browser() 	
 	colnames(year.inventory)=c(colnames(year.machinery.use.inventory))
 	colnames(year.inventory.output)=c(colnames(year.inventory))
 	
#  	print(year.inventory)_______________________________
 	                         
 	write.table(year.inventory.output, file="output/year.inventory.csv", sep=",", row.names=FALSE)
 	
 	
  	print("Computing GWP using AR5 according to Myhre et al. 2013")
	cat("\n","\n", "\n")
	
 	#____________________________________________________________________________________________________________________________________________________________________________________________________________________________
 	#____________________________________________________________________________________________________________________________________________________________
 	input=year.inventory
 	
#  	print("adding the amount of indirect N2O produced from nitrate leaching and ammonia volatilisation according to DeKlein et al. 2006")
#  	print(head(input))
 	input[,8]=as.numeric(as.character(input[,8]))+as.numeric(as.character(LCA.env$nitrate.leaching[,4]))*0.0075/0.636+as.numeric(as.character(soil.emissions[,7]))*0.824*0.01/0.636
# print(LCA.env$nitrate.leaching)
#  	print(head(input))
 	
 	LCA.env$input=input
 	#print(LCA.env$input)
 	lapply(1:nrow(LCA.env$input), function(i) LCIA.GWP(LCA.env, i))
 	
 	#print(LCA.env$GWP)
 	total.GWP=LCA.env$GWP
#  	browser()
 	
 	#print("computing each contribution to GWP")
 #computing contribution for machinery
         LCA.env$input=year.machinery.use.inventory
         input=LCA.env$input
        
#          ##print(head(LCA.env$input))
              
 	lapply(1:nrow(LCA.env$input), function(i) LCIA.GWP(LCA.env, i))
 	
 	year.machinery.use.GWP=LCA.env$GWP
 	#print(year.machinery.use.GWP)
#  	browser()
 	
 	 LCA.env$input=year.pesticides.inventory
#  	 ##print(head(LCA.env$input))
 	lapply(1:nrow(LCA.env$input), function(i) LCIA.GWP(LCA.env, i))
 	
 	year.pesticides.GWP=LCA.env$GWP
  	#print("pesticides")
  	#print(year.pesticides.GWP)
 	
 	
 	 LCA.env$input=year.fertilisers.inventory
#  	 ##print(head(LCA.env$input))
 	lapply(1:nrow(LCA.env$input), function(i) LCIA.GWP(LCA.env, i))
 	
 	year.fertilisers.GWP=LCA.env$GWP 	
  	#print("fertilisers")
 	#print(year.fertilisers.GWP)
#  	browser()
 	
 	LCA.env$input=year.machinery.production.maintenance.repairs.inventory
 	#print(head(LCA.env$input))
 	lapply(1:nrow(LCA.env$input), function(i) LCIA.GWP(LCA.env, i))
 	
 	year.machinery.production.maintenance.repairs.GWP=LCA.env$GWP
  	#print("year.machinery.production.maintenance.repairs.GWP")
  	#print(year.machinery.production.maintenance.repairs.GWP)
 	
 	
 	LCA.env$input=year.seed.inventory
 	#print(head(LCA.env$input))
 	lapply(1:nrow(LCA.env$input), function(i) LCIA.GWP(LCA.env, i))
 	
 	year.seed.GWP=LCA.env$GWP
 	#print("year.seed.GWP")
  	#print(year.seed.GWP)

#  	print(LCA.env$nitrate.leaching[,4])
#  	print(soil.emissions[,7])
 	indirect.N2O.GWP=as.numeric(as.character(LCA.env$nitrate.leaching[,4]))*0.0075/0.636*265+as.numeric(as.character(soil.emissions[,7]))*0.824*0.01/0.636*265
#  	print(indirect.N2O.GWP)
#  	browser()
 	
 	soil.CO2.GWP=soil.emissions[,4]
 	soil.CH4.GWP=soil.emissions[,5]*28
 	soil.N2O.GWP=soil.emissions[,6]*265
 	
 	
 	#computing contribution for different process
 	machinery.contribution=year.machinery.use.GWP/abs(total.GWP)*100
#  	for (i in (1:length(machinery.contribution))){
# 						    ##print(machinery.contribution[i])
# 						    ##print(year.machinery.use.GWP[i])
# 						    ##print(total.GWP[i])
# 						    ##print(i)
# 						    }

						    
# 	##print("pesticides")					    
	pesticides.contribution=year.pesticides.GWP/abs(total.GWP)*100
#  	for (i in (1:length(pesticides.contribution))){
# 						    ##print(pesticides.contribution[i])
# 						    ##print(year.pesticides.GWP[i])
# 						    ##print(total.GWP[i])
# 						    ##print(i)
# 						    }
						    
# 	##print("fertiliser")
	fertilisers.contribution=year.fertilisers.GWP/abs(total.GWP)*100				    
# 	for (i in (1:length(fertilisers.contribution))){
# 						    ##print(fertilisers.contribution[i])
# 						    ##print(year.fertilisers.GWP[i])
# 						    ##print(total.GWP[i])
# 						    ##print(i)
# 						    }
# 	browser()					    
						    
	machinery.production.maintenance.repairs.contribution=year.machinery.production.maintenance.repairs.GWP/abs(total.GWP)*100					    
#  	for (i in (1:length(machinery.production.maintenance.repairs.contribution))){
# 						    ##print(machinery.production.maintenance.repairs.contribution[i])
# 						    ##print(year.machinery.production.maintenance.repairs.GWP[i])
# 						    ##print(total.GWP[i])
# 						    ##print(i)
# 						    }
	seed.contribution=year.seed.GWP/abs(total.GWP)*100
#  	for (i in (1:length(seed.contribution))){
# 						    ##print(seed.contribution[i])
# 						    ##print(year.seed.GWP[i])
# 						    ##print(total.GWP[i])
# 						    ##print(i)
# 						    }
# 	browser()
# 	##print("N2O")					    
	soil.N2O.contribution=soil.N2O.GWP/abs(total.GWP)*100
# 	for (i in (1:length(soil.N2O.contribution))){
# 						    ##print(soil.N2O.contribution[i])
# 						    ##print(soil.N2O.GWP[i])
# 						    ##print(total.GWP[i])
# 						    ##print(i)
# 						    }
	soil.CO2.contribution=soil.CO2.GWP/abs(total.GWP)*100
# 	for (i in (1:length(soil.CO2.contribution))){
# 						    ##print(soil.CO2.contribution[i])
# 						    ##print(soil.CO2.GWP[i])
# 						    ##print(total.GWP[i])
# 						    ##print(i)
# 						    }
# 						    ##print("CO2")
	soil.CH4.contribution=soil.CH4.GWP/abs(total.GWP)*100
# 	for (i in (1:length(soil.CH4.contribution))){
# 						    ##print(soil.CH4.contribution[i])
# 						    ##print(soil.CH4.GWP[i])
# 						    #print(total.GWP[i])
# 						    #print(i)
# 						    }
# 						    browser()
	indirect.N2O.contribution=indirect.N2O.GWP/abs(total.GWP)*100
# 	for (i in (1:length(indirect.N2O.contribution))){
# 						    #print(indirect.N2O.contribution[i])
# 						    #print(indirect.N2O.GWP[i])
# 						    #print(total.GWP[i])
# 						    #print(i)
# 						    }
         #print(head(soil.emissions))
         #print(machinery.contribution)
#        #print("machinary")
         #print(machinery.production.maintenance.repairs.contribution)
         #print(fertilisers.contribution)
         #print(pesticides.contribution)
         #print(seed.contribution)
        #print(soil.CO2.contribution)
         #print(soil.CH4.contribution)
         #print(soil.N2O.contribution)
         #print(indirect.N2O.contribution)
#   	browser()
        #contribution table
        contribution=cbind(soil.emissions[,1:3], round(machinery.contribution, 1), round(machinery.production.maintenance.repairs.contribution,1), round(fertilisers.contribution, 1), round(pesticides.contribution, 1), round(seed.contribution, 1), round(soil.CO2.contribution,1), round(soil.CH4.contribution,1), round(soil.N2O.contribution,1), round(indirect.N2O.contribution,1))
        contribution=as.data.frame(contribution)
        colnames(contribution)=c(colnames(soil.emissions[,1:3]), "machinery use (%)", "machinery production maintenance and repairs (%)", "fertilisers (%)", "pesticides (%)", "seed (%)", "soil CO2 emissions (%)", "soil CH4 emissions (%)", "soil N2O emissions (%)", "indirect N2O emissions (%)")
        write.table(contribution, file="output/GWP contribution.ha.csv", sep=",", row.names=FALSE)
        
        
        
        
        print("Computing the acidification potential according to Styles et al., 2014")
        cat("\n", "\n", "\n")
  	
        #____________________________________________________________________________________________________________________________________________________________
        #_______________________________________________________________________________________________________________________________________________________________________________________________________________
        
         	LCA.env$input=year.inventory
 	#print(LCA.env$input)
 	lapply(1:nrow(LCA.env$input), function(i) LCIA.AP(LCA.env, i))
 	
 	#print(LCA.env$GWP)
 	total.AP=LCA.env$AP
 	
 	
 	print("Computing the eutrophication potential according to Styles et al., 2014")
	cat("\n", "\n", "\n")
 	
 	
 	#____________________________________________________________________________________________________________________________________________________________
	#____________________________________________________________________________________________________________________________________________________________
	LCA.env$input=year.inventory
 	#print(LCA.env$input)
 	lapply(1:nrow(LCA.env$input), function(i) LCIA.EP(LCA.env, i))
 	
 	#print(LCA.env$GWP)
 	total.EP=LCA.env$EP
#  	print(total.EP)




	print("Preparing the final table")
	cat("\n", "\n")
	#_____________________________________#########################
	#______________________________________________________________________________________________________________________________________________________________________________________
	
	
	LCA.results=data.frame(year.inventory[,1:3], round(year.inventory[,4],1), round(total.GWP,1), round(total.EP,2), round(total.AP,2))
	colnames(LCA.results)=c(colnames(year.inventory[,1:3]), "CED (GJeq ha-1 y-1)", "GWP (kg of CO2eq ha-1 y-1)", "EP (kg of PO4 eq ha-1 y-1)", "AP (kg of SO2eq ha-1 y-1)")
	write.table(LCA.results, file="output/LCA.results.ha.csv", sep=",", row.names=FALSE)
	
	


	
 	print("Processing successfully completed")
	
}
