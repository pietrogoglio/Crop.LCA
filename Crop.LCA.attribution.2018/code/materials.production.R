materials.production=function(LCA.env) {
materials=get("materials", envir=LCA.env)
material=get("material", envir=LCA.env)
location=get("location", envir=LCA.env)
amount=get("amount", envir=LCA.env)

#     rm(output)
   columns=length(5:ncol(materials))
#    print(columns)
   rows=length(amount)
#    print(rows)
   output=matrix(nrow=rows, ncol=columns)
#    print(output)
#    browser()
for (l in (1:nrow(materials))) {
# 				  print("panta rei")
# 				  print(l)
# 				  if (l==18){
# 					  browser()
# 				  }
                                   if (((location==materials[l, 1])==TRUE)&((material==materials[l,2])==TRUE)) {

                           
				      for (column in (5:ncol(materials))) {
				          
				          output[,column-4]=amount*materials[l,column]
# 					  if (column==5) {
# 					    output=outputi
# 					  }
# 					  else{
# 					    output=data.frame(output, outputi)
# 					    }
# print(output)
# browser()
				      
				      
					}
#                                   print(output)
					output=data.frame(output)
 					 colnames(output)=c("Energy consumption GJ kg-1", "CO2 kg kg-1", "CO2 biogenic kg kg-1", "CH4 kg kg-1", "N2O kg kg-1", "CO kg kg-1", "NH3 kg kg-1", "NO2 kg kg-1", "SO2 kg kg-1", "NMVOC kg kg-1",  "PM kg kg-1", "SF6 kg kg-1", "NO3 to water kg kg-1", "PO4 kg kg-1", "P kg kg-1") 
#  				        colnames(output)=c(1:11)
#   				      print(output)
				      }
				   
                                  





















  }
#   print(output)
  assign("output", output, envir=LCA.env)
#   print(output)
  
}