


rass=function(LCA.env){

#                            print("this function read the assumptions variable and covert the argument contained in columns 2 in a new variable with content what it is contained in the corresponding cell in the 4th column")
                           assumptions=get("assumptions", envir=LCA.env)
			    for (i in (1:nrow(assumptions))) {

                                                                  x=assumptions[i, 2]
#                                                                   print(x)
                                                                  x=as.character(x)
                                                                  y=assumptions[i, 4]
#                                                                   print(y)
#                                                                   is(x)
                                                                  
								assign(x, y, envir=LCA.env)
                                                              
#                                                               print(x)
			  



			    }


# print("the variables made are lobal variables, therefore they are now saved in the R working space")

}
