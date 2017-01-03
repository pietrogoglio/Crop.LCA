variableremoval=function(e=.GlobalEnv){
remove(list=ls(e)[sapply(ls(e),function(n){!is.function(get(n))})],envir=e)
} 