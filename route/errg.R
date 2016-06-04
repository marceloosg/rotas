tryCatch.W.E <- function(expr)
       {
                    W <- NULL
                    w.handler <- function(w){ # warning handler
                         	W <<- w
                         	invokeRestart("muffleWarning")
                            }
                     list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                                       				     warning = w.handler),
                            	 warning = W)
                }
error_route <- function(from, to, 
                        structure = 'route', mode = 'driving', 
                        output="simple",alternatives=TRUE,key=key) 
        {
        tries=1
        maxtries=5
        geo=tryCatch.W.E(route_key(from=from, to=to, 
                                   structure=structure , mode=mode , 
                                   output=output,alternatives=alternatives
                                   ,key=key) 
                         )
        while(sum(is.na(geo$value$lat))>0 & tries <= maxtries){
                Sys.sleep(1)
                tries=tries+1
                geo=tryCatch.W.E(route_key(...))
                print(c(sum(is.na(geo$value)),geo$value,"Try:",tries,"Warning:",geo$warning))
        }
        return(geo$value)
}
error_geo <- function(fun,...)
        {
        tries=1
        maxtries=5
        geo=tryCatch.W.E(fun(...))
        while(sum(is.na(geo$value$lat))>0 & tries <= maxtries){
                Sys.sleep(1)
                tries=tries+1
                geo=tryCatch.W.E(fun(...))
                print(c(sum(is.na(geo$value)),geo$value,"Try:",tries,"Warning:",geo$warning))
        }
        return(geo$value)
}
error_rev <- function(fun,...)
{
        tries=1
        maxtries=5
        geo=tryCatch.W.E(fun(...))
        while(sum(is.na(geo$value))>0 & tries <= maxtries){
                Sys.sleep(1)
                tries=tries+1
                geo=tryCatch.W.E(fun(...))
                print(c(sum(is.na(geo$value)),geo$value,"Try:",tries,"Warning:",geo$warning))
        }
        return(geo$value)
}
mygeom=function(a,b){
        i=1
        g=ggmap(map)+geom_line(data=rbind(a[i],b[i]),aes(x=lon,y=lat),color="blue")
        for(i in 2:dim(a)[1]){
             g=g+geom_line(data=rbind(a[i],b[i]),aes(x=lon,y=lat),color="blue") 
        }
        return(g)
}

safetry.in <- function(expr)
{
        W <- NULL
        w.handler <- function(w){ # warning handler
                W <<- w
                invokeRestart("muffleWarning")
        }
        .lastError=NULL
        ret=list(value = withCallingHandlers(
                tryCatch(eval(expr), 
                         error = function(e){
                                 .lastError <<-e
                                 e}),
                warning = w.handler),
                warning = W,!is.null(.lastError)|!is.null(W),list(.lastError,W))
        ret
}
safetry = function(expr){
        save.expr=expr
        errorcount=0
        nosuccess=T
        while(errorcount < 5 && nosuccess==T)
        {
        ret=safetry.in(save.expr)
        print(c(errorcount,ret))
        nosuccess=ret[[3]]
        errorcount=errorcount+1
        if(nosuccess) Sys.sleep(3)
        }
        val=ret[[1]]
        if(errorcount==3 && nosuccess==T){
                val=NULL
        }
        return(val)
}
