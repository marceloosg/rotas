i=i+1
to <- address
j=which(cache==address)
progress=as.integer(i/length(fullceps)*100)
if(length(j) > 0){
        print(c("ok",progress))
        mapa=c(mapa,j)
} else
{
        t=t+1
        if(t %% 3==0){
                print("No need to sleep!")
                Sys.sleep(7)
        }
        else{
                print(c("progress",progress,"new entry:",t))
        }
        novo=novo+1
        print(c(novo,"route",address))
        #       to=paste(strsplit(to,",")[[1]][-4],collapse=",")
        geo=safetry(expression(checkaddress(i)))
        if(is.null(geo) ){
                if( i == 0 ) stop("allwrong")
                print("saving")
                output=paste0(prefix,"/distancia-min-enderecos-",today,".xlsx")
                
                write.xlsx(routecache,output)
                file.copy(output,paste0(prefix,"/distancia-min-enderecos-cep.xlsx"),overwrite=TRUE)
                stop(c("too many tries"))
        }
        rev=as.character(geo$addr)
        route_dft <-safetry(expression(route(from, rev, 
                                             structure = 'route', mode = 'driving', 
                                             output="simple",alternatives=TRUE)))
        if(is.null(route_dft) ){
                if( i == 0) stop("allwrong")
                
                print("saving")
                output=paste0(prefix,"/distancia-min-enderecos-",today,".xlsx")
                write.xlsx(routecache,output)
                file.copy(output,paste0(prefix,"/distancia-min-enderecos-cep.xlsx"),overwrite=TRUE)
                stop("too many tries")
        }        
        if( length(route_dft$route ) == 0 ){
                route_dft$route="A"
        }
        full=data.table(route_dft)[,.(leg,lon,lat,km=sum(km,na.rm=TRUE),minutes=sum(minutes,na.rm=TRUE)),by=route]
        index=which(is.na(full$leg)==TRUE)
        index=index[index<=dim(full)[1]]
        rotas=full[index,]
        print("cache")
        route_df=rotas[which.min(rotas$km),]
        cache=c(cache,to)
        print("dfr")
        dfr=select(route_df,-leg)
        dfr$addr=to
        shortrev=unlist(strsplit(as.character(rev),","))
        dfr$ll=paste(geo$lon,geo$lat)
        dfr$short=shortrev[1]
        dfr$Endereco_Considerado=rev
        dfr$Conferir="Sim"
        if(geo$sinal){
                dfr$Conferir="Não"
        }
        dfr$Distancia=geo$dist
        dfr$place=geo$place
        dfr$soma=geo$soma
        dfr$sinal=geo$sinal
        dfr$precisao=geo$precisao
        print(c(dfr$addr,motocli[i]$Cep,dfr$Endereco_Considerado,dfr$sinal,dfr$Distancia))
        compadd=c(compadd,rev)
        routecache=rbindlist(list(routecache,dfr))
        mapa=c(mapa,dim(routecache)[1])
}
print(c("WRONG ADDRESS/TOTAL:",sum(routecache$sinal=="FALSE"),dim(routecache)[1]))