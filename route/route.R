library(ggmap)
library(data.table)
library(xlsx)
library(dplyr)
#source("rot.R")
#source("errg.R")
rota=c()
kms=c()
cache=c()
routecache=c()
compadd=c()
key=as.character(unlist(read.table("key")[1]))
fullceps=lapply(1:dim(motocli)[1],function(i){
       with(motocli[i],paste(sub("Rua", "R",TipoLogradouro),".",Endereco,
                            ",",NumRua,"-",Bairro,
                            ", São Paulo - SP,Brasil"))})

if(file.exists("xlsx/distancia-min-enderecos-cep.xlsx"))
{
rota=select(data.table(read.xlsx("xlsx/distancia-min-enderecos-cep.xlsx",1)),-NA.)
#rota=unique(rota,by="Endereco_Considerado")
kms=c(rota$km)
cache=c(as.character(rota$addr))
compadd=rota$Endereco_Considerado
routecache=rota
} else
{
print("No route cache found. Calculating from scratch.")
#quit()
}
i=0
from <- 'Dr Clovis de Oliveira,339, São Paulo,São Paulo,Brazil'
mapa=c()
novo=0
t=0

for(address in fullceps){
i=i+1

to <- address
j=which(cache==address)
#print(c("class",class(j),j))
progress=as.integer(i/length(fullceps)*100)
if(length(j) > 0){
        print(c("ok",progress))
        mapa=c(mapa,j)
}
else
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
        route_dft <- route(from, to, 
                               structure = 'route', mode = 'driving', 
                               output="simple",alternatives=TRUE)
        if( length(route_dft$route ) == 0 ){
                route_dft$route="A"
        }
        full=data.table(route_dft)[,.(leg,lon,lat,km=sum(km,na.rm=TRUE),minutes=sum(minutes,na.rm=TRUE)),by=route]
#        rotas=filter(full,lon==route_dft$lon[1], lat==route_dft$lat[1]) 
        index=which(is.na(full$leg)==TRUE)
        index=index[index<=dim(full)[1]]
        rotas=full[index,]
        print("cache")
        route_df=rotas[which.min(rotas$km),]
        cache=c(cache,to)
       print("dfr")
   #     adfr=data.table(t(lapply(select(route_df,-route),as.double)))
   #     dfr=data.table(t(lapply(as.matrix(adfr),sum,na.rm=TRUE)))
   #     colnames(dfr)=colnames(adfr)
        dfr=select(route_df,-leg)
        dfr$addr=to
#        geo=error_geo(geo_key,to,key=key)
         print("lock")
         #geo=geo_key(to,key=key)
         geo=geocode(to)
         #        rev=esdrror_rev(revgeocode,unlist(data.table(t(geo))))
         print("unlock")
         rev=revgeocode(unlist(data.table(t(geo))))

        shortrev=unlist(strsplit(as.character(rev),","))
   #      dfr$cep=shortrev[4]
        dfr$ll=paste(geo$lon,geo$lat)
        dfr$short=shortrev[1]
        dfr$Endereco_Considerado=rev
   #     differs=(trimws(dfr$cep,"both")==motocli$Cep[i])
        print(c(dfr$short,motocli$Endereco[i]))
        compadd=c(compadd,rev)
    #    dfr$lat=route_df$lat[dim(route_df)[1]]
    #    dfr$lon=route_df$lon[dim(route_df)[1]]
 #       dfr$leg=route_df$leg[dim(route_df)[1]-1]
#        print(c("dim",dim(dfr)))
        routecache=rbindlist(list(routecache,dfr))
        mapa=c(mapa,dim(routecache)[1])
}

}
print("endloop")
final=motocli
final$Entregador=sub("MOTOBOY A", "KLEVERTON",motocli$Entregador)
final$Entregador=sub("MOTOBOY B", "CAIO",final$Entregador)
final$Entregador=sub("MOTOBOY C", "ROMILSON",final$Entregador)
tfinal=data.table(indice=mapa,routecache[mapa],final[1:length(mapa)])
ifood=grep("IFOOD",tfinal$NomeArquivoWeb)
pweb=grep("ECLT",tfinal$NomeArquivoWeb)
tfinal$NomeArquivoWeb="Telefone/outros"
tfinal$NomeArquivoWeb[ifood]="IFOOD"
tfinal$NomeArquivoWeb[pweb]="Pedidos via Web"
abre=unclass(as.POSIXlt(tfinal$DT_HO_ABRE,format="%d/%m/%Y %H:%M:%S"))
sai=as.POSIXlt(tfinal$HrSaida,format="%H:%M:%S")
usai=unclass(sai)
volta=as.POSIXlt(tfinal$HrVolta,format="%H:%M:%S")
preparo=usai$hour*60-abre$hour*60+usai$min-abre$min+usai$sec/60-abre$sec/60
tempo=(as.double((volta-sai)/60))
tfinal[,Tempo_Entrega:=tempo]
tfinal[,Tempo_Preparo:=preparo]
tfinal$Endereco_Considerado=compadd[mapa]
selecao=select(tfinal,Endereco,short,
               Data_Pedido=DT_HO_ABRE,Entregador,
               km,Razao,TelCli,Tempo_Estimado_Minutos=minutes,
               Valor_Total_do_Pedido=VLR_TOTAL,
               DINHEIRO,	VALE_REF,			CARTAO,
               OUTROS,	Desconto=VLR_DESC_TOT,	Taxa_de_Entrega=VLR_SERV,
               Canal=NomeArquivoWeb,TipoLogradouro,	Endereco, Endereco_Considerado,	
               NumRua, ComplEnd,Obs,E_Mail,	Bairro,	Cidade,	Estado,	Cep,
                Tempo_Preparo,Tempo_Entrega,lat,lon,HrSaida,HrVolta,Pesquisa_Google=addr)
#removed,
stat=FALSE
sprint=selecao
sprint$Data_Pedido=as.Date(sprint$Data_Pedido,format("%d/%m/%Y"))
setorder(sprint,-Data_Pedido,-km)
maxdate=max(sprint$Data_Pedido)
today=as.character(maxdate,format="%d.%m.%Y")
print(c("Max-date",today))
oldopt=options()
options(xlsx.date.format="d/m/yyyy")
#write.xlsx(tfinal,paste0("xlsx/distancia-min-pedidos-completo-",today,".xlsx"),append=stat)

output=paste0("xlsx/distancia-min-pedidos-resumido-",today,".xlsx")
write.xlsx(sprint,output)
file.copy(output,paste0("xlsx/distancia-min-pedidos-resumido.xlsx"),overwrite=TRUE)

#write.xlsx(sprint,"xlsx/distancia-min-pedidos-resumido.xlsx",append=stat)
#write.xlsx(routecache,paste0("xlsx/distancia-min-enderecos-",today,".xlsx"),append=stat)
#write.xlsx(routecache,"xlsx/distancia-min-enderecos.xlsx",append=stat)
output=paste0("xlsx/distancia-min-enderecos-",today,".xlsx")

write.xlsx(routecache,output)
file.copy(output,paste0("xlsx/distancia-min-enderecos-cep.xlsx"),overwrite=TRUE)
options(oldopt)
write.csv(today,"uptodate")
#
#qmplot(lon, lat, data = tfinal,geom = c("point"), size = as.double(VLR_TOTAL),alpha=0)+  geom_density2d(data = tfinal, aes(x = lon, y = lat)) + stat_density2d(data = tfinal, aes(x = lon, y = lat,  fill = ..level.., alpha=sqrt(..level..)/3 ), geom = 'polygon',alpha=0.2)+scale_fill_gradient(low = "white", high = "red")  +geom_jitter(data=tfinal, aes(x = lon, y = lat),alpha=min(as.double(km)/20,1),color="green")
#final=select(motocli,Entregador,Endereco=enderecos,Cep,Cliente,TelCli)
#qmap('São Paulo, São Paulo', zoom = 16) +
 #       geom_path(
  #              aes(x = lon, y = lat),  colour = 'red', size = 1.5,
   #             data = route_df, lineend = 'round')
#rr=ldply(routecache$ll,function(ll){print(c(ll,as.character(ll)));r=t(as.double(unlist(strsplit(as.character(ll)," "))));colnames(r)=c("lon","lat");data.table(r)})
#tm=data.table((data.table(rr)+select(routecache,lon,lat)))/2
#ss=data.table((data.table(rr)-select(routecache,lon,lat))^2)
#dis=sqrt(ss$lat*cos(tm$lat/360*3.14159268)^2+ss$lon)*6300000/360*3.1415
