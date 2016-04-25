library(ggmap)
library(data.table)
library(xlsx)
library(dplyr)
rota=c()
kms=c()
cache=c()
routecache=c()
if(file.exists("distancia-min-enderecos.xlsx"))
{
rota=select(data.table(read.xlsx("distancia-min-enderecos.xlsx",1)),-NA.)
kms=c(rota$km)
cache=c(as.character(rota$addr))
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
for(address in enderecos){
i=i+1

to <- address
j=which(cache==address)
#print(c("class",class(j),j))
progress=as.integer(i/length(enderecos)*100)
if(length(j) > 0){
        print(c("ok",progress))
        mapa=c(mapa,j)
}
else
{
        t=t+1
        if(t %% 3==0){
#                print(routecache)
                Sys.sleep(10)
        }
        else{
                print(c("progress",progress,"new entry:",t))
        }
        novo=novo+1
#        print(c(novo,"route",address))
        route_dft <- route(from, to, structure = 'route', mode = 'driving', output="simple",alternatives=TRUE)
        if( length(route_dft$route ) == 0 ){
                route_dft$route="A"
        }
        full=data.table(route_dft)[,.(leg,lon,lat,km=sum(km,na.rm=TRUE),minutes=sum(minutes,na.rm=TRUE)),by=route]
#        rotas=filter(full,lon==route_dft$lon[1], lat==route_dft$lat[1]) 
        index=which(is.na(full$leg)==TRUE)-1
        index=index[index<=dim(full)[1]]
        rotas=full[index,]
 #       print("cache")
        route_df=rotas[which.min(rotas$km),]
        cache=c(cache,address)
#        print("dfr")
   #     adfr=data.table(t(lapply(select(route_df,-route),as.double)))
   #     dfr=data.table(t(lapply(as.matrix(adfr),sum,na.rm=TRUE)))
   #     colnames(dfr)=colnames(adfr)
        dfr=route_df
        dfr$addr=address
    #    dfr$lat=route_df$lat[dim(route_df)[1]]
    #    dfr$lon=route_df$lon[dim(route_df)[1]]
 #       dfr$leg=route_df$leg[dim(route_df)[1]-1]
#        print(c("dim",dim(dfr)))
        routecache=rbindlist(list(routecache,dfr))
        mapa=c(mapa,dim(routecache)[1])
}

}
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
selecao=select(tfinal,A_Conferir,Data_Pedido=DT_HO_ABRE,Entregador,Pesquisa_Google=addr,km,Razao,TelCli,Tempo_Estimado_Minutos=minutes,Valor_Total_do_Pedido=VLR_TOTAL,
               DINHEIRO,	VALE_REF,			CARTAO,		OUTROS,	Desconto=VLR_DESC_TOT,	Taxa_de_Entrega=VLR_SERV,
               Canal=NomeArquivoWeb,TipoLogradouro,	Endereco,	NumRua	,ComplEnd,Obs, E_Mail,	Bairro,	Cidade,	Estado,	Cep,
                Tempo_Preparo,Tempo_Entrega,lat,lon,HrSaida,HrVolta)
stat=FALSE
sprint=selecao
sprint$Data_Pedido=as.Date(sprint$Data_Pedido,format("%d/%m/%Y"))
setorder(sprint,-Data_Pedido,-km)
maxdate=max(sprint$Data_Pedido)
today=as.character(maxdate,format="%d.%m.%Y")
print(c("Max-date",today))
oldopt=options()
options(xlsx.date.format="d/m/yyyy")
write.xlsx(tfinal,paste0("distancia-min-pedidos-completo-",today,".xlsx"),append=stat)
write.xlsx(sprint,paste0("distancia-min-pedidos-resumido-",today,".xlsx"),append=stat)
write.xlsx(sprint,"distancia-min-pedidos-resumido.xlsx",append=stat)
write.xlsx(routecache,paste0("distancia-min-enderecos-",today,".xlsx"),append=stat)
write.xlsx(routecache,"distancia-min-enderecos.xlsx",append=stat)
options(oldopt)
#
#qmplot(lon, lat, data = tfinal,geom = c("point"), size = as.double(VLR_TOTAL),alpha=0)+  geom_density2d(data = tfinal, aes(x = lon, y = lat)) + stat_density2d(data = tfinal, aes(x = lon, y = lat,  fill = ..level.., alpha=sqrt(..level..)/3 ), geom = 'polygon',alpha=0.2)+scale_fill_gradient(low = "white", high = "red")  +geom_jitter(data=tfinal, aes(x = lon, y = lat),alpha=min(as.double(km)/20,1),color="green")
#final=select(motocli,Entregador,Endereco=enderecos,Cep,Cliente,TelCli)
#qmap('São Paulo, São Paulo', zoom = 16) +
 #       geom_path(
  #              aes(x = lon, y = lat),  colour = 'red', size = 1.5,
   #             data = route_df, lineend = 'round')
