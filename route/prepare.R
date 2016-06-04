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

source("motocost.R")
print("print")
tfinal$TaxaMotoexpress=motocost(as.numeric(as.character(tfinal$km)))
selecao=select(tfinal,
               Data=DT_HO_ABRE,
               Endereço=addr,
               km,
               TaxaMotoexpress,
               Entregador)
stat=FALSE
sprint=selecao
sprint$Data=as.Date(sprint$Data,format("%d/%m/%Y"))
setorder(sprint,-Data,-km)
maxdate=max(sprint$Data)
today=as.character(maxdate,format="%d.%m.%Y")
print(c("Max-date",today))