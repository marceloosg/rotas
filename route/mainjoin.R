library(digest)
motos=motoboy[motoboy$Entregador!="" ,]
motos=motos[motos$FLAG_CANC=="*" ,]
motos=motos[motos$Entregador!="CLIENTE RETIRA" ,]
motos=motos[grep("TESTE",toupper(motos$Cliente),invert=TRUE),]
motos$TelCli=gsub("(^0|^)11","",motos$TelCli)
moto=motos
clientes=cliente
clientes=filter(clientes,Estado=="SP"  & Cep!="0000-000")
clientes$Telefone=gsub("(^0|^)11","",clientes$Telefone)
moto=moto[as.Date(moto$DT_HO_ABRE,format("%d/%m/%Y")) >= as.Date(changedate),]
motocli=merge(moto,clientes,by.x="COD_CTRL",by.y="cod_ctrl",all.x = TRUE)
mato=motos[as.Date(motos$DT_HO_ABRE,format("%d/%m/%Y")) >= as.Date(startdate),]
mato=mato[as.Date(mato$DT_HO_ABRE,format("%d/%m/%Y")) < as.Date(enddate),]
m=merge(mato,clientes,by.x="COD_CTRL",by.y="cod_ctrl",all.x = TRUE)
m=unique(m)
m$DT_HO_ABRE=(as.POSIXct(m$DT_HO_ABRE,format="%d/%m/%Y %H:%M:%S"))
enderecos=paste(motocli$TipoLogradouro,motocli$Endereco ,motocli$NumRua,",",motocli$Bairro,
                ",",motocli$Cidade,", São Paulo, Brasil")
endereco=paste(m$TipoLogradouro,m$Endereco ,m$NumRua,",",m$Bairro,
               ",",m$Cidade,", São Paulo, Brasil")
mm=m
isweb=is.na(mm$cod_cli)
sisweb=which(isweb=="TRUE")
mm$isweb=isweb
mm[isweb,]$cod_cli=unlist(lapply(paste(mm[isweb]$Cliente,endereco[isweb]),digest))
mm$canal="Telefone"
isifood=grep("IFOOD",mm$NomeArquivoWeb)
iseclet=grep("ECLT",mm$NomeArquivoWeb)
mm[iseclet,]$canal="PedidosViaWeb"
mm[isifood,]$canal="IFOOD"
iseclet=intersect(iseclet,sisweb)
isifood=intersect(isifood,sisweb)
offline=setdiff(1:dim(mm)[1],iseclet)
offline=setdiff(offline,isifood)


source("identifyclients.R")
setkey(clientes,cod_cli)
#clientes=filter(clientes,!(cod_cli %in% unique(mapcode$fonte)))
index=which(mm$cod_cli %in% mapcode$fonte)
mmm=mm
mm[index]$cod_cli=mapcode[mm[index]$cod_cli]$target


source("makesum.R")
rawcli=unique(select(clientes,-cod_ctrl))
sd=as.character(as.Date(startdate),format("%d.%m.%y"))
ed=as.character(as.Date(enddate),format("%d.%m.%y"))
sumcli=makesum(mm,paste(sd,ed,sep="-"))
sumcli$StartDate=as.Date(startdate,format("%Y-%m-%d"))
sumcli$ChangeDate=as.Date(changedate,format("%Y-%m-%d"))
sumcli$EndDate=as.Date(enddate,format("%Y-%m-%d"))
print(sumcli)
#sumcliweb=makesum(mm[iseclet,],"web-ecletica")
#sumclifood=makesum(mm[isifood,],"web-ifood")
#sumclibase=makesum(mm[offline,],"ecletica")
#sumario=data.frame(rbindlist(list(sumcli,sumclifood,sumcliweb,sumclibase)))
#rownames(sumario)=c("Todos clientes","Clientes exclusivos IFOOD","Clientes exclusivos Pedidos Via Web","Clientes cadastrados Ecletica")
#        sumario
#
#bp=barplot(counts, main="Número de clientes", horiz=TRUE,
#        names.arg=cnames)





