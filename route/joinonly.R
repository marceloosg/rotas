library(data.table)
library(plyr)
library(dplyr)
library(digest)
library(xlsx)
changedate="2016-05-01"
motos=motoboy[motoboy$Entregador!="" ,]
motos=motos[motos$FLAG_CANC=="*" ,]
motos=motos[motos$Entregador!="CLIENTE RETIRA" ,]
motos=motos[grep("TESTE",toupper(motos$Cliente),invert=TRUE),]
motos$TelCli=gsub("(^0|^)11","",motos$TelCli)
moto=motos
changedate
clientes=cliente
clientes=filter(clientes,Estado=="SP" & Cep!="0000-000")
clientes$Telefone=gsub("(^0|^)11","",clientes$Telefone)
moto=moto[as.Date(moto$DT_HO_ABRE,format("%d/%m/%Y")) >= as.Date(changedate),]
m=merge(moto,clientes,by.x="COD_CTRL",by.y="cod_ctrl",all.x = TRUE)
m=unique(m)
m$DT_HO_ABRE=(as.POSIXct(m$DT_HO_ABRE,format="%d/%m/%Y %H:%M:%S"))
mm=m
isweb=is.na(mm$cod_cli)
sisweb=which(isweb=="TRUE")
mm$isweb=isweb
endereco=paste(m$TipoLogradouro,m$Endereco ,m$NumRua,",",m$Bairro,
               ",",m$Cidade,", São Paulo, Brasil")
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
index=which(mm$cod_cli %in% mapcode$fonte)
#moto$cod_cli=mm$cod_cli
nindex=mapcode[mm[index]$cod_cli]$target
mm[index]$cod_cli=nindex
moto$cod_cli=mm$cod_cli
mmm=merge(moto,clientes,by.x="cod_cli",by.y="cod_cli",all.x = TRUE)
motocli=mmm
setkey(motocli,COD_CTRL,
      cod_cli)
motocli=unique(motocli)
nocli=which(is.na(as.integer(motocli$cod_cli)))
if(length(nocli) > 0){
        write.xlsx(select(motocli[nocli,],COD_CTRL,DT_HO_ABRE,VLR_TOTAL,TelCli,Cliente),"naocadastrados.xlsx")
        motocli=motocli[!nocli]
}

