motos=motoboy[motoboy$Entregador!="" ,]
motos=motos[motos$FLAG_CANC=="*" ,]
#motos=motos[motos$VLR_ITEM_CANC=="0",]
motos=motos[motos$Entregador!="CLIENTE RETIRA" ,]
motos=motos[grep("TESTE",toupper(motos$Cliente),invert=TRUE),]
motos$TelCli=gsub("(^0|^)11","",motos$TelCli)
rm(moto)
moto=motos
rm(clientes)
clientes=cliente
clientes=filter(clientes,Estado=="SP" & Cep!="0000-000")
clientes$Telefone=gsub("(^0|^)11","",clientes$Telefone)
clientes$Data_Cadastro=as.Date(clientes$Data_Cadastro,format("%d/%m/%Y"))
        
moto=moto[as.Date(moto$DT_HO_ABRE,format("%d/%m/%Y")) >= as.Date(changedate),]
moto=moto[as.Date(moto$DT_HO_ABRE,format("%d/%m/%Y")) < as.Date(enddate),]
NumeroDePedidos=dim(moto)[1]
stopifnot(NumeroDePedidos > 0)
m=merge(moto,clientes,by.x="COD_CTRL",by.y="cod_ctrl",all.x = TRUE)
m=unique(m)
m$DT_HO_ABRE=(as.POSIXct(m$DT_HO_ABRE,format="%d/%m/%Y %H:%M:%S"))
m$DT_HO_FECHA=(as.POSIXct(m$DT_HO_FECHA,format="%d/%m/%Y %H:%M:%S"))

mm=m
isweb=is.na(mm$cod_cli)
sisweb=which(isweb=="TRUE")
mm$isweb=isweb
endereco=paste(m$TipoLogradouro,m$Endereco ,m$NumRua,",",m$Bairro,
               ",",m$Cidade,", São Paulo, Brasil")
mm[isweb,]$cod_cli=unlist(lapply(toupper(paste(mm[isweb]$Cliente,endereco[isweb])),digest))
mm$canal="Telefone"
isifood=grep("IFOOD",mm$NomeArquivoWeb)
iseclet=grep("ECLT",mm$NomeArquivoWeb)
mm[iseclet,]$canal="PedidosViaWeb"
mm[isifood,]$canal="IFOOD"
iseclet=intersect(iseclet,sisweb)
isifood=intersect(isifood,sisweb)
offline=setdiff(1:dim(mm)[1],iseclet)
offline=setdiff(offline,isifood)
mm$data_abre=as.Date(mm$DT_HO_ABRE,format="%d/%m/%Y")
mm$hora_abre=as.character.Date(mm$DT_HO_ABRE,format="%H:%M:%S")
mm$data_fecha=as.Date(mm$DT_HO_FECHA,format="%d/%m/%Y")
mm$hora_fecha=as.character.Date(mm$DT_HO_FECHA,format="%H:%M:%S")