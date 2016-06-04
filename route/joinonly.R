library(data.table)
library(plyr)
library(dplyr)
library(digest)
library(xlsx)
changedate="2016-01-25"
enddate="2016-06-30"
source("preId.R")

source("identifyclients.R")

setkey(clientes,cod_cli)
index=which(mm$cod_cli %in% mapcode$fonte)
#moto$cod_cli=mm$cod_cli
nindex=mapcode[mm[index]$cod_cli]$target
print(c(length(index),length(nindex)))
mm[index]$cod_cli=nindex
setkey(moto,COD_CTRL)
moto=moto[mm$COD_CTRL]
moto=data.table(moto,select(mm,cod_cli,data_abre,data_fecha,hora_abre,hora_fecha))
cli=unique(clientes[!(cod_cli %in% mapcode$fonte),])

mmm=merge(moto,cli,by.x="cod_cli",by.y="cod_cli",all.x = TRUE)
motocli=mmm
setkey(motocli,COD_CTRL,
      cod_cli)
motocli=unique(motocli)
nocli=which(is.na(as.integer(motocli$cod_cli)))
if(length(nocli) > 0){
        write.xlsx(dplyr::select(motocli[nocli,],COD_CTRL,DT_HO_ABRE,VLR_TOTAL,TelCli,Cliente),"naocadastrados.xlsx")
        mc=motocli[nocli]
        motocli=motocli[!nocli]
}

#cli=as.integer(factor(a$cod_cli))  mc$cli=gsub("[a-z]","",mc$cod_cli)

#datas=as.integer(as.Date(a$DT_HO_ABRE,format="%d/%m/%Y %H:%M:%S"))
