library(data.table)
library(plyr)
library(dplyr)
library(xlsx)
#filtro de pedidos - eliminando retiradas e pedidos testes
startdate="2016-02-01"
changedate="2016-03-01"
enddate="2016-04-01"
source("mainjoin.R")
sumclia=sumcli
startdate="2016-03-01"
changedate="2016-04-01"
enddate="2016-05-01"
source("mainjoin.R")
sumclib=sumcli
startdate="2015-01-01"
changedate="2016-04-01"
enddate="2016-06-01"
source("mainjoin.R")
sumclic=sumcli
sumario=rbindlist(list(sumclia,sumclib,sumclic))
name=("clientes/sumario.xlsx")
write.xlsx(sumcli,name)