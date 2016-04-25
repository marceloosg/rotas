library(data.table)
motoboy=data.table(read.csv("motoboy.utf8.csv",sep=";",colClasses ="character"))
cliente=data.table(read.csv("entidades.utf8.csv",sep=";",colClasses ="character"))
#pedidos=data.table(read.csv("detpagto.csv",sep=";",colClasses ="character"))

#ft=c();for(i in 1:(dim(pedidos)[1]-50 )){
#        a=table(wday(
#                as.POSIXct(pedidos$DataHora[i:(i+50)],format="%d/%m/%Y %H:%M:%S")
#        ))
        
#        tt=data.table(t(as.vector(a)))
#        colnames(tt)=rownames(a)
#                ft=rbindlist(list(ft,tt),fill = TRUE,use.names = TRUE )
#}                         
#library(RODBC)
#myconn <-odbcConnect("Eclt", uid="root", pwd="superdb")
#motoboy$VLR_TOTAL
