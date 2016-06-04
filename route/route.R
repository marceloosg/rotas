library(ggmap)
library(data.table)
library(xlsx)
library(dplyr)
source("errg.R")
source("validate_address.R")

today=Sys.Date()
prefix="xlsx"
rota=c()
kms=c()
cache=c()
routecache=c()
compadd=c()
key=as.character(unlist(read.table("key")[1]))
fullceps=lapply(1:dim(motocli)[1],function(i){
       with(motocli[i],paste(sub("Rua", "R",TipoLogradouro),".",Endereco,
                            ",",NumRua,
                            ", São Paulo - SP,Brasil"))})
printreport=T
if(file.exists(paste0(prefix,"/distancia-min-enderecos-cep.xlsx")))
{
        rota=select(data.table(read.xlsx(paste0(prefix,"/distancia-min-enderecos-cep.xlsx"),2)),-id)
        kms=c(rota$km)
        cache=c(as.character(rota$addr))
        compadd=rota$Endereco_Considerado
        routecache=rota
        printreport=sum(rota$Conferir=="Sim",rota$Conferir=="Não")==0
} else
{
        print("No route cache found. Calculating from scratch.")
}
i=0
from1 <- 'Dr Clovis de Oliveira,339, São Paulo,São Paulo,Brazil'
from2 <- 'Rua Tapinas,85,Itaim Bibi,São Paulo,São Paulo,Brazil'
from=from1
mapa=c()
novo=0
t=0
print(paste("from:",from))
for(address in fullceps){
        source("mainroute.R")
}
print("endloop")
source("prepare.R")

if(printreport){
        options(xlsx.date.format="d/m/yyyy")
        output=paste0(prefix,"/distancia-min-pedidos-resumido-",today,".xlsx")
        write.xlsx(tfinal,output)
        write.xlsx(sprint,paste0(prefix,"/distancia-min-pedidos-resumido.xlsx"))
        output=paste0(prefix,"/distancia-min-enderecos-",today,".xlsx")
} else
{
        write.xlsx(routecache,output)
        file.copy(output,paste0(prefix,"/distancia-min-enderecos-cep.xlsx"),overwrite=TRUE)
        write.csv(data.table(data=today,from=from),paste0(prefix,"uptodate"))
}
