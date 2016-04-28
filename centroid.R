library(ggmap)
library(xlsx)
library(data.table)
library(dplyr)
pedidos=read.xlsx("auto/distancia-min-pedidos-resumido.xlsx",1)
lat=0
lon=0
n=0
fat=0
latm=0
lonm=0
fatpx=data.table(x=pedidos$lon,y=pedidos$lat)
h=hclust(dist(fatpx))
index=as.factor(cutree(h,k=3))
table(index)
pedidos$index=index
ped=data.table(select(pedidos,lat,lon))
pedidos$isdup=duplicated(ped)
ped$isdup=duplicated(ped)
#ped=filter(ped,isdup==TRUE)
p=data.table(pedidos)
plat=0
plon=0
platm=0
plonm=0
pfat=0
for(i in 1:dim(pedidos)[1]){
     n=n+1
     lat=lat+pedidos$lat[i]
     lon=lon+pedidos$lon[i]
     diff=as.double(pedidos$Valor_Total_do_Pedido[i])-as.double(pedidos$Taxa_de_Entrega[i])
     latm=latm+pedidos$lat[i]*diff
     lonm=lonm+pedidos$lon[i]*diff
     fat=fat+diff
     print(c(lon,lat)/n)
     if(ped[i]$isdup){
             plat=plat+ped[i]$lat
             plon=plon+ped[i]$lon
             platm=platm+ped[i]$lat*diff
             plonm=plonm+ped[i]$lon*diff
             pfat=pfat+diff
     }
}
lat=lat/n
lon=lon/n
latm=latm/fat
lonm=lonm/fat
pn=sum(ped$isdup)
plat=plat/pn
plon=plon/pn
platm=platm/pfat
plonm=plonm/pfat
print(c(lat,lon))
code=geocode("Dr Clovis de Oliveira,339, São Paulo,São Paulo,Brazil")
library(data.table)
cen=data.table(lat=lat,lon=lon)
fatp=data.table(lat=latm,lon=lonm)
pf=data.table(lat=plat,lon=plon)
pfm=data.table(lat=platm,lon=plonm)
cent=p[,.(lon=mean(lon),lat=mean(lat)),by=index]

#map=get_map(location=code,maptype="roadmap",zoom=12)
#g=ggmap(map)+geom_point(data=code,aes(x=lon,y=lat))+
 #       geom_point(data=cen,aes(x=lon,y=lat),colour="red")+
  #      geom_point(data=pedidos,aes(x=lon,y=lat,pch=index,colour=index),size=5,alpha=0.5)+
   #     geom_point(data=fatp,aes(x=lon,y=lat),pch=19,colour="purple",size=5,alpha=0.5)+
    #    geom_point(data=pf,aes(x=lon,y=lat),pch=17,colour="cyan",size=5,alpha=0.5)+
     #   geom_point(data=pfm,aes(x=lon,y=lat),pch=18,colour="brown",size=5,alpha=0.5)+
      #  geom_point(data=filter(ped,isdup==TRUE),aes(x=lon,y=lat),pch=3,size=5,colour="yellow",alpha=0.5)+
       # geom_point(data=cent,aes(x=lon,y=lat,pch=index),colour="black",size=10,alpha=0.5)
write.xlsx(table(index),"grupos.xlsx")
addr=c()
for(i in 1:3){addr=c(addr,revgeocode(c(cent[i,]$lon,cent[i,]$lat)))
}
cent$addr=addr
gp=p[,.(valor=sum(as.double(Valor_Total_do_Pedido)-as.double(Taxa_de_Entrega))),by=index]
gp$addr=addr
gpm=data.table(filter(pedidos,isdup==TRUE))
gpm=gpm[,.(valor=sum(as.double(Valor_Total_do_Pedido)-as.double(Taxa_de_Entrega))),by=index]
setorder(gpm,index)
centgp=merge(cent,gp,by="index")
write.xlsx(centgp,"grupos-faturamento.xlsx")
write.xlsx(gpm,"grupos-frequente-faturamento-.xlsx")
map=get_map(location=c(pf$lon,pf$lat),maptype="roadmap",zoom=13)
#ggmap(map)+geom_point(data=code,aes(x=lon,y=lat),size=5,colour="white")+geom_point(data=pf,aes(x=lon,y=lat),size=50,alpha=0.5,colour=rgb(0,1,0))+geom_point(data=pf,aes(x=lon,y=lat),size=100,alpha=0.3,colour=rgb(0,1,0))+geom_point(data=pf,aes(x=lon,y=lat),size=150,alpha=0.3,colour=rgb(1,0,0))+geom_point(data=pedidos,aes(x=lon,y=lat),size=2,alpha=0.2,color="blue")
ggmap(map)+
        geom_point(data=pf,aes(x=lon,y=lat),size=100 ,alpha=0.6,colour=rgb(0,0,1))+
        geom_point(data=pf,aes(x=lon,y=lat),size=200,alpha=0.3,colour=rgb(0,0.2,1))+
        geom_point(data=pf,aes(x=lon,y=lat),size=300,alpha=0.3,colour=rgb(0.2,0,1))+
        geom_point(data=pedidos,aes(x=lon,y=lat),size=10 ,alpha=0.6,colour=rgb(1,1,1))
#map=get_map(location=c(lon=lon,lat=lat),maptype="roadmap",zoom=13)
#code=geocode("Dr Clovis de Oliveira,339, São Paulo,São Paulo,Brazil")
#ggmap(map)+geom_point(aes(x=lon,y=lat),color="red",pch=1,size=4)
