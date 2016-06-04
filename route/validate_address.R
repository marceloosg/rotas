source("get_target.R")
checkaddress=function(index){

addr=fullceps[index][[1]]
if(is.na(motocli$Cep[i])){
region=motocli[index]$Bairro
}
else{
        region=motocli[index]$Cep
}
        
bairro=paste0(region,", São Paulo - SP, Brazil")
#addr=sub(motocli[index]$Bairro,"",addr)
tgt=getx(addr)
if(length(tgt)==0){
        u=unlist(strsplit(addr," "))
        addr=paste(u[grep("Y",u,invert =T)],collapse = " ")
        tgt=getx(addr)
        tries=0
        u=unlist(strsplit(addr,","))
        w=unlist(strsplit(u[1]," "))
        maxtries=length(w)
        while(length(tgt)==0 & tries< maxtries){
                tries=tries+1
                addr=paste(w[-tries],collapse = " ")
                tgt=getx(addr)
        }
}
y=geocode(bairro,output = "all")
a=y$results[[1]]
center=data.frame(addr=a$formatted_address,a$geometry$location,place=a$place_id)
abound=a$geometry$bounds
if(is.null(a$geometry$bounds)){
 abound=a$geometry$viewport
}

        bounds=ldply(abound,function(b){
               data.frame(b,place=a$place_id) 
        })
br=rbindlist(list(center,bounds))
if(length(tgt)==0){
        best=center
        best$sinal=FALSE
        best$dist=-1
        best$soma=0
        best$precisao="CENTRO DO BAIRRO"
        best
}
else{
dlm=br[2]$lat-tgt$lat
dlx=br[3]$lat-tgt$lat
dnm=br[2]$lng-tgt$lng
dnx=br[3]$lng-tgt$lng
tgt$sinal=dlm*dlx<0 & dnm*dnx < 0

tgt$dist=ldist(tgt,br[1,])
if(dim(tgt)[1] > 1 && tgt[1,]$dist > 1000){
        best=tgt[which.min(tgt$dist),]
}
else{
        best=tgt[1,]
}
best$soma=sum(tgt$sinal)
best$addr=as.character(best$addr)
best
}
}

ldist=function(a,b)
{
        m=(a$lat+b$lat)/2
        6300000*3.14159268/180*sqrt((a$lat-b$lat)^2*cos(m*3.14159268/180)^2+(a$lng-b$lng)^2)
}
