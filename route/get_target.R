monta=function(a){
        comp=filter(ldply(a$address_components,function(b){data.frame(b)}),long_name=="São Paulo")
        if(dim(comp)[1] >= 6)
                data.frame(addr=a$formatted_address,a$geometry$location,place=a$place_id,precisao=a$geometry$location_type)
}

getx=function(addr){
        x=geocode(addr,output = "all")
        tgt=ldply(x$results,monta)
}

viewaddr=function(tgt,br){
        map=get_map(location=c(br[1]$lng,br[1]$lat),zoom=15,maptype="roadmap")
        ggmap(map)+geom_point(data=merge(x=br$lng,y=br$lat),aes(x=x,y=y))+geom_point(data=tgt,aes(x=lon,y=lat))
}