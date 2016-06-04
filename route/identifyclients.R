rm(semcadastro)
semcadastro=mm
#[mm$isweb]
#select only those that can be found in clientes database
maptel=intersect(semcadastro$TelCli ,clientes$Telefone)
mapreduce=ldply(maptel,function(tel){
        #order match
        if(tel == "970198617"){
                print(tel)
        }
        smap=unique(semcadastro[which(semcadastro$TelCli==tel)]$cod_cli)
        #client list match
cmap=unique(clientes[which(clientes$Telefone==tel)]$cod_cli)
if("995" %in% union(cmap,smap)){
        print("ok")
} 
        smap=setdiff(smap,cmap)
        mapdt=c()
        if(length(cmap)>0 & length(smap)>0){
                base=cmap[1] # select the first as reference
                maps=setdiff(union(smap,cmap),base) # map those orders which match the telefone (ignoring cod_ctrl?)
                if(length(maps)>0)
                        mapdt=data.table(fonte=maps,target=base)
        }
        return(mapdt)
})
mapreduce=unique(mapreduce)
#select those that are not inside the maped subset
nomapping=!(semcadastro$cod_cli %in% mapreduce$fonte)
mapname=unique(semcadastro[nomapping]$Cliente)

mapnamereduce=ldply(mapname,function(nome){
        nome=toupper(nome)
        smap=unique(semcadastro[nomapping][which(toupper(semcadastro[nomapping]$Cliente)==nome)]$cod_cli)
        cmap=unique(clientes[which(toupper(clientes$Razao)==nome)]$cod_cli)
        mapdt=c()
        if(length(cmap)>0){
                if("995" %in% union(cmap,smap)){
                        print("ok")
                } 
                #check if target is in the present mapping
                targets=union(cmap,smap)
                p=(targets %in% mapreduce$target)
                ispresent=sum(p)
                if(ispresent > 0){
                        base=targets[p]
                }
                else{
                        #check if source is in the present mapping
                        p=(targets %in% mapreduce$fonte)
                        if(sum(p)> 0){
                                base=filter(mapreduce,fonte==targets[p])$target
                        }else{
                                base=cmap[1]
                        }
                }
                maps=targets[targets!=base]
                if(length(maps) > 1) 
                        mapdt=data.table(fonte=maps,target=base)
        }
        ret=mapdt
        return(ret)
})
#select those that are not inside the maped subset
#nonamemapping=!(semcadastro[nomapping]$cod_cli %in% mapnamereduce$fonte)



mapcode=unique(data.table(rbindlist(list(mapreduce,mapnamereduce))))
mapcode=mapcode[which(fonte != target)]
setkey(mapcode,fonte)

