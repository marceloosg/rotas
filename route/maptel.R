maptel=unique(clientes$Telefone)
telmapping=ldply(maptel,function(tel){
        dup=which(clientes$Telefone==tel)
        is.na()
        first=dup[1]
        rest=dup[-1]
        if(length(rest) > 0){
                data.table(fonte=clientes[dup[1]]$cod_cli, target=clientes[rest]$cod_cli)
        }
})