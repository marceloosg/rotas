makesum=function(mm,nome){
        clientes_antigos=(unique(mm[which(mm$DT_HO_ABRE < as.POSIXct(changedate,format="%Y-%m-%d"))]$cod_cli))
        clientes_abril=(unique(mm[which(mm$DT_HO_ABRE >= as.POSIXct(changedate,format="%Y-%m-%d"))]$cod_cli))
        novos_clientes=setdiff(clientes_abril,clientes_antigos)
        clientes_fieis=setdiff(clientes_abril,novos_clientes)
        clientes_perdidos=setdiff(clientes_antigos,clientes_abril)
        
        lan=length(clientes_antigos)
        lcf=length(clientes_fieis)
        lcp=length(clientes_perdidos)
        lnc=length(novos_clientes)
        lca=length(clientes_abril)
        tot=lca+lan
        print(nome)
        #print(c(lca+lan,"----------",
    #     print(      novos_clientes)
        counts=c(tot,lan,lcp,lnc,lcf,lca)
        sumcli=data.table(t(counts))
        cnames=c("Total", "Antigos","Perdidos","Novos", "Mantidos","Atual")
        colnames(sumcli)=cnames
        webcli=c()
        if(length(mm[isweb,])>0 ){
                webcli=setdiff(clientes_perdidos,mm[mm$isweb=="FALSE"]$cod_cli)
                
                raw=select(mm[isweb,],cod_cli,TelCli, canal,VLR_TOTAL,Cliente )  
                raw=raw[cod_cli %in% webcli,]
                raw$Razao=""
                raw$TelCli=gsub("(^0|^)11","",raw$TelCli)
                raw=unique(raw[cod_cli %in% webcli,])
                index=raw$cod_cli %in% clientes$cod_cli
                raw[index]$Razao=rawcli[raw[index]$cod_cli]$Razao
                raw$ultima_compra=unlist(lapply(raw$cod_cli,function(code){
                        as.character(max(mm[cod_cli==code,]$DT_HO_ABRE))
                        }))
                if(dim(raw)[1] >0){
                        write.xlsx(raw,paste0("clientes/clientes_perdidos_semcadastro_",nome,".xlsx"))
                }
                else
                {
                 #       print(c("Warning empty record",nome,length(webcli),length( clientes_perdidos)))
                        
                }
      }
        if(length(mm[!isweb,])>0){
                webcli=setdiff(clientes_perdidos,webcli)
                raw=rawcli[cod_cli %in% webcli,]
          
                raw$ultima_compra=unlist(lapply(raw$cod_cli,function(code){
                        as.character(max(mm[cod_cli==code,]$DT_HO_ABRE))
                }))
                raw$TotalGasto=unlist(lapply(raw$cod_cli,function(code){
                        sum(as.numeric(mm[cod_cli==code,]$VLR_TOTAL),na.rm = T)
                        }))
                raw$NumeroCompras=unlist(lapply(raw$cod_cli,function(code){
                        sum(mm$cod_cli==code,na.rm = T)
                }))
                if(dim(raw)[1] >0){
                        write.xlsx(raw,paste0("clientes/clientes_perdidos_cadastrados_",nome,".xlsx"))
                }
                leftover=setdiff(webcli,raw$cod_cli)
                if(length(leftover)>0)
                {
                        write.xlsx(data.table(cod_cli=leftover),paste0("clientes/clientes_bugados_",nome,".xlsx"))
                }
        }
        sumcli
}