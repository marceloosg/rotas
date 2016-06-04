numerodecompras=data.table(table(motocli$cod_cli))
colnames(numerodecompras)=c("codigo","Numero_de_Compras")
i=0
cadastro=ldply(numerodecompras$codigo,function(alvo){
  
        codes=filter(mapcode,target==alvo)
      #  print(c(i,alvo))
        if(dim(codes)[1] > 0){
                targets=union(codes$target,codes$fonte)
                if(length(targets) > 1){
                        print(targets)
                }
        }else{
                targets=alvo
        }
        mydata=clientes[targets]
        setorder(mydata,Data_Cadastro)
        telefones=paste(unique(mydata$Telefone),collapse=",")
        datas=mydata$Data_Cadastro
        datas[is.na(datas)]=Sys.Date()
        data=min(datas)
        emails=mydata$E_Mail
        email=paste(unique(emails[!is.na(emails)],collapse=","))
        data.table(Data_Cadastro=data,
                   select(cli[alvo,],Razao),
                   Telefones=telefones,
                   Emails=email)
})
print("done with cadastro")
compras=data.table(codigo=numerodecompras$codigo,
                   cadastro,
                   Numero_de_compras=numerodecompras$Numero_de_Compras)
setorder(compras,Data_Cadastro)

sumario=motocli[  , .(Valor_Total=sum(as.numeric(VLR_TOTAL)),
                      Valor_Medio=mean(as.numeric(VLR_TOTAL))),
                  by=cod_cli]
freq=ldply(compras$codigo,function(index){
        curcli=filter(motocli,cod_cli==index)
        setorder(curcli,data_abre)
        tam=dim(curcli)[1]
        daydiff=as.integer(curcli$data_abre[2:tam]-curcli$data_abre[1:(tam-1)])
        data.table(cod_cli=index,frequencia_compra=mean(daydiff))
#        print(meandaydiff)
}
)

today=Sys.Date()
mc30=motocli[today-as.Date(moto$DT_HO_ABRE,format("%d/%m/%Y")) < 31,]

freq30=ldply(unique(mc30$cod_cli),function(index){
        curcli=filter(mc30,cod_cli==index)
        setorder(curcli,data_abre)
        tam=dim(curcli)[1]
        daydiff=as.integer(curcli$data_abre[2:tam]-curcli$data_abre[1:(tam-1)])
        data.table(cod_cli=index,frequencia_compra_30=mean(daydiff))
#        print(meandaydiff)
}
)
sumario30=mc30[ cod_cli %in% compras$codigo ,
                .(Numero_de_Compras_30_dias=.N,
                  Valor_Total_30_dias=sum(as.numeric(VLR_TOTAL)),
                  Valor_Medio_30_dias=mean(as.numeric(VLR_TOTAL))),
                by=cod_cli]

analisecompras=merge(compras,sumario,by.y="cod_cli",by.x="codigo",all=T)
analisecompras=merge(analisecompras,freq,by.y="cod_cli",by.x="codigo",all=T)

analisecompras=merge(analisecompras,sumario30,by.y="cod_cli",by.x="codigo",all=T)
analisecompras=merge(analisecompras,freq30,by.y="cod_cli",by.x="codigo",all=T)

setorder(analisecompras,Data_Cadastro)

options(xlsx.date.format="d/m/yyyy")
write.xlsx(analisecompras,
           paste0("analise_compras.",as.character(today,format="%d.%m.%Y"),".xlsx"),
           showNA=F,row.names=F)

#h=hist(as.integer(clientes$Data_Cadastro)-16923,breaks=30,xlim=c(-350,10),freq = F)