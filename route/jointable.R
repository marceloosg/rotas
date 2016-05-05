library(dplyr)
library(data.table)
motos=motoboy[motoboy$Entregador!="" ,]
motos=motos[motos$FLAG_CANC=="*" ,]
motos=motos[motos$Entregador!="CLIENTE RETIRA" ,]
moto=motos[grep("TESTE",toupper(motos$Cliente),invert=TRUE),]
clientes=cliente
clientes$Telefone=gsub("-| Telefone|^11","",(cliente$Telefone))
clientes$Telefone=as.double(clientes$Telefone)
clientes=filter(clientes,Cidade=="SAO PAULO",Estado=="SP",Cep!="0000-000")
clientes=unique(clientes,by="Telefone")
moto$TelCli=gsub("(^0|^)11","",motos$TelCli)
moto$TelCli=as.double(moto$TelCli)
telefones=setdiff(unique(moto$TelCli),clientes$Telefone)
moto$A_Conferir="Não"
aconferir=c()
for(tel in telefones){
        tags=unique(moto[TelCli==tel,]$Cliente)
        
        for(tag in tags){
	conferir=FALSE
                select.ind=which(moto$TelCli==tel & moto$Cliente== tag)
        ldata=(grep(paste("(^| )",tag,sep=""), clientes$Razao))
                l=length(ldata)
                if(l == 0){
#                 print(c("warning",l,tel,unique(moto[TelCli==tel,]$DT_HO_ABRE)))
                        count=rep(0,dim(clientes)[1])
                        first=TRUE
                        value=4
                        for(word in unlist(strsplit(tag," "))){
                                ind=grep(paste0("(^| )",word),clientes$Razao)
                                if(length(ind) > 0){
                                        first=FALSE
                                        count[ind]=count[ind]+value
                                        if(value>1){
                                                value=value-1
                                        }
                                }
                                else{
                                        if(first){
                                                break
                                        }
                                }
                        }
                        mind=which.max(count)
                        maxcount=unique(which(count==count[mind]))
                        if(length(maxcount)==1 & max(count) > 0){
                                moto[select.ind]$Cliente=clientes[mind]$Razao                                
                        }
                        else{
				conferir=TRUE
			#	print(paste("Verificar cliente/pedido:",tag,"Codigo do pedido:[", moto[select.ind]$COD_CTRL,"]" ))
                           #     print(c("warning unable to verify client:",tag,"in",clientes[mind]$Razao,"max",max(count),"l",length(maxcount) ))
                        }
                }
                else{
                        if(l == 1){
                           #     print(c("found",clientes[ldata]$Razao))
                                moto[select.ind]$Cliente=clientes[ldata]$Razao
                        }
                        else{
                         	conferir=TRUE
				 #      print(c("warning unable to verify client:",tag,"in",ldata,"nomes:",clientes[ldata]$Razao))
                        }
                }
		if(conferir){
				print(paste("Verificar pedido:",tag,
					    "Codigo do pedido:[", moto[select.ind]$COD_CTRL,
				            "] Data:", moto[select.ind]$DT_HO_ABRE ))
				moto$A_Conferir[select.ind]="Sim"
				cdatas=moto[select.ind]$DT_HO_ABRE
				cctrl= moto[select.ind]$COD_CTRL
				ccliente=tag
				rec=data.table(Data=cdatas,COD_CTRL=cctrl,Cliente=tag)
				aconferir=rbindlist(list(aconferir,rec),use.names=TRUE,fill=TRUE)	
				
		}
       }
}
motocli=merge(moto,clientes,by.x="TelCli",by.y="Telefone",all=FALSE)
motocli=motocli[as.Date(motocli$DT_HO_ABRE,format("%d/%m/%Y")) >= as.Date("2016-04-01"),]
enderecos=paste(motocli$TipoLogradouro,motocli$Endereco ,motocli$NumRua,",",motocli$Bairro,
                ",",motocli$Cidade,", São Paulo, Brasil")
library(xlsx)
write.xlsx(aconferir,"../xlsx/aconferir.xlsx")
