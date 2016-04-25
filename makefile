all:distancia-min-pedidos-resumido.xlsx

%.frm:	~/Dropbox/backup/Eclt/%.frm 
	sudo updatedata.sh $@
	touch $@

%.MYD:	~/Dropbox/backup/Eclt/%.MYD
	sudo updatedata.sh $@
	touch $@

%.MYI:	~/Dropbox/backup/Eclt/%.MYI
	sudo updatedata.sh $@
	touch $@

data/motoboy.csv: frans006.frm frans006.MYI frans006.MYD
	/usr/bin/mysql -u root -psuperdb Eclt < script.motoboy > data/motoboy.csv 
	
data/entidades.csv: entidades.frm entidades.MYI entidades.MYD
	/usr/bin/mysql -u root -psuperdb Eclt < script.entidades > data/entidades.csv

motoboy.csv: data/motoboy.csv
	./makedata motoboy

entidades.csv: data/entidades.csv
	./makedata entidades

distancia-min-pedidos-resumido.xlsx: motoboy.csv entidades.csv
	(./makeroute.R &&	echo . |mail -s "planilha $(shell date)" -a $@ "<verdure.saladas@gmail.com>")|| echo .|mail -s "erro on planilha $(date)"

