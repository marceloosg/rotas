day=$(shell date|cut -d ' ' -f 3)

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

data/motoboy.csv: frans006.frm frans006.MYI frans006.MYD script.motoboy
	
	/usr/bin/mysql -u root -psuperdb Eclt-$(day) < script.motoboy > data/amotoboy.csv 
	diff -q data/amotoboy.csv  data/motoboy.csv && mv data/amotoboy.csv data/motoboy.csv
data/entidades.csv: entidades.frm entidades.MYI entidades.MYD script.entidades
	/usr/bin/mysql -u root -psuperdb Eclt-$(day) < script.entidades > data/aentidades.csv
	diff -q data/aentidades.csv  data/entidades.csv && mv data/aentidades.csv data/entidades.csv
motoboy.csv: data/motoboy.csv
	./makedata motoboy

entidades.csv: data/entidades.csv
	./makedata entidades

distancia-min-pedidos-resumido.xlsx: motoboy.csv entidades.csv
	(./makeroute.R &&	echo . |mail -s "planilha $(shell date)" -a $@ "<verdure.saladas@gmail.com>")|| echo .|sendmail -s "erro on planilha $(date)" marcelo

