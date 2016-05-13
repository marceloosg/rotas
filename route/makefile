day=$(shell  date|sed "s/  */ /g"|cut -d ' ' -f 3)
update=/usr/bin/updatedata.sh
path="~/Dropbox"
cpvend=table/cpvend.frm table/cpvend.MYI table/cpvend.MYD

all:mailed

table/%.frm:	~/Dropbox/backup/Eclt/%.frm $(update)
	sudo $(update) $@ && touch $@

table/%.MYD:	~/Dropbox/backup/Eclt/%.MYD $(update)
	sudo $(update) $@ && touch $@

table/%.MYI:	~/Dropbox/backup/Eclt/%.MYI $(update)
	sudo $(update) $@ && touch $@


data/cpvend.csv: $(cpvend)	scripts/script.cpvend
	/usr/bin/mysql -u root -psuperdb Eclt$(day) < scripts/script.cpvend > data/acpvend.csv 
	diff -q data/acpvend.csv  data/cpvend.csv || mv data/acpvend.csv data/cpvend.csv

data/motoboy.csv: table/frans006.frm table/frans006.MYI table/frans006.MYD scripts/script.motoboy
	/usr/bin/mysql -u root -psuperdb Eclt$(day) < scripts/script.motoboy > data/amotoboy.csv 
	diff -q data/amotoboy.csv  data/motoboy.csv || mv data/amotoboy.csv data/motoboy.csv

data/entidades.csv: table/det_pagto.frm table/det_pagto.MYI table/det_pagto.MYD table/entidades.frm table/entidades.MYI table/entidades.MYD scripts/script.clientes
	/usr/bin/mysql -u root -psuperdb Eclt$(day) < scripts/script.clientes > data/aentidades.csv
	diff -q data/aentidades.csv  data/entidades.csv || mv data/aentidades.csv data/entidades.csv

cpvend.csv:  data/cpvend.csv scripts/makedata
	scripts/makedata cpvend

motoboy.csv: data/motoboy.csv scripts/makedata
	scripts/makedata motoboy

entidades.csv: data/entidades.csv scripts/makedata
	scripts/makedata entidades

xlsx/distancia-min-pedidos-resumido.xlsx	uptodate route.R: motoboy.csv entidades.csv
	./makeroute.R 1>log  2>erro || printf "Subject: Erro na planilha:\n$(cat erro)."|sendmail marcelo


mailed:	xlsx/distancia-min-pedidos-resumido.xlsx	uptodate
	echo .| mail -s "Planilha de rotas,$(shell cat uptodate|tail -n 1|cut -d ',' -f 2|sed 's/^.//;s/.$$//') " -a $< "<verdure.saladas@gmail.com>"
