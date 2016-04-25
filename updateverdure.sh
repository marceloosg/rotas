cd /home/marcelo/deia/verdure/motoboy/autos
status=$(make|grep Nada )
test -z "$status" && sendmail -s "erro na planilha" marcelo
