file=$1
diff -q  /home/marcelo/Dropbox/backup/Eclt/$file /var/lib/mysql/Eclt/$file 	
status=$?
if [ "$status" = 1 ]
then
day=$(date|cut -d ' ' -f 3)
test ! -e /var/lib/mysql/Eclt-$day && mkdir /var/lib/mysql/Eclt-$day
rsync -avz --progress /home/marcelo/Dropbox/backup/Eclt/$file /var/lib/mysql/Eclt-$day/$file
rsync -avz --progress /home/marcelo/Dropbox/backup/Eclt/$file /var/lib/mysql/Eclt/$file
exit 0
else
exit 1
fi
#sync && echo 3 | sudo tee /proc/sys/vm/drop_caches
