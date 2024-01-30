
global basedirectory "G:\My Drive\Work\Papers\PCIV code\"
cd "$basedirectory"
use temp_all.dta, clear
 
pciv logvolume tm1-tm359 dat (logprice = logtax), cluster(statefip) first rf 

pciv logvolume tm1-tm359 dat (logprice = logtax), cluster(statefip) first rf wt(wt)


