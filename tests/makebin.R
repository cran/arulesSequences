
### ceeboo 2008

library(arulesSequences)

data(zaki)

arulesSequences:::makebin(zaki, "zaki")
arulesSequences:::write_cspade(zaki, "zaki.asc")

exe <- system.file("exec", "makebin", package = "arulesSequences")
system(paste(exe, "zaki.asc makebin.data"))

system("cmp zaki.data makebin.data")

exe <- system.file("exec", "getconf", package = "arulesSequences")
system(paste(exe, "-i makebin -o makebin"))

system("cmp zaki.conf makebin.conf")

###
