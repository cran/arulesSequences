
library("arulesSequences")

### ceeboo 2008

data(zaki)

z <- as(zaki, "timedsequences")
as(as(z, "sequences"), "data.frame")

s <- similarity(z)
s
all(s == similarity(z, z))

as(s, "dist")

similarity(z, strict = TRUE) - s

similarity(z, method = "dice") - s
similarity(z, method = "cosine") - s

similarity(z, method = "subset", strict = TRUE)
similarity(z, method = "subset")

is.subset(z)
is.subset(z, proper = TRUE)

###
