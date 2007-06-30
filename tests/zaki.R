
library("arulesSequences")

## basic tests using the small running 
## example from the paper. 
##
## ceeboo 2007

## data set

data(zaki)

zaki.txt <- 
    read_baskets(con  = system.file("misc", "zaki.txt", 
                                    package = "arulesSequences"),
                 info = c("sequenceID","eventID","SIZE"))

all.equal(zaki, zaki.txt)

## methods of class sequences

s1 <- cspade(zaki, parameter = list(support = 0.4), 
                   control   = list(verbose =TRUE))
s1
s2 <- cspade(zaki, parameter = list(support = 0.4, maxsize = 2, maxlen = 2))
s2

nitems(s1)
nitems(s1, itemsets = TRUE)
nitems(s2)
nitems(s2, itemsets = TRUE)
labels(s1, setSep = "->", seqStart = "", seqEnd = "")
summary(s1)
inspect(s1)

data.frame(items  = itemLabels(s1), 
           counts = itemFrequency(s1))
data.frame(items  = itemLabels(s2),
           counts = itemFrequency(s2)) 
data.frame(itemsets = itemLabels(s2, itemsets = TRUE),
           counts   = itemFrequency(s2, itemsets = TRUE))

as(s2, "data.frame")

info(s2) <- info(s2)
info(s2)

itemInfo(s2) <- itemInfo(s2)
itemInfo(s2)

## fixme?
t <- itemTable(s2)
rownames(t) <- 
itemLabels(s2)[as.integer(rownames(t))]
t
t <- itemTable(s2, itemsets = TRUE)
rownames(t) <- 
itemLabels(s2, itemsets = TRUE)[as.integer(rownames(t))]
t


d1 <- as(s1, "data.frame")
d1$size    <- size(s1)
d1$length  <- size(s1, type = "length")
d1$ritems  <- ritems(s1, "max")
d1$maximal <- is.maximal(s1)
d1

as(s1@elements, "data.frame")

d1[s1 %in%  c("D", "F"), 1:2]
d1[s1 %ain% c("D", "F"), 1:2]
d1[s1 %pin% "D", 1:2]

as(subset(s1, x %ain% c("D", "F")), "data.frame")
as(subset(s1, support == 1), "data.frame")

match(s2,s1)
match(s1,s2)

# problem with new-style S4
# and rbind of data.frame()
s <- unique(c(s1,s2))           # uses duplicated
match(s1, s)
all.equal(s1, s)

## rules

r1 <- ruleInduction(s1, confidence = 0.5)
r1
r2 <- ruleInduction(s2, confidence = 0.5)
r2

labels(r1, itemSep = "->", setStart = "", setEnd = "")
summary(r1)
inspect(r1)

as(r2, "data.frame")

as(subset(r2, lhs(x) %in%  c("B", "F")), "data.frame")
as(subset(r2, lhs(x) %ain% c("B", "F")), "data.frame")
as(subset(r2, confidence == 1), "data.frame")

match(r2, r1)
match(r1, r2)

r <- unique(c(r1, r2))
match(r1, r)
all.equal(r1, r)

s <- as(r2, "sequences")
match(s, s2)

## timed

z <- as(zaki, "timedsequences")
all.equal(z, c(z[1], z[-1]))

## fixme: different orders of item labels
#all.equal(z, c(z[1,reduce=TRUE], z[-1,reduce=TRUE]))

###
