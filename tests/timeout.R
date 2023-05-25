
library("arulesSequences")

## CB 2023

t <- read_baskets(con  = system.file("misc", "test.txt", package =
                                      "arulesSequences"),
                  info = c("sequenceID", "eventID", "SIZE"))
summary(t)
## use low support
## IGNORE_RDIFF_BEGIN
s <- try(cspade(t, parameter = list(support = 0), 
                   control   = list(verbose = TRUE, timeout = 1)))
## IGNORE_RDIFF_END
if (!inherits(s, "try-error")) {
    stop("assert 'timeout' failed")
}
dir(tempdir())
