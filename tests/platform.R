
### ceeboo 2012

if (.Platform$OS  == "windows" && 
    .Platform$GUI == "RTerm") {

    ## fake
    .Platform$GUI <- "Rgui"

    library("arulesSequences")

    ## use example data from paper
    data(zaki)
    ## mine frequent sequences
    s1 <- cspade(zaki, parameter = list(support = 0.4)) 
    summary(s1)
}

###
