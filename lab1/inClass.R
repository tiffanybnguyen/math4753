ddt <- read.csv("DDT.csv")
ddt
ddt[3]
ddt[3,]
ddt[[3]]
ddt$SPECIES

head(ddt)
ddt[ddt$LENGTH > 30 & ddt$SPECIES == "LMBASS"]

# answers
ddt[1,2]
