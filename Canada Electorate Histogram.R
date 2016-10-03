rm(list=ls())
library(dplyr)
library(weights)
# Read data
dt = read.csv("Canada Data/Merge/output.csv", header=TRUE)

# Keep only elected people
elected = dt[dt$Elected==1,]

# Generate party turnover stats
electorateHist = elected %>% group_by(Riding) %>% mutate(lagParty=lag(Party)) %>% summarise(numRows = n()-1, numTurnover=sum(Party!=lagParty & !is.na(lagParty))) %>% filter(numRows>1) %>% mutate(percTurnover = numTurnover/numRows)

# Plot weighted histogram of party turnover
pdf("Canada Data/Merge/ridingHist.pdf",width=7,height=4)
par(mar=c(5.1,3.1,2.1,2.1))
wtd.hist(x=electorateHist$percTurnover, w=electorateHist$numRows, xlab="Proportion of Elections that Result in Turnover", ylab="", yaxt="n", main="",xlim=c(0,1))
mtext("Weighted Frequency\n(Number of Ridings)",side=2,line=1)
dev.off()