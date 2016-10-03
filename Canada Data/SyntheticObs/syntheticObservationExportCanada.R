#Synthetic observation export
library(dplyr)
rm(list=ls())
dataT = read.csv("../Merge/output.csv", header=TRUE)

# Discard data before 1945 general election
dataT = dataT[as.character(dataT$Date)>="1945-06-11",]
stopifnot(nrow(dataT)==31638, min(as.character(dataT$Date))=="1945-06-11")
# Replication, verify all extraneous data was discarded correctly.

# Code party in government
dataT$PartyInGovt = 0
dataT[as.character(dataT$Date)>="1935-01-01" & as.character(dataT$Date)<="1948-11-14" & dataT$Party=="Lib",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1948-11-15" & as.character(dataT$Date)<="1957-06-20" & dataT$Party=="Lib",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1957-06-21" & as.character(dataT$Date)<="1963-04-21" & dataT$Party=="P.C.",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1963-04-22" & as.character(dataT$Date)<="1968-04-19" & dataT$Party=="Lib",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1968-04-20" & as.character(dataT$Date)<="1979-06-02" & dataT$Party=="Lib",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1979-06-02" & as.character(dataT$Date)<="1980-03-01" & dataT$Party=="P.C.",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1980-03-02" & as.character(dataT$Date)<="1984-09-15" & dataT$Party=="Lib",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1984-09-17" & as.character(dataT$Date)<="1993-11-02" & dataT$Party=="P.C.",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="1993-11-03" & as.character(dataT$Date)<="2006-02-05" & dataT$Party=="Lib",]$PartyInGovt = 1
dataT[as.character(dataT$Date)>="2006-02-06" & dataT$Party=="C",]$PartyInGovt = 1

# Sanity check vote percentage calculations.
dataT = dataT %>% group_by(Date, Province, Riding) %>% summarise(totalVotes=sum(Votes)) %>% merge(dataT,.) %>% mutate(VotePctCalc = round(100*Votes/totalVotes,2))
dataT$Diff = dataT$VotePct - dataT$VotePctCalc
stopifnot(nrow(dataT[dataT$Diff>0,])==0)

# Drop unserious candidates
print(paste("Vote threshold check"))
for(i in seq(1,10))
{
    print(paste0("Below ",i,"%: ",nrow(dataT[dataT$VotePct<i,]),". Major candidates below ",i,"%: ",nrow(dataT[dataT$VotePct<i & (dataT$Party=="Lib" | dataT$Party=="P.C." | dataT$Party=="C" | dataT$Party=="CA" | dataT$Party=="N.D.P." | dataT$Party=="Ref." | dataT$Party=="B.Q."),]),". Elected below ",i,"%: ",nrow(dataT[dataT$Elected & dataT$VotePct<i,])))
}
# Dropping ~10,000 candidates, losing 0 elected and only a few hundred from organized political parties. 
# Compensate by including all major party candidates.
dtSerious = dataT[dataT$Party=="Lib" | dataT$Party=="P.C." | dataT$Party=="C" | dataT$Party=="CA" | dataT$Party=="N.D.P." | dataT$Party=="Ref." | dataT$Party=="CCF" | dataT$Party=="B.Q." | dataT$VotePct>5,]
dtSerious = dtSerious[dtSerious$Votes>-1,]
stopifnot(nrow(dtSerious)==22799) # Replication: Verify correct row count drop

# Recode party labels to only include major parties and other; no substantive reason to keep variation in that regard.
dtSerious[dtSerious$Party!="Lib" & dtSerious$Party!="P.C." & dtSerious$Party!="C" & dtSerious$Party!="CA" & dtSerious$Party!="N.D.P." & dtSerious$Party!="Ref." & dtSerious$Party!="CCF" & dtSerious$Party!="B.Q.",]$Party = "Ind."
dtSerious$Party = relevel(dtSerious$Party, "Ind.")

# Give me all those members whose last election status was 1 (i.e. they retired or died rather than lost in their last election)
ghostCandidates = dtSerious[dtSerious$Incumbent==1 | dtSerious$Elected==1,] %>% group_by(ID) %>% summarise(name=last(Name), exitDataSet=last(Elected), exitDate=last(Date), termsServed=last(TermsServed), Party=last(Party)) %>% mutate(needGhostRow = as.integer(as.character(exitDate)!="2015-10-19") * as.integer(exitDataSet)) %>% select(ID, name, exitDate, needGhostRow, termsServed, Party) %>% filter(needGhostRow==1)

write.csv(ghostCandidates,"synthIn.csv", row.names=FALSE)

