#Synthetic observation export
library(dplyr)
rm(list=ls())

dt = read.csv("../Merge/output.csv", header=TRUE)
stopifnot(nrow(dt)==10267) # Replication, verify all rows loaded exactly

# Code party in government
dt$PartyInGovt = 0
dt[as.character(dt$Date)>="1940-04-01" & as.character(dt$Date)<="1949-12-12" & dt$Party=="Labour",]$PartyInGovt = 1
dt[as.character(dt$Date)>="1949-12-13" & as.character(dt$Date)<="1957-12-11" & dt$Party=="National",]$PartyInGovt = 1
dt[as.character(dt$Date)>="1957-12-12" & as.character(dt$Date)<="1960-12-11" & dt$Party=="Labour",]$PartyInGovt = 1
dt[as.character(dt$Date)>="1960-12-12" & as.character(dt$Date)<="1972-12-07" & dt$Party=="National",]$PartyInGovt = 1
dt[as.character(dt$Date)>="1972-12-08" & as.character(dt$Date)<="1975-12-12" & dt$Party=="Labour",]$PartyInGovt = 1
dt[as.character(dt$Date)>="1975-12-13" & as.character(dt$Date)<="1984-07-26" & dt$Party=="National",]$PartyInGovt = 1
dt[as.character(dt$Date)>="1984-07-26" & as.character(dt$Date)<="1990-11-01" & dt$Party=="Labour",]$PartyInGovt = 1
dt[as.character(dt$Date)>="1990-11-02" & as.character(dt$Date)<="1996-11-12" & dt$Party=="National",]$PartyInGovt = 1
dt[as.character(dt$Date)>="1996-11-13" & as.character(dt$Date)<="1999-12-04" & (dt$Party=="National" | dt$Party=="NZF"),]$PartyInGovt = 1
dt[as.character(dt$Date)>="1999-12-05" & as.character(dt$Date)<="2008-11-18" & (dt$Party=="Labour" | dt$Party=="Alliance"),]$PartyInGovt = 1
dt[as.character(dt$Date)>="2008-11-19" & dt$Party=="National",]$PartyInGovt = 1
table(dt$PartyInGovt)

# Calculate vote percentage
dt = dt %>% group_by(Date, Electorate) %>% summarise(totalVotes=sum(Votes)) %>% merge(dt,.) %>% mutate(VotePct = round(100*Votes/totalVotes,2))
# Calculate lagged vote percentage (we'll lose some obs for incumbents who came in in 1946 because we don't have 1943 vote data!)
dt = dt %>% mutate(LastVotePct = lag(VotePct, order_by=ID))

# Pull list of parties who have elected people
dt %>% group_by(Party) %>% tally(Elected, sort=TRUE)

# Recode never elected parties into independents
dt %>% group_by(Party) %>% tally(sort=TRUE)
nrow(dt[dt$Party!="National" & dt$Party!="Labour" & dt$Party!="Maori" & dt$Party!="NZF" & dt$Party!="Social Credit" & dt$Party!="UFNZ" & dt$Party!="United New Zealand" & dt$Party!="ACT" & dt$Party!="Alliance" & dt$Party!="Progressive" & dt$Party!="Mana" & dt$Party!="GP" & dt$Party!="NL" & dt$Party!="United NZ",])
dt[dt$Party!="National" & dt$Party!="Labour" & dt$Party!="Maori" & dt$Party!="NZF" & dt$Party!="Social Credit" & dt$Party!="UFNZ" & dt$Party!="United New Zealand" & dt$Party!="ACT" & dt$Party!="Alliance" & dt$Party!="Progressive" & dt$Party!="Mana" & dt$Party!="GP" & dt$Party!="NL",]$Party = "I"
dt %>% group_by(Party) %>% tally()
dt$Party = relevel(dt$Party, "I")

# Drop unserious candidates
dtSerious = dt[dt$Party=="National" | dt$Party=="Labour" | dt$Party=="Maori" | dt$Party=="NZF" | dt$Party=="Social Credit" | dt$Party=="UFNZ" | dt$Party=="ACT" | dt$Party=="Alliance" | dt$Party=="Progressive" | dt$Party=="Mana" | dt$Party=="GP" | dt$Party=="NL" | dt$Party=="United NZ" | dt$Elected==1 | dt$VotePct>5, ]
# Drop uncontested electorate
dtSerious = dtSerious[dtSerious$Votes>-1,]

# Give me all those members whose last election status was 1 (i.e. they retired or died rather than lost in their last election)
ghostCandidates = dtSerious[dtSerious$Incumbent==1 | dtSerious$Elected==1,] %>% group_by(ID) %>% summarise(name=last(Name), exitDataSet=last(Elected), exitDate=last(Date), termsServed=last(TermsServed), Party=last(Party)) %>% mutate(needGhostRow = as.integer(as.character(exitDate)<"2014-09-20") * as.integer(exitDataSet)) %>% select(ID, name, exitDate, needGhostRow, termsServed, Party) %>% filter(needGhostRow==1)

write.csv(ghostCandidates,"synthIn.csv", row.names=FALSE)

