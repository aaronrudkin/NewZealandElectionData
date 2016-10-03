rm(list=ls())
library(survival) # Survival modeling
library(stargazer) # Output tables
library(sandwich) # Robust SEs
library(lmtest) # Robust SEs
library(dplyr) # Data manipulation 
library(car) # Diagnostics of regression

#setwd("~/Dropbox/Field Paper/")
# ENSURE THAT WORKING DIRECTORY IS SET PROPERLY TO ROOT OF FIELD PAPER


secondHighest = function(x) # Code snippet from StackOverflow
{
    sort(unique(x), decreasing=TRUE)[2L]
}

logitCoeffToPredictedProb = function(logitCoefs, values)
{
    sum = sum(logitCoefs*values)
    expValue = exp(sum)
    return(expValue / (1+expValue))
}

Mode <- function(x) { # Code snippet from StackOverflow
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

olsClusteredStandardErrors <- function(form, data, robust=FALSE, cluster=NULL,digits=3){
    r1 <- lm(form, data)
    if(length(cluster)!=0){
        data <- na.omit(data[,c(colnames(r1$model),cluster)])
        r1 <- lm(form, data)
    }
    X <- model.matrix(r1)
    n <- dim(X)[1]
    k <- dim(X)[2]
    if(robust==FALSE & length(cluster)==0){
        se <- sqrt(diag(solve(crossprod(X)) * as.numeric(crossprod(resid(r1))/(n-k))))
        res <- cbind(coef(r1),se)
    }
    if(robust==TRUE){
        u <- matrix(resid(r1))
        meat1 <- t(X) %*% diag(diag(crossprod(t(u)))) %*% X
        dfc <- n/(n-k)    
        se <- sqrt(dfc*diag(solve(crossprod(X)) %*% meat1 %*% solve(crossprod(X))))
        res <- cbind(coef(r1),se)
    }
    if(length(cluster)!=0){
        clus <- cbind(X,data[,cluster],resid(r1))
        colnames(clus)[(dim(clus)[2]-1):dim(clus)[2]] <- c(cluster,"resid")
        m <- dim(table(clus[,cluster]))
        dfc <- (m/(m-1))*((n-1)/(n-k))
        uclust  <- apply(resid(r1)*X,2, function(x) tapply(x, clus[,cluster], sum))
        se <- sqrt(diag(solve(crossprod(X)) %*% (t(uclust) %*% uclust) %*% solve(crossprod(X)))*dfc)   
        res <- cbind(coef(r1),se)
    }
    res <- cbind(res,res[,1]/res[,2],(1-pnorm(abs(res[,1]/res[,2])))*2)
    res1 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res)),nrow=dim(res)[1])
    rownames(res1) <- rownames(res)
    colnames(res1) <- c("Estimate","Std. Error","t value","Pr(>|t|)")
    return(res1)
}

############## PREP DATA #############
# Read the election data
dt = read.csv("New Zealand Data/Merge/output.csv", header=TRUE)
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
NZdtSerious = dt[dt$Party=="National" | dt$Party=="Labour" | dt$Party=="Maori" | dt$Party=="NZF" | dt$Party=="Social Credit" | dt$Party=="UFNZ" | dt$Party=="ACT" | dt$Party=="Alliance" | dt$Party=="Progressive" | dt$Party=="Mana" | dt$Party=="GP" | dt$Party=="NL" | dt$Party=="United NZ" | dt$Elected==1 | dt$VotePct>5, ]
# Drop uncontested electorate
NZdtSerious = NZdtSerious[NZdtSerious$Votes>-1,]
stopifnot(nrow(NZdtSerious)==7353) # Replication: Verify correct row count drop
######################### END PREP


######################## PRE-ANALYSIS STUFF
baseIncumbencyRate = glm(Elected ~ Incumbent, family=binomial(link=logit), data=NZdtSerious)
govtIncumbencyRate = glm(Elected ~ Incumbent + PartyInGovt, family=binomial(link=logit), data=NZdtSerious)
# Base model: Elected ~ Incumbent -> 87% incumbent re-election rate; 89% for party in gov't.

# Initial model suffers from Multicollinearity
testModel = lm(Votes ~ PartyInGovt + TermsServed + Incumbent + as.factor(Party) + CabinetNow + CabinetEver + CabinetImportant + CabinetPM, data=NZdtSerious)
vif(testModel)
# Huge inflation from CabinetEver and CabinetNow--some inflation from Incumbent and TermsServed
# Prefer TermsServed going forward, drop CabinetEver; it's not a key part of the analysis to begin with.


############ MODEL 1: CROSS-SECTIONAL DATA #####
# Cross-sectional data:
crossSecModelNZ = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party), data=NZdtSerious)
summary(crossSecModelNZ) # Non-robust standard errors

crossSecMartinNZ = lm(VotePct ~ CabinetNow + Incumbent + as.factor(Party), data=NZdtSerious)
summary(crossSecMartinNZ)

# Test for normal errors -- somewhat normal
#pdf("qqPlotCrossSectional.pdf")
#qqPlot(crossSecModelNZ)
#dev.off()

# Test for influence using the old Cook's Distance rule of thumb.
cutoff <- 4/((nrow(NZdtSerious)-length(crossSecModelNZ$coefficients)-2)) 
#plot(crossSecModelNZ, which=4, cook.levels=cutoff)

olsClusteredStandardErrors(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + Party, NZdtSerious, cluster="Electorate")
summary(lm(VotePct ~ TermsServed + CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + as.factor(Party) + as.factor(Electorate), data=NZdtSerious))$coefficients[1:18,]
summary(lm(VotePct ~ Incumbent + CabinetNow + PartyInGovt + as.factor(Party), data=NZdtSerious))
summary(lm(VotePct ~ Incumbent + CabinetNow, data=NZdtSerious[NZdtSerious$Party=="National",]))
summary(lm(VotePct ~ Incumbent + CabinetNow, data=NZdtSerious[NZdtSerious$Party=="Labour",]))

########### MODEL 2: Controlling for past performance
probSelectIntoCab = glm(CabinetNow ~ TermsServed + LastVotePct, family=binomial(link=logit), data=NZdtSerious[NZdtSerious$PartyInGovt==1 & NZdtSerious$Incumbent==1,])

prevCandidates = NZdtSerious[NZdtSerious$LastVotes>0 & NZdtSerious$Incumbent==1,]
prevCandidates$FDVotePct = prevCandidates$VotePct - prevCandidates$LastVotePct
firstDiffModelNZ = lm(FDVotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party), data=prevCandidates)
controlShareModelNZ = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party) + LastVotePct, data=prevCandidates)
indFixEffectModelNZ = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(ID), data=prevCandidates)
martinFDModelNZ = lm(FDVotePct ~ CabinetNow + PartyInGovt, data=prevCandidates)
summary(controlShareModelNZ)
summary(firstDiffModelNZ)
summary(indFixEffectModelNZ)

cutoff <- 4/((nrow(prevCandidates)-length(controlShareModelNZ$coefficients)-2)) 
#plot(controlShareModelNZ, which=4, cook.levels=cutoff) # Leverage plot
############

########### MODEL 3: CROSS-SECTIONAL LOGIT #####
logitModelNZ = glm(Elected ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party), family=binomial(link=logit), data=NZdtSerious)
summary(logitModelNZ)
logitCoeffToPredictedProb(c(-6.12,4.76,3.37),c(1,1,0))
logitCoeffToPredictedProb(c(-6.12,4.76,3.37),c(1,1,1))

logitMartinNZ = glm(Elected ~ CabinetNow + Incumbent + as.factor(Party), family=binomial(link=logit), data=NZdtSerious)
summary(logitMartinNZ)

preMMP = glm(Elected ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party), family=binomial(link=logit), data=NZdtSerious[as.character(NZdtSerious$Date)<="1994-01-01",])
postMMP = glm(Elected ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party), family=binomial(link=logit), data=NZdtSerious[as.character(NZdtSerious$Date)>="1994-01-01",])
logitCoeffToPredictedProb(c(coef(preMMP)[1], coef(preMMP)[2], coef(preMMP)[5], coef(preMMP)[6], coef(preMMP)[10]), c(1,0,1,1,1))
logitCoeffToPredictedProb(c(coef(preMMP)[1], coef(preMMP)[2], coef(preMMP)[5], coef(preMMP)[6], coef(preMMP)[10]), c(1,1,1,1,1))
# 39% -> 33%

logitCoeffToPredictedProb(c(coef(postMMP)[1], coef(postMMP)[2], coef(postMMP)[5], coef(postMMP)[6], coef(postMMP)[10]), c(1,0,1,1,1))
logitCoeffToPredictedProb(c(coef(postMMP)[1], coef(postMMP)[2], coef(postMMP)[5], coef(postMMP)[6], coef(postMMP)[10]), c(1,1,1,1,1))
# 27% -> 49%

# For a better estimate of uncertainty, bootstrap the CI of the first-difference and each coefficient
set.seed(19861108) # For replication, fix seed for bootstraping.
iterations = 1000
predProb0NZ = numeric(iterations)
predProb1NZ = numeric(iterations)
coefMatrixNZ = matrix(data=NA, nrow=iterations, ncol=18)
coefMatrixMNZ = matrix(data=NA, nrow=iterations, ncol=15)
for(i in 1:iterations)
{
    if(i%%100==0) 
    { 
        print(paste(i,"samples for bootstrap")) 
    }
    
    bootstrapData = NZdtSerious[sample(nrow(NZdtSerious), nrow(NZdtSerious), replace=TRUE),]
    logitModelSample = glm(Elected ~ CabinetNow + TermsServed + CabinetImportant + CabinetPM + PartyInGovt + as.factor(Party), family=binomial(link=logit), data=bootstrapData)
    martinModelSample = glm(Elected ~ CabinetNow + Incumbent + as.factor(Party), family=binomial(link=logit), data=bootstrapData)
    coefMatrixNZ[i,] = coef(logitModelSample)
    coefMatrixMNZ[i,] = coef(martinModelSample)
    predProb0NZ[i] = logitCoeffToPredictedProb(
        c(
            coef(logitModelSample)[1], 
            coef(logitModelSample)[2], 
            coef(logitModelSample)[3], 
            coef(logitModelSample)[6], 
            coef(logitModelSample)[10]
        ), 
        c(1, 0, 2, 1, 1)
    )
    predProb1NZ[i] = logitCoeffToPredictedProb(
        c(
            coef(logitModelSample)[1], 
            coef(logitModelSample)[2], 
            coef(logitModelSample)[3], 
            coef(logitModelSample)[6], 
            coef(logitModelSample)[10]
        ), 
        c(1, 1, 2, 1, 1)
    )
}
logitEstimatesNZ = numeric(ncol(coefMatrixNZ))
logitEstimatesMNZ = numeric(ncol(coefMatrixMNZ))
CIsNZ = matrix(NA, ncol=2, nrow=ncol(coefMatrixNZ))
CIsMNZ = matrix(NA, ncol=2, nrow=ncol(coefMatrixMNZ))
for(i in 1:ncol(coefMatrixNZ))
{
    coefInterest = coefMatrixNZ[,i]
    logitEstimatesNZ[i] = mean(coefInterest)
    CIsNZ[i,] = quantile(coefInterest, c(0.025, 0.975))
}
for(i in 1:ncol(coefMatrixMNZ))
{
    coefInterestM = coefMatrixMNZ[,i]
    logitEstimatesMNZ[i] = mean(coefInterestM)
    CIsMNZ[i,] = quantile(coefInterestM, c(0.025, 0.975))
}
names(logitEstimatesNZ) = names(logitModelSample$coefficients)
names(logitEstimatesMNZ) = names(martinModelSample$coefficients)
rownames(CIsNZ) = names(logitModelSample$coefficients)
rownames(CIsMNZ) = names(martinModelSample$coefficients)
print(CIsNZ)

summary(glm(Elected ~ as.factor(Party) + Incumbent + CabinetNow + CabinetImportant + CabinetPM + PartyInGovt, family=binomial(link=logit), data=NZdtSerious))
###########

############ MODEL 4: SURVIVAL DATA #####
survivalSubset = NZdtSerious[NZdtSerious$Incumbent==1,c("ID","Name","Incumbent","TermsServed","CabinetNow","CabinetImportant","CabinetPM","Date","Elected","Party", "PartyInGovt")]
survivalDataNZ = survivalSubset %>% mutate(start=TermsServed-1, end=TermsServed, death=as.integer(!Elected==1), pName=as.character(Party)) %>% select(ID, start, end, death, CabinetNow, CabinetImportant, CabinetPM, PartyInGovt, pName)
survivalDataNZ$pName = as.factor(survivalDataNZ$pName)
survivalDataNZ$pName = relevel(survivalDataNZ$pName, "I")
survivalDVNZ = Surv(survivalDataNZ$start, survivalDataNZ$end, survivalDataNZ$death)
survivalModelNZ = coxph(survivalDVNZ ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + pName, data=survivalDataNZ)
summary(survivalModelNZ)
survModelPlotDataNZ = data.frame(CabinetNow=c(0, 1), CabinetImportant=c(0,0), CabinetPM=c(0,0), PartyInGovt=c(1,1), pName=c("Labour","Labour"))

ciMatrixSurvivalNZ = list(cbind(summary(survivalModelNZ)$conf.int[,3], 
                                summary(survivalModelNZ)$conf.int[,4]))

# Diagnostics: Is the hazard proportional?
# Null hypothesis: Proportional hazard is appropriate; reject null: proportional hazard is not appropriate.
cox.zph(survivalModelNZ)

syntheticObs = read.csv("New Zealand Data/SyntheticObs/synthOut.csv")
syntheticSurvive = syntheticObs %>% mutate(start=TermsServed-1, end=TermsServed, death=1, pName=as.character(Party)) %>% select(ID, start, end, death, CabinetNow, CabinetImportant, CabinetPM, PartyInGovt, pName)

combinedSurvivalDataNZ = rbind(survivalDataNZ, syntheticSurvive)
combinedSurvivalDVNZ = Surv(combinedSurvivalDataNZ$start, combinedSurvivalDataNZ$end, combinedSurvivalDataNZ$death)
combinedModelNZ = coxph(combinedSurvivalDVNZ ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + pName, data=combinedSurvivalDataNZ)

ciMatrixCombNZ = list(cbind(summary(combinedModelNZ)$conf.int[,3], summary(combinedModelNZ)$conf.int[,4]))

# Save data for plot generator
save(NZdtSerious, survivalModelNZ, combinedModelNZ, survModelPlotDataNZ, 
     crossSecModelNZ, crossSecMartinNZ, controlShareModelNZ, 
     firstDiffModelNZ, martinFDModelNZ, predProb0NZ, predProb1NZ,
     logitModelNZ, logitMartinNZ, logitEstimatesNZ, logitEstimatesMNZ,
     CIsNZ, CIsMNZ, ciMatrixSurvivalNZ, ciMatrixCombNZ, 
     survivalDataNZ, combinedSurvivalDataNZ, survivalDVNZ, combinedSurvivalDVNZ,
     preMMP, postMMP,
     file="New Zealand Data/Merge/OutputNZ.RData")



##### Party List Data ####
listData = NZdtSerious[NZdtSerious$Date=="1996-10-12" | NZdtSerious$Date=="1999-11-27" | NZdtSerious$Date=="2002-07-27" | NZdtSerious$Date=="2005-09-17" | NZdtSerious$Date=="2008-11-08" | NZdtSerious$Date=="2011-11-26" | NZdtSerious$Date=="2014-09-20" ,]
# Only grab general elections. By-elections for electorate members are electorate by-elections
# List resignations go further down the list, so no by-election is relevant

# Lagged cabinet variable (now can do a diff in diff)
listData = listData %>% arrange(ID,Date) %>% mutate(LastCabinet=lag(CabinetNow))
govtListData = listData[listData$PartyInGovt==1 & (listData$Party=="National" | listData$Party=="Labour"),]
govtListData %>% group_by(Party, Date) %>% summarise(minList=min(ListPosition), maxList=max(ListPosition), numCand=n())

# Recode unlisted MPs to be tied at the bottom of the list
govtListData[govtListData$Party=="Labour" & govtListData$Date=="1999-11-27" & govtListData$LastListPosition==0,]$LastListPosition=61
govtListData[govtListData$Party=="Labour" & govtListData$Date=="2002-07-27" & govtListData$LastListPosition==0,]$LastListPosition=66
govtListData[govtListData$Party=="Labour" & govtListData$Date=="2005-09-17" & govtListData$LastListPosition==0,]$LastListPosition=75
govtListData[govtListData$Party=="Labour" & govtListData$Date=="2008-11-08" & govtListData$LastListPosition==0,]$LastListPosition=76
govtListData[govtListData$Party=="Labour" & govtListData$Date=="2011-11-26" & govtListData$LastListPosition==0,]$LastListPosition=78
govtListData[govtListData$Party=="Labour" & govtListData$Date=="2014-09-20" & govtListData$LastListPosition==0,]$LastListPosition=71
govtListData[govtListData$Party=="Labour" & govtListData$Date=="1996-10-12" & govtListData$ListPosition==0,]$ListPosition=61
govtListData[govtListData$Party=="Labour" & govtListData$Date=="1999-11-27" & govtListData$ListPosition==0,]$ListPosition=66
govtListData[govtListData$Party=="Labour" & govtListData$Date=="2002-07-27" & govtListData$ListPosition==0,]$ListPosition=75
govtListData[govtListData$Party=="Labour" & govtListData$Date=="2005-09-17" & govtListData$ListPosition==0,]$ListPosition=76
govtListData[govtListData$Party=="Labour" & govtListData$Date=="2008-11-08" & govtListData$ListPosition==0,]$ListPosition=78
govtListData[govtListData$Party=="Labour" & govtListData$Date=="2011-11-26" & govtListData$ListPosition==0,]$ListPosition=71
govtListData[govtListData$Party=="Labour" & govtListData$Date=="2014-09-20" & govtListData$ListPosition==0,]$ListPosition=65
govtListData[govtListData$Party=="National" & govtListData$Date=="1999-11-27" & govtListData$LastListPosition==0,]$LastListPosition=66
govtListData[govtListData$Party=="National" & govtListData$Date=="2002-07-27" & govtListData$LastListPosition==0,]$LastListPosition=65
govtListData[govtListData$Party=="National" & govtListData$Date=="2005-09-17" & govtListData$LastListPosition==0,]$LastListPosition=66
govtListData[govtListData$Party=="National" & govtListData$Date=="2008-11-08" & govtListData$LastListPosition==0,]$LastListPosition=66
govtListData[govtListData$Party=="National" & govtListData$Date=="2011-11-26" & govtListData$LastListPosition==0,]$LastListPosition=73
govtListData[govtListData$Party=="National" & govtListData$Date=="2014-09-20" & govtListData$LastListPosition==0,]$LastListPosition=76
govtListData[govtListData$Party=="National" & govtListData$Date=="1996-10-12" & govtListData$ListPosition==0,]$ListPosition=66
govtListData[govtListData$Party=="National" & govtListData$Date=="1999-11-27" & govtListData$ListPosition==0,]$ListPosition=64
govtListData[govtListData$Party=="National" & govtListData$Date=="2002-07-27" & govtListData$ListPosition==0,]$ListPosition=65
govtListData[govtListData$Party=="National" & govtListData$Date=="2005-09-17" & govtListData$ListPosition==0,]$ListPosition=65
govtListData[govtListData$Party=="National" & govtListData$Date=="2008-11-08" & govtListData$ListPosition==0,]$ListPosition=72
govtListData[govtListData$Party=="National" & govtListData$Date=="2011-11-26" & govtListData$ListPosition==0,]$ListPosition=75
govtListData[govtListData$Party=="National" & govtListData$Date=="2014-09-20" & govtListData$ListPosition==0,]$ListPosition=75

# Create delta variable
govtListData = govtListData %>% mutate(FD=ListPosition-LastListPosition)

# delta for each possible group.
enter = govtListData[as.character(govtListData$Date)>="1997-01-01" & govtListData$LastCabinet==0 & govtListData$CabinetNow==1,]$FD
stay = govtListData[as.character(govtListData$Date)>="1997-01-01" & govtListData$LastCabinet==1 & govtListData$CabinetNow==1,]$FD
exit = govtListData[as.character(govtListData$Date)>="1997-01-01" & govtListData$LastCabinet==1 & govtListData$CabinetNow==0,]$FD
stayout = govtListData[as.character(govtListData$Date)>="1997-01-01" & govtListData$LastCabinet==0 & govtListData$CabinetNow==0,]$FD
# T test is not quite the appropriate statistic, but the DiM is obviously not an effect
t_test(enter,stayout)
# Can't reject null hypothesis re: cabinet list placing.

# See, no real difference
mean(enter)
mean(stay)
mean(exit)
mean(stayout)

# Inference about trajectory over time
mean(govtListData[govtListData$Incumbent==0,]$ListPosition)
mean(govtListData[govtListData$Incumbent==0 & govtListData$Elected==1,]$ListPosition)
mean(govtListData[govtListData$Incumbent==1 & govtListData$TermsServed==1,]$FD)
mean(govtListData[govtListData$Incumbent==1 & govtListData$TermsServed==2,]$FD)
mean(govtListData[govtListData$Incumbent==1 & govtListData$TermsServed==3,]$FD)
#### End party list discussion


############ Appendix: Decade disaggregation ####
decadeDecomposition = matrix(NA,nrow=7,ncol=12)
for(i in 5:11)
{
    minDate = 1900 + (10*i)
    maxDate = minDate + 10
    print(paste0("Decade: ",minDate,"-",maxDate))
    candSubset = NZdtSerious[as.character(NZdtSerious$Date)>paste0(minDate,"-01-01") & as.character(NZdtSerious$Date)<paste0(maxDate,"-01-01"),]
    print(nrow(candSubset))

    crossSectionalModelVotesDecade = lm(VotePct ~ TermsServed + CabinetNow + CabinetImportant + CabinetPM + as.factor(Party) + PartyInGovt, data=candSubset)
    timeSeriesModelVotesDecade = lm(VotePct ~ TermsServed + PartyInGovt + CabinetNow + CabinetImportant + CabinetPM + LastVotePct + as.factor(Party), data=candSubset[candSubset$Incumbent==1 & candSubset$LastVotes>0,])
    logitModelDecade = glm(Elected ~ TermsServed + CabinetNow + CabinetImportant + CabinetPM + as.factor(Party) + PartyInGovt, family=binomial(link=logit), data=candSubset)
    decadeDecomposition[i-4,] = 
        round(c(
            coeftest(crossSectionalModelVotesDecade,sandwich)[3,1], 
            coeftest(crossSectionalModelVotesDecade,sandwich)[3,1] - 1.96*coeftest(crossSectionalModelVotesDecade,sandwich)[3,2], 
            coeftest(crossSectionalModelVotesDecade,sandwich)[3,1] + 1.96*coeftest(crossSectionalModelVotesDecade,sandwich)[3,2], 
            coeftest(crossSectionalModelVotesDecade,sandwich)[3,4],
            coeftest(timeSeriesModelVotesDecade,sandwich)[4,1], 
            coeftest(timeSeriesModelVotesDecade,sandwich)[4,1] - 1.96*coeftest(timeSeriesModelVotesDecade,sandwich)[4,2], 
            coeftest(timeSeriesModelVotesDecade,sandwich)[4,1] + 1.96*coeftest(timeSeriesModelVotesDecade,sandwich)[4,2], 
            coeftest(timeSeriesModelVotesDecade,sandwich)[4,4],
            coef(summary(logitModelDecade))[3,1],
            coef(summary(logitModelDecade))[3,1] - 1.96*coef(summary(logitModelDecade))[3,2], 
            coef(summary(logitModelDecade))[3,1] + 1.96*coef(summary(logitModelDecade))[3,2], 
            coef(summary(logitModelDecade))[3,4]
        ),2)
}
rownames(decadeDecomposition) = c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s")
colnames(decadeDecomposition) = c("Model 1",
                                  "Model 1 CIL",
                                  "Model 1 CIH",
                                  "Model 1 P", 
                                  "Model 2", 
                                  "Model 2 CIL",
                                  "Model 2 CIH",
                                  "Model 2 P", 
                                  "Model 3", 
                                  "Model 3 CIL",
                                  "Model 3 CIH",
                                  "Model 3 P")

decadeDecomposition
write.table(decadeDecomposition, "Includes/decadeDecompositionNZ.txt",quote=FALSE, sep="\t")