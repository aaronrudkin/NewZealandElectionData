rm(list=ls())
library(survival) # Survival modeling
library(stargazer) # Output tables
library(sandwich) # Robust SEs
library(lmtest) # Robust SEs
library(dplyr) # Data manipulation 
library(car) # Diagnostics of regression

#setwd("~/Dropbox/Field Paper/")
# ENSURE THAT WORKING DIRECTORY IS SET PROPERLY TO ROOT OF FIELD PAPER

###### Support functions ####
secondHighest = function(x) # Code snippet from StackOverflow--used to get second highest of a list
{
    sort(unique(x), decreasing=TRUE)[2L]
}

logitCoeffToPredictedProb = function(logitCoefs, values) # Convert series of logit coefficients and values to predicted prob.
{
    sum = sum(logitCoefs*values)
    expValue = exp(sum)
    return(expValue / (1+expValue))
}

Mode <- function(x) { # Code snippet from StackOverflow. Mode.
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Implementation of clustered standard errors
# From: https://diffuseprior.wordpress.com/2012/06/15/standard-robust-and-clustered-standard-errors-computed-in-r/
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
#### End support functions

############## PREP DATA #############
# Read the election data
dataT = read.csv("~/Dropbox/Field Paper/Canada Data/Merge/output.csv", header=TRUE)

# Discard data before 1945 general election
dataT = dataT[as.character(dataT$Date)>="1945-06-11",]

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

# Recode party labels to only include major parties and other; no substantive reason to keep variation in that regard.
dataT[dataT$Party!="Lib" & dataT$Party!="P.C." & dataT$Party!="C" & dataT$Party!="CA" & dataT$Party!="N.D.P." & dataT$Party!="Ref." & dataT$Party!="CCF" & dataT$Party!="B.Q.",]$Party = "Ind."
dataT$Party = relevel(dataT$Party, "Ind.")
dataT = dataT[dataT$Votes>-1,]

crossSecModelCA = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party), data=dataT)
crossSecMartinCA = lm(VotePct ~ CabinetNow + Incumbent + as.factor(Party), data=dataT)

prevCandidates = dataT[dataT$LastVotes>0 & dataT$Incumbent==1,]
# Code y_t,i - y_{t-1},i
prevCandidates$FDVotePct = prevCandidates$VotePct - prevCandidates$LastVotePct
firstDiffModelCA = lm(FDVotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party), data=prevCandidates)
# Control for past vote share
controlShareModelCA = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party) + LastVotePct, data=prevCandidates)
# Indivdiual fixed effects
indFixEffectModelCA = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(ID), data=prevCandidates)

logitModelCA = glm(Elected ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party), family=binomial(link=logit), data=dataT)
logitMartinCA = glm(Elected ~ CabinetNow + Incumbent + as.factor(Party), family=binomial(link=logit), data=dataT)

survivalSubset = dataT[dataT$Incumbent==1,c("ID","Name","Incumbent","TermsServed","CabinetNow","CabinetImportant","CabinetPM","Date","Elected","Party", "PartyInGovt")]
# Whip data into spells
survivalDataCA = survivalSubset %>% mutate(start=TermsServed-1, end=TermsServed, death=as.integer(!Elected==1), pName=as.character(Party)) %>% select(ID, start, end, death, CabinetNow, CabinetImportant, CabinetPM, PartyInGovt, pName)
# Recode party name
survivalDataCA$pName = as.factor(survivalDataCA$pName)
survivalDataCA$pName = relevel(survivalDataCA$pName, "Ind.")
# Setup survival DV
survivalDVCA = Surv(survivalDataCA$start, survivalDataCA$end, survivalDataCA$death)
# And the model
survivalModelCA = coxph(survivalDVCA ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + pName, data=survivalDataCA)
summary(survivalModelCA)