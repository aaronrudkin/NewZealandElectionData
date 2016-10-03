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
dataT = read.csv("Canada Data/Merge/output.csv", header=TRUE)
stopifnot(nrow(dataT)==42246) # Replication, verify all rows loaded exactly

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

# Recode party labels to only include major parties and other; no substantive reason to keep variation in that regard.
dataT[dataT$Party!="Lib" & dataT$Party!="P.C." & dataT$Party!="C" & dataT$Party!="CA" & dataT$Party!="N.D.P." & dataT$Party!="Ref." & dataT$Party!="CCF" & dataT$Party!="B.Q.",]$Party = "Ind."
dataT$Party = relevel(dataT$Party, "Ind.")
dataT = dataT[dataT$Votes>-1,]

# Drop unserious candidates
CAdtSerious = dataT[dataT$Party=="Lib" | dataT$Party=="P.C." | dataT$Party=="C" | dataT$Party=="CA" | dataT$Party=="N.D.P." | dataT$Party=="Ref." | dataT$Party=="CCF" | dataT$Party=="B.Q." | dataT$VotePct>5,]
stopifnot(nrow(CAdtSerious)==22799) # Replication: Verify correct row count drop

# For age models, only keep candidates with age.
dtAge = CAdtSerious[CAdtSerious$Age>0,]
######################### END PREP

######################## PRE-ANALYSIS STUFF ################

# Base model: Elected ~ Incumbent -> 76% incumbent re-election rate; 82% for party in gov't.
baseIncumbencyRate = glm(Elected ~ Incumbent, family=binomial(link=logit), data=CAdtSerious)
govtIncumbencyRate = glm(Elected ~ Incumbent + PartyInGovt, family=binomial(link=logit), data=CAdtSerious)

# Pre-emptively check to see whether variables of interest are likely to perform badly.
# Multicollinearity: not interested in looking at results; just interested in ensuring we don't
# have a multicollinearity problem including the terms.
testModel = lm(Votes ~ PartyInGovt + TermsServed + Incumbent + as.factor(Party) + CabinetNow + CabinetEver + CabinetImportant + CabinetPM, data=CAdtSerious)
vif(testModel)
# Huge inflation from CabinetEver and CabinetNow--some inflation from Incumbent and TermsServed
# TermsServed is a more informative superset of Incumbent. CabinetEver is probably not substantively necessary
# given that time series models should control for past benefits of being in cabinet (re: personal vote, name recog.)
########################


############# MODEL 1: Cross-sectional model #######
# Run cross-sectional models using both raw votes and vote percentage
crossSecModelCA = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party), data=CAdtSerious)
summary(crossSecModelCA) # Non-robust standard errors

# How many losses are there that are more narrow than the estimate of effect size?
narrowLoses = CAdtSerious %>% group_by(Date, Province, Riding) %>% summarise(tV = sum(VotePct), mV = max(VotePct), sV=secondHighest(VotePct)) %>% mutate(diff = mV-sV) %>% filter(diff<coef(crossSecModelCA)[2])
nrow(narrowLoses) # Ever
summary(droplevels(narrowLoses[narrowLoses$Date=="2015-10-19",]$Date)) # In this election

# Diagnostics
# Test for normal errors -- approximately normal
# qqPlot(crossSecModelCA) # Comment out for faster execution

# Test for influence using the old Cook's Distance rule of thumb.
cutoff <- 4/((nrow(CAdtSerious)-length(crossSecModelCA$coefficients)-2)) 
#plot(crossSecModelCA, which=4, cook.levels=cutoff) # Comment out for faster execution

# Martin Model
crossSecMartinCA = lm(VotePct ~ CabinetNow + Incumbent + as.factor(Party), data=CAdtSerious)
summary(crossSecMartinCA)

# Alternative cross-sectional model specifications
# Riding clustered standard errors
altSpec11CA = olsClusteredStandardErrors(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + Party, CAdtSerious, robust=TRUE, cluster="Riding")
# Sub incumbent
altSpec12CA = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + Incumbent + as.factor(Party), data=CAdtSerious)
# Riding FEs
altSpec13CA = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party) + as.factor(Riding), data=CAdtSerious)
# Add Age to model
altSpec14CA = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + Age + as.factor(Party), data=dtAge)
# End alternstive cross-sectional model specifications.

############ MODEL 2: Cross-sectional, "panel" data; include prior performance. ######
# Probability of selection into cabinet
probSelectIntoCab = glm(CabinetNow ~ Age + TermsServed + LastVotePct, family=binomial(link=logit), data=CAdtSerious[CAdtSerious$PartyInGovt==1 & CAdtSerious$Incumbent==1,])
# Endogeneity in selection into cabinet

# Get list of incumbent candidates
prevCandidates = CAdtSerious[CAdtSerious$LastVotes>0 & CAdtSerious$Incumbent==1,]
# Code y_t,i - y_{t-1},i
prevCandidates$FDVotePct = prevCandidates$VotePct - prevCandidates$LastVotePct

# Not technically first differences since we don't take delta of predictors, just a variable name.
firstDiffModelCA = lm(FDVotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party), data=prevCandidates)
# Control for past vote share
controlShareModelCA = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party) + LastVotePct, data=prevCandidates)
# Indivdiual fixed effects
indFixEffectModelCA = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(ID), data=prevCandidates)
# Martin's model
martinFDModelCA = lm(FDVotePct ~ CabinetNow + PartyInGovt, data=prevCandidates)
summary(controlShareModelCA)
summary(firstDiffModelCA)
summary(indFixEffectModelCA)

# Another leverage diagnostic
cutoff <- 4/((nrow(prevCandidates)-length(controlShareModelCA$coefficients)-2)) 
# plot(controlShareModelCA, which=4, cook.levels=cutoff) # Leverage plot
############ 

########### MODEL 3: CROSS-SECTIONAL LOGIT #######
# Base logit model
logitModelCA = glm(Elected ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party), family=binomial(link=logit), data=CAdtSerious)
summary(logitModelCA)
# Martin's logit model
logitMartinCA = glm(Elected ~ CabinetNow + Incumbent + as.factor(Party), family=binomial(link=logit), data=CAdtSerious)

# For a better estimate of uncertainty, bootstrap each parameter and
# the estimates of the effect on the modal case
set.seed(19861108) # For replication, fix seed for bootstrapping to my birthdate.
iterations = 1000
predProb0CA = numeric(iterations)
predProb1CA = numeric(iterations)
coefMatrixCA = matrix(data=NA, nrow=iterations, ncol=14)
coefMatrixMCA = matrix(data=NA, nrow=iterations, ncol=11)
for(i in 1:iterations)
{
    # Print progress, because this is slow
    if(i%%100==0) 
    { 
        print(paste(i,"samples for bootstrap")) 
    }

    # Resample
    bootstrapData = CAdtSerious[sample(nrow(CAdtSerious), nrow(CAdtSerious), replace=TRUE),]

    # Run both models
    logitModelSample = glm(Elected ~ CabinetNow + TermsServed + CabinetImportant + CabinetPM + PartyInGovt + as.factor(Party), family=binomial(link=logit), data=bootstrapData)
    martinModelSample = glm(Elected ~ CabinetNow + Incumbent + as.factor(Party), family=binomial(link=logit), data=bootstrapData)

    # Store coefficients
    coefMatrixCA[i,] = coef(logitModelSample)
    coefMatrixMCA[i,] = coef(martinModelSample)
    
    # Store predicted prob for 2nd term incumbent liberal
    predProb0CA[i] = logitCoeffToPredictedProb(
        c(
            coef(logitModelSample)[1], 
            coef(logitModelSample)[2], 
            coef(logitModelSample)[3], 
            coef(logitModelSample)[6], 
            coef(logitModelSample)[11]
        ), 
        c(1, 0, 2, 1, 1)
    )
    # And for cabinet minister with same qualities
    predProb1CA[i] = logitCoeffToPredictedProb(
        c(
            coef(logitModelSample)[1], 
            coef(logitModelSample)[2], 
            coef(logitModelSample)[3], 
            coef(logitModelSample)[6], 
            coef(logitModelSample)[11]
        ), 
        c(1, 1, 2, 1, 1)
    )
}
# Compile estimates
logitEstimatesCA = numeric(ncol(coefMatrixCA))
logitEstimatesMCA = numeric(ncol(coefMatrixMCA))
CIsCA = matrix(NA, ncol=2, nrow=ncol(coefMatrixCA))
CIsMCA = matrix(NA, ncol=2, nrow=ncol(coefMatrixMCA))
for(i in 1:ncol(coefMatrixCA))
{
    coefInterest = coefMatrixCA[,i]
    logitEstimatesCA[i] = mean(coefInterest)
    CIsCA[i,] = quantile(coefInterest, c(0.025, 0.975))
}
for(i in 1:ncol(coefMatrixMCA))
{
    coefInterestM = coefMatrixMCA[,i]
    logitEstimatesMCA[i] = mean(coefInterestM)
    CIsMCA[i,] = quantile(coefInterestM, c(0.025, 0.975))
}
# Fix names and row names
names(logitEstimatesCA) = names(logitModelSample$coefficients)
names(logitEstimatesMCA) = names(martinModelSample$coefficients)
rownames(CIsCA) = names(logitModelSample$coefficients)
rownames(CIsMCA) = names(martinModelSample$coefficients)
print(CIsCA)

# Age model alternate specification
altSpec31CA = glm(Elected ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + Age + as.factor(Party), family=binomial(link=logit), data=CAdtSerious)
############ END MODEL 3

############## MODEL 4: Cox PH ######
# Survival Data Sans Retirements
survivalSubset = CAdtSerious[CAdtSerious$Incumbent==1,c("ID","Name","Incumbent","TermsServed","CabinetNow","CabinetImportant","CabinetPM","Date","Elected","Party", "PartyInGovt")]
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
# Modal case for plotting survival plot
survModelPlotDataCA = data.frame(CabinetNow=c(0, 1), CabinetImportant=c(0,0), CabinetPM=c(0,0), PartyInGovt=c(1,1), pName=c("Lib","Lib"))

# Get CIs of estimates
ciMatrixSurvivalCA = list(cbind(summary(survivalModelCA)$conf.int[,3], 
                      summary(survivalModelCA)$conf.int[,4]))

# Diagnostics: Is the hazard proportional?
# Null hypothesis: Proportional hazard is appropriate; reject null: proportional hazard is not appropriate.
cox.zph(survivalModelCA)
# Some evidence PartyInGovt is not proportional--this isn't really a surpVirise--but Cabinet variable does seem appropriate.


############## MODEL 4: Cox PH with Synthetic Obs ####
# Read synthetic obs
syntheticObs = read.csv("Canada Data/SyntheticObs/synthOut.csv")
syntheticSurvive = syntheticObs %>% mutate(start=TermsServed-1, end=TermsServed, death=1, pName=as.character(Party)) %>% select(ID, start, end, death, CabinetNow, CabinetImportant, CabinetPM, PartyInGovt, pName)

# Combine with base data
combinedSurvivalDataCA = rbind(survivalDataCA, syntheticSurvive)

# Same model
combinedSurvivalDVCA = Surv(combinedSurvivalDataCA$start, combinedSurvivalDataCA$end, combinedSurvivalDataCA$death)
combinedModelCA = coxph(combinedSurvivalDVCA ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + pName, data=combinedSurvivalDataCA)
ciMatrixCombCA = list(cbind(summary(combinedModelCA)$conf.int[,3], summary(combinedModelCA)$conf.int[,4]))

# Same diagnostic
cox.zph(combinedModelCA)

############ APPENDIX: Decompose by Decade ######
decadeDecomposition = matrix(NA,nrow=7,ncol=12)
for(i in 5:11)
{
    minDate = 1900 + (10*i)
    maxDate = minDate + 10
    print(paste0("Decade: ",minDate,"-",maxDate))
    candSubset = CAdtSerious[as.character(CAdtSerious$Date)>paste0(minDate,"-01-01") & as.character(CAdtSerious$Date)<paste0(maxDate,"-01-01"),]
    print(nrow(candSubset))
    print(nrow(candSubset[candSubset$Incumbent==1,]))

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
write.table(decadeDecomposition, "Includes/decadeDecompositionCA.txt",quote=FALSE, sep="\t")
# Write as text, stargazer doesn't handle this well
# Have separate python script to crosswalk into LaTeX table.

##### Output data for composite table generation.
save(CAdtSerious, survivalModelCA, combinedModelCA, survModelPlotDataCA, 
           crossSecModelCA, crossSecMartinCA, controlShareModelCA, 
           firstDiffModelCA, martinFDModelCA, predProb0CA, predProb1CA,
           logitModelCA, logitMartinCA, logitEstimatesCA, logitEstimatesMCA,
           CIsCA, CIsMCA, ciMatrixSurvivalCA, ciMatrixCombCA, 
           survivalDataCA, combinedSurvivalDataCA, survivalDVCA, combinedSurvivalDVCA,
            altSpec11CA, altSpec12CA, altSpec13CA, altSpec14CA, 
            indFixEffectModelCA, altSpec31CA,
            file="Canada Data/Merge/OutputCA.RData")
