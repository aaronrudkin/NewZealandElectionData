rm(list=ls())
library(survival) # Survival modeling
library(stargazer) # Output tables
library(sandwich) # Robust SEs
library(lmtest) # Robust SEs

####### MODIFY DATA PATHS ON LINES 19 AND 21 BEFORE RUNNING CODE.

####### NOTES ON HOW PRINTED FIGURES DIFFER FROM REPLICATION CODE
# 1) Coefficients of interest are bolded for visual effect
# 2) New Zealand discussion graph is generated in New Zealand 
##################################################################

#setwd("~/Dropbox/Field Paper/Canada Data/Merge/")
#### Generate figures and plots from stashed RData
setwd("../Canada Data/Merge/")
load("OutputCA.RData")
setwd("../../New Zealand Data/Merge/")
load("OutputNZ.RData")
setwd("../../Includes/")
##### 

# Histogram of party tenure, Canada
pdf("histogramTenureCA.pdf")
hist(CAdtSerious[CAdtSerious$Incumbent==1,]$TermsServed, xlab="Terms Served", yaxt="n", ylab="", main="Histogram of Incumbent Tenure (Canada)")
dev.off()

pdf("histogramTenureNZ.pdf")
hist(NZdtSerious[NZdtSerious$Incumbent==1,]$TermsServed, xlab="Terms Served", yaxt="n", ylab="", main="Histogram of Incumbent Tenure (NZ)")
dev.off()


# Survival Model sans synth obs, Canada
pdf("survivalModelCA.pdf")
plot(survfit(survivalModelCA, newdata=survModelPlotDataCA), conf.int=TRUE, main="Canada: Survival Model (No Synthetic Obs)", xlab="Terms in Government", ylab="Survival Rate", col=c("black","red"))
legend(x=8,y=0.95,c("Backbencher","Cabinet"),col=c("black","red"),lty=1)
dev.off()

# Survival Model with synth obs, Canada
pdf("combinedModelCA.pdf")
plot(survfit(combinedModelCA, newdata=survModelPlotDataCA), conf.int=TRUE, main="Canada: Survival Model (Synthetic Obs)", xlab="Terms in Government", ylab="Survival Rate", col=c("black","red"))
legend(x=8,y=0.95,c("Backbencher","Cabinet"),col=c("black","red"),lty=1)
dev.off()

# Survival Model sans synth obs, Canada
pdf("survivalModelNZ.pdf")
plot(survfit(survivalModelNZ, newdata=survModelPlotDataNZ), conf.int=TRUE, main="NZ: Survival Model (No Synthetic Obs)", xlab="Terms in Government", ylab="Survival Rate", col=c("black","red"))
legend(x=8,y=0.95,c("Backbencher","Cabinet"),col=c("black","red"),lty=1)
dev.off()

# Survival Model with synth obs, Canada
pdf("combinedModelNZ.pdf")
plot(survfit(combinedModelNZ, newdata=survModelPlotDataNZ), conf.int=TRUE, main="NZ: Survival Model (Synthetic Obs)", xlab="Terms in Government", ylab="Survival Rate", col=c("black","red"))
legend(x=8,y=0.95,c("Backbencher","Cabinet"),col=c("black","red"),lty=1)
dev.off()

# Table 1: Cross-sectional models.
stargazer(crossSecModelCA, crossSecMartinCA, crossSecModelNZ, crossSecMartinNZ,
          se=list(coeftest(crossSecModelCA, sandwich)[,2], coeftest(crossSecMartinCA, sandwich)[,2],
                  coeftest(crossSecModelNZ, sandwich)[,2], coeftest(crossSecMartinNZ, sandwich)[,2]), 
          title="Cross-Sectional OLS",ci=TRUE,out="model1.tex", 
          column.labels=c("CA Main", "CA Martin", "NZ Main", "NZ Martin"), column.separate=c(1,1), 
          notes=c("Additional party fixed effects suppresssed","CIs from Heteroskedasticity-robust standard error estimates"), 
          omit=c(".*B.Q.", ".*CA", ".*CCF", ".*N.D.P.", ".*Ref.",".*ACT",".*Alliance",".*GP",".*Mana",".*Maori",".*NL",".*NZF",".*Progressive",".*Social Credit",".*UFNZ"), 
          covariate.labels=c("Cabinet Now", "Cab. Important", "Prime Minister", "Party in Gov't", "Terms Served", "Incumbent", "Party In Gov't", "Conservative FE", "Liberal FE", "Progressive Cons. FE","Labour FE","National FE"), 
          no.space=TRUE, digits=2, omit.stat=c("f","rsq","res.dev","ser"),
          table.placement = "!htb")

# Table 2: Control for past performance
stargazer(controlShareModelCA, firstDiffModelCA, martinFDModelCA, 
          controlShareModelNZ, firstDiffModelNZ, martinFDModelNZ,
          se=list(coeftest(controlShareModelCA, sandwich)[,2], 
                  coeftest(firstDiffModelCA, sandwich)[,2],
                  coeftest(martinFDModelCA, sandwich)[,2],
                  coeftest(controlShareModelNZ, sandwich)[,2], 
                  coeftest(firstDiffModelNZ, sandwich)[,2],
                  coeftest(martinFDModelNZ, sandwich)[,2]), 
          dep.var.labels=c("VotePct", "$\\Delta$ VotePct", "VotePct", "$\\Delta$ VotePct"),
          title="Past Performance Models",ci=TRUE,out="model2.tex", 
          column.labels=c("CA Control", "CA $\\Delta$", "CA Martin", "NZ Control", "NZ $\\Delta$", "NZ Martin"), 
          column.separate=c(1,1,1), 
          notes=c("Additional party fixed effects suppresssed","CIs from Heteroskedasticity-robust standard error estimates"), 
          omit=c(".*B.Q.", ".*CA", ".*CCF", ".*N.D.P.", ".*Ref.",".*ACT",".*Alliance",".*GP",".*Mana",".*Maori",".*NL",".*NZF",".*Progressive",".*Social Credit",".*UFNZ"), 
          covariate.labels=c("Cabinet Now", "Cab. Important", "Prime Minister", "Party in Gov't", "Terms Served", "Conservative FE", "Liberal FE", "Progressive Cons. FE","Labour FE", "National FE", "Prev. VotePct"), 
          no.space=TRUE, digits=2, omit.stat=c("f","rsq","res.dev","ser"),
          table.placement = "!htb")

# Modal example for Canadian logit interpretation
# Modal example; how does the first-term incumbent Liberal do?
print(paste("Mean predicted probability for noncabinet example:", mean(predProb0CA)))
print(paste("CI for noncabinet example: ",quantile(predProb0CA, c(0.025, 0.975))))
print(paste("Mean predicted probability for cabinet example:",mean(predProb1CA)))
print(paste("CI for cabinet example: ",quantile(predProb1CA, c(0.025, 0.975))))
print(paste("Mean delta: ",mean(predProb1CA-predProb0CA)))
print(paste("CI for delta: ",quantile(predProb1CA-predProb0CA,c(0.025,0.975))))

# Table 3: Cross-Sectional Logits
stargazer(logitModelCA, logitMartinCA, logitModelNZ, logitMartinNZ, title="Cross-Sectional Logit",
          out="model3.tex", column.labels=c("CA Main", "CA Martin", "NZ Main", "NZ Martin"), ci=TRUE, 
          column.separate=c(1,1, 1, 1), notes=c("Additional party fixed effects suppresssed","Estimates and CIs from 1000 nonparametric bootstrap samples"), 
          omit=c(".*B.Q.", ".*CA", ".*CCF", ".*N.D.P.", ".*Ref.",".*ACT",".*Alliance",".*GP",".*Mana",".*Maori",".*NL",".*NZF",".*Progressive",".*Social Credit",".*UFNZ"), 
          covariate.labels=c("Cabinet Now", "Cab. Important", "Prime Minister", "Party in Gov't", "Terms Served", "Incumbent", "Conservative FE", "Liberal FE", "Progressive Cons. FE", "Labour FE", "National FE"), 
          no.space=TRUE, digits=2, omit.stat=c("f","rsq","res.dev","ll","aic"),
          coef=list(logitEstimatesCA, logitEstimatesMCA, logitEstimatesNZ, logitEstimatesMNZ),
          ci.custom = list(CIsCA, CIsMCA, CIsNZ, CIsMNZ),
          dep.var.labels=c("Pr(Elected)"),
          table.placement = "!htb")
          
# This table is marginally too wide--I manually edit the .tex file to shrink it by 10%

# Table 4: Survival Model
ciMatrixSurvivalNZ[[1]][3,2] = 0.000
ciMatrixSurvivalNZ[[1]][12,2] = 0.000
# Stargazer will not properly plot the original data due to data errors in Prime Minister and a minor
# party fixed effect we suppress. I manually override these effects to 0. Essentially, no Prime Minister
# ever loses re-election in the dataset and so the model cannot meaningfully interpret the covariate.
stargazer(survivalModelCA, survivalModelNZ, title="Survival Analysis",
          ci=TRUE,out="model4.tex", 
          column.labels=c("CA Survival", "NZ Survival"),
          notes=c("Additional party fixed effects suppresssed"), 
          omit=c(".*B.Q.", ".*CA", ".*CCF", ".*N.D.P.", ".*Ref.",".*ACT",".*Alliance",".*GP",".*Mana",".*Maori",".*NL",".*NZF",".*Progressive",".*Social Credit",".*UFNZ"), 
          covariate.labels=c("Cabinet Now", "Cab. Important", "Prime Minister", "Party In Gov't", "Conservative FE", "Liberal FE", "Progressive Cons. FE", "Labour FE", "National FE"), 
          no.space=TRUE, digits=2, 
          omit.stat=c("rsq","ll", "wald", "lr", "max.rsq", "logrank"), 
          dep.var.labels=c("Survival Hazard", "Survival Hazard"),
          coef=list(summary(survivalModelCA)$conf.int[,1], summary(survivalModelNZ)$conf.int[,1]), 
          ci.custom=list(ciMatrixSurvivalCA[[1]], ciMatrixSurvivalNZ[[1]]),
          p=list(summary(survivalModelCA)$coefficients[,5], summary(survivalModelNZ)$coefficients[,5]),
          column.separate=c(1,1),
          table.placement = "!htb")

# Table 5: Survival Model with Synth Obs
stargazer(combinedModelCA, combinedModelNZ, title="Survival Analysis w/ Synthetic Obs",
          ci=TRUE,out="model5.tex", 
          notes=c("Additional party fixed effects suppresssed"), 
          column.labels=c("CA Survival", "NZ Survival"),
          omit=c(".*B.Q.", ".*CA", ".*CCF", ".*N.D.P.", ".*Ref.",".*ACT",".*Alliance",".*GP",".*Mana",".*Maori",".*NL",".*NZF",".*Progressive",".*Social Credit",".*UFNZ"), 
          covariate.labels=c("Cabinet Now", "Cab. Important", "Prime Minister", "Party In Gov't", "Conservative FE", "Liberal FE", "Progressive Cons. FE", "Labour FE", "National FE"), 
          no.space=TRUE, digits=2, 
          omit.stat=c("rsq","ll", "wald", "lr", "max.rsq", "logrank"), 
          dep.var.labels=c("Survival Hazard", "Survival Hazard"),
          coef=list(summary(combinedModelCA)$conf.int[,1], summary(combinedModelNZ)$conf.int[,1]), 
          ci.custom=list(ciMatrixCombCA[[1]], ciMatrixCombNZ[[1]]),
          p=list(summary(combinedModelCA)$coefficients[,5], summary(combinedModelNZ)$coefficients[,5]),
          table.placement = "!htb")



# Appendix: Alternate specifications
print("Beginning Appendix generation. This may take up to 1-2 minutes.")

# This objects take some time to calculate and I want to run them outside stargazer to avoid crashing issues
ridingFE = coeftest(altSpec13CA,sandwich)
ridingFECoef = ridingFE[1:14,1]
ridingFEse = ridingFE[1:14,2]
ridingFEp = ridingFE[1:14, 4]

indFixEffect = coeftest(indFixEffectModelCA, sandwich)
indFixEffectCoef = indFixEffect[1:6,1]
indFixEffectse = indFixEffect[1:6,2]
indFixEffectp = indFixEffect[1:6,4]
##### Hack to make stargazer take non-standard model objects
# stargazer doesn't want to take the overrided coefficients for a few of the models
# so I create an LM with the same functional form as the model it's replacing, put
# that in stargazer, and then override data manually. This does not affect any
# presented results.
stargazerOverride = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed + as.factor(Party), data=CAdtSerious)
stargazerOverride2 = lm(VotePct ~ CabinetNow + CabinetImportant + CabinetPM + PartyInGovt + TermsServed, data=CAdtSerious)
rownames(altSpec11CA) = names(altSpec13CA$coefficients[1:14])
#### 

# Alternate specifications, appendix 3.
stargazer(list(stargazerOverride, altSpec12CA, stargazerOverride, altSpec14CA, stargazerOverride2, altSpec31CA),
          se=list(altSpec11CA[,2], 
                  coeftest(altSpec12CA, sandwich)[,2],
                  ridingFEse,
                  coeftest(altSpec14CA, sandwich)[,2], 
                  indFixEffectse,
                  summary(altSpec31CA)$coef[,2]), 
          coef=list(altSpec11CA[,1], 
                    NULL, 
                    ridingFECoef, 
                    NULL, 
                    indFixEffectCoef, 
                    NULL),
          p=list(altSpec11CA[,4], 
                 NULL,
                 ridingFEp,
                 NULL, 
                 indFixEffectCoef,
                 NULL),
          dep.var.labels=c("VotePct", "Pr(Elected)"),
          title="Alternate Specifications",ci=TRUE,out="appendix3.tex", 
          column.labels=c("Cluster SE", "Incumbent", "Riding FE", "Age OLS", "Ind. Fix Effect", "Age Logit"), 
          #column.separate=c(4,1,1), 
          notes=c("Additional party fixed effects suppresssed","OLS CIs from Heteroskedasticity-robust standard error estimates"), 
          omit=c(".*B.Q.", ".*CA", ".*CCF", ".*N.D.P.", ".*Ref.",".*ACT",".*Alliance",".*GP",".*Mana",".*Maori",".*NL",".*NZF",".*Progressive",".*Social Credit",".*UFNZ", "(ID)",".*as.factor(Riding)"), 
          covariate.labels=c("Cabinet Now", "Cab. Important", "Prime Minister", "Party in Gov't", "Terms Served", "Incumbent", "Age", "Conservative FE", "Liberal FE","P.C. FE"), 
          no.space=TRUE, digits=2, omit.stat=c("all"),
          table.placement = "!h")

# Appendix 4, New Zealand logit pre/post mmp
stargazer(preMMP, postMMP, title="Cross-Sectional Logit (NZ)",
          out="appendix4.tex", column.labels=c("Pre-MMP", "Post-MMP"), ci=TRUE, 
          column.separate=c(1,1, 1, 1), notes=c("Additional party fixed effects suppresssed"), 
          omit=c(".*B.Q.", ".*CA", ".*CCF", ".*N.D.P.", ".*Ref.",".*ACT",".*Alliance",".*GP",".*Mana",".*Maori",".*NL",".*NZF",".*Progressive",".*Social Credit",".*UFNZ"), 
          covariate.labels=c("Cabinet Now", "Cab. Important", "Prime Minister", "Party in Gov't", "Terms Served", "Labour FE", "National FE"), 
          no.space=TRUE, digits=2, omit.stat=c("f","rsq","res.dev","ll","aic"),
          dep.var.labels=c("Pr(Elected)"),
          table.placement = "!htb")
