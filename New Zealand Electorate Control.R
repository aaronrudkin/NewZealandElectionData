rm(list=ls())
library(dplyr)
library(weights)

# Plotting line segments from baseX to lastX, but only where values are in validX
plotLineChunks = function(validX, baseX, lastX, y, col, electorate)
{
    validX = validX[validX>=baseX & validX<=lastX]
    if(baseX<ceiling(baseX))
    {
        lines(c(2+baseX,2+ceiling(baseX)),c(y,y),col=pCol,lwd=1.5)
        baseX=ceiling(baseX)
    }
    for(i in baseX+1:lastX)
    {
        if(length(validX[validX==i-1])>0 & length(validX[validX==i])>0)
        {
            lines(c(2+i-1, 2+i), c(y,y), col=pCol,lwd=1.5)
        }
    }
    if(lastX>floor(lastX))
    {
        lines(c(2+floor(lastX),2+lastX),c(y,y),col=pCol,lwd=1.1)
    }
}

# Colour mappings for plot
partyCol = function(party)
{
    if(party=="National") { return("blue") }
    else if(party=="Labour") { return("red") }
    else if(party=="GP" | party=="Alliance" | party=="NL" | party=="Progressive" | party=="Social Credit") { return("orange") }
    else if(party=="NZF" | party=="ACT" | party=="I") { return("black") }
    else { print(party)
            return("purple") }
}

# Read the election data
dt = read.csv("New Zealand Data/Merge/output.csv", header=TRUE)
stopifnot(nrow(dt)==10267) # Replication, verify all rows loaded exactly

# Iterate through every election
electionDates = c("1946-11-27","1949-11-30", "1951-09-01", "1954-11-13", "1957-11-30", "1960-11-26", "1963-11-30", "1966-11-26", "1969-11-29", "1972-11-25", "1975-11-29", "1978-11-25", "1981-11-28", "1984-07-14", "1987-08-15", "1990-10-27", "1993-11-06", "1996-10-12", "1999-11-27", "2002-07-27", "2005-09-17", "2008-11-08", "2011-11-26", "2014-09-20","2017-12-31")
dt$Electionnum = 0
for(i in 1:(length(electionDates)-1))
{
    print(electionDates[i])
    # Number the elections
    tryCatch({
        dt[dt$Date==electionDates[i],]$Electionnum = i
    })
    # Number the by-elections
    if(i<length(electionDates))
    {
        tryCatch({
            dt[as.character(dt$Date)>electionDates[i] & as.character(dt$Date)<electionDates[i+1],]$Electionnum = i+0.5
        },error=function(e){})
    }
}

# Pull only people elected through the electorate
elected = dt[dt$ElectedElectorate==1,]

# Histogram of electoral competition
electorateHist = elected %>% group_by(Electorate) %>% mutate(lagParty=lag(Party)) %>% summarise(numRows = n()-1, numTurnover=sum(Party!=lagParty & !is.na(lagParty))) %>% filter(numRows>1) %>% mutate(percTurnover = numTurnover/numRows)

# Output histogram
pdf("New Zealand Data/Merge/electorateHist.pdf",width=7,height=4)
par(mar=c(5.1,3.1,2.1,2.1))
wtd.hist(x=electorateHist$percTurnover, w=electorateHist$numRows, 
         xlab="Proportion of Elections that Result in Turnover", 
         ylab="", 
         yaxt="n", main="",xlim=c(0,1))
mtext("Weighted Frequency\n(Number of Electorates)",side=2,line=1)
dev.off()

# Summary stat
summary(electorateHist$percTurnover)

# Appendix 3 plot
whichElectorates = elected %>% group_by(Electorate) %>% summarise(numRows = n(), numLab = sum(Party=="Labour"), numParties=length(unique(Party))) %>% mutate(labControl=numLab/numRows) %>% filter(numParties>1) %>% filter(numRows>8) %>% select(Electorate, labControl, numLab, numRows)
electorateNames = whichElectorates[order(whichElectorates$labControl, whichElectorates$numRows, decreasing=TRUE),]$Electorate
pdf("New Zealand Data/Merge/electorateControl.pdf")
    par(mar=c(3.5,2,1,2))
    plot(c(20),c(50),type="n",
         #main="NZ Contested Electorate Control over Time", 
         xlab="",ylab="",
         xlim=c(0,28), ylim=c(0,115),xaxt="n",yaxt="n",axes=F)
    
    # Loop through every electorate
    for(i in 1:length(electorateNames))
    {
        # Plot label
        text(3,110-2*i,electorateNames[i],cex=0.4,pos=2)
        
        # Get all winners by election number
        winners = elected[elected$Electorate==electorateNames[i],] %>% arrange(Electionnum)
        # Which elections did the electorate exist for
        exist = winners$Electionnum
        
        # Who was the first winner
        baseWinner = winners[1,]$Party
        baseID = winners[1,]$ID
        pCol = partyCol(baseWinner)
        baseNumber=1
        
        # Loop through every winner
        for(j in 1:nrow(winners))
        {
            # If there's a party+person switch, update our info and plot what we have
            if(winners[j,]$Party!=baseWinner & baseID!=winners[j,]$ID)
            {
                pCol = partyCol(baseWinner)
                plotLineChunks(exist, winners[baseNumber,]$Electionnum, winners[j,]$Electionnum, 110-2*i, pCol,electorateNames[i])
                points(2+winners[baseNumber,]$Electionnum, 110-2*i, col=pCol, pch="|", cex=0.8)
                baseNumber = j            
                baseWinner = winners[j,]$Party
                baseID = winners[j,]$ID
            }
        }
        # Last unresolved chunk
        pCol = partyCol(baseWinner)
        plotLineChunks(exist, winners[baseNumber,]$Electionnum, winners[nrow(winners),]$Electionnum, 110-2*i, pCol,electorateNames[i])
        points(2+winners[baseNumber,]$Electionnum, 110-2*i, col=pCol, pch="|", cex=0.8)
    }
    # Legend text, custom
    text(2.7,115,"Legend:",cex=0.7,pos=4,font=2)
    points(6.0,115,col="red",pch="-",cex=1.5)
    text(6.2,115,"Labour",cex=0.7,pos=4)
    points(11.0,115,col="blue",pch="-",cex=1.5)
    text(11.2,115,"National",cex=0.7,pos=4)
    points(16.0,115,col="orange",pch="-",cex=1.5)
    text(16.2,115,"Minor/Left",cex=0.7,pos=4)
    points(21.0,115,col="black",pch="-",cex=1.5)
    text(21.2,115,"Minor/Right", cex=0.7, pos=4)
    
    # Legend box
    lines(c(2.6,25.4),c(112.7,112.7),col="black")
    lines(c(2.6,25.4),c(117.7,117.7),col="black")
    lines(c(2.6,2.6),c(112.7,117.7),col="black")
    lines(c(25.4,25.4),c(112.7,117.7),col="black")
    text(3,110,"Electorate",cex=0.4,pos=2,font=2)
    
    # Axis
    axis(1, at=seq(3,27,3), labels=substring(electionDates[seq(1,25,3)],1,4),cex.axis=0.7)
    mtext("Election Year",1,2.5)
    mtext("Labour Control",2,0.5)
    dev.off()