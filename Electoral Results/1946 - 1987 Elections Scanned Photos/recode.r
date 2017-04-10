library(dplyr)
setwd("~/Dropbox/Field Paper/New Zealand Data/1946 - 1987 Elections")
dat = read.csv("NZ Data Typed.txt", header=TRUE, sep="\t")

# Recode dates into exact dates
dat$exactDate = dat$Date
dat[dat$Date==1946,]$exactDate="1946-11-27"
dat[dat$Date==1949,]$exactDate="1949-11-30"
dat[dat$Date==1951,]$exactDate="1951-09-01"
dat[dat$Date==1954,]$exactDate="1954-11-13"
dat[dat$Date==1957,]$exactDate="1957-11-30"
dat[dat$Date==1960,]$exactDate="1960-11-26"
dat[dat$Date==1963,]$exactDate="1963-11-30"
dat[dat$Date==1966,]$exactDate="1966-11-26"
dat[dat$Date==1969,]$exactDate="1969-11-29"
dat[dat$Date==1972,]$exactDate="1972-11-25"
dat[dat$Date==1975,]$exactDate="1975-11-29"
dat[dat$Date==1978,]$exactDate="1978-11-25"
dat[dat$Date==1981,]$exactDate="1981-11-28"
dat[dat$Date==1984,]$exactDate="1984-07-14"
dat[dat$Date==1987,]$exactDate="1987-08-15"

dat[dat$Riding=="Avon" & dat$Date==1947,]$exactDate = "1947-05-28"
dat[dat$Riding=="Mount Albert" & dat$Date==1947,]$exactDate = "1947-09-24"
dat[dat$Riding=="Westland" & dat$Date==1947,]$exactDate = "1947-12-03"
dat[dat$Riding=="Brooklyn" & dat$Date=="1951.01",]$exactDate = "1951-02-17"
dat[dat$Riding=="Brooklyn" & dat$Date=="1951.02",]$exactDate = "1951-09-01"
dat[dat$Riding=="Dunedin North" & dat$Date==1953,]$exactDate = "1953-12-12"
dat[dat$Riding=="Onehunga" & dat$Date==1953,]$exactDate = "1953-12-19"
dat[dat$Riding=="Onslow" & dat$Date=="1954.01",]$exactDate = "1954-07-07"
dat[dat$Riding=="Onslow" & dat$Date=="1954.02",]$exactDate = "1954-11-13"
dat[dat$Riding=="Patea" & dat$Date=="1954.01",]$exactDate = "1954-07-31"
dat[dat$Riding=="Patea" & dat$Date=="1954.02",]$exactDate = "1954-11-13"
dat[dat$Riding=="Riccarton" & dat$Date==1956,]$exactDate = "1954-10-27"
dat[dat$Riding=="Bay of Plenty" & dat$Date=="1957.01",]$exactDate = "1957-04-06"
dat[dat$Riding=="Bay of Plenty" & dat$Date=="1957.02",]$exactDate = "1957-11-30"
dat[dat$Riding=="Hamilton" & dat$Date==1959,]$exactDate = "1959-05-02"
dat[dat$Riding=="Hurunui" & dat$Date==1961,]$exactDate = "1961-06-10"
dat[dat$Riding=="Waitaki" & dat$Date==1962,]$exactDate = "1962-03-10"
dat[dat$Riding=="Buller" & dat$Date==1962,]$exactDate = "1962-07-07"
dat[dat$Riding=="Timaru" & dat$Date==1962,]$exactDate = "1962-07-21"
dat[dat$Riding=="Otahuhu" & dat$Date=="1963.01",]$exactDate = "1963-03-16"
dat[dat$Riding=="Northern Maori" & dat$Date=="1963.01",]$exactDate = "1963-03-16"
dat[dat$Riding=="Northern Maori" & dat$Date=="1963.02",]$exactDate = "1963-11-30"
dat[dat$Riding=="Grey Lynn" & dat$Date=="1963.01",]$exactDate = "1963-05-18"
dat[dat$Riding=="Grey Lynn" & dat$Date=="1963.02",]$exactDate = "1963-11-30"
dat[dat$Riding=="Southern Maori" & dat$Date==1967,]$exactDate = "1967-03-11"
dat[dat$Riding=="Fendalton" & dat$Date==1967,]$exactDate = "1967-04-15"
dat[dat$Riding=="Petone" & dat$Date==1967,]$exactDate = "1967-04-15"
dat[dat$Riding=="Eastern Maori" & dat$Date==1967,]$exactDate = "1967-08-12"
dat[dat$Riding=="Palmerston North" & dat$Date==1967,]$exactDate = "1967-12-02"
dat[dat$Riding=="Hutt" & dat$Date==1968,]$exactDate = "1968-08-03"
dat[dat$Riding=="Marlborough" & dat$Date==1970,]$exactDate = "1970-02-21"
dat[dat$Riding=="Sydenham" & dat$Date==1974,]$exactDate = "1974-11-02"
dat[dat$Riding=="Nelson" & dat$Date==1976,]$exactDate = "1976-02-28"
dat[dat$Riding=="Mangere" & dat$Date==1977,]$exactDate = "1977-03-26"
dat[dat$Riding=="Pahiatua" & dat$Date==1977,]$exactDate = "1977-04-30"
dat[dat$Riding=="Rangitikei" & dat$Date=="1978.01",]$exactDate = "1978-02-18"
dat[dat$Riding=="Rangitikei" & dat$Date=="1978.02",]$exactDate = "1978-11-25"
dat[dat$Riding=="Christchurch Central" & dat$Date=="1979",]$exactDate = "1979-08-18"
dat[dat$Riding=="Northern Maori" & dat$Date=="1980",]$exactDate = "1980-06-07"
dat[dat$Riding=="Onehunga" & dat$Date=="1980",]$exactDate = "1980-06-07"
dat[dat$Riding=="East Coast Bays" & dat$Date=="1980",]$exactDate = "1980-09-06"
dat[dat$Riding=="Timaru" & dat$Date=="1985",]$exactDate = "1985-06-15"

# Create Elected variable
dat = dat %>% group_by(Riding, Date) %>% mutate(Elected = as.numeric(Votes == max(Votes)))

# Verify the elected variable was created right
sanity_check = dat %>% group_by(Riding, exactDate) %>% summarize(numElected = sum(Elected))
table(sanity_check$numElected)
sanity_check2 = dat %>% group_by(exactDate) %>% summarize(numElected = sum(Elected))
View(sanity_check2)


write.csv(dat, "NZ Data Typed.txt", quote=FALSE, sep="\t", eol="\r")

