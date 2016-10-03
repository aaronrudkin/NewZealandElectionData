import requests
import bs4
import sys
import unidecode
from datetime import datetime

with open("cabinet.net","w") as f:
	f.close()

def cleanSpacing(text):
	return " ".join(text.split()).strip()

def fixDate(ds):
	if ds=="present":
		ds = "18 Oct. 2015"

	day, month, year = ds.split(" ")
	if month=="Sept.":
		ds = day+" Sep "+year
	if len(day)<2:
		ds = "0"+ds
	
	try:
		ds_new = datetime.strptime(ds,"%d %b %Y")
	except:
		try:
			ds_new = datetime.strptime(ds,"%d %b. %Y")
		except:
			try:
				ds_new = datetime.strptime(ds,"%d %B %Y")
			except:
				print "huh? dead on "+ds
				sys.exit(-1)

	if int(year)<1900:
		return ds_new.isoformat(" ").strip().split(" ")[0]
	else:
		return ds_new.strftime("%Y-%m-%d")

def writeLine(myStr):
	line = unidecode.unidecode(myStr)
	print line
	with open("cabinet.txt","a") as f:
		f.write(line)
		f.write("\n")

# 17-28 are what i'm interested in
posNameSet = []
baseURL = "http://www.pco-bcp.gc.ca/mgm/dtail.asp?lang=eng&mbtpid=1&mstyid="
ministry = 1
for x in [i for j in (range(1,27),range(31,33)) for i in j]:
	minSet = {}
	#print "Getting ministry "+str(x)
	finalURL = baseURL+str(x)
	reqFile = requests.get(finalURL).text.encode("iso-8859-1")
	parser = bs4.BeautifulSoup(reqFile,"html5lib")
	beginSet = parser.find("div",{"id": "wb-main-in"})
	partyName = beginSet.find_all("p")[0].find_all("strong")[0].contents[0].strip()
	dateLength = beginSet.find_all("p")[0].contents[-1].strip()
	#print partyName
	#print dateLength
	beginGovDate, endGovDate = [fixDate(d) for d in dateLength.split(" - ")]

	ministerTable = beginSet.find("table",{"class": "detail"}).find_all("tr")
	k = 0
	for row in ministerTable:
		k=k+1
		if k<3:
			continue
		else:
			cellSet = row.find_all("td")
			if cellSet[0]["headers"][0]=="PositionTitle_1":
				posName = cellSet[0].find("strong").contents[0]
				hasPPosName = 0
				posNameSet.append(posName)
			else:
				name, dates = cellSet
				personName = cleanSpacing(name.contents[0].strip().replace("Rt Hon. ","").replace("Hon. ","").replace("Sir ",""))
				if personName=="Vacant":
					continue

				try:
					fixFirst, fixLast = personName.rsplit(" ",1)
				except:
					print personName
					sys.exit(-1)
				if len(fixLast)<2:
					fixFirst, fixLast, fixJunk = personName.rsplit(" ",2)

				fixName = fixLast+", "
				firstNames = fixFirst.split(" ")
				for myName in firstNames:
					fixName+=myName[0].upper()+". "
				fixName = fixName.strip()

				if len(name.contents)>1:
					emType = name.find("em").contents[0]
					try:
						emType = emType.strip()
					except:
						emType = ""
				else:
					emType = ""

				dateTime = dates.contents[0]
				beginTime, endTime = dateTime.split(" - ")
				beginTime = beginTime.strip()
				endTime = endTime.strip()
				if len(endTime)<2:
					endTime = "18 Oct. 2015"	

				tryBegin = fixDate(beginTime)
				tryEnd = fixDate(endTime)

				if not personName in minSet:
					minSet[personName]= [tryBegin, tryEnd]
				if personName in minSet:
					if tryBegin >= minSet[personName][1]:
						minSet[personName][1] = tryEnd
					if tryBegin < minSet[personName][0] and tryEnd <= minSet[personName][1]:
						minSet[personName][0] = tryBegin
						
					

				if tryBegin<=beginGovDate and tryEnd>=endGovDate:
					wholeTime = 1
				else:
					wholeTime = 0

				if personName!="Vacant" and emType!="Senator" and emType!="Acting Minister" and "Senate" not in posName:
					#if hasPPosName==0:
					#	print "Position name: "+posName
					#	hasPPosName=1
					writeLine(personName+"\t"+fixName+"\t"+posName+"\t"+tryBegin+"\t"+tryEnd+"\t"+str(wholeTime)+"\t"+str(ministry))

	#print minSet

	ministry = ministry + 1
