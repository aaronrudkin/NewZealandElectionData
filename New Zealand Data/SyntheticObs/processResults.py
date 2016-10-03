import sys
import re
import traceback
import math
import pprint
import time
import datetime
import csv 
startTime = time.time()

replay = 1 # Set to 1 to replicate, set to 0 to re-code

electionDates = ["1943-09-25", "1946-11-27", "1949-11-30", "1951-09-01", "1954-11-13", "1957-11-30", "1960-11-26", "1963-11-30", "1966-11-26", "1969-11-29", "1972-11-25", "1975-11-29", "1978-11-25", "1981-11-28", "1984-07-14", "1987-08-15", "1990-10-27", "1993-11-06", "1996-10-12", "1999-11-27", "2002-07-27", "2005-09-17", "2008-11-08", "2011-11-26", "2014-09-20", "9999-99-99"]

# Load cabinet
print "Loading cabinet appointments..."
cabinetLines = [x.strip() for x in open("cabinet.txt","r").read().split("\r") if len(x)]
appointees = []
for cL in cabinetLines:
	name, begin, end, important, pm = cL.split("\t")
	important = int(important)
	pm = int(pm)

	fixNames = name.replace('"','').replace('.','').strip().rsplit(", ",1)
	fixName = fixNames[0].upper()+", "+fixNames[1].strip()
	lastName = fixNames[0].upper().strip()
	firstName = fixNames[1].strip()

	if "/" in begin: # Excel dates
		beginMonth, beginDay, beginYear = begin.split("/")
		newBegin = beginYear+"-"+beginMonth.zfill(2)+"-"+beginDay.zfill(2)

		endMonth, endDay, endYear = end.split("/")
		newEnd = endYear+"-"+endMonth.zfill(2)+"-"+endDay.zfill(2)
	else: # Non-excel proper date
		newBegin = begin
		newEnd = end
	
	skip=0

	for a in appointees:
		if a["name"]==fixName and a["begin"]==begin and a["end"]==end and a["important"]==important:
			skip=1
			break
	if not skip:
		appointees.append({"name": fixName, "lastName": lastName, "firstname": firstName, "important": important, "begin": newBegin, "end": newEnd, "pm": pm})

print len(appointees), "cabinet appointees loaded..."

# Load my previous coding decisions.
try:
	useInputPtr = 0
	useInputSet = [int(x) for x in open("decisions.txt","r").read().split(", ")]
	print "Loaded previous classification inputs."
except:
	useInputSet = []
newInputSet = []

def takeInput(score, replay=0):
	global useInputSet, useInputPtr, newInputSet
	if replay==1:
		try:
			res = useInputSet[useInputPtr]
		except:
			print "Invalid saved data"
			sys.exit(-1)
		useInputPtr = useInputPtr+1
		return res
	else:
		result = ""
		try:
			prevRes = str(useInputSet[useInputPtr])
		except:
			prevRes = ""
		useInputPtr = useInputPtr+1
		if prevRes!="":
			result = raw_input("Accept as same? Previous choice was "+str(prevRes)+": ")
			if result=="":
				result = int(prevRes)
		else:
			result = ""
			while result=="":
				result = int(raw_input("Accept as same? "))

		if int(result)==1:
			newInputSet.append(1)
			return 1
		elif int(result)==0:
			newInputSet.append(0)
			return 0


def levenshtein(s1, s2):
	if len(s1) < len(s2):
		return levenshtein(s2, s1)

	if len(s2) == 0:
		return len(s1)

	previous_row = range(len(s2) + 1)
	for i, c1 in enumerate(s1):
		current_row = [i + 1]
		for j, c2 in enumerate(s2):
			insertions = previous_row[j + 1] + 1 # j+1 instead of j since previous_row and current_row are one character longer
			deletions = current_row[j] + 1       # than s2
			substitutions = previous_row[j] + (c1 != c2)
			current_row.append(min(insertions, deletions, substitutions))
		previous_row = current_row

	sim = 1 - (float(previous_row[-1]) / float(len(s1)))
	return sim 

def doLookup(memberData):
	pass

def loadNickname(filename):
	#print filename
	nicknames = open(filename,"r").read().lower().split("\n")
	nnPairs = {}
	i=0
	for nn in nicknames:
		if not len(nn):
			continue
		i=i+1
		if i==1:
			continue
		id, name, nickname = nn.split(", ")
		if name in nnPairs:
			nnPairs[name].append(nickname)
		else:
			nnPairs[name] = [nickname]
	return nnPairs

# Load member and party nicknames
print "Loading nicknames and party nicknames"
nicknames = loadNickname("nicknames.csv")

def checkNickname(nnDB, name1, name2):
	nnPairs = nnDB
	name1 = name1.lower()
	name2 = name2.lower()
	if name1 in nnPairs:
		for nn in nnPairs[name1]:
			if nn==name2:
				return 1
	if name2 in nnPairs:
		for nn in nnPairs[name2]:
			if nn==name1:
				return 1
	return 0


def scoreMatch(person1, person2):
	flags = []
	global partyNicknames, nicknames
	score = 0

	person1["firstname"] = person1["firstname"].lower().replace(",","")
	person2["firstname"] = person2["firstname"].lower().replace(",","")

	person1["firstname"] = person1["firstname"].strip()
	person2["firstname"] = person2["firstname"].strip()
	p1fns = [x for x in re.split("\. |\.| ",person1["firstname"]) if x != "," and len(x)>0]
	p2fns = [x for x in re.split("\. |\.| ",person2["firstname"]) if x != "," and len(x)>0]
	if person1["firstname"] == person2["firstname"] or person1["firstname"].replace("-"," ")==person2["firstname"] or person2["firstname"].replace("-"," ")==person1["firstname"]:
		score=score+35
		flags.append("full name match")
	elif (len(p1fns)==1 and len(p2fns)==2) or (len(p1fns)==2 and len(p2fns)==1):
		shorter = p1fns[0] if len(p1fns)==1 else p2fns[0]
		longer = p1fns if len(p1fns)==2 else p2fns
		if shorter==longer[0] or shorter==longer[1]:
			flags.append("1-to-2 name match")
			score=score+25
	else:
		if "." in person1["firstname"] and ". " not in person1["firstname"]:
			person1["firstname"] = person1["firstname"].replace(".",". ")
		if "." in person2["firstname"] and ". " not in person2["firstname"]:
			person2["firstname"] = person2["firstname"].replace(".",". ")
		
		scoreDivider = max(len(p1fns),len(p2fns))
		for j in range(0,len(p1fns)):
			matched = 0
			for k in range(0,len(p2fns)):
				if p1fns[j]==p2fns[k]:
					flags.append("name match")
					score = score + (35/scoreDivider)
					break
				elif len(p1fns[j])==1 and p1fns[j][0]==p2fns[k][0]:
					flags.append("initial match")
					score = score + (25/scoreDivider)
					break
				elif len(p2fns[k])==1 and p1fns[j][0]==p2fns[k][0]:
					flags.append("initial match")
					score = score + (25/scoreDivider)
					break
				elif "-" in p1fns[j] and not "-" in p2fns[k] and p1fns[j].split("-")[0]==p2fns[k]:
					flags.append("hyphen match")
					score = score + (30/scoreDivider)
					break
				elif "-" in p2fns[k] and not "-" in p1fns[j] and p2fns[k].split("-")[0]==p1fns[j]:
					flags.append("hyphen match")
					score = score + (30/scoreDivider)
					break
				else:
					nicknameMatch = checkNickname(nicknames, p1fns[j], p2fns[k])
					if nicknameMatch:
						flags.append("nickname match")
						score=score+(30/scoreDivider)
						break
					elif len(p1fns[j])>=3 and len(p2fns[k])>=3:
						if levenshtein(p1fns[j],p2fns[k])>=0.4:
							flags.append("partial levenshtein")
							score = score + int(levenshtein(p1fns[j],p2fns[k]) * (30/scoreDivider))
							break

	return (score, flags)


csvFile = open("synthOut.csv", "w")
writer = csv.writer(csvFile, quoting=csv.QUOTE_NONNUMERIC)
writer.writerow(['ID', 'Name', 'Date','Elected','CabinetNow','CabinetImportant','CabinetPM','TermsServed','Party', 'PartyInGovt'])

csvRead = open("synthIn.csv", "r")
reader = csv.reader(csvRead)
print "Beginning to iterate through election data."
i=0
for row in reader:
	i=i+1
	if i==1:
		continue

	if len(row)==0:
		break

	id, name, date, junk, termsServed, party = row

	# Remove junk from names
	name = name.replace("Rt. Hon. ","").replace("Hon. ","").replace("Hon.","").replace("Sir ","").replace("Lord ","")
	name = name.replace("Dr. ","").replace("R. H. ","").replace("Right ","").replace("Honorable ","")
	name = name.replace('"',"").replace("-"," ").replace("Le ","").replace(", K.B.C.,","").replace(", K.","")
	name = name.replace("K.C.M.G.C","").replace("Tres ","")
	name = name.replace("  "," ")
	name = name.strip()

	if ", " in name:
		lastname, firstname = [x.strip() for x in name.split(", ",1)]
		lastname = lastname.strip().replace(",","")
		firstname = firstname.strip().replace(".","")
	else:
		try:
			lastname, firstname = [x.strip() for x in name.split(" ",1)]
			lastname = lastname.strip().replace(",","")
			firstname = firstname.strip().replace(".","")
		except:
			print "ERROR IN DATA FILE"
			print line
			testVar = raw_input("Confirm and fix please.")
			lastname = name.strip()
			firstname = "?"
	lastname = lastname.upper()

	id = int(id)
	elected = 0
	termsServed = int(termsServed)+1
	party = str(party)
	date = str(date)
	servedNow = 0
	important = 0
	pm = 0

	# Which election are we on?
	eN=0
	for d in electionDates:
		if date<d:
			break
		else:
			eN=eN+1
			continue

	try:
		lastElecDate = date
	except:
		lastElecDate = date
	currElecDate = electionDates[eN]
	print i, date, lastElecDate, currElecDate, lastname, firstname

	# Cabinet match
	exactCabinetMatch = [x for x in appointees if x["lastName"].strip()==lastname.strip() and x["firstname"].strip()==firstname.strip() and (x["begin"]<currElecDate and x["end"]>lastElecDate)]
	if not len(exactCabinetMatch):
		familyCabinetMatch = [x for x in appointees if x["lastName"]==lastname and (x["begin"]<date and x["end"]>lastElecDate)]
		if not len(familyCabinetMatch):
			print i, lastname, date, currElecDate, "No family match"
			servedNow = 0
		elif len(familyCabinetMatch)==1:
			print i, lastname, date, currElecDate, "Family match 1"
			score, flags = scoreMatch({"firstname": firstname}, familyCabinetMatch[0])
			if score>=24:
				print i, "Perfect family match."
				pm = familyCabinetMatch[0]["pm"]
				important = familyCabinetMatch[0]["important"]
				servedNow = 1
			elif score>=18:
				print i, "Good family match."
				namePadLength = max(len(name),len(familyCabinetMatch[0]["name"]))+5
				print "=====\nCAB MATCH FAMILY HUMAN INTERVENTION REQUIRED. IS THIS THE SAME PERSON?\n====="
				print "Election: "+str(electionnum)+" on "+currElecDate+"."
				print "NEW NAME: "+name.ljust(namePadLength)+"\tPARTY: "+party+"\tINCUMBENT? "+str(incumbent)+"\tTERMS? "+str(incumbentTerms)
				print "OLD NAME: "+familyCabinetMatch[0]["name"].ljust(namePadLength)+"\tSCORE: "+str(score)
				print "EVIDENCE OF MATCH:"
				print flags

				samePerson = takeInput(score, replay)
				if samePerson:
					pm = familyCabinetMatch[0]["pm"]
					important = familyCabinetMatch[0]["important"]
					servedNow = 1
				else:
					pass
		else:
			print i, "Multiple family matches"
			maxScore = 0
			plausibleResults = []
			for match in familyCabinetMatch:
				score, flags = scoreMatch({"firstname": firstname}, match)
				if score>=20:
					match["score"] = score
					if len(plausibleResults) and score>plausibleResults[0]["score"]*1.5:
						plausibleResults = [match]
					else:
						plausibleResults.append(match)
					if score > maxScore:
						maxScore = score

			if len(plausibleResults)==0:
				print i, "Plausible Family Match: 0"
				pass
			elif len(plausibleResults)==1:
				if plausibleResults[0]["score"]>=24:
					print i, "Plausible Family Match: 1 Good"
					pm = plausibleResults[0]["pm"]
					important = plausibleResults[0]["important"]
					servedNow = 1
				else:
					print i, "Plausible Family Match: 1 OK"
					namePadLength = max(len(name),len(plausibleResults[0]["name"]))+5
					print "=====\nCAB MATCH MULTI HUMAN INTERVENTION REQUIRED. IS THIS THE SAME PERSON?\n====="
					print "Election: "+str(electionnum)+" on "+currElecDate+"."
					print "NEW NAME: "+name.ljust(namePadLength)+"\tPARTY: "+party+"\tINCUMBENT? "+str(incumbent)+"\tTERMS? "+str(incumbentTerms)
					print "OLD NAME: "+plausibleResults[0]["name"].ljust(namePadLength)+"\tSCORE: "+str(plausibleResults[0]["score"])
					print "EVIDENCE OF MATCH:"
					print flags
	
					samePerson = takeInput(score, replay)
					if samePerson:
						pm = plausibleResults[0]["pm"]
						important = plausibleResults[0]["important"]
						servedNow = 1
					else:
						pm = 0
						important = 0
						servedNow = 0
			else:
				pR = sorted(plausibleResults, key=lambda k: (k["score"], k["pm"], k["important"]), reverse=True)
				if pR[0]["score"]>=24:
					print i, "Plausible Family Match: Good"
					pm = pR[0]["pm"]
					important = pR[0]["important"]
					servedNow = 1
				else:
					print i, "Plausible Family Match: OK"
					namePadLength = max(len(name),len(plausibleResults[0]["name"]))+5
					print "=====\nMULTI pR HUMAN INTERVENTION REQUIRED. IS THIS THE SAME PERSON?\n====="
					print "Election: "+str(electionnum)+"."
					print "NEW NAME: "+name.ljust(namePadLength)+"\tPARTY: "+party+"\tINCUMBENT? "+str(incumbent)+"\tTERMS? "+str(incumbentTerms)
					print "OLD NAME: "+pR[0]["name"].ljust(namePadLength)+"\tSCORE: "+str(pR[0]["score"])

					samePerson = takeInput(score, replay)
					if samePerson:
						pm = pR[0]["pm"]
						important = pR[0]["important"]
						servedNow = 1
					else:
						pm = 0
						important = 0
						servedNow = 0						
	elif len(exactCabinetMatch)==1:
		print i, "Exact Match 1 Perfect"
		servedNow = 1
		pm = exactCabinetMatch[0]["pm"]
		important = exactCabinetMatch[0]["important"]
	else:
		print i, "Exact Match Many Best"
		bestMatch = sorted(exactCabinetMatch, key=lambda k: (k["pm"], k["important"]), reverse=True)[0]
		servedNow=1
		pm = bestMatch["pm"]
		important = bestMatch["important"]

	if currElecDate>="1940-04-01"  and currElecDate<="1949-12-12" and party=="Labour":
		partyInGovt = 1
	elif currElecDate>="1949-12-13"  and currElecDate<="1957-12-11" and party=="National":
		partyInGovt = 1
	elif currElecDate>="1957-12-12"  and currElecDate<="1960-12-11" and party=="Labour":
		partyInGovt = 1
	elif currElecDate>="1960-12-12"  and currElecDate<="1972-12-07" and party=="National":
		partyInGovt = 1
	elif currElecDate>="1972-12-08"  and currElecDate<="1975-12-12" and party=="Labour":
		partyInGovt = 1
	elif currElecDate>="1975-12-13"  and currElecDate<="1984-07-26" and party=="National":
		partyInGovt = 1
	elif currElecDate>="1984-07-26"  and currElecDate<="1990-11-01" and party=="Labour":
		partyInGovt = 1
	elif currElecDate>="1990-11-02"  and currElecDate<="1996-11-12" and party=="National":
		partyInGovt = 1
	elif currElecDate>="1996-11-13"  and currElecDate<="1999-12-04" and (party=="National" or party=="NZF"):
		partyInGovt = 1
	elif currElecDate>="1999-12-05"  and currElecDate<="2008-11-18" and (party=="Labour" or party=="Alliance"):
		partyInGovt = 1
	elif currElecDate>="2008-11-19" and party=="National":
		partyInGovt = 1
	else:
		partyInGovt=0
		
	writer.writerow([id, name, currElecDate, elected, servedNow, important, pm, termsServed, party, partyInGovt])


newInputSet = [str(x) for x in newInputSet]
if len(newInputSet):
	strVar = ", ".join(newInputSet)
	with open("outputCodingDecisions.txt","w") as f:
		f.write(strVar)

endTime = time.time()
if endTime-startTime>60:
	seconds = round((endTime-startTime)%60)
	minutes = math.floor((endTime-startTime)/60)
	print "Total execution time: "+str(minutes)+" minutes, "+str(seconds)+" seconds"
else:
	print "Total execution time: "+str(round(endTime-startTime))+" seconds"
