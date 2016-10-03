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

electionDates = ["0000-00-00", "1867-08-07","1872-07-20","1874-01-22","1878-09-17","1882-06-20","1887-02-22","1891-03-05","1896-06-23","1900-11-07","1904-11-03","1908-10-26","1911-09-21","1917-12-17","1921-12-06","1925-10-29","1926-09-14","1930-07-28","1935-10-14","1940-03-26","1945-06-11","1949-06-27","1953-08-10","1957-06-10","1958-03-31","1962-06-18","1963-04-08","1965-11-08","1968-06-25","1972-10-30","1974-07-08","1979-05-22","1980-02-18","1984-09-04","1988-11-21","1993-10-25","1997-06-02","2000-11-27","2004-06-28","2006-01-23","2008-10-14","2011-05-02","2015-10-19","2019-01-01"]

# Load cabinet
print "Loading cabinet appointments..."
cabinetLines = [x.strip() for x in open("cabinet.txt","r").read().split("\n") if len(x)]
appointees = []
for cL in cabinetLines:
	name, junk1, position, begin, end, junk2, junk3 = cL.split("\t")

	if any(x in position.lower() for x in ["defence","health","justice","finance","public works","foreign","revenue"]):
		important = 1
	else:
		important = 0
	fixNames = name.strip().rsplit(" ",1)
	fixName = fixNames[1].upper()+", "+fixNames[0].strip()
	lastName = fixNames[1].upper().strip()
	firstName = fixNames[0].strip()
	skip=0

	for a in appointees:
		if a["name"]==fixName and a["begin"]==begin and a["end"]==end and a["important"]==important:
			skip=1
			break
	if not skip:
		appointees.append({"name": fixName, "lastName": lastName, "firstname": firstName, "important": important, "begin": begin, "end": end, "pm": 0})

print "Loading Prime Ministerial times."
pmLines = [x for x in open("pm.txt","r").read().split("\n") if len(x)]
for pm in pmLines:
	name, begin, end = pm.split("\t")
	pm = 1
	fixNames = name.strip().rsplit(" ",1)
	lastName = fixNames[1].upper().strip()
	firstName = fixNames[0].strip()
	fixName = fixNames[1].upper()+", "+fixNames[0].strip()
	appointees.append({"name": fixName, "lastName": lastName, "firstname": firstName, "important": 1, "begin": begin, "end": end, "pm": 1})

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
		lastname = lastname.strip()
		firstname = firstname.strip()
	else:
		lastname = name.strip()
		firstname = ""

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
	print i, date, lastElecDate, currElecDate

	# Cabinet match
	exactCabinetMatch = [x for x in appointees if x["lastName"].strip()==lastname.strip() and x["firstname"].strip()==firstname.strip() and (x["begin"]<currElecDate and x["end"]>lastElecDate)]
	if not len(exactCabinetMatch):
		print i, "No exact..."
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

	if currElecDate>="1935-01-01" and currElecDate <="1948-11-14" and party=="Lib":
		partyInGovt=1
	elif currElecDate>="1948-11-15" and currElecDate <="1957-06-20" and party=="Lib":
		partyInGovt=1
	elif currElecDate>="1957-06-21" and currElecDate <="1963-04-21" and party=="P.C.":
		partyInGovt=1
	elif currElecDate>="1963-04-22" and currElecDate <="1968-04-19" and party=="Lib":
		partyInGovt=1
	elif currElecDate>="1968-04-20" and currElecDate <="1979-06-02" and party=="Lib":
		partyInGovt=1
	elif currElecDate>="1979-06-02" and currElecDate <="1980-03-01" and party=="P.C.":
		partyInGovt=1
	elif currElecDate>="1980-03-02" and currElecDate <="1984-09-15" and party=="Lib":
		partyInGovt=1
	elif currElecDate>="1984-09-17" and currElecDate <="1993-11-02" and party=="P.C.":
		partyInGovt=1
	elif currElecDate>="1993-11-03" and currElecDate <="2006-02-05" and party=="Lib":
		partyInGovt=1
	elif currElecDate>="2006-02-06" and party=="C":
		partyInGovt=1
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
