import sys
import re
import traceback
import math
import pprint
import time
import datetime
import csv 
startTime = time.time()

replay = 1 # Set to 1 to replicate, set to 0 to make coding decisions
INCLUDELISTELECTIONS = 1

electionDates = ["1943-09-25", "1946-11-27", "1949-11-30", "1951-09-01", "1954-11-13", "1957-11-30", "1960-11-26", "1963-11-30", "1966-11-26", "1969-11-29", "1972-11-25", "1975-11-29", "1978-11-25", "1981-11-28", "1984-07-14", "1987-08-15", "1990-10-27", "1993-11-06", "1996-10-12", "1999-11-27", "2002-07-27", "2005-09-17", "2008-11-08", "2011-11-26", "2014-09-20", "9999-99-99"]

print "Load all elections."
lines = [x for x in open("in.txt","r").read().splitlines() if len(x)]

print len(lines), "candidates loaded..."

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

print "Loading party lists..."
partyListLines = [x.strip() for x in open("partyLists.txt","r").read().split("\n") if len(x)]
partyLines = []
for pL in partyListLines:
	date, party, name, listNum, elected = pL.split("\t")
	if "," in name:
		familyname = name.split(",",1)[0]
		firstname = name.split(",",1)[1]
	else:
		familyname = name.strip()
		firstname = ""

	elected = int(elected)
	listNum = int(listNum)
	partyLines.append({"date": date, "party": party, "name": name, "lastname": familyname, "firstname": firstname, "elected": elected, "listNum": listNum})
	#print date, party, "\t", familyname, "\t", firstname, "\t", listNum, elected

print len(partyLines), "party list members loaded..."

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


def scoreMatch(person1, person2, scoreType=0, debug=0):
	flags = []
	global partyNicknames, nicknames
	score = 0

	pre1 = person1["firstname"]
	pre2 = person2["firstname"]

	person1["firstname"] = person1["firstname"].lower().replace(",","")
	person2["firstname"] = person2["firstname"].lower().replace(",","")

	initialSet=0
	if (pre1==pre1.upper() and pre2==pre2.upper()) or (len(pre1.strip())<3 and len(pre2.strip())<3):
		initialSet=1

	if not scoreType:
		if person1["electorate"] == person2["electorate"]:
			score=score+40
			flags.append("electorate")
		else:
			person2["electorate"] = person2["electorate"].replace("--","")
			person1["electorate"] = person1["electorate"].replace("--","")
	
			if levenshtein(person1["electorate"],person2["electorate"])>=0.45:
				score = score + int(10*levenshtein(person1["electorate"], person2["electorate"]))
				flags.append("partial electorate match, rename?")
		if person1["party"] == person2["party"]:
			score=score+20
			flags.append("party match")
		elif person1["party"]=="I" or person2["party"]=="I" or person1["party"]=="Ind." or person2["party"]=="Ind." or person1["party"]=="Independent" or person2["party"]=="Independent":
			score=score+5
			flags.append("independent benefit of doubt")

	person1["firstname"] = person1["firstname"].strip()
	person2["firstname"] = person2["firstname"].strip()
	p1fns = [x for x in re.split("\. |\.| ",person1["firstname"]) if x != "," and len(x)>0]
	p2fns = [x for x in re.split("\. |\.| ",person2["firstname"]) if x != "," and len(x)>0]

	checkIt=1
	if person1["firstname"] == person2["firstname"] or person1["firstname"].replace("-"," ")==person2["firstname"] or person2["firstname"].replace("-"," ")==person1["firstname"]:
		score=score+35
		checkIt=0
		flags.append("full name match")
	elif (len(p1fns)==1 and len(p2fns)==2) or (len(p1fns)==2 and len(p2fns)==1):
		checkIt=0
		shorter = p1fns[0] if len(p1fns)==1 else p2fns[0]
		longer = p1fns if len(p1fns)==2 else p2fns
		if shorter==longer[0] or shorter==longer[1]:
			flags.append("1-to-2 name match")
			score=score+25
		else:
			checkIt=1

	if checkIt:
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
				elif initialSet==1 and p1fns[j][0]==p2fns[k][0]:
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


allocatedID = 1
rowSet = []
winCount = 0
i=0
incumbencyParameter = ""

csvFile = open("output.csv", "w")

writer = csv.writer(csvFile, quoting=csv.QUOTE_NONNUMERIC)
writer.writerow(['ID','Name','Party', 'Date','Electorate','Incumbent','TermsServed','CabinetNow', 'CabinetEver', 'CabinetImportant', 'CabinetPM','Votes','Elected','ElectedList', 'ElectedElectorate', 'LastVotes','ListPosition','LastListPosition'])

print "Beginning to iterate through election data."
for line in lines:
	i=i+1
	if i==1:
		continue

	if len(line.strip())==0:
		break

	try:
		electorate, date, name, party, votes, elected, incumbentOverride = line.split("\t")
		if not len(incumbentOverride):
			incumbentOverride = 0
	except:
		try:
			electorate, date, name, party, votes, elected = line.split("\t")
			incumbentOverride = 0
		except:
			print line
			print line.split("\t")
			print traceback.format_exc()
			sys.exit(-1)

	dateMonth, dateDay, dateYear = date.split("/")
	date = dateYear+"-"+dateMonth.zfill(2)+"-"+dateDay.zfill(2)

	# Remove junk from names
	name = name.replace("  "," ").replace(".","")
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

	elected = int(elected)
	votes = int(votes)
	incumbent = 0
	idNumber = 0
	incumbentTerms = int(incumbentOverride)
	incumbentOverride = int(incumbentOverride)
	servedEver = 0
	servedNow = 0
	votePct = 0
	pm = 0
	important = 0
	rowNumber = -1
	lastVotes = 0
	listPosition = 0
	lastListPosition = 0

	# Which election do we get incumbency data from?
	lastElection = 0
	for eD in electionDates:
		if date<=eD:
			break		
		lastElection=lastElection+1
	searchMin = electionDates[lastElection-1]
	try:
		searchMax = electionDates[lastElection]
	except:
		searchMax = electionDates[-1]

	firstPassSubset = [x for x in rowSet if x["date"]>=searchMin and x["date"]<date]

	if len(firstPassSubset):
		exactMatch = [x for x in firstPassSubset if x["name"]==name and x["party"]==party and x["electorate"]==electorate]
		if len(exactMatch)>1:
			eM2 = sorted(exactMatch, key=lambda k: k["date"], reverse=True)[0]
			exactMatch = [eM2]

		if len(exactMatch)==1:
			#print date, "/", electorate, "/", name, ". Category: Exact match."
			if exactMatch[0]["elected"] == 1:
				incumbent = 1
				incumbentTerms = int(exactMatch[0]["incumbentTerms"]) + 1
			else:
				incumbent = 0
				incumbentTerms = 0
			servedEver = exactMatch[0]["servedEver"] or exactMatch[0]["servedNow"]
			idNumber = exactMatch[0]["id"]
			rowNumber = exactMatch[0]["rowIndex"]
			lastVotes = exactMatch[0]["votes"]
			lastListPosition = exactMatch[0]["listPosition"]
		elif len(exactMatch)==0:
			exactFamilyNameMatch = [x for x in firstPassSubset if x["lastname"].upper()==lastname.upper()]
			#if not len(exactFamilyNameMatch):
			#	print electorate, lastname, date, votes, elected

			if not len(exactFamilyNameMatch):
				#print date, "/", electorate, "/", name, ". Category: No family match."
				incumbent=0 # for now, later do fuzzy match
			elif len(exactFamilyNameMatch)==1:
				score, flags = scoreMatch({"firstname": firstname, "party": party, "electorate": electorate}, exactFamilyNameMatch[0],0)
				#if not "full name match" in flags and score>0:
				#	print date, "/", electorate, "/", name, ". Match for family names? ", score, flags
				if score>=45 or (score>=35 and "full name match" in flags):
					#print date, "/", electorate, "/", name, ". Category: Family name high score.", score, flags, exactFamilyNameMatch[0]["name"]
					if exactFamilyNameMatch[0]["elected"]==1:
						#print date, "/", electorate, "/", name, "has an family name match to someone who was elected."
						incumbent=1
						incumbentTerms = int(exactFamilyNameMatch[0]["incumbentTerms"]) + 1
					else:
						incumbent=0
						incumbentTerms = 0
					servedEver = exactFamilyNameMatch[0]["servedEver"] or exactFamilyNameMatch[0]["servedNow"]
					idNumber = exactFamilyNameMatch[0]["id"]
					rowNumber = exactFamilyNameMatch[0]["rowIndex"]
					lastVotes = exactFamilyNameMatch[0]["votes"]
					lastListPosition = exactFamilyNameMatch[0]["listPosition"]
				else:
					if score>=15 and (votes>1000 or exactFamilyNameMatch[0]["votes"]>1000):
						#print date, "/", electorate, "/", name, ". Category: Family name medium score."
						namePadLength = max(len(name),len(exactFamilyNameMatch[0]["name"]))+5
						electoratePadLength = max(len(electorate), len(exactFamilyNameMatch[0]["electorate"]))+5
						partyPadLength = max(len(party), len(exactFamilyNameMatch[0]["party"]))+5
						print "=====\nID MATCH HUMAN INTERVENTION REQUIRED. IS THIS THE SAME PERSON?\n====="
						print "Election: "+date+"."
						print "NEW NAME: "+name.ljust(namePadLength)+"\tELECTORATE: "+electorate.ljust(electoratePadLength)+"\tPARTY: "+party.ljust(partyPadLength)+"\tELECTED? "+str(elected)+"\tVOTES: "+str(votes)
						print "OLD NAME: "+exactFamilyNameMatch[0]["name"].ljust(namePadLength)+"\tELECTORATE: "+exactFamilyNameMatch[0]["electorate"].ljust(electoratePadLength)+"\tPARTY: "+exactFamilyNameMatch[0]["party"].ljust(partyPadLength)+"\tELECTED? "+str(exactFamilyNameMatch[0]["elected"])+"\tVOTES: "+str(exactFamilyNameMatch[0]["votes"])+"\tSCORE: "+str(score)
						print "EVIDENCE OF MATCH:"
						print flags

						doLookup(exactFamilyNameMatch[0])

						samePerson = takeInput(score, replay)
						if samePerson:
							if exactFamilyNameMatch[0]["elected"]==1:
								incumbent = 1
								incumbentTerms = int(exactFamilyNameMatch[0]["incumbentTerms"]) + 1
							else:
								incumbent = 0
								incumbentTerms = 0
							servedEver = exactFamilyNameMatch[0]["servedEver"] or exactFamilyNameMatch[0]["servedNow"]
							idNumber = exactFamilyNameMatch[0]["id"]
							rowNumber = exactFamilyNameMatch[0]["rowIndex"]
							lastVotes = exactFamilyNameMatch[0]["votes"]
							lastListPosition = exactFamilyNameMatch[0]["listPosition"]
						print "Thank you.\n====="
					else:
						pass
						#print date, "/", electorate, "/", name, ". Category: Theoretical family match but it is no good."
			else:
				plausibleResults = []
				maxScore=0
				for match in exactFamilyNameMatch:
					score, flags = scoreMatch({"firstname": firstname, "party": party, "electorate": electorate}, match,0)
					if score>=50:
						match["score"] = score
						if len(plausibleResults) and score>plausibleResults[0]["score"]*1.5:
							plausibleResults = [match]
						else:
							plausibleResults.append(match)
						if score > maxScore:
							maxScore = score
				if len(plausibleResults)==0:
					# Not an incumbent?
					if maxScore>0:
						print "Found multiple potential family matches for "+name+" in "+electorate+", but none are real matches."
						test = raw_input("Press enter to continue, this is a blocking message.")
					incumbent=0
				elif len(plausibleResults)==1:
					#print date, "/", electorate, "/", name, ". Category: Multiple family name matches, one plausible."
					if plausibleResults[0]["elected"]==1:
						incumbent=1
						incumbentTerms = int(plausibleResults[0]["incumbentTerms"]) + 1
					else:
						incumbent=0
						incumbentTerms = 0
					servedEver = plausibleResults[0]["servedEver"] or plausibleResults[0]["servedNow"]
					idNumber = plausibleResults[0]["id"]
					rowNumber = plausibleResults[0]["rowIndex"]
					lastVotes = plausibleResults[0]["votes"]
					lastListPosition = plausibleResults[0]["listPosition"]
				elif len(plausibleResults)>1:
					#print date, "/", electorate, "/", name, ". Category: Multiple family name matches, multiple plausible."						
					if plausibleResults[0]["elected"]==1:
						incumbent=1
						incumbentTerms = int(plausibleResults[0]["incumbentTerms"]) + 1
					else:
						incumbent=0
						incumbentTerms = 0
					idNumber = plausibleResults[0]["id"]
					servedEver = plausibleResults[0]["servedEver"] or plausibleResults[0]["servedNow"]
					rowNumber = plausibleResults[0]["rowIndex"]
					lastVotes = plausibleResults[0]["votes"]
					lastListPosition = plausibleResults[0]["listPosition"]
	else: # No prior elections
		#print date, "/", electorate, "/", name, ". Category: No matches at all"
		if incumbentOverride:
			incumbent = 1 
			incumbentTerms = int(incumbentOverride)
		else:
			incumbent = 0
			incumbentTerms = 0
		rowNumber = -1

	if not idNumber:
		idNumber = allocatedID
		allocatedID = allocatedID+1
	
	# Which election are we on?
	eN=0
	for d in electionDates:
		if date>d:
			eN=eN+1
			continue
		else:
			break
	try:
		lastElecDate = electionDates[eN-1]
	except:
		lastElecDate = "0000-00-00"
	currElecDate = electionDates[eN]
	try:
		nextElecDate = electionDates[eN+1]
	except:
		nextElecDate = "9999-99-99"
	# begin before lastElecDate = cabinet
	# begin after lastElecDate and before today = appointed to cabinet
	if currElecDate!=date: # this is a by-election.
		pass
		#print "error, election dates don't line up"
		#sys.exit(-1)

	# Cabinet match
	debug=0
	#if "clark" in name.lower() and date>"2008-05-01" and date<"2009-01-01" and votes>1000:
	#	print "DEBUG MODE ACTIVATED"
	#	debug=1

	# Cutoff to prevent name matches with minor candidates that are clearly not cabinet members
	checkForCabinet = 0
	if (party=="Labour" or party=="National") or (elected==1) or (votes>500) or (incumbent==1 and votes>100):
		checkForCabinet = 1

	if checkForCabinet:
		exactCabinetMatch = [x for x in appointees if x["lastName"].upper().strip()==lastname.upper().strip() and x["firstname"].upper().strip()==firstname.upper().strip() and (x["begin"]<date and x["end"]>lastElecDate)]

		if not len(exactCabinetMatch):
			if debug:
				print "No Exact"
			familyCabinetMatch = [x for x in appointees if x["lastName"].upper().strip()==lastname.upper().strip() and (x["begin"]<date and x["end"]>lastElecDate)]

			if not len(familyCabinetMatch):
				servedNow = 0
			elif len(familyCabinetMatch)==1:
				score, flags = scoreMatch({"firstname": firstname}, familyCabinetMatch[0],1, debug)
				if score>=25:
					if debug:
						print "EXACTLY one cabinet match"
					pm = familyCabinetMatch[0]["pm"]
					important = familyCabinetMatch[0]["important"]
					servedNow = 1
					servedEver = 1
				elif score>5:
					if debug:
						print "Cabinet match manual intervention"
					namePadLength = max(len(name),len(familyCabinetMatch[0]["name"]))+5
					print "=====\nCAB MATCH FAMILY HUMAN INTERVENTION REQUIRED. IS THIS THE SAME PERSON?\n====="
					print "Election: "+date+". Electorate: "+electorate
					print "NEW NAME: "+name.ljust(namePadLength)+"\tPARTY: "+party+"\tINCUMBENT? "+str(incumbent)+"\tTERMS? "+str(incumbentTerms)
					print "OLD NAME: "+familyCabinetMatch[0]["name"].ljust(namePadLength)+"\tSCORE: "+str(score)
					print "EVIDENCE OF MATCH:"
					print flags
	
					samePerson = takeInput(score, replay)
					if samePerson:
						pm = familyCabinetMatch[0]["pm"]
						important = familyCabinetMatch[0]["important"]
						servedNow = 1
						servedEver = 1
					else:
						servedNow = 0
				else:
					if debug:
						print score
						print "Exactly one cabinet match, but it's no good"
						print name
						print familyCabinetMatch
						raw_input("press enter to continue")
					pass
			else:
				maxScore = 0
				plausibleResults = []
				for match in familyCabinetMatch:
					score, flags = scoreMatch({"firstname": firstname}, match,1)
					if score>5:
						match["score"] = score
						if len(plausibleResults) and score>plausibleResults[0]["score"]*1.5:
							plausibleResults = [match]
						else:
							plausibleResults.append(match)
						if score > maxScore:
							maxScore = score

				if len(plausibleResults)==0:
					if debug==1:
						print name
						print familyCabinetMatch
						print "Some family name results, none plausible"
						raw_input("press enter to continue")
					pass
				elif len(plausibleResults)==1:
					if debug==1:
						print plausibleResults
						print "Some family name results, 1 plausible"
						raw_input("press enter to continue")
	
					if plausibleResults[0]["score"]>=20:
						pm = plausibleResults[0]["pm"]
						important = plausibleResults[0]["important"]
						servedNow = 1
						servedEver = 1
					else:
						namePadLength = max(len(name),len(plausibleResults[0]["name"]))+5
						print "=====\nCAB MATCH MULTI HUMAN INTERVENTION REQUIRED. IS THIS THE SAME PERSON?\n====="
						print "Election: "+date+". Electorate: "+electorate
						print "NEW NAME: "+name.ljust(namePadLength)+"\tPARTY: "+party+"\tINCUMBENT? "+str(incumbent)+"\tTERMS? "+str(incumbentTerms)
						print "OLD NAME: "+plausibleResults[0]["name"].ljust(namePadLength)+"\tSCORE: "+str(plausibleResults[0]["score"])
						print "EVIDENCE OF MATCH:"
						print flags
		
						samePerson = takeInput(score, replay)
						if samePerson:
							pm = plausibleResults[0]["pm"]
							important = plausibleResults[0]["important"]
							servedNow = 1
							servedEver = 1
						else:
							servedNow = 0
				else:
					pR = sorted(plausibleResults, key=lambda k: (k["score"], k["pm"], k["important"]), reverse=True)
					if pR[0]["score"]>=20:
						pm = pR[0]["pm"]
						important = pR[0]["important"]
						servedNow = 1
						servedEver = 1
					else:
						namePadLength = max(len(name),len(plausibleResults[0]["name"]))+5
						print "=====\nMULTI pR HUMAN INTERVENTION REQUIRED. IS THIS THE SAME PERSON?\n====="
						print "Election: "+date+". Electorate: "+electorate
						print "NEW NAME: "+name.ljust(namePadLength)+"\tPARTY: "+party+"\tINCUMBENT? "+str(incumbent)+"\tTERMS? "+str(incumbentTerms)
						print "OLD NAME: "+pR[0]["name"].ljust(namePadLength)+"\tSCORE: "+str(pR[0]["score"])
	
						samePerson = takeInput(score, replay)
						if samePerson:
							pm = pR[0]["pm"]
							important = pR[0]["important"]
							servedNow = 1
							servedEver = 1
						else:
							servedNow = 0		

					if debug==1:
						print "Some family name results, multi plausible"
						print "End result? ", servedNow, servedEver, important, pm
						print pR[0]
						raw_input("press enter to continue")

		elif len(exactCabinetMatch)==1:
			servedNow = 1
			servedEver = 1
			pm = exactCabinetMatch[0]["pm"]
			important = exactCabinetMatch[0]["important"]
			#print date, name, elected, votes, preID
			#print "We have found a cabinet match"
			#print exactCabinetMatch
	
			if debug:
				print "Exact match, just one"
				print "End result? ", servedNow, servedEver, important, pm
				print pR[0]
				raw_input("press enter to continue")
	
			#test = raw_input("press enter to continue")
		else:
			bestMatch = sorted(exactCabinetMatch, key=lambda k: (k["pm"], k["important"]), reverse=True)[0]
			servedNow=1
			servedEver=1
			pm = bestMatch["pm"]
			important = bestMatch["important"]

			if debug:
				print "Exact match, multi"
				print bestMatch
				print "End result? ", servedNow, servedEver, important, pm
				raw_input("press enter to continue")
	else:
		servedNow=0
		servedEver=0
		important=0
		pm=0


	electedList = 0
	if date>="1992-01-01":
		partyList = [x for x in partyLines if x["party"]==party and x["date"]==date]
		if len(partyList):
			memberMatch = [x for x in partyList if x["lastname"].strip().upper()==lastname.strip().upper()]
			if len(memberMatch)==1:
				if date>"1998-01-01":		
					checkExact = [x for x in memberMatch if x["firstname"].strip().upper()==firstname.strip().upper() or x["firstname"].split()[0].strip().upper()==firstname.strip().split()[0].strip().upper() or checkNickname(nicknames, x["firstname"].split()[0].strip(), firstname.split()[0].strip())]
					if not len(checkExact) and (memberMatch[0]["elected"]>0 or elected):
						memList = {"firstname": memberMatch[0]["firstname"]}
						memLocal = {"firstname": firstname}
						score, flags = scoreMatch(memList, memLocal, scoreType=1)
						if score>0:
							if elected==0 and memberMatch[0]["elected"]==2:
								electedList = 1
							elif elected!=memberMatch[0]["elected"]:
								print "Bizarre setup"
								print score, elected, memberMatch[0]["elected"], lastname, firstname
								print memberMatch
								raw_input("Press enter to continue")
							listPosition = memberMatch[0]["listNum"]
						else:
							pass
					elif len(checkExact)==1:
						if elected==0 and checkExact[0]["elected"]==2:
							electedList=1
						elif elected==checkExact[0]["elected"]:
							pass
						else:
							print "Bizarre setup"
							print score, elected, memberMatch[0]["elected"], lastname, firstname
							print memberMatch
							raw_input("Press enter to continue")	
						listPosition = checkExact[0]["listNum"]
				else:
					if elected==memberMatch[0]["elected"]:
						pass
						#print "All good, party list status matches electoral status"
					elif elected==0 and memberMatch[0]["elected"]==2:
						#print "Elected from list!"
						electedList=1
					else:
						print memberMatch
						print elected, votes, lastname, firstname, incumbent
						print "Status does not match!", memberMatch[0]["elected"], lastname, firstname, elected, memberMatch[0]["listNum"]
						raw_input("Press enter to continue")
					listPosition = memberMatch[0]["listNum"]

			elif len(memberMatch)>1:
				exactMatch = [x for x in memberMatch if x["firstname"].strip().upper()==firstname.strip().upper() or x["firstname"].split()[0].strip().upper()==firstname.strip().split()[0].strip().upper() or checkNickname(nicknames, x["firstname"].split()[0].strip(), firstname.split()[0].strip())]
				if not len(exactMatch):
					validMatches = []
					for mem in memberMatch:
						memList = {"firstname": mem["firstname"]}
						memLocal = {"firstname": firstname}
						score, flags = scoreMatch(memList, memLocal, scoreType=1)
						mem["score"] = score
						if score:
							validMatches.append(mem)

					if len(validMatches)==0 and (elected or len([x for x in memberMatch if x["elected"]])):
						pass

					elif len(validMatches)==1:
						if validMatches[0]["elected"]==2 and elected==0:
							electedList=1
						elif validMatches[0]["elected"]==elected:
							pass
						else:
							print "One valid match but there's a discrepancy"
							print validMatches[0]
							print lastname, firstname, elected, electorate
							raw_input("Press enter to continue")
						listPosition = validMatches[0]["listNum"]

					elif len(validMatches)>1 and (elected or len([x for x in validMatches if x["elected"]])):
						print "STILL MULTIPLE MATCHES UGH"
						print validMatches
						print lastname, firstname, elected
						raw_input("Multiple matches, press enter")

				elif len(exactMatch)==1:
					if elected==0 and exactMatch[0]["elected"]==2:
						electedList=1
					elif elected==exactMatch[0]["elected"]:
						pass
					else:
						print "Apparent discrepancy!!!!"
						print lastname, firstname, elected
						print exactMatch
						raw_input("Uh oh!!!")
					listPosition = exactMatch[0]["listNum"]
				else:
					print "Somehow there are two people with the exact same name."
					print memberMatch
					raw_input("Press enter to continue")

	if INCLUDELISTELECTIONS:
		electedElectorate = elected
		elected = electedList or electedElectorate

	currentRow = { "electorate": electorate, "date": date, "name": name, 
			"lastname": lastname, "firstname": firstname, 
			"party": party, "elected": elected, "electedList": electedList, "votes": votes, "id": idNumber,
			"incumbent": incumbent, "incumbentTerms": incumbentTerms,
			"servedEver": servedEver, "servedNow": servedNow, "pm": pm,
			"important": important, "matchedNext": -1, "rowIndex": len(rowSet), "listPosition": listPosition, "lastListPosition": lastListPosition, "electedElectorate": electedElectorate}

	writer.writerow([idNumber, name, party, date, electorate, incumbent, incumbentTerms, servedNow, servedEver, important, pm, votes, elected, electedList, electedElectorate, lastVotes, listPosition, lastListPosition])

	rowSet.append(currentRow)

	# Code designed to ensure that no one ever double matches
	if rowNumber>-1:
		if rowSet[rowNumber]["matchedNext"]>-1:
			jamNumber = rowSet[rowNumber]["matchedNext"]
			if rowSet[rowNumber]["id"]!=rowSet[jamNumber]["id"] or currentRow["id"]!=rowSet[jamNumber]["id"]:
				# It's OK if the double match is two by-elections or weird cabinet weirdness from pre-1900
				print "=====ALERT====="
				print "My current line found multiple matches. Let's investigate:"
				print "Me, I am: #", currentRow["id"], currentRow["lastname"], currentRow["firstname"], "rep ", currentRow["party"], "from ", currentRow["electorate"], "on ", currentRow["date"]
				print "I matched: #", rowSet[rowNumber]["id"], rowSet[rowNumber]["lastname"], rowSet[rowNumber]["firstname"], "rep ", rowSet[rowNumber]["party"], "from ", rowSet[rowNumber]["electorate"], "on ", rowSet[rowNumber]["date"]

				print "But so did: #", rowSet[jamNumber]["id"], rowSet[jamNumber]["lastname"], rowSet[jamNumber]["firstname"], "rep ", rowSet[jamNumber]["party"], "from ", rowSet[jamNumber]["electorate"], "on ", rowSet[jamNumber]["date"]
				sys.exit(-1)
		else:
			rowSet[rowNumber]["matchedNext"] = rowSet[-1]["rowIndex"]

	if elected:
		winCount=winCount+1
		if winCount%50==0:
			print str(i)+"... Added 50 winners, including winner "+name+" in "+electorate+" ("+date+")"
			pass
	else:
		pass


newInputSet = [str(x) for x in newInputSet]
if len(newInputSet):
	strVar = ", ".join(newInputSet)
	with open("decisions.txt","w") as f:
		f.write(strVar)

print "\n\n========="
print "Sanity check of resulting data!"

print "DATE, TOT, INC, NAT, PMs, !CAB, CABE, CABN, Who is PM?, Party Split"
for eD in electionDates[1:-1]:
	year, junk = eD.split("-",1)
	print eD, 
	print len([x for x in rowSet if x["date"]==eD and x["elected"]==1]),
	print len([x for x in rowSet if x["date"]==eD and x["elected"]==1 and x["incumbent"]==1]),
	print len([x for x in rowSet if x["date"]==eD and x["elected"]==1 and x["party"]=="National"]),
	print len([x for x in rowSet if x["date"]==eD and x["pm"]==1]),
	print len([x for x in rowSet if x["date"]==eD and x["important"]==1]),
	print len([x for x in rowSet if x["date"]==eD and x["servedEver"]==1]),
	print len([x for x in rowSet if x["date"]==eD and x["servedNow"]==1]),
	print [x["name"] for x in rowSet if x["date"]==eD and x["pm"]==1],
	electedNum = [x for x in rowSet if x["date"]==eD and x["elected"]==1]
	partySet = {}
	for e in electedNum:
		if e["party"] in partySet:
			partySet[e["party"]] += 1
		else:
			partySet[e["party"]] = 1
	print partySet

print "Non-major party cabinet ministers:"
print "Votes, Name, Party, Elected?"
for guy in [x for x in rowSet if x["party"]!="National" and x["party"]!="Labour" and x["servedEver"]==1]:
	print guy["date"], guy["electorate"], guy["votes"], guy["name"], guy["party"], guy["elected"]


endTime = time.time()
if endTime-startTime>60:
	seconds = round((endTime-startTime)%60)
	minutes = math.floor((endTime-startTime)/60)
	print "Total execution time: "+str(minutes)+" minutes, "+str(seconds)+" seconds"
else:
	print "Total execution time: "+str(round(endTime-startTime))+" seconds"
