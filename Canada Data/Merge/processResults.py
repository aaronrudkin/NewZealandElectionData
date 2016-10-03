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

electionDates = ["0000-00-00", "1867-08-07","1872-07-20","1874-01-22","1878-09-17","1882-06-20","1887-02-22","1891-03-05","1896-06-23","1900-11-07","1904-11-03","1908-10-26","1911-09-21","1917-12-17","1921-12-06","1925-10-29","1926-09-14","1930-07-28","1935-10-14","1940-03-26","1945-06-11","1949-06-27","1953-08-10","1957-06-10","1958-03-31","1962-06-18","1963-04-08","1965-11-08","1968-06-25","1972-10-30","1974-07-08","1979-05-22","1980-02-18","1984-09-04","1988-11-21","1993-10-25","1997-06-02","2000-11-27","2004-06-28","2006-01-23","2008-10-14","2011-05-02","2015-10-19"]

print "Load all elections, sort to fit by-elections."
# Sort the rows so that by-elections are processed at the proper time so incumbency is set properly for by-elected members
lines = [x for x in open("input.txt","r").read().split("\n") if len(x)]
skipLines = lines[1:]
sortedLines = sorted(skipLines, key=lambda l: float(l.rsplit("\t",1)[1]))
lines = [lines[0]] + sortedLines

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

# Load birthdates
print "Loading birthdays"
bDates = [x.strip() for x in open("birthdays.csv","r").read().split("\n") if len(x)]
actualBDs = ["0000-00-00"]
for bDate in bDates:
	try:
		id, line = bDate.split(", ")
		#print int(len(actualBDs))==int(id), id, len(actualBDs), line, len(line)

		if len(line)==0:
			actualBDs.append("")
		else:
			if len(line)==7:
				line = line+"-01"
			elif len(line)==10:
				pass
			else:
				print len(line), "error bad line"
				sys.exit()
			actualBDs.append(line)
	except:
		actualBDs.append("")

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
partyNicknames = loadNickname("partyNicknames.csv")

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


def scoreMatch(person1, person2, scoreType=0):
	flags = []
	global partyNicknames, nicknames
	score = 0

	person1["firstname"] = person1["firstname"].lower().replace(",","")
	person2["firstname"] = person2["firstname"].lower().replace(",","")

	if not scoreType:
		if person1["riding"] == person2["riding"]:
			score=score+40
			flags.append("riding match")
		else:
			person2["riding"] = person2["riding"].replace("--","")
			person1["riding"] = person1["riding"].replace("--","")
	
			if levenshtein(person1["riding"],person2["riding"])>=0.45:
				score = score + int(10*levenshtein(person1["riding"], person2["riding"]))
				flags.append("partial riding match, rename?")
		if person1["party"] == person2["party"]:
			score=score+20
			flags.append("party match")
		elif checkNickname(partyNicknames, person1["party"], person2["party"]):
			score=score+15
			flags.append("party nickname match")
		elif person1["party"]=="Ind." or person2["party"]=="Ind." or person1["party"]=="Unknown" or person2["party"]=="Unknown":
			score=score+5
			flags.append("independent benefit of doubt")

		if person1["preID"] and person1["preID"]==person2["preID"]:
			score = score+100
			flags.append("exact parliamentary ID match")
		elif (person1["preID"] or person2["preID"]) and person1["preID"]!=person2["preID"]:
			score = score-100
			flags.append("different parliamentary ID, definitely the wrong person.")

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


allocatedID = 1
rowSet = []
winCount = 0
i=0
incumbencyParameter = ""

csvFile = open("output.csv", "w")

writer = csv.writer(csvFile, quoting=csv.QUOTE_NONNUMERIC)
writer.writerow(['ID', 'Name', 'Age', 'Incumbent', 'TermsServed', 'CabinetNow', 'CabinetEver', 'CabinetImportant', 'CabinetPM', 'Date', 'Party', 'Riding', 'Province','Votes', 'VotePct', 'Elected', 'LastVotes', 'LastVotePct'])

print "Beginning to iterate through election data."
for line in lines:
	i=i+1
	if i==1:
		print line.strip()+"\tINCUMBENT\tTERMS\tID"
		continue

	if len(line.strip())==0:
		break

	preID, province, riding, date, name, party, job, votes, percent, elected, electionnum = line.split("\t")

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
	try:
		electionnum = int(electionnum)
	except:
		electionnum = float(electionnum)
	elected = int(elected)
	preID = int(preID)
	percent = float(percent)
	votePct = percent
	votes = int(votes)
	incumbent=0
	idNumber = 0
	incumbentTerms = 0
	servedEver = 0
	servedNow = 0
	pm = 0
	important = 0
	rowNumber = -1
	lastVotes = 0
	lastVotePct = 0
	birthday = ""
	if electionnum==1:
		pass
	else:
		# Which election do we get incumbency data from?
		try:
			if math.floor(electionnum)!=electionnum:
				searchElection = math.floor(electionnum)
			else:
				searchElection = electionnum-1
		except:	
			print traceback.format_exc()
			sys.exit(-1)

		if preID:
			firstPassSubset = [x for x in rowSet if (x["province"]==province or x["preID"]==preID) and x["electionnum"]>=searchElection and x["electionnum"]<electionnum]
		else:
			firstPassSubset = [x for x in rowSet if x["province"]==province and x["electionnum"]>=searchElection and x["electionnum"]<electionnum]

		if len(firstPassSubset):
			if not preID:
				exactMatch = [x for x in firstPassSubset if x["name"]==name and x["party"]==party and x["riding"]==riding]
			else:
				exactMatch = [x for x in firstPassSubset if preID==x["preID"]]

			if len(exactMatch)>1:
				eM2 = sorted(exactMatch, key=lambda k: (k["electionnum"], k["date"]), reverse=True)[0]
				exactMatch = [eM2]

			if len(exactMatch)==1:
				if exactMatch[0]["elected"] == 1:
					incumbent = 1
					incumbentTerms = exactMatch[0]["incumbentTerms"] + 1
				else:
					incumbent = 0
					incumbentTerms = 0
				servedEver = exactMatch[0]["servedEver"] or exactMatch[0]["servedNow"]
				idNumber = exactMatch[0]["id"]
				rowNumber = exactMatch[0]["rowIndex"]
				lastVotes = exactMatch[0]["votes"]
				lastVotePct = exactMatch[0]["votePct"]
			elif len(exactMatch)==0:
				exactFamilyNameMatch = [x for x in firstPassSubset if x["lastname"]==lastname]
				if not len(exactFamilyNameMatch):
					incumbent=0 # for now, later do fuzzy match
				elif len(exactFamilyNameMatch)==1:
					score, flags = scoreMatch({"firstname": firstname, "party": party, "riding": riding, "preID": preID}, exactFamilyNameMatch[0],0)
					if score>=45 or (score==45 and len(firstname)==0) or (score>=30 and preID==0 and exactFamilyNameMatch[0]["preID"]==0 and ("full name match" in flags or (("party match" in flags or "party nickname match" in flags) and ("name match" in flags or "1-to-2 name match" in flags)))):
						if exactFamilyNameMatch[0]["elected"]==1:
							incumbent=1
							incumbentTerms = exactFamilyNameMatch[0]["incumbentTerms"] + 1
						else:
							incumbent=0
							incumbentTerms = 0
						servedEver = exactFamilyNameMatch[0]["servedEver"] or exactFamilyNameMatch[0]["servedNow"]
						idNumber = exactFamilyNameMatch[0]["id"]
						rowNumber = exactFamilyNameMatch[0]["rowIndex"]
						lastVotes = exactFamilyNameMatch[0]["votes"]
						lastVotePct = exactFamilyNameMatch[0]["votePct"]
					else:
						if score>20 and (preID or exactFamilyNameMatch[0]["preID"] or votes>1000 or exactFamilyNameMatch[0]["votes"]>1000):
							namePadLength = max(len(name),len(exactFamilyNameMatch[0]["name"]))+5
							ridingPadLength = max(len(riding), len(exactFamilyNameMatch[0]["riding"]))+5
							partyPadLength = max(len(party), len(exactFamilyNameMatch[0]["party"]))+5
							print "=====\nID MATCH HUMAN INTERVENTION REQUIRED. IS THIS THE SAME PERSON?\n====="
							print "Election: "+str(electionnum)+" on "+date+". Province: "+province
							print "NEW NAME: "+name.ljust(namePadLength)+"\tRIDING: "+riding.ljust(ridingPadLength)+"\tPARTY: "+party.ljust(partyPadLength)+"\tELECTED? "+str(elected)+"\tVOTES: "+str(votes)+"\tPREID: "+str(preID)
							print "OLD NAME: "+exactFamilyNameMatch[0]["name"].ljust(namePadLength)+"\tRIDING: "+exactFamilyNameMatch[0]["riding"].ljust(ridingPadLength)+"\tPARTY: "+exactFamilyNameMatch[0]["party"].ljust(partyPadLength)+"\tELECTED? "+str(exactFamilyNameMatch[0]["elected"])+"\tVOTES: "+str(exactFamilyNameMatch[0]["votes"])+"\tPREID: "+str(exactFamilyNameMatch[0]["preID"])+"\tSCORE: "+str(score)
							print "EVIDENCE OF MATCH:"
							print flags

							doLookup(exactFamilyNameMatch[0])

							samePerson = takeInput(score, replay)
							if samePerson:
								if exactFamilyNameMatch[0]["elected"]==1:
									incumbent = 1
									incumbentTerms = exactFamilyNameMatch[0]["incumbentTerms"] + 1
								else:
									incumbent = 0
									incumbentTerms = 0
								servedEver = exactFamilyNameMatch[0]["servedEver"] or exactFamilyNameMatch[0]["servedNow"]
								idNumber = exactFamilyNameMatch[0]["id"]
								rowNumber = exactFamilyNameMatch[0]["rowIndex"]
								lastVotes = exactFamilyNameMatch[0]["votes"]
								lastVotePct = exactFamilyNameMatch[0]["votePct"]
							print "Thank you.\n====="
				else:
					plausibleResults = []
					maxScore=0
					for match in exactFamilyNameMatch:
						score, flags = scoreMatch({"firstname": firstname, "party": party, "riding": riding, "preID": preID}, match,0)
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
							print "Found multiple potential family matches for "+name+" in "+province+", "+riding+", but none are real matches."
							test = raw_input("Press enter to continue, this is a blocking message.")
						incumbent=0
					elif len(plausibleResults)==1:
						if plausibleResults[0]["elected"]==1:
							incumbent=1
							incumbentTerms = plausibleResults[0]["incumbentTerms"] + 1
						else:
							incumbent=0
							incumbentTerms = 0
						servedEver = plausibleResults[0]["servedEver"] or plausibleResults[0]["servedNow"]
						idNumber = plausibleResults[0]["id"]
						rowNumber = plausibleResults[0]["rowIndex"]
						lastVotes = plausibleResults[0]["votes"]
						lastVotePct = plausibleResults[0]["votePct"]
					elif len(plausibleResults)>1:
						
						#print "hmmm, still found multiple matches that work... what do you think?"
						basePreID = -1
						for f in plausibleResults:
							if basePreID == -1:
								basePreID = f["preID"]
							elif basePreID!=f["preID"]:
								print "Uh oh, error"
								print plausibleResults
								test = raw_input("Press enter to continue")
								sys.exit(-1)
							else:
								pass

						if plausibleResults[0]["elected"]==1:
							incumbent=1
							incumbentTerms = plausibleResults[0]["incumbentTerms"] + 1
						else:
							incumbent=0
							incumbentTerms = 0
						idNumber = plausibleResults[0]["id"]
						servedEver = plausibleResults[0]["servedEver"] or plausibleResults[0]["servedNow"]
						rowNumber = plausibleResults[0]["rowIndex"]
						lastVotes = plausibleResults[0]["votes"]
						lastVotePct = plausibleResults[0]["votePct"]
						
		else: # No prior elections in this province
			incumbent=0
			incumbentTerms = 0
			rowNumber = -1

	ageAtElection = 0
	if preID:
		birthday = actualBDs[preID]
		if len(birthday):
			birthObj = datetime.datetime.strptime(birthday, "%Y-%m-%d")
			currObj = datetime.datetime.strptime(date, "%Y-%m-%d")
			ageAtElection = int(math.floor(abs(currObj-birthObj).days/365))
	else:
		birthday = ""

	if not idNumber:
		idNumber = allocatedID
		allocatedID = allocatedID+1
	
	if votes==-1:
		if percent>95.0:
			elected=1
		else:
			print "Found a unanimously acclaimed person with a low percentage vote? error?"
			sys.exit(-1)

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
		lastElectDate = "0000-00-00"
	currElecDate = electionDates[eN]
	try:
		nextElecDate = electionDates[eN+1]
	except:
		nextElecDate = "2020-01-01"
	# begin before lastElecDate = cabinet
	# begin after lastElecDate and before today = appointed to cabinet
	if currElecDate!=date: # this is a by-election.
		pass
		#print "error, election dates don't line up"
		#sys.exit(-1)

	# Cabinet match
	if preID:
		exactCabinetMatch = [x for x in appointees if x["lastName"].strip()==lastname.strip() and x["firstname"].strip()==firstname.strip() and (x["begin"]<date and x["end"]>lastElecDate)]
		if not len(exactCabinetMatch):
			familyCabinetMatch = [x for x in appointees if x["lastName"]==lastname and (x["begin"]<date and x["end"]>lastElecDate)]
			if not len(familyCabinetMatch):
				#print "No current cabinet service. Past? ", servedEver
				servedNow = 0
			elif len(familyCabinetMatch)==1:
				score, flags = scoreMatch({"firstname": firstname}, familyCabinetMatch[0],1)
				if score>=24:
					pm = familyCabinetMatch[0]["pm"]
					important = familyCabinetMatch[0]["important"]
					servedNow = 1
					servedEver = 1
				elif score>=18:
					namePadLength = max(len(name),len(familyCabinetMatch[0]["name"]))+5
					print "=====\nCAB MATCH FAMILY HUMAN INTERVENTION REQUIRED. IS THIS THE SAME PERSON?\n====="
					print "Election: "+str(electionnum)+" on "+date+". Province: "+province
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
					#print "Weak match, not adding!"
					#print lastname, firstname
					#print familyCabinetMatch
					#print score, flags
					#test = raw_input("press enter to continue")
					pass
			else:
				maxScore = 0
				plausibleResults = []
				for match in familyCabinetMatch:
					score, flags = scoreMatch({"firstname": firstname}, match,1)
					if score>=20:
						match["score"] = score
						if len(plausibleResults) and score>plausibleResults[0]["score"]*1.5:
							plausibleResults = [match]
						else:
							plausibleResults.append(match)
						if score > maxScore:
							maxScore = score

				if len(plausibleResults)==0:
					pass
				elif len(plausibleResults)==1:
					if plausibleResults[0]["score"]>=24:
						#print "We have exactly one plausible match for a cabinet member, so we are automatically accepting it."
						pm = plausibleResults[0]["pm"]
						important = plausibleResults[0]["important"]
						servedNow = 1
						servedEver = 1
					else:
						namePadLength = max(len(name),len(plausibleResults[0]["name"]))+5
						print "=====\nCAB MATCH MULTI HUMAN INTERVENTION REQUIRED. IS THIS THE SAME PERSON?\n====="
						print "Election: "+str(electionnum)+" on "+date+". Province: "+province
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
					if pR[0]["score"]>=24:
						#print "We have more than one plausible result for a cabinet member--or the person has more than one cabinet appointment."
						#print "We're taking the most important cabinet ministry, and tie-breaking by best match, because the match is good enough"
						pm = pR[0]["pm"]
						important = pR[0]["important"]
						servedNow = 1
						servedEver = 1
					else:
						namePadLength = max(len(name),len(plausibleResults[0]["name"]))+5
						print "=====\nMULTI pR HUMAN INTERVENTION REQUIRED. IS THIS THE SAME PERSON?\n====="
						print "Election: "+str(electionnum)+" on "+date+". Province: "+province
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
		elif len(exactCabinetMatch)==1:
			servedNow = 1
			servedEver = 1
			pm = exactCabinetMatch[0]["pm"]
			important = exactCabinetMatch[0]["important"]
			#print date, name, elected, votes, preID
			#print "We have found a cabinet match"
			#print exactCabinetMatch
			#test = raw_input("press enter to continue")
		else:
			bestMatch = sorted(exactCabinetMatch, key=lambda k: (k["pm"], k["important"]), reverse=True)[0]
			servedNow=1
			servedEver=1
			pm = bestMatch["pm"]
			important = bestMatch["important"]
			#print "last", lastElecDate, "current", currElecDate, "match current", date, "next", nextElecDate
			#print "Multiple cabinet match"
			#pprint.pprint(exactCabinetMatch)
			#print "Selected"
			#pprint.pprint(bestMatch)
			#test = raw_input("press enter to continue")
	else:
		appointedCabinet = 0
		pastCabinet = 0
		
	currentRow = {"province": province, 
			"riding": riding, "date": date, "name": name, 
			"lastname": lastname, "firstname": firstname, 
			"party": party, "elected": elected, "electionnum": electionnum, 
			"votes": votes, "votePct": votePct, "percent": percent, "id": idNumber, "elected": elected, 
			"incumbent": incumbent, "incumbentTerms": incumbentTerms, "preID": preID,
			"servedEver": servedEver, "servedNow": servedNow,
			"birthday": birthday, "age": ageAtElection, "pm": pm,
			"important": important, "matchedNext": -1, "rowIndex": len(rowSet)}


	writer.writerow([idNumber, name, ageAtElection, incumbent, incumbentTerms, servedNow, servedEver, important, pm, date, party, riding, province, votes, percent, elected, lastVotes, lastVotePct])

	rowSet.append(currentRow)

	# Code designed to ensure that no one ever double matches
	if rowNumber>-1:
		if rowSet[rowNumber]["matchedNext"]>-1:
			jamNumber = rowSet[rowNumber]["matchedNext"]
			# It's OK if the double match is two by-elections or weird cabinet weirdness from pre-1900
			if currentRow["preID"] != rowSet[rowNumber]["preID"] or rowSet[rowNumber]["preID"] != rowSet[jamNumber]["preID"]:
				print "=====ALERT====="
				print "My current line found multiple matches. Let's investigate:"
				print "Me, I am: #", currentRow["preID"], currentRow["lastname"], currentRow["firstname"], "rep ", currentRow["party"], "from ", currentRow["riding"], currentRow["province"], "in ", currentRow["electionnum"]
				print "I matched: #", rowSet[rowNumber]["preID"], rowSet[rowNumber]["lastname"], rowSet[rowNumber]["firstname"], "rep ", rowSet[rowNumber]["party"], "from ", rowSet[rowNumber]["riding"], rowSet[rowNumber]["province"], "in ", rowSet[rowNumber]["electionnum"]

				print "But so did: #", rowSet[jamNumber]["preID"], rowSet[jamNumber]["lastname"], rowSet[jamNumber]["firstname"], "rep ", rowSet[jamNumber]["party"], "from ", rowSet[jamNumber]["riding"], rowSet[jamNumber]["province"], "in ", rowSet[jamNumber]["electionnum"]
				sys.exit(-1)
		else:
			rowSet[rowNumber]["matchedNext"] = rowSet[-1]["rowIndex"]

	#sys.exit(-1)
	
	if elected:
		winCount=winCount+1
		if winCount%50==0:
			print str(i)+"... Added 50 winners, including winner "+name+" in "+province+", "+riding+" (Election #"+str(electionnum)+")"
			pass
	else:
		#print str(i)+"... Added loser "+name+" in "+province+", "+riding+" ("+str(electionnum)+")"
		pass



newInputSet = [str(x) for x in newInputSet]
if len(newInputSet):
	strVar = ", ".join(newInputSet)
	with open("outputCodingDecisions.txt","w") as f:
		f.write(strVar)

print "\n\n========="
print "Sanity check of resulting data!"
print "elected in 1st election", 180, len([x for x in rowSet if x["electionnum"]==1 and x["elected"]==1])
print "elected in 42nd election", 338, len([x for x in rowSet if x["electionnum"]==42 and x["elected"]==1])
print "incumbents in 42nd election, should be high but not 338", len([x for x in rowSet if x["electionnum"]==42 and x["incumbent"]==1])
print "number of PMs ran in 42nd election", 1, len([x for x in rowSet if x["electionnum"]==42 and x["pm"]==1])
print "conservative candidates in 2015 election?", len([x for x in rowSet if x["electionnum"]==42 and x["party"]=="C"])
print "conservatives elected in 2015 election?", len([x for x in rowSet if x["electionnum"]==42 and x["party"]=="C" and x["elected"]==1])
print "incumbent conservatives in 2015 election?", len([x for x in rowSet if x["electionnum"]==42 and x["party"]=="C" and x["incumbent"]==1])
print "incumbent ministers running in 2015 election--20-ish?", len([x for x in rowSet if x["electionnum"]==42 and x["servedNow"]==1])
print "incumbent ministers elected?", len([x for x in rowSet if x["electionnum"]==42 and x["elected"]==1 and x["servedNow"]==1])

endTime = time.time()
if endTime-startTime>60:
	seconds = round((endTime-startTime)%60)
	minutes = math.floor((endTime-startTime)/60)
	print "Total execution time: "+str(minutes)+" minutes, "+str(seconds)+" seconds"
else:
	print "Total execution time: "+str(round(endTime-startTime))+" seconds"
