import sys
import re
import codecs

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

def filterName(name):
	try:
		last, first = name.split(", ")
		newName = last+", "+first.split(" ")[0]
	except:
		newName = name
	newName = re.sub("[^A-Za-z]","",newName).upper()
	return newName

def printOut(text):
	with codecs.open("1996.txt","a") as f:
		f.write(text+"\r\n")
	print text

with open("nz data 1996 no parties.txt") as f:
	lines = [x.strip() for x in f.read().split("\n")]

with open("partymemberpairs.txt") as f:
	pmlines = [x.strip() for x in f.read().split("\n")]

pairs = {}
membernames = []
for pml in pmlines:
	if not len(pml):
		continue

	try:
		party, name, junk = [x.upper().strip() for x in pml.split("\t",2)]
	except:
		try:
			party, name = [x.upper().strip() for x in pml.split("\t",1)]
		except:
			print pml
			sys.exit(-1)

	if not name in pairs:
		pairs[filterName(name)] = party
	else:
		print "uh oh, duplicate name."
		sys.exit(-1)
	membernames.append(filterName(name))

i=0
found=0
missed=0
badmiss=0
for l in lines:
	i=i+1
	if i==1:
		print l
		continue
	
	try:
		date, electorate, elNum, name, party, votes, elected = l.split("\t")
	except:
		date, electorate, elNum, name, party, votes = l.split("\t")
		elected = ""	
	nameUse = filterName(name)

	if len(party):
		printOut(date+"\t"+electorate+"\t"+elNum+"\t"+name+"\t"+party+"\t"+votes+"\t"+elected)
		print "pre-wrote "+name
	else:
		if nameUse.upper() in pairs:
			newParty = pairs[nameUse]
			printOut(date+"\t"+electorate+"\t"+elNum+"\t"+name+"\t"+newParty+"\t"+votes+"\t"+elected)
			print "i think "+name+" is "+pairs[nameUse]
			found=found+1
		else:
			bestGuess = ""
			bestScore = -999
			for mn in membernames:
				score = levenshtein(nameUse, mn)
				if score>bestScore:
					bestGuess = mn
					bestScore = score

			print "couldn't find "+name+", but best guess was "+bestGuess+" ("+str(bestScore)+")"
			if raw_input("Is this correct?").lower()=="y" and bestScore>=0.5:
				newParty = pairs[bestGuess]
				printOut(date+"\t"+electorate+"\t"+elNum+"\t"+name+"\t"+newParty+"\t"+votes+"\t"+elected)				
				found=found+1
			else:
				printOut(date+"\t"+electorate+"\t"+elNum+"\t"+name+"\t"+party+"\t"+votes+"\t"+elected)				
				missed=missed+1
				if int(votes)>1000:
					badmiss=badmiss+1

print found, missed, badmiss