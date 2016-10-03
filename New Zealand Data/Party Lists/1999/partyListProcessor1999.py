import bs4
import sys
import unidecode

def outputChunk(date, party,n(electedText)
	for i in range(0,len(names)):
		if len(electedText)<=i:
			elected=0
			electedText.append("")
		elif "Seat Allocation" in ' '.join(electedText[i].split()):
			print i, electedText[i], names[i]
			elected=2
		elif "Electorate" in electedText[i] or len(electedText[i])>4:
			print i, electedText[i], names[i]
			elected=1
		else:
			print i, electedText[i], names[i]
			elected=0

		if party=="LABOUR PARTY":
			party = "Labour"
		elif party=="ALLIANCE":
			party = "Alliance"
		elif party=="ACT NEW ZEALAND":
			party = "ACT"
		elif party=="GREEN PARTY":
			party = "GP"
		elif party=="NATIONAL PARTY":
			party = "National"
		elif party=="NEW ZEALAND FIRST PARTY":
			party = "NZF"
		elif party=="UNITED NZ PARTY":
			party = "UFNZ"

		print date+"\t"+party+"\t"+unidecode.unidecode(' '.join(names[i].split()))+"\t"+str(i+1)+"\t"+str(elected)

party1 = ""
party2 = ""
numbers1 = []
numbers2 = []
names1 = []
names2 = []
elected1 = []
elected2 = []
fileSet = ["1999 parties.html"]
dateSet = ["1999-11-27"]
i=0
for f in fileSet:
	date = dateSet[i]
	i=i+1
	data = open(f,"r").read()
	parser = bs4.BeautifulSoup(data,"html5lib")
	tables = parser.find_all("table")
	for table in tables[1:]:
		rows = table.find_all("tr")
		for row in rows:
			columns = row.find_all("td")
			if len(columns)==1:
				pass
			elif len(columns)==2:
				if party1:
					outputChunk(date, party1, names1, electedText1)
					names1 = []
					electedText1 = []
				if party2:
					outputChunk(date, party2, names2, electedText2)
					names2 = []
					electedText2 = []

				if columns[0].find("b"):
					party1 = columns[0].find("b").contents[0]
				else:
					party1 = ""
				if columns[1].find("b"):
					party2 = columns[1].find("b").contents[0]
				else:
					party2 = ""
			else:
				if columns[0].find("p"):
					if party1:
						names1 = [x.strip() for x in columns[1].find("p").strings]
						electedText1 = [x.strip() for x in columns[2].find("p").find("i").strings]
						print party1
						print electedText1
					if party2:
						names2 = [x.strip() for x in columns[4].find("p").strings]
						electedText2 = [x.strip() for x in columns[5].find("p").find("i").strings]
				else:
					pass

	if party1:
		outputChunk(date, party1, names1, electedText1)
	if party2:
		outputChunk(date, party2, names2, electedText2)