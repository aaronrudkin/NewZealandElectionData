import bs4
import unidecode

fileSet = ["1999 parties.html", "2002 parties.html", "2005 parties.html", "2008 parties.html", "2011 parties.html", "2014 parties.html"]
dateSet = ["1999-11-27", "2002-07-27", "2005-09-17", "2008-11-08", "2011-11-26", "2014-09-20"]
i=1
for f in fileSet[1:]:
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
				if columns[0].find("font"):
					partyName = columns[0].find("font").find("b").contents[0]
					if partyName=="ACT New Zealand":
						partyName="ACT"
					elif partyName=="Green Party":
						partyName="GP"
					elif "Jim Anderton's Progressive" in partyName:
						partyName="Progressive"
					elif partyName=="Labour Party":
						partyName="Labour"
					elif "ori Party" in partyName:
						partyName = "Maori"
					elif partyName=="National Party":
						partyName = "National"
					elif partyName=="New Zealand First Party":
						partyName = "NZF"
					elif "United Future" in partyName:
						partyName = "UFNZ"
					else:
						pass
				else:
					pass
			else:
				listNum = int(columns[0].find("font").contents[0])
				name = columns[1].find("font").contents[0]
				electedStatus = columns[2].find("font").contents
				status = 0
				if electedStatus[0]==u'\xa0':
					status = 0
				elif "Electorate" in electedStatus[0]:
					status = 1
				elif "Allocation" in electedStatus[0]:
					status = 2
				else:
					status = -999
				print date+"\t"+partyName+"\t"+unidecode.unidecode(name)+"\t"+str(listNum)+"\t"+str(status)

