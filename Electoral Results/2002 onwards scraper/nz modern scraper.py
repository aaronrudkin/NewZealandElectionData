import traceback
import requests
import bs4
import sys
import codecs
import unicodedata

elections = [("http://www.electionresults.govt.nz/electionresults_2002/electorateindex.html","2002-07-27"),("http://www.electionresults.govt.nz/electionresults_2005/electorateindex.html","2005-09-17"),("http://www.electionresults.govt.nz/electionresults_2008/electorateindex.html","2008-11-08"),("http://www.electionresults.govt.nz/electionresults_2011/electorateindex.html","2011-11-26"),("http://www.electionresults.govt.nz/electionresults_2014/electorateindex.html","2014-09-20")]

byElections = [("http://www.electionresults.govt.nz/2004_byelection/","2004-07-20",1), ("http://www.electionresults.govt.nz/2009_mt_albert_byelection/","2009-06-13"), ("http://www.electionresults.govt.nz/2010_mana_byelection/","2010-11-20"), ("http://www.electionresults.govt.nz/2011_botany_byelection/","2011-03-05"), ("http://www.electionresults.govt.nz/2011_te_tai_tokerau_byelection/","2011-06-25"), ("http://www.electionresults.govt.nz/2013_ikaroa_rawhiti_byelection/","2013-06-29"), ("http://www.electionresults.govt.nz/2013_christchurch_east_byelection/","2013-11-30"), ("http://www.electionresults.govt.nz/2015_northland_byelection/","2015-03-28")]

def remove_accents(input_str):
	nfkd_form = unicodedata.normalize('NFKD', input_str)
	only_ascii = nfkd_form.encode('ASCII', 'ignore')
	return only_ascii

def printOut(text):
	with codecs.open("nzmodern.txt","a") as f:
		f.write(text+"\r\n")
	print text

def processElection(electionURL, eDate):
	page = requests.get(electionURL).text
	parser = bs4.BeautifulSoup(page,"html.parser")
	mainTable = parser.find_all("table",{"class": "maintable"})[0].find_all("table")[0]
	tableRows = mainTable.find_all("tr")[2:]
	baseFile = electionURL.rsplit("/",1)[0]
	for row in tableRows:
		cells = row.find_all("td")
		for cell in cells:
			try:
				url = cell.find("a")["href"]
				processElectorate(baseFile+"/"+url, 0, eDate)
			except:
				print traceback.format_exc()

def processElectorate(electorateURL, byElection, eDate):
	if byElection==0:
		fieldsOfInterest = [3,4,5]
	elif byElection==2:
		fieldsOfInterest = [0,4,5]
	else:
		fieldsOfInterest = [0,1,3]

	page = requests.get(electorateURL).text
	parser = bs4.BeautifulSoup(page, "html.parser")
	electorateName = remove_accents(parser.find("title").contents[0].split(" -- ")[1].strip())
	try:
		mainTable = parser.find_all("table",{"class": "maintable"})[0].find_all("table")[1]
		winner = remove_accents(mainTable.find_all("tr")[3].find_all("td")[0].contents[0].split("(")[0].strip())
	except:
		mainTable = parser.find_all("table",{"class": "maintable"})[0].find_all("table")[0]
		winner = remove_accents(mainTable.find_all("tr")[3].find_all("td")[0].contents[0].split("(")[0].strip())
		
	electorateNumber = int(mainTable.find_all("tr")[1].find_all("td")[0].contents[0])
	tableRows = mainTable.find_all("tr")[6:-2]
	foundElected = 0
	for row in tableRows:
		candidateName = ""
		cells = row.find_all("td")
		try:
			candidateName = remove_accents(cells[fieldsOfInterest[0]].contents[0].strip())
			if len(candidateName.strip())<3:
				continue
			
			partyName = cells[fieldsOfInterest[1]].contents[0]
			votes = int(cells[fieldsOfInterest[2]].contents[0].replace(",",""))
			elected = 1 if winner==candidateName or winner==candidateName.split(" (")[0] else 0
			foundElected = foundElected or elected
		except: 
			print traceback.format_exc()
			continue # Empty row in results
		printOut(eDate+"\t"+electorateName+"\t"+str(electorateNumber)+"\t"+candidateName+"\t"+partyName+"\t"+str(votes)+"\t"+str(elected))
	if not foundElected:
		print "!!!!!!!!!!!! "+electorateName+" !!!!!!!!!!"


for election in elections:
	processElection(election[0], election[1])
for byElection in byElections:
	if len(byElection)==2:
		processElectorate(byElection[0],1,byElection[1])
	else: # Set a flag for the 2004 by-election which has a weird format.
		processElectorate(byElection[0],2,byElection[1])