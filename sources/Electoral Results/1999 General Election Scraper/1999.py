import requests
import bs4
import csv
import unicodedata
import codecs

url = "http://www.electionresults.govt.nz/electionresults_1999/e9/html/e9_partVIII.html"
def remove_accents(input_str):
	if type(input_str)==type(""):
		input_str = input_str.decode()
	nfkd_form = unicodedata.normalize('NFKD', input_str)
	only_ascii = nfkd_form.encode('ASCII', 'ignore')
	return only_ascii

def printOut(text):
	with codecs.open("1999.txt","a") as f:
		f.write(text+"\r\n")
	print text

def doElection(url, date):
	folder = url.rsplit("/",2)[0]+"/"
	page = requests.get(url).text
	parser = bs4.BeautifulSoup(page,"html.parser")
	rows = parser.find_all("table")[2].find_all("tr")[1:]
	for row in rows:
		electorateNumber = row.find_all("td")[0].contents[0]
		electorateName = remove_accents(row.find_all("td")[1].find("a").contents[0])
		#print electorateNumber, electorateName
		link = row.find_all("td")[4].find("a")["href"].split("/",1)[1]
		dl = requests.get(folder+link).text
		csvVer = csv.reader(remove_accents(dl).split("\n"),delimiter=",")
		i=0
		foundWinner = 0
		for line in csvVer:
			i=i+1
			if i==1:
				continue
			elif i==2:
				result = remove_accents(" ".join(line[5].split(" - ")[0].split()))
				continue

			name, party, vote, perc = line[1:5]
			name = remove_accents(name)
			if not len(name) and i>5:
				break
			elif not len(name):
				continue
			elected = 1 if name.upper().strip()==result.upper().strip() else 0
			foundWinner = foundWinner or elected
			printOut(date+"\t"+electorateNumber+"\t"+electorateName+"\t"+name+"\t"+party+"\t"+vote+"\t"+str(elected))
		if not foundWinner:
			print "!!!!!!!!!!!!!!!!"
			print result
	return

doElection(url, "1999-11-27")
