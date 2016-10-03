import requests
import bs4

for i in xrange(1,151):
	url = "http://www.lop.parl.gc.ca/About/Parliament/FederalRidingsHistory/hfer-party.asp?lang=E&Party="+str(i)
	dat = requests.get(url).text
	parser = bs4.BeautifulSoup(dat,"html.parser")
	try:
		sum = parser.find("b").contents[0]
		det = parser.find_all("font")[1].contents[0]
		print sum+"\t"+det
	except:
		print i
		pass