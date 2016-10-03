# Canadian government election results scraper

import bs4
import requests
import traceback
import sys
import codecs
import unidecode

def main():
	# First read main elections
	baseURL = "http://www.parl.gc.ca/About/Parliament/FederalRidingsHistory/hfer.asp?Language=E&Search=Gres&ridProvince=0&submit1=Search&genElection="
	for i in xrange(1,43):
		pageURL = baseURL+str(i)
		page = requests.get(pageURL).text.encode("iso-8859-1")
		try:
			readPage = bs4.BeautifulSoup(page,"html5lib")
		except:
			try:
				readPage = bs4.BeautifulSoup(page,"html.parser")
			except:
				print traceback.format_exc()
				sys.exit(-1)
	
		table = readPage.find("table").find_all("tr")
		for row in table:
			# We expect to find a province row
			try:
				tdSet = row.find_all("td")
				if len(tdSet)==1: # We're in a header row
					try:
						provCheck = tdSet[0].find("h5") # We're in a province row
						provName = provCheck.contents[0]
					except:
						if tdSet[0]["class"][0]=="rid": # We're in a riding row:
							ridingName = tdSet[0].find("a").contents[0]
							elecDate = tdSet[0].find("b").contents[1].rsplit(" ",1)[1][1:-1].replace("/","-")
							print elecDate
							break
			except:
				print traceback.format_exc()
				sys.exit(-1)

if __name__=="__main__":
	main()
