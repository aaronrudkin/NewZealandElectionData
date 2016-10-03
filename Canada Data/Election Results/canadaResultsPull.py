# Canadian government election results scraper

import bs4
import requests
import traceback
import sys
import codecs
import unidecode

prematchIDs = [""]

with open("output.txt","w") as f:
	f.close()

def main():
	writeLine("PARLID\tPROVINCE\tRIDING\tDATE\tNAME\tPARTY\tJOB\tVOTES\tPERC\tELECTED\tELECTIONNUM")

	# First read main elections
	print "Main elections"
	baseURL = "http://www.parl.gc.ca/About/Parliament/FederalRidingsHistory/hfer.asp?Language=E&Search=Gres&ridProvince=0&submit1=Search&genElection="
	electionDates = ["0000-00-00"]
	for i in xrange(1,43):
		electionDates.append(readPage(baseURL+str(i),i, []))

	# Now read by-elections
	print "By Elections"
	byElectURL = "http://www.parl.gc.ca/About/Parliament/FederalRidingsHistory/hfer.asp?Language=E&Search=B"
	byElectCont = requests.get(byElectURL).text
	try:
		processByElect = bs4.BeautifulSoup(byElectCont,"html5lib")
	except:
		processByElect = bs4.BeautifulSoup(byElectCont,"html.parser")
	firstSelect = processByElect.find_all("select")[0].find_all("option")
	electDates = [str(x["value"]) for x in firstSelect if len(x["value"])]# and str(x["value"])>"1949/06"]
	electDates.reverse()
	byElectFinal = "http://www.parl.gc.ca/About/Parliament/FederalRidingsHistory/hfer.asp?Language=E&Search=Bres&ridProvince=0&genElection=0&submit1=Search&byElection="
	for date in electDates:
		readPage(byElectFinal+date,-1, electionDates)

	with open("birthdays.csv","w") as f:
		f.close()

	for i in xrange(1,len(prematchIDs)):
		key = i
		id = prematchIDs[i]
		print str(i)+"/"+str(len(prematchIDs)),
		value = readBio(id)
		with open("outBirthdates.csv","a") as f:
			f.write(str(key)+", "+value+"\n")

def decode(myStr):
	return unidecode.unidecode(myStr)

def writeLine(myStr):
	firstStrip = ' '.join(myStr.split("\n"))
	firstStrip = ' '.join(firstStrip.split("\r"))
	line = decode(firstStrip)
	print line
	with open("output.txt","a") as f:
		f.write(line)
		f.write("\n")

def readBio(url):
	print "Bio/DOB ", url
	try:
		res = requests.get("http://www.lop.parl.gc.ca"+url).text
	except:
		time.sleep(5)
		res = requests.get("http://www.lop.parl.gc.ca"+url).text

	try:
		processorBio = bs4.BeautifulSoup(res, "html5lib")
	except:
		processorBio = bs4.BeautifulSoup(res, "html.parser")

	try:
		dob = processorBio.find("span",{"id":"ctl00_cphContent_DateOfBirthData"}).contents[0]
	except:
		return ""

	if len(dob)==4:
		dob = dob+"-01-01"
	elif "." in dob:
		dob = dob.replace(".","-")		
	else:
		print "hmmmmm"
		print dob
	return dob

def readPage(pageURL, elecNum, electionDates):
	if elecNum<0:
		print "Reading by-election ", pageURL #, electionDates
	else:
		print "Reading election ", elecNum

	# Read the page into BS4
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
					else: # no idea what would be here
						# alternate province for by-elections
						provName = tdSet[0].find("b").contents[0]
			else: # we're in a header or candidate row.
				if tdSet[0].has_attr("bgcolor"):
					if tdSet[0]["bgcolor"]=="#00224a": # header row
						continue
					else: # no idea
						pass
				else: # candidate row
					# 0: candidate name, link if elected
					try:
						candidateName = tdSet[0].find("a").contents[0]
						candidateURL = tdSet[0].find("a")["href"]
						if not candidateURL in prematchIDs:
							prematchIDs.append(candidateURL)
							newID = len(prematchIDs)-1
						else:
							newID = prematchIDs.index(candidateURL)
					except:
						newID = 0
						candidateName = tdSet[0].find("font").contents[0]

					candidateName = candidateName.replace("Rt. Hon. ","").replace("Hon. ","").strip()

					# 1: party
					partyName = tdSet[1].find("a").contents[0]
					partyCluster = {"Lib": "Liberal", "I Lib": "Liberal", "S.C.": "Social Credit", "CCF": "NDP", "CA": "Conservative", "Ref.": "Conservative", "B.Q.": "Bloc", "BPC": "Bloc", "C": "Conservative", "Cons.": "Conservative", "G.P.": "Green", "I CCF": "NDP", "I PC": "Conservative", "Ind.": "Independent", "LLab": "Other", "LP": "Other", "LPP": "Other", "N.D.P.": "NDP", "N/A": "Independent", "NP": "Other", "P.C.": "Conservative"}
					try:
						clusterID = partyCluster[partyName]
					except:
						if "Ral. Cr" in partyName:
							clusterID = "Social Credit"
						else:
							clusterID = "Other"

					# 2: job
					try:
						job = str(tdSet[2].find("font").contents[0].replace("&nbsp;","").strip())
					except:
						job = ""

					# 3: raw vote count
					try: # check if candidate was acclaimed
						if tdSet[5].find("font"): 
							voteCount = -1
							votePerc = 100.00
						else:
							voteCount = int(tdSet[3].find("font").contents[0].replace("&nbsp;","").replace(",","").strip())

							# 4: percentage
							votePerc = float(tdSet[4].find("font").contents[0].replace("&nbsp;","").replace("%","").strip())

					except:
						voteCount = 0
						votePerc = 0.0

					# 5: elected?
					try:
						if votePerc == 100.00:
							elected=1
						elif tdSet[5].find("img"):
							elected=1
						else:
							elected=0
					except:
						print traceback.format_exc()
						sys.exit(-1)
						elected = -1

					if elecNum==-1:
						for j in range(0,len(electionDates)):
							try:
								if elecDate>electionDates[j] and elecDate<electionDates[j+1]:
									break
							except:
								pass
						elecNum = j+0.5
					else:
						pass

					writeLine(str(newID)+"\t"+provName+"\t"+ridingName+"\t"+elecDate+"\t"+candidateName+"\t"+partyName+"\t"+job+"\t"+str(voteCount)+"\t"+str(votePerc)+"\t"+str(elected)+"\t"+str(elecNum))
		except:
			print traceback.format_exc()
			sys.exit(-1)
			pass
	return elecDate

if __name__=="__main__":
	main()
	#readBio("http://www.lop.parl.gc.ca/parlinfo/Files/Parliamentarian.aspx?Item=62BFD797-8D32-425B-91B1-1422CA16694F&Language=E")
