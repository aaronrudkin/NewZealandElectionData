import re
import codecs
def printOut(text):
	with codecs.open("1990.txt","a") as f:
		f.write(text+"\r\n")
	print text

with open("Electorate Totals.csv","r") as f:
	lines = f.read().split("\n")

with open("CandidateNames.txt","r") as f:
	clines = f.read().split("\n")

candidates = {}
baseElectorate = ""
for cline in clines:
	if re.match("^[1-9][0-2]?\. ",cline):
		results = re.search("^([1-9][0-2]?\. )(.*)\((.*)\)",cline)
		candidates[baseElectorate].append((results.group(2), results.group(3)))
	else:
		baseElectorate = cline.strip()
		candidates[baseElectorate] = []

for line in lines:
	voteTotal = [int(x) for x in line.split(",")[1:] if x!=""]
	electorateName = line.split(",")[0]
	if electorateName=="":
		break
	i=0
	for candidate in candidates[electorateName]:
		elected = 1 if voteTotal[i]==max(voteTotal) else 0
		printOut("1990-10-27\t"+electorateName+"\t"+candidate[0]+"\t"+candidate[1]+"\t"+str(voteTotal[i])+"\t"+str(elected))
		i=i+1
