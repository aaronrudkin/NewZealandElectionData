months = ["", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
lines = [x.strip() for x in open("nz ministers 2002-2008.txt","r").read().split("\n") if len(x.strip())]
for l in lines:
	name, dateRange = l.split("\t")
	begin, end = dateRange.split(" - ")
	bD, bM, bY = begin.split(" ")
	eD, eM, eY = end.split(" ")
	bD = str(int(bD)).zfill(2)
	eD = str(int(eD)).zfill(2)
	bM = str(months.index(str(bM))).zfill(2)
	eM = str(months.index(str(eM))).zfill(2)
	newBegin = bY+"-"+bM+"-"+bD
	newEnd = eY+"-"+eM+"-"+eD
	print name+"\t"+newBegin+"\t"+newEnd
