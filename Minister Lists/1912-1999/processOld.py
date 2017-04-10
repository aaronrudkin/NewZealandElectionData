lines = [x.strip() for x in open("nz ministers.csv","r").read().split("\n") if len(x.strip())]
for l in lines:
	name, name2, begin, end, important, pm = l.split(",")
	fullName = name+","+name2
	beginmonth, beginday, beginyear = begin.split("/")
	endmonth, endday, endyear = end.split("/")
	newBegin = beginyear+"-"+beginmonth.zfill(2)+"-"+beginday.zfill(2)
	newEnd = endyear+"-"+endmonth.zfill(2)+"-"+endday.zfill(2)
	print fullName+","+newBegin+","+newEnd+","+important+","+pm
