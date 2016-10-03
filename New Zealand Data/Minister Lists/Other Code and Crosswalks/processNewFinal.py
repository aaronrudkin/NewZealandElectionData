lines = [x.strip() for x in open("nz ministers new.csv","r").read().split("\n") if len(x.strip())]
for l in lines:
	name, begin, end, important, pm = l.split(",")
	firstName, lastName = name.rsplit(" ",1)
	fullName = '"'+lastName+', '+firstName+'"'
	beginmonth, beginday, beginyear = begin.split("/")
	endmonth, endday, endyear = end.split("/")
	newBegin = beginyear+"-"+beginmonth.zfill(2)+"-"+beginday.zfill(2)
	newEnd = endyear+"-"+endmonth.zfill(2)+"-"+endday.zfill(2)
	print fullName+","+newBegin+","+newEnd+","+important+","+pm
