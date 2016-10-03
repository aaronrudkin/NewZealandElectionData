dat = open("model3.tex","r").read()
if not r"\scalebox" in dat:
	dat = dat.replace(r"\begin{tabular}",r"\scalebox{0.9}{\begin{tabular}")
	dat = dat.replace(r"\end{tabular}",r"\end{tabular}}")
	print dat
	with open("model3.tex","w") as f:
		f.write(dat)
else:
	print "already ok"

dat = open("appendix3.tex","r").read()
if not r"\resizebox" in dat:
	dat = dat.replace(r"\begin{tabular}",r"\resizebox{\textwidth}{!}{\begin{tabular}")
	dat = dat.replace(r"\end{tabular}",r"\end{tabular}}")
	print dat
	with open("appendix3.tex","w") as f:
		f.write(dat)
else:
	print "already ok"