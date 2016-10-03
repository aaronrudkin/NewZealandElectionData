def starConverter(p):
	if float(p)<=0.01:
		return "***"
	elif float(p)<=0.05:
		return "**"
	elif float(p)<=0.1:
		return "*"
	else:
		return ""

def doNum(n):
	if not "." in n:
		return n+".00"
	else:
		before, after = n.split(".")
		if len(after)==1:
			return n+"0"
		elif len(after)==2:
			return n
		else:
			pass

def crosswalkFile(file, title):
	base = r"""
\begin{table}[!htb] \centering 
"""
	base = base + r"  \caption{"+title+"}\n"
	base = base + r"""
  \label{} 
\scalebox{0.9}{\begin{tabular}{@{\extracolsep{5pt}}lccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & Cross-Sectional OLS & Control For Past OLS & Cross-Sectional Logit \\
\hline \\[-1.8ex] 
"""

	data = [x.strip() for x in open(file+".txt","r").read().split("\n") if len(x)]
	i=0
	for row in data:
		if i:
			tabSet = row.split("\t")
			base = base + "Decade: "+tabSet[0]+" & "+doNum(tabSet[1])+starConverter(tabSet[4])+" & "+doNum(tabSet[5])+starConverter(tabSet[8])+" & "+doNum(tabSet[9])+starConverter(tabSet[12])+" \\\\\n"
			base = base + " & ("+doNum(tabSet[2])+", "+doNum(tabSet[3])+") & ("+doNum(tabSet[6])+", "+doNum(tabSet[7])+") & ("+doNum(tabSet[10])+", "+doNum(tabSet[11])+") \\\\\n"
		i=i+1
	base = base + r"""\hline 
\hline \\[-1.8ex]
\textit{Note:} & \multicolumn{3}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\
 & \multicolumn{3}{r}{OLS CIs from Heteroskedasticity-robust standard errors} \\ 
\end{tabular}}
\end{table}
"""

	with open(file+".tex","w") as f:
		f.write(base)

crosswalkFile("decadeDecompositionCA","Decade Disaggregation of Main Model Results, Canada")
crosswalkFile("decadeDecompositionNZ","Decade Disaggregation of Main Model Results, NZ")