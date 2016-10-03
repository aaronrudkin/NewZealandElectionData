1. To replicate the paper and regenerate the results, run COMPILE from the Terminal (Mac OS X / Linux). In order, this does the following:
	i) 	Assembles the panel data using the raw data files
		and pre-saved replication scripts. (5+ minutes Canada, 
		1 minute NZ)
	ii)	Generates synthetic observations for the fifth main
		model (~20 seconds)
	iii)	Runs data analysis for Canada, then for New Zealand. 
		Due to the large dataset and processor-intensive 
		bootstrap process for Model 3, this will take 5-10 
		minutes. This saves the model objects.
	iv)	Intakes these model objects into the script which 
		generates tables and plots, outputting them to the 
		Includes/ folder.
	v)	Runs scripts which modify the LaTeX output to correct
		for some scaling issues from stargazer and runs the
		script to generate Appendix 1.
	vi)	Removes the saved model objects.

	If you plan to manually run code, the assumed working directory 
	is this folder; many file loads use relative paths. Please
	ensure you have correctly set your path in R.

	Code run in Rscript 3.3.1; Python 2.7.12.
	

2. Location and contents of scripts:
	Paper and analysis:

	/Canada Electorate Histogram.r
		Generates unused histogram graph for Canada
	/Canada Full Dataset.r
		Runs alternate specification without dropping
		nuisance candidates
	/CanadaDataAnalysis.r
		Main data analysis for Canada
	/ENP Calculator.r
		Function to calculate ENP scores for Case Selection
		section
	/New Zealand Electorate Control.r
		Generates the histogram graph for New Zealand,
		and Appendix 4.
	/NewZealandDataAnalysis.r
		Main data analysis for New Zealand
	Includes/genDecadeDecomposition.py
		Converts R's table output for the decade de-compo
		action graphs into LaTeX
	Includes/GeneratePlotsAndFigures.r
		Converts model objects saved by data analysis files
		into plots and figures
	Includes/shrinkTex.py
		Modifies outputted tables and figures to fit the
		screen properly.

	Constructing Dataset:

	Canada Data/Cabinet/canCabinet.py
		Scrapes cabinet appointments from Library of
		Parliament
	Canada Data/Election Results/canadaResultsPull.py
		Scrapes Canadian election data and birthdates
	Canada Data/Merge/processResult.py
		Assembles scraped Canadian data into panel
	Canada Data/Party Names/canadaPartyNames.py
		Scrapes all parties so I can assemble a list
		for fuzzy matching of minor party name changes.
	Canada Data/SyntheticObs/
			syntheticObservationExportCanada.R
		Exports the list of observations that need
		synthetic observations added to them.
	Canada Data/SyntheticObs/processResults.py
		Adds cabinet metadata and ID values to the
		synthetic observations to get them ready for
		R.

	(New Zealand follows a similar structure, but there
	are several more scrapers)

3. Datasets for final analysis:
	Canada Data/Merge/output.csv
	New Zealand Data/Merge/output.csv
		Main panel datasets
	
	Canada Data/SyntheticObs/synthOut.csv
	New Zealand Data/SyntheticObs/synthOut.csv
		Synthetic observations