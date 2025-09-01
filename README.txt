This documentation contains all data and code necessary to reproduce the results of the paper
“The Effect of State Corporate Taxes on Unemployment Rates” by Vivian Johnson. The code was written in R version [4.4.0 (2024-04-24)]

The documentation consists of one main Documentation folder with two main subfolders.
- Code:

	- source_code: Contains the .Rmd file (analysis_code.Rmd) that is needed to produce the results of the paper. It also contains the .Rmd file
	(data_analysis.Rmd) that is needed to produce the rendered data analysis and exploration of the variables.
	- supplementary_code: Contains the R code that cleans the imported data. The source code references the dataset that was saved as a .csv after having 	cleaned it. However the source code is there as documentation.
- Data:
	- original_data: This folder contains the original data that was obtained before it was cleaned. The folders minimum_wage_data, state_gdp_data,
	state_population_data, state_tax_rt, and state_unemployment_data each correspond to the raw data files for each variable as they were not obtained 	from the same place.
	- analysis_data: This folder contains the cleaned data files that were used for analysis. The data in this folder was cleaned using the 		data_cleaning.R code located in the supplementary_code folder.

To reproduce the results of this paper:

1. Download the folder [econ_project_vjohnson] to your computer. All file paths in the code are relative and assume the folders are in the current structure,     to the structural integrity of the folder itself should not be altered.

2. Open the folder “source_code” within the [documentation] folder

3. Open the file [analysis_code.Rmd]

4. Run the code in the file