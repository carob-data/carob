# R script for "carob"
# license: GPL (>=3)

## ISSUES

# Rejected because no provenance is provided, but the N, P, K and yield data are likely from LSMS-ISA and the original sources should be used for that. Also, most variables are clearly derived from lon/lat, not actual observations 


carob_script <- function(path) {

"
Farm-Level Rice Yield Data and Climate Change Impact Projections Across Rice Ecologies in West Africa (2012–2080)

The dataset is an Excel file with two sheets covering rice yield modeling and climate impact assessment across 12 West African countries (Benin, Burkina Faso, C&ocirc;te d'Ivoire, Ghana, Guinea, Mali, Niger, Nigeria, Senegal, Sierra Leone, The Gambia, and Togo), spanning four rice ecologies: irrigated lowland dry season (IL-Dry), irrigated lowland wet season (IL-Wet), rainfed lowland (RL), and rainfed upland (RU).Sheet 1 &ndash; Baseline Data set (16,049 records, 20 variables): Contains observed farm-level data collected between 2012 and 2020. Each record corresponds to a unique plot (identified by a code) and includes baseline rice yield (t/ha), key climate variables (maximum and minimum temperature, precipitation, relative humidity, solar radiation), site characteristics (elevation), soil properties (available soil water capacity, total nitrogen, pH, soil organic carbon, clay and sand content), and fertilizer inputs (N, P, K).Sheet 2 &ndash; RCP4.5_8.5 (28,616 records, 13 variables): Contains modeled future rice yields for the same plots under two climate scenarios &mdash; RCP 4.5 and RCP 8.5 &mdash; for four time horizons: 2030, 2050, 2070, and 2080. Alongside projected absolute yields (t/ha), it includes the percentage change (&Delta;%) relative to the baseline yield, allowing direct quantification of climate change impacts on rice production across ecologies, countries, and scenarios.
"

	uri <- "doi:10.5281/zenodo.21075806"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "none",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-10-16",
		carob_completion = 0,	
		carob_effort = 4
	)
	

	f1 <- ff[basename(ff) == "Dataset_modeling%20and%20impact%20assessment.xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="Baseline Data set")
	r1b <- carobiner::read.excel(f1, sheet="RCP4.5_8.5")

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		country = r1a[["Country"]],
		year = r1a[["Year"]],
		yield = r1a[["Baseline_Rice_Yield"]],
		elevation = r1a[["Elevation"]]
	)
##r1a: "Code", "Max_Temperature_baseline", "Min_Temperature_baseline", "Precipitation_baseline", "Relative_humidity_baseline", "Solar_Radiation_baseline", "ASWC", "Ntot", "pH_H2O", "SOC", "Clay_cont", "sand_cont", "Ninput", "Pinput", "Kinput", "Ecology"


	d1b <- data.frame(
		country = r1b[["Country"]],
		yield = r1b[["Baseline Rice Yield"]]
	)
	
	carobiner::write_files(path, meta, d)
}
