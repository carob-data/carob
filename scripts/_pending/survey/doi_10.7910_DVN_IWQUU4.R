# R script for "carob"

carob_script <- function(path) {

"The Myanmar Agricultural Performance Survey (MAPS) is a nationwide phone panel consisting of approximately 5,500 households. The objective of the survey is to collect data on farm characteristics and agricultural assets, area and crops planted, access to inputs, crop marketing, and constraints in agricultural activities. The respondents interviewed are a sub-sample of the Myanmar Household Welfare Survey. A novel sampling strategy in combination with the development of household and population weights allows for estimates that are nationally, regionally, and urban/rural representative. MAPS Round 4 survey was implemented by phone by Myanmar Survey Research (MSR) from June to July, 2023. The MAPS Round 4 sample has 5,001 combined respondents from Myanmar Household Welfare Survey (MHWS) Round 5 and MAPS Round 3. This includes 1,342 respondents not previously interviewed for any round of MAPS but interviewed in MHWS. MAPS Round 4 compares agricultural production in the pre/post monsoon 2022 and the pre/post monsoon 2023."

	uri <- "doi:10.7910/DVN/IWQUU4"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	return(TRUE)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Robert Hijmans",
		carob_date = "2025-04-07"
	)
	

	f <- ff[basename(ff) == "MAPS_R4.dta"]
	r <- carobiner::read.dta(f) 
	
	b <- read.csv(ff[basename(ff) == "MAPS_R4_codebook.csv"])
	unique(b[,1:2])
	
	d <- data.frame(
		country = "Myanmar",
		hhid = as.character(r$hhid),
		on_farm = TRUE,
		is_survey = TRUE,
		#date <- r$date ??
		adm1 = r$b_06,
		adm3 = r$d_01
	)
			
	carobiner::write_files(path, meta, d)
}



