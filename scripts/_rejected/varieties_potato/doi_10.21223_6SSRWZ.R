# R script for "carob"
# license: GPL (>=3)

## ISSUES
# added variables male, female
# The dataset does not have the location
# The data does not have yield

carob_script <- function(path) {

"
Dataset for: Selected LBHT families,  pedigree  and number of genotypes tested in Kenya

Selected 27 LBHT families for phenotyping and genotyping in Kenya as part of a genomic selection strategy for late blight resistance. The date set  is informing the list of families, the pedigree and number of genotypes evaluated.
"

	uri <- "doi:10.21223/6SSRWZ"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-08-26",
		carob_completion = 90,	
		carob_effort = 4
	)
	
	f1 <- ff[basename(ff) == "01_Data_Genotyping_List_LBTH-Kenya.xlsx"]
	#f2 <- ff[basename(ff) == "02_Genotyping_list_LBHT-Kenya.xlsx"]      

	r1 <- carobiner::read.excel(f1)
	#r2 <- carobiner::read.excel(f2)

	d <- data.frame(
	  country = "Kenya",
	  male = r1$Male,
	  female = r1$Female,
		variety = r1$Clones,
		variety_pedigree = paste(r1$Male,r1$Female,sep = " X "),
		crop = "potato"
	)|> unique()

	d$trial_id <- "1"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA

	d$longitude <- NA
	d$latitude <- NA
	d$geo_from_source <- FALSE

	d$planting_date <- NA
	d$harvest_date  <- NA

  d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- NA
  d$fertilizer_type <- NA

	d$yield <- NA
	d$yield_part <- "tubers"
	d$yield_moisture <- NA
	d$yield_isfresh <- TRUE
	
	carobiner::write_files(path, meta, d)
}



