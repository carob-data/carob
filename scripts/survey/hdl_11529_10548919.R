# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. dataset focuses on a rice-wheat system, so every variable captured is a combination of the 2 crops


carob_script <- function(path) {

"
Data on scoping study for laser land levelling services in the rice-wheat cropping systems of western Nepal

The data comes from a survey designed to understand the scope of LLL services in four districts of Mid- and Far-western developmental regions of Nepal Terai that are part of the IGB and have the potential to scale-up LLL. Samples were collected from the districts Banke, Bardiya, Kailali and Kanchanpur. The International Maize and Wheat Improvement Center, Nepal, led the data collection in 2015. From each district, sub-districts (Village Developmental Committees, or VDCs) were purposively selected from Cereal Systems Initiatives for South Asia (CSISA) project (https://csisa.org/) working areas. The project was operational in 10 VDCs. The sampling strategy consisted of 50 percent of samples randomly selected from the list within CSISA project area and 50 percent of samples randomly selected beyond the CSISA project areas from the same VDCs. From each VDC, 40 sample households were selected randomly, resulting in an overall sample of 400 households (40 households from 10 sub-districts of four districts). An additional six samples were collected from one of the districts due to a higher proportion of rice-wheat systems in that district, bringing the overall sample size to 406.
"

	uri <- "hdl:11529/10548919"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIMMYT",
		publication = "doi:10.1108_JED_10_2022_0201",
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-07-09",
		carob_completion = 90,	
		carob_effort = 6
	)
	
	f <- ff[basename(ff) == "Data_LLL_ Western_Nepal.xlsx"]

	r <- carobiner::read.excel(f)

	d <- data.frame(
	  hhid=as.character(r$hhid),
	  country="Nepal",
	  adm1=r$q11b_region,
	  adm3=tools::toTitleCase(tolower(r$q11c_district)),
	  farmland=r$farm_sizeha,
	  hh_size=r$hh_size,
	  sex=r$sex,
	  education=as.character(r$education),#years of formal education attained
	  cropland_used=r$culti_area_RW,
	  land_prep_cost=r$Tillcost_Rsha,
	  irrigation_cost=r$irrigcost_RW_perfarm,
	  crop="rice;wheat",#survey is from a rice-wheat system, variables captured are for a system rather than a single crop
	  yield=r$GY_R_W_kgha,
	  currency="NPR"
	)
	
	d$trial_id <- paste(d$hhid,d$adm3,sep = "_")
	d$sex <- ifelse(d$sex==1,"male","female")
	d$adm1 <- gsub("mwr","Mid-Western",d$adm1)
	d$adm1 <- gsub("fwr","Far-Western",d$adm1)
	
	d$on_farm <- TRUE 
	d$is_survey <- TRUE
	d$irrigated <- TRUE
	d$geo_from_source <- FALSE
	d$planting_date <- as.character(NA)
	
  d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	
	#location data
	loc <- data.frame(
	  adm1 = c("Far-Western", "Far-Western", "Mid-Western", "Mid-Western"),
	  adm2 = c("Mahakali", "Seti", "Bheri", "Bheri"),
	  adm3 = c("Kanchanpur", "Kailali", "Banke", "Bardiya"),
	  longitude = c(80.3233, 80.8808, 81.8242, 81.4032),
	  latitude = c(28.8617, 28.7416, 28.087, 28.3836),
	  geo_uncertainty = c(39487, 47766, 39293, 36249),
	  geo_source = c("GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3")
	)
	
	d <- merge(d,loc,by=c("adm1","adm3"),all.x = TRUE)
	
	carobiner::write_files(path, meta, d)
}
