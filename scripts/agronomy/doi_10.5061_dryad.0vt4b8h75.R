# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Long-term tillage and cover cropping differentially influenced soil nitrous oxide emissions from cotton cropping system

Climate-smart agricultural practices, such as no-tillage (NT) and cover cropping, have been widely adopted and are anticipated to yield multiple benefits, including soil carbon sequestration, enhancing soil health, and crop yield stability. However, their influence on nitrous oxide (N2O) emissions varies, with the potential of both increasing and decreasing N2O emissions. Increasing N2O emissions under these practices may potentially offset the climate mitigation benefits from increased soil carbon sequestration. We investigated N2O emissions in response to 42 years of long-term adoption of NT and legume cover crop, under nitrogen rates of 0 and 67 kg N ha-1, in a continuous cotton system in the Southeastern US. Intensive manual chamber-based measurements were conducted over two growing seasons (2021-2022 and 2022-2023). Long-term NT did not significantly (p &gt; 0.05) affect cumulative N2O emissions during the study period. Hairy vetch cover crop-grown plots emitted 2-3 times more N2O than those without cover crops in 2021-2022, with no significant effect observed in 2022-2023. Cumulative emissions in cover crop plots were greater compared to those in no cover crop plots when fertilized with 67 kg N ha-1 in 2021-2022; however, this trend did not persist in 2022-2023. While interannual variability exists, our results generally suggest that long-term NT may not increase N2O emissions, hence its adoption could enhance its broader soil health and climate benefits via soil carbon sequestration. Whereas managing legume cover crop residues in N-fertilized systems is critical to mitigate N2O emissions.
"

	uri <- "doi:10.5061/dryad.0vt4b8h75"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
		data_organization = "UTK", # University of Tennessee at Knoxville
		publication = "doi:10.1002/agj2.21661",
		project = NA,
		carob_date = "2026-07-06",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;land_prep_method;cover_crop",
		response_vars = "N2O_emission;CO2_emission", 
		carob_contributor = "Cedric Ngakou",
		carob_completion = 100,	
		notes = NA,
		carob_effort = 1
	)
	

	f1 <- ff[basename(ff) == "Dhaliwal_et_al_2024_AGRONJ.xlsx"]
	f2 <- ff[basename(ff) == "README.md"]

	r1 <- carobiner::read.excel(f1, sheet="N2O+CO2+VWC+temp", na= "NA")
	r2 <- carobiner::read.excel(f1, sheet="Soil biogeochemical", na= "NA")
	
	### process
	
	d1 <- data.frame(
	  N_fertilizer = ifelse(grepl("67", r1$`N rate`), 67, 0) ,
	  cover_crop = ifelse(grepl("HV", r1$`Cover crop`), "vetch", "none"),
	  land_prep_method = ifelse(grepl("CT", r1$Tillage), "conventional", "none") ,
	  rep = as.integer(gsub("R", "", r1$Replications)),
	  N2O_emission = as.numeric(r1$`N2O flux`),
	  CO2_emission = as.numeric(r1$`CO2 flux`),
	  #soil_temperature = as.numeric(r1$`Soil temperature`),
	  date = as.character(r1$`Sampling date`)
	)
	
	dd <- aggregate(. ~ date + N_fertilizer+ cover_crop + land_prep_method + rep,d1, function(X) mean(X) )
	
	#####
	d2 <- data.frame(
	  N_fertilizer = ifelse(grepl("67", r2$`N rate`), 67, 0),
	  cover_crop =  ifelse(grepl("HV", r2$`Cover crop`), "vetch", "none"),
	  land_prep_method = ifelse(grepl("CT", r2$Tillage), "conventional", "none"),
	  rep =  as.integer(gsub("R", "", r2$Replications)),
	  soil_NH4 = r2$`Soil NH4+`,
	  soil_N03 = r2$`Soil NO3-`,
	  depth = 10,
	  date = as.character(r2$`Sampling date`)
	  
	)
	
	d <- merge(dd, d2, by= intersect(names(dd), names(d2)), all = TRUE)
	
	d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	d$crop <-"cotton"
	d$country <- "United States"
	d$location <- "West Tennessee Research and Education Center"
	d$longitude <- -88.85  # from paper
	d$latitude <- 35.63
	d$trial_id <- "1" 
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$yield_part <- "none"
	d$yield <- NA
	d$yield_moisture <- NA_real_
	d$yield_isfresh <- NA
	d$geo_from_source <- TRUE # from paper 
	d$planting_date <- NA_character_
	d$irrigated <- NA
	
	carobiner::write_files(path, meta, d)
}

