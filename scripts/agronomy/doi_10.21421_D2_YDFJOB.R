# R script for "carob"

carob_script <- function(path) {
  
"Title:Sorghum productivity and water use under Phosphorus fertilization in the Sudan savanna of Nigeria 
Abstract: Low soil fertility and water shortage are major constraints to food production and food security in semi-arid environments. Field experiments were conducted during two growing seasons (2014 and 2015) in Nigeria. The study examined the effects of Phosphorus (P) applications on crop transpiration (ETc) water use efficiency (WUE)and agronomy phosphorus use efficiency (APUE) and sorghum productivity. The experiments were arranged in split plot design with five (5) P-fertilizer levels(0, 15, 30, 45 and 60 kg P205 ha-1) as the main plot and three varieties (CSR01, ICSV400 and local) as sub-plot in four replications.Results showed significant differences (P<0.05) among the P levels and sorghum varieties for grain yield in both locations and seasons. P increased grain yield by 19-39% over control treatment.The highest mean yield of 3156 kg ha-1 at Minjibir and 2929 kg ha-1 at BUK indicate optimum yield was recorded at the 45 kgP205ha-1 application rate and significantly higher than P rates at 0, 15 and 30kgha-1 respectively.Grain yield WUE was highly significantamongP-fertilizer levels and varieties, however, no significant differences between P-fertilizer rates for biomass WUE.P-application increased grain WUE of sorghum by 20-39%, the ICSV400 estimated the mean highest value of 9.3 and 8.6 kg ha-1mm-1 over CSR-01 and local at both locations.The study observed that the application of P could be an effective fertilization strategy to enhance sorghum yield and water use in low-rainfall cropping system and drought prone environment."
  

	uri <- "doi:10.21421/D2/YDFJOB"
	group <- "agronomy"
	ff	 <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication=NA,#"issn-2315-5094", https://oar.icrisat.org/10842/
		carob_contributor="Siyabusa Mkuhlani",
		carob_date="2022-09-12",
		data_type="experiment",
		data_organization = "ICRISAT",
		project = NA,
		modified_by = "Eduardo Garcia Bendito",
		last_modified = "2024-01-18",
		response_vars= "yield",
		treatment_vars= "P_fertilizer"
	)
  
	f <- ff[basename(ff) == "Data file of Sorghum productivity and water use under phosphorous fertilization.xlsx"]
	d <- carobiner::read.excel(f)

	##Convert First Row to Header
	e <- d[,c(1,2,4,5,6,14,15,16)]
	colnames(e) <- c('year', 'adm1','rep','P_fertilizer','variety','yield','dmy_stems','seed_weight')
	e$country <- "Nigeria"
	e$crop <- "sorghum"
	
	e$trial_id <- paste0('P_fert_', e$adm1)
	e$on_farm <- FALSE
	e$is_survey <- FALSE
	
	## RH covert P205 to P!
	e$P_fertilizer <- e$P_fertilizer / 2.29
	
	## RH check in paper if N or K were applied
	## EGB: Apparently there was N and K applied according to a paper (https://oar.icrisat.org/10842/).
	## RIS added as ISSN, since no DOI was available
	e$N_fertilizer <- 60
	e$K_fertilizer <- 30/1.21 # K2O to K
	e$fertilizer_type <- "urea; SSP; KCl"
	e$treatment <- paste0("N", as.integer(e$N_fertilizer), "P", as.integer(e$P_fertilizer), "K", as.integer(e$K_fertilizer))
	
	#Replace values in a data frame
	e$location <- ifelse(d$Location == "BUK", "Bayero University", d$Location)
	e$longitude <- ifelse(e$location == "Minjibir", 8.637, 8.429)
	e$latitude <- ifelse(e$location == "Minjibir", 12.192, 11.975)
	e$geo_from_source <- FALSE

##RH
## wrong
##	 e$planting_date <- "2014-06-01" # Assuming this start date...

	e$planting_date <- paste0(e$year, "-06-01")
	e$harvest_date <- as.character(as.Date(e$planting_date) + d$Mat_c_day)
	e$year <- NULL
	e$adm1 <- 'Kano'
	
	e$rep <- as.integer(e$rep)
	e$yield_part <- "grain"
	
	# Additional variables
#	e$flowering_date <- as.character(as.Date(e$planting_date) + d$Flo_c_day)
	e$flowering_days <- d$Flo_c_day # Two terms that mean the same thing?
	e$plant_height <- d$PH_M_cm
	e$dmy_stems <- d$`Stalk yield` # Not sure if it is DMY... 

	e$irrigated <- FALSE
	
	e$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, e, path=path)

}
