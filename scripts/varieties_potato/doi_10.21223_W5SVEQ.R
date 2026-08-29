# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Raw data has no information on plot area, location, longitude and latitude
# There is no mention of the related publication on the dataverse 
# But there is a published paper related to the data https://arpgweb.com/pdf-files/jac7(1)7-13.pdf
# Currently waiting on author to confirm and verify if the paper was published based on this particular 
# missing variables will be added after author verification


carob_script <- function(path) {

"
Dataset for: National Performance Trials and Durability, Uniformity and Stability data collected to support release of 2 LB resistant varieties in Kenya

The results of National Performance trials in Kenya. It holds the average of 2 seasons: April-August 2015 and October 2015 to February 2016.
"
	uri <- "doi:10.21223/W5SVEQ"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "yield",
		notes = NA, 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-08-28",
		carob_completion = 100,
		carob_effort = 2
	)


	f1 <- ff[basename(ff) == "4415_Data.xlsx"]
	f2 <- ff[basename(ff) == "4415_Material_List.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

	d1 <- data.frame(
	  country = "Kenya",
	  crop = "potato",
	  variety_code = r1$ACCENUMB,            
	  yield_marketable = r1$MTWP,                   #plot area not provided in raw data
	  yield = r1$TTWP,
	  tuber_density = r1$TNTP,
	  yield_part = "tubers",
	  potato_leafroll_virus_resistance = r1$PRLV,
	  potato_virus_X_resistance = r1$PVX,
	  potato_virus_Y_resistance = r1$PVY,
	  resistance_scale_name = "SES 7 pt scale"
	)

	d2 <- data.frame(
	  variety_code = r2$Accession_Number,
	  variety = r2$Accession_Name,
	  stringsAsFactors = FALSE
	)

	pf <- r2[["Female_AcceNumb"]]
	pm <- r2[["Male_AcceNumb"]]
	pf[!is.na(pf) & trimws(pf) == ""] <- NA   # blank cells (e.g. Shangi's parents), not just NA cells
	pm[!is.na(pm) & trimws(pm) == ""] <- NA
	d2$variety_pedigree <- ifelse(
		is.na(pf) & is.na(pm),
		NA_character_,
		paste(ifelse(is.na(pf), "?", pf), ifelse(is.na(pm), "?", pm), sep = " * ")
	)

	d <- merge(d1, d2, by = "variety_code", all.x = TRUE, sort = FALSE)

	d$trial_id <- as.character(as.integer(as.factor(1))) 
	d$on_farm <- FALSE       
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE
	d$irrigated <- FALSE
	d$longitude <- NA        
	d$latitude  <- NA
	d$elevation <- NA
	d$planting_date <- as.character(as.Date(NA))
	d$harvest_date  <- as.character(as.Date(NA))
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$S_fertilizer <- d$fertilizer_type <- NA
	d$yield_moisture <- NA
	d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}
