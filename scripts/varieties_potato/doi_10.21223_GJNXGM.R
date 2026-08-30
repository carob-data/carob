# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The data dictionary pairs Category 6 "YAT = National Youth Service" with Category 7 "NYS = Yatta Station" for trait "Countries".
#    it reads as swapped (YAT looks like the Yatta abbreviation while NYS like the National Youth Service one). 
#    Only "YAT" occurs in 4035_data.xlsx; "NYS" is never used. YAT is treated below as the NYS Yatta School of Agriculture, Kithimani
# 2. The location of site "SUJ" ("Sujan") could not be identified

carob_script <- function(path) {

"
Dataset for: At least 7 promising clones from LTVR x LBHT and SAP 2013 population selected and bulked up for 2017 multiplication trials in Kenya
Performances of selected CIP clones over two years  at several sites with different water management supply regimes
"
	uri <- "doi:10.21223/GJNXGM"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "irrigated",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-08-28",
		carob_completion = 100,	
		carob_effort = 3
	)

	f1 <- ff[basename(ff) == "4035_data.xlsx"]
	f2 <- ff[basename(ff) == "4035_material_list.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

	nms <- grep("rain", names(r1), value=TRUE, ignore.case=TRUE)
	long <- reshape(r1, varying=nms, v.names="yield", timevar="what", times=nms, direction="long")
	long$Total_Yield <- long$yield_above_mean_of_Checks <- long$id <- long$Order <- NULL
	long$trial_id <- as.character(as.integer(factor(long$what, levels=unique(long$what))))
	long <- long[(!is.na(long$yield)) & (long$yield > 0), ]
    long$yield <- long$yield * 1000
    names(long)[1] <- "variety_code"

	v <- unique(long[, c("trial_id", "what")])
	x <- trimws(do.call(rbind, strsplit(v$what, "_")))[, c(1,3,4)]
	y <- do.call(rbind, lapply(strsplit(x[,3], "-"), \(x) trimws(x[1:2])))
	z <- do.call(rbind, lapply(strsplit(x[,2], "/"), \(x) paste0("20", gsub("20", "", rep_len(x, 2)))))
	d <- data.frame(v$trial_id, paste(tolower(x[,1]), "rains"), z, y)
	names(d) <- c("trial_id", "season", "planting_date", "harvest_date", "site_code", "irrigated")
	d$irrigated <- grepl("IR", d$irrigated)

	pf <- r2[["Parent Female"]]
	pm <- r2[["Parent Male"]]
	vars <- data.frame(
	  variety_code = r2$`Accession number`,
	  variety = r2$`Accession name`,
      variety_pedigree = paste(ifelse(is.na(pf), "?", pf), ifelse(is.na(pm), "?", pm), sep = " x ")
	)

	long <- merge(long, d2, by = "variety_code", all.x = TRUE)

	# geo_from_source = FALSE hence geocodes where accessed from the following sources:
	#  MA  (Machakos)                         - https://www.geodatos.net/en/coordinates/kenya/machakos
	#  UON (University of Nairobi, Kabete)    - https://yandex.com/maps/org/university_of_nairobi_kabete_campus/92933407828/
	#                                            elevation: https://whatismyelevation.com (DEM at that lat/lon)
	#  KU  (Kutus)                            - https://www.findlatitudeandlongitude.com/l/County,+C74,+Kutus,+Kirinyaga+County,+Central+Kenya,+Kenya/6781536/
	#                                            elevation: https://whatismyelevation.com (DEM at that lat/lon)
	#  Hbay (Homa Bay)                        - https://www.countrycoordinate.com/city-homa-bay-kenya/
	#  YAT (NYS Yatta School of Agriculture,
	#       Kithimani, Machakos County)       - coordinates for Kithimani: https://www.getamap.net/maps/kenya/eastern/_kithimani/
	#                                            elevation: https://whatismyelevation.com (DEM at that lat/lon)
	#  SUJ (Sujan) and TM                     - NOT resolved

	geo <- data.frame(
	  site_code = c("SUJ", "MA", "UON", "KU", "Hbay", "YAT", "TM"),
	  location  = c("Sujan", "Machakos", "University of Nairobi (Kabete Campus)",
	                "Kutus", "Homa Bay", "NYS Yatta School of Agriculture (Kithimani)", "TM"),
	  longitude = c(NA, 37.26521, 36.733401, 37.236539, 34.453097, 37.45000, NA),
	  latitude  = c(NA, -1.52233, -1.256836, -0.474087, -0.535043, -1.18333, NA),
	  elevation = c(NA, 1619, 1881, 1700, 1193, 1325, NA),
      geo_from_source = FALSE
	)

	d <- merge(d, geo, by = "site_code", all.x = TRUE)
	d$site_code <- NULL
    d$country <- "Kenya"
    d$on_farm <- FALSE
    d$is_survey <-FALSE
    d$crop <- "potato"
    d$yield_part <- "tubers"
	d$yield_moisture <- NA
	d$yield_isfresh <- NA	
	long$what <- NULL 
	carobiner::write_files(path, meta, d, long=long)
}


