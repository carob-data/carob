# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Crop conflict: the Dataverse title and keywords say "cowpea varieties", but the LABEL sheets of both files
#    are titled "SPRAYING REGIME AND SOWING DATE ON SESAME" and record capsules per plant (sesame bears capsules,
#    cowpea bears pods). No variety is recorded. Crop is set to "sesame"; query sent to the data contact
#    (Nurudeen Abdul Rahman, IITA).
# 2. Harvest area conflict: the LABEL sheets give a 6 m2 harvest area. File 001 2014 yields are consistent with
#    6 m2 (e.g. 0.019 kg/plot * 10000/6 = 31.67 kg/ha), but in the 2015 sheets and file 002 2014 kg/ha = kg/plot * 3125,
#    i.e. a 3.2 m2 harvest area. The reported kg/ha values are used as given.
# 3. File 001 2014 yields are very low (13-93 kg/ha) compared with the other sheets (62-531 kg/ha).
# 4. SAS sheets are subsets of the RAW sheets and are not used. In file 002, "SAS 2015" and the 2015 part of
#    "SAS ALL YRS" contain conversion factors (3125, 6250, 9375) instead of yields.
# 5. DSTRT (district) is coded 1-4 without a key; district names are unknown, so no coordinates.
#    Rep 1 and reps 2-3 of the same trial are in different districts.
# 6. Planting dates are given only as "mid July", "late July" and "mid August"; assumed the 15th, 25th and 15th.
# 7. Not included (no standard variable): capsules per plant (NCAPS), branches per plant (NBRNCH),
#    leaves per plant (NLVES), and PLNTHSD (probably plant stand at harvest per plot; unit and meaning not documented).
# 8. The insecticide product and application timing are not documented.

carob_script <- function(path) {

"
Africa RISING-Spraying Regime Effects on the Grain Yield of Cowpea Varieties in Northern Ghana

The data set evaluate adaptability and suitability of cowpea varieties to different ecozones.
"

	uri <- "doi:10.7910/DVN/J73BQ7"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA,
		data_organization = "IITA;MOFA;SARI",
		publication = NA,
		project = "Africa RISING",
		design = "3 by 3 Factorial Design",
		data_type = "experiment",
		treatment_vars = "planting_date;insecticide_times",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-09-29",
		carob_completion = 90,
		carob_effort = 3
	)

	f1 <- ff[basename(ff) == "001_sprayingRegimeCowpea_Ghana.xls"]
	f2 <- ff[basename(ff) == "002_sprayingCowpea_Ghana.xls"]

	r1 <- carobiner::read.excel(f1, sheet="RAW 2014")
	r2 <- carobiner::read.excel(f1, sheet="RAW 2015")
	r3 <- carobiner::read.excel(f2, sheet="RAW 2014")
	r4 <- carobiner::read.excel(f2, sheet="RAW 2015")

	d1 <- data.frame(
	  trial_id="001_2014", 
	  year=2014, 
	  district=NA,
		rep=r1[[1]], 
		pdate=r1[[2]], 
		spray=r1[[3]], 
		yield=r1[[5]], 
		plant_height=r1[[6]])

	d2 <- data.frame(trial_id="001_2015", year=2015, district=r2[[1]],
		rep=r2[[2]], pdate=r2[[3]], spray=r2[[4]], yield=r2[[9]], plant_height=r2[[6]])

	y <- r3[[10]]
	y[is.na(y)] <- r3[[9]][is.na(y)] * 3125      # see ISSUES 9
	
	d3 <- data.frame(
	  trial_id="002_2014", 
	  year=2014, 
	  district=r3[[1]],
		rep=r3[[2]], 
		pdate=r3[[3]], 
		spray=r3[[4]], 
		yield=y, 
		plant_height=r3[[5]])

	d4 <- data.frame(
	  trial_id="002_2015", 
	  year=2015, 
	  district=NA,
		rep=r4[[1]], 
		pdate=r4[[2]], 
		spray=r4[[3]], 
		yield=r4[[8]], 
		plant_height=r4[[5]])

	## same variables, different plots: bind the rows
	d <- rbind(d1, d2, d3, d4)
	d$plant_height[d$plant_height > 500] <- NA

	pd <- c("-07-15", "-07-25", "-08-15")         # mid July, late July, mid August (ISSUES 6)
	d$planting_date <- paste0(d$year, pd[d$pdate])
	d$insecticide_used <- TRUE
	d$insecticide_times <- as.integer(d$spray)
	d$treatment <- paste0(c("mid July", "late July", "mid August")[d$pdate], " planting; ",
					c("once", "twice", "thrice")[d$spray], " sprayed")
	d$rep <- as.integer(d$rep)
	d$location <- ifelse(is.na(d$district), NA, paste("district", d$district))
	d$year <- d$pdate <- d$spray <- d$district <- NULL

	d$country <- "Ghana"
	d$adm1 <- "Northern"
	d$longitude <- NA                 # see ISSUES 5
	d$latitude <- NA
	d$geo_from_source <- FALSE

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$crop <- "sesame"                            # see ISSUES 1
	d$yield_part <- "seed"
	d$yield_isfresh <- FALSE
	d$yield_moisture <- d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- d$harvest_date <- NA

	carobiner::write_files(path, meta, d)
}
