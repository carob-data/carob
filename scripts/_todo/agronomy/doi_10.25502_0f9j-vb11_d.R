# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm cassava seed comparison in Tanzania, study year 2020. About 102 plots; the CSV has many empty rows that were dropped.
# FarmCat from the dictionary: CTRL = control, CSE = cassava seed entrepreneur, QDSR = quality declared seed (root farmers). The title's "certified vs recycled" wording is not how the codes are defined, so the dictionary labels were used.
# FRY is fresh root yield, t/ha, converted x1000 to kg/ha. It does not match tRwt_kg / plot_size (correlation about 0.3; median FRY 19.5 t/ha vs about 0.13 t/ha from plot weight and area), so plot weight was not used as yield.
# uMFRY_CBSD is unmarketable fresh root yield (t/ha) from CBSD root necrosis. yield_marketable = (FRY - uMFRY_CBSD) x 1000 when that difference is >= 0.
# plot_size is m2 (dictionary: "mteres squared"). Many plots are larger than the plot_area maximum of 350 m2; values were left as recorded.
# StpAcre is stems per acre. stem_density = StpAcre / 0.4046863.
# Avg_psh is in the same range as MaxSh_cm, so it was used as plant_height in cm.
# Coordinates and altitude are in the file. Shinyanya was spelled Shinyanga (the same region is also spelled correctly in the file).

## ISSUES
# What CTRL, CSE and QDSR mean in the certified-vs-recycled contrast is only the dictionary text above.
# tRwt_kg, plot_size and FRY do not reconcile. Plot area may not be the harvested area.
# Ruaha is stored as adm1 as written; it is not a current Tanzanian region name.
# One form puts Golomoti-style issues aside: district "Mtwara Rural" has a trailing space, fixed with trimws.
# Age (0-30) has no unit.
# N/P/K and irrigation are not reported. planting_date is the study year 2020 only.
# Disease and mite scores were not checked against a published 1-5 or percent scale. Some incidence values may exceed 100.
# write_files: plot_area out of bounds; missing N/P/K and irrigated.

## NOT PROCESSED
# Age: unit not stated.
# tRwt_kg, Tnr, tSwt_kg, tNo_shoots: plot totals that do not reconcile with FRY and plot_size.
# MaxSh_cm, AveExsh_cm: other height summaries; Avg_psh used as plant height.
# AvShtppt: average shoots per plant.
# StpAcre was converted to stem_density. CuttpAcre is cuttings per acre; AvgCutph (per hectare) was kept instead.
# r2Inc, r3Inc: components of tCBSDinc (r2Inc+r3Inc). The total incidence was kept.
# ID, FarmerID: identifiers. FarmerID is not a household survey id in a useful cleaned form (mixed with the seed class).
# AEZone codes SZ, EZ, WLZ, ELZ: kept as ae_zone_ (see NEW VARIABLES).
# data_dictionary.csv.

## NEW VARIABLES
# ae_zone_: AEZone code (SZ, EZ, WLZ, ELZ). The dictionary expands these only as "agro-ecological zone".
# cuttings_per_ha_: AvgCutph.
# cgm_severity_, cgm_incidence_: cassava green mite score and incidence as recorded (incidence looks like a percent, so it was not stored as pest_incidence).


carob_script <- function(path) {

"
Cassava Yield Study, Certified seed vs recycled seed performance 2020

Evaluation of yield benefits of certified cassava seed.
"

	uri <- "doi:10.25502/0f9j-vb11/d"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety;seed_source",
		response_vars = "yield;plant_height",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.8,
		notes = NA
	)

	f <- ff[basename(ff) == "best-cassava-yield-study-data-2020.csv"]
	r <- read.csv(f, na.strings=c("", "NA"), check.names=FALSE)
	r <- r[!is.na(r$Cultivar) & trimws(r$Cultivar) != "", ]

	num <- function(x) {
		x <- gsub(",", "", trimws(as.character(x)))
		x[x %in% c("", "NA")] <- NA
		as.numeric(x)
	}

	fry <- num(r$FRY)
	um <- num(r$uMFRY_CBSD)
	market <- (fry - um) * 1000
	market[market < 0] <- NA

	cls <- trimws(r$FarmCat)
	cls <- c(CTRL="control", CSE="cassava seed entrepreneur", QDSR="quality declared seed")[cls]

	adm1 <- trimws(r$Region)
	adm1[tolower(adm1) == "shinyanya"] <- "Shinyanga"

	d <- data.frame(
		country = "Tanzania",
		adm1 = carobiner::fix_name(adm1, "title"),
		adm2 = carobiner::fix_name(trimws(r$District), "title"),
		location = carobiner::fix_name(trimws(r$Village), "title"),
		latitude = num(r$Lat),
		longitude = num(r$Long),
		elevation = num(r$Altitude),
		trial_id = paste(trimws(r$Region), trimws(r$Village), "2020", sep="_"),
		planting_date = "2020",
		variety = trimws(r$Cultivar),
		seed_source = cls,
		treatment = paste(trimws(r$Cultivar), cls),
		ae_zone_ = trimws(r$AEZone),
		plot_area = num(r$plot_size),
		yield = fry * 1000,
		yield_marketable = market,
		plant_height = num(r$Avg_psh),
		stem_density = num(r$StpAcre) / 0.4046863,
		cuttings_per_ha_ = num(r$AvgCutph),
		cgm_severity_ = as.character(num(r$CGM_sev)),
		cgm_incidence_ = num(r$CGMinc),
		crop = "cassava",
		yield_part = "roots",
		yield_isfresh = TRUE,
		on_farm = TRUE,
		is_survey = FALSE,
		geo_from_source = !is.na(num(r$Lat)) & !is.na(num(r$Long)),
		record_id = seq_len(nrow(r))
	)
	# one trial per village, not per seed class
	d$trial_id <- as.character(as.integer(as.factor(paste(d$adm2, d$location, "2020"))))

	long <- rbind(
		data.frame(record_id=d$record_id, disease="cassava mosaic", disease_severity=as.character(num(r$CMD_sev)), disease_incidence=as.character(num(r$CMDinc)), tissue_="leaf"),
		data.frame(record_id=d$record_id, disease="cassava brown streak", disease_severity=as.character(num(r$CBSDsev)), disease_incidence=as.character(num(r$LCBSDinc)), tissue_="leaf"),
		data.frame(record_id=d$record_id, disease="cassava brown streak", disease_severity=as.character(num(r$rCBSDsev)), disease_incidence=as.character(num(r$tCBSDinc)), tissue_="root")
	)
	long <- long[!(is.na(long$disease_severity) & is.na(long$disease_incidence)), ]

	carobiner::write_files(path, meta, d, long=long)
}
