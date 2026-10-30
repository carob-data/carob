# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plant-level counts of fall armyworm, stemborers, leaf folders and natural enemies
#   on maize at Batchenga (Ndjie, Okongolo) and Mbalmayo (Avebe, Memiam), Centre, Cameroon,
#   2020 and 2021. 65535 rows is the Excel limit; 29018 rows with no treatment were dropped.
# One row is one plant at one sampling date (not aggregated).
# Sampling_date is either day/month/year (21/10/2020) or day-Mon-yy (13-Oct-20). Months
#   are matched in English so the result does not depend on the locale.
# WAP and WAS (4, 6, 8, 11) are converted to DAP = weeks x 7. planting_date = sampling date minus DAP.
# Cropping_season 1 and 2 are stored as first and second. Season  codes are not defined further.
# fertilized_ is TRUE or FALSE only when the treatment or variety text says so.
#   "unfertilized" is handled before "fertilized".
# pest_incidence is FAW larvae. pest_severity is FAWSev as text (values run past 9, up to 100;
#   the scale is not documented). Other counts stay in their own columns.
# Variety is kept as recorded, including CMS8704-fertilized / CMS8704-unfertilized.
# No coordinates in the file. The dataset coverage point (2.3232, 11.43653) is not these towns.

## ISSUES
# 1. Georeference Batchenga (Ndjie, Okongolo) and Mbalmayo (Avebe, Memiam), Centre, Cameroon.
#    adm2 (Lekié, Nyong-et-So'o) was not set; confirm GADM names.
# 2. No yield in this file (yield components are doi:10.25502/zsnk-kk83/d). No N/P/K rates.
# 3. FAWSev, SBSev and LeafFolderSev look like percentages but that is not stated.
# 4. GREMEC has no active ingredient or rate. OMO, Tithonia, wood ash and soil treatment have no rates.
# 5. Stem borers are not identified to species. Confirm planting dates derived from WAP.
# 6. write_files: missing yield, yield_part, yield_moisture, N/P/K, longitude, latitude, harvest_date.
#    The count columns with a trailing underscore are not in terminag yet.

## NOT PROCESSED
# ID: row number.
# 29018 empty rows (no treatment, variety, date or counts).
# data_dictionary.csv: definitions used above.

## NEW VARIABLES
# agroecosystem_: forest or transition forest-savannah
# fertilized_: from the treatment or variety name; NA when not stated
# sampling_method_: Destructive or Non-destructive
# plant_id_: plant number
# faw_eggmass_: FAW egg masses
# faw_pupae_: FAW pupae
# sb_eggmass_: stemborer egg masses
# sb_larvae_: stemborer larvae
# sb_pupae_: stemborer pupae
# sb_severity_: stemborer severity, scale not documented
# charops_: Charops sp. count
# cotesia_icipe_: Cotesia icipe count
# coccygidium_luteum_: Coccygidium luteum count
# other_parasitoids_: other parasitoids
# predatory_wasps_: predatory wasps
# earwigs_: earwigs
# ants_: ants
# predatory_beetle_: predatory beetles
# cricket_: crickets (dictionary: other pest)
# leaffolder_count_: leaf-folder count
# leaffolder_severity_: leaf-folder severity, scale not documented

carob_script <- function(path) {

"
Efficacy and cost-effectiveness of different technologies against Spodoptera frugiperda (Lepidoptera: Noctuidae) in the Tropical humid forest of Cameroon

Assess the field biophysical efficacy of different products against FAW and determine the cost-effectiveness of different technologies;
"

	uri <- "doi:10.25502/h02h-bq18/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Coordinating elucidation of science and effectiveness of Local Innovations for Managing Fall armyworm in Africa",
		design = "On-farm comparison of FAW control treatments, some with and without fertilizer, at Batchenga and Mbalmayo; repeated plant counts",
		data_type = "on-farm experiment",
		treatment_vars = "treatment;variety",
		response_vars = "pest_incidence;pest_severity",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 68,
		carob_effort = 0.9
	)

	f <- ff[basename(ff) == "local_innovation_data.csv"]
	r <- read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)
	r <- r[!is.na(r$Treatment) & trimws(r$Treatment) != "", ]

	mon <- c(jan = "01", feb = "02", mar = "03", apr = "04", may = "05", jun = "06",
		jul = "07", aug = "08", sep = "09", oct = "10", nov = "11", dec = "12")
	rawd <- trimws(r$Sampling_date)
	date <- rep(NA_character_, length(rawd))
	i <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", rawd)
	date[i] <- as.character(as.Date(rawd[i], "%d/%m/%Y"))
	j <- grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2}$", rawd)
	parts <- strsplit(rawd[j], "-")
	dday <- sprintf("%02d", as.integer(vapply(parts, `[`, "", 1)))
	dmon <- unname(mon[tolower(vapply(parts, `[`, "", 2))])
	dyy <- paste0("20", vapply(parts, `[`, "", 3))
	date[j] <- paste(dyy, dmon, dday, sep = "-")

	w <- toupper(trimws(r$WAP))
	okw <- grepl("^[0-9]+WAS$|^[0-9]+WAP$", w)
	dap <- rep(NA_integer_, length(w))
	dap[okw] <- as.integer(sub("W(AP|AS)$", "", w[okw])) * 7L
	planting <- as.character(as.Date(date) - dap)

	tr <- trimws(r$Treatment)
	va <- trimws(r$Variety)
	va[!is.na(va) & va == ""] <- NA_character_
	both <- paste(tr, va)
	unfert <- grepl("unfertilized|without fertilizer", both, ignore.case = TRUE)
	fert <- grepl("fertilized|with fertilizer", both, ignore.case = TRUE) & !unfert
	fertilized <- rep(NA, length(unfert))
	fertilized[unfert] <- FALSE
	fertilized[fert] <- TRUE

	loc <- carobiner::fix_name(trimws(r$Loc), "title")
	site <- carobiner::fix_name(trimws(r$Field), "title")
	year <- as.character(r$Year)
	sn <- r$Cropping_season
	season <- ifelse(sn == 1, "first", ifelse(sn == 2, "second", NA_character_))
	gs <- trimws(r$Crop_Stage)
	gs[gs == ""] <- NA_character_
	meth <- trimws(r$method)
	meth[meth == ""] <- NA_character_
	sev <- r$FAWSev

	d <- data.frame(
		country = "Cameroon",
		adm1 = "Centre",
		location = loc,
		geo_from_source = FALSE,
		site = site,
		agroecosystem_ = trimws(r$Agroecosystem),
		planting_date = planting,
		date = date,
		DAP = dap,
		season = season,
		treatment = tr,
		variety = va,
		fertilized_ = as.logical(fertilized),
		insecticide_used = tr == "Insecticide-GREMEC",
		insecticide_product = ifelse(tr == "Insecticide-GREMEC", "GREMEC", NA_character_),
		block_id = as.character(r$Block),
		rep = as.integer(r$Block),
		sampling_method_ = meth,
		growth_stage = gs,
		plant_id_ = as.character(r$Plant),
		crop = "maize",
		stress = "pests",
		pest_species = "Spodoptera frugiperda",
		pest_incidence = as.integer(r$FAWLar),
		pest_severity = ifelse(is.na(sev), NA_character_, as.character(sev)),
		faw_eggmass_ = as.integer(r$NbEggmassFAW),
		faw_pupae_ = as.integer(r$FAWPup),
		sb_eggmass_ = as.integer(r$NBEggmassSB),
		sb_larvae_ = as.integer(r$SBLar),
		sb_pupae_ = as.integer(r$SBPup),
		sb_severity_ = ifelse(is.na(r$SBSev), NA_character_, as.character(r$SBSev)),
		charops_ = as.integer(r$Charops_sp),
		cotesia_icipe_ = as.integer(r$Cotesia_icipe),
		coccygidium_luteum_ = as.integer(r$Coccygidium_luteum),
		other_parasitoids_ = as.integer(r$Other_Parasitoids),
		predatory_wasps_ = as.integer(r$Predatory_wasps),
		earwigs_ = as.integer(r$Ear_wigs),
		ants_ = as.integer(r$Ants),
		predatory_beetle_ = as.integer(r$Predatory_beetle),
		cricket_ = as.integer(r$Cricket),
		leaffolder_count_ = as.integer(r$NbLeafFolder),
		leaffolder_severity_ = ifelse(is.na(r$LeafFolderSev), NA_character_, as.character(r$LeafFolderSev)),
		trial_id = paste(loc, site, year, season, sep = "_"),
		on_farm = TRUE,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)
	d$agroecosystem_[!is.na(d$agroecosystem_) & d$agroecosystem_ == ""] <- NA_character_
	d$record_id <- seq_len(nrow(d))
	# insecticide_used is FALSE for every treatment other than Insecticide-GREMEC
	d$fertilized_ <- as.logical(d$fertilized_)

	carobiner::write_files(path, meta, d)
}
