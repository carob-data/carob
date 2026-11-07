# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field trials and farmer plots of seed degeneration in Ecuador. Tuber weight in the harvest file is g per plant (dictionary), so it is not stored as yield in kg/ha.
# Coordinates, altitude, and planting and harvest dates come from Locations_Data. Virus columns are DAS-ELISA incidence coded 0 or 1.
# Weather Temp is degrees C. The dictionary describes rain as cubic millimeters per hour and also labels the unit m3. The number is kept as recorded and is not stored as precipitation in mm. HR has no unit in the dictionary.

## ISSUES
# No fertilizer rates. Plot area in the description is 49 m2, and plants per plot are not in the harvest file, so g/plant was not converted to kg/ha.
# write_files: missing carob_contributor, hhid. unknown variables: seed_management_, cycle_, plot_manager_, plant_number_, tuber_number_, tuber_weight_g_, the tuber-symptom columns, the virus columns, aphid_count_, humidity_, rain_recorded_.

## NOT PROCESSED
# None of the measurement sheets were dropped. The dictionary workbook is documentation.
# Dictionary_Potato_seed_degeneration_in_Ecuador.xls: documentation or a codebook, not an observation table.

## NEW VARIABLES
# seed_management_: positive selection, roguing, or random selection.
# tuber_weight_g_, tuber_number_: grams and tubers per plant.
# Virus incidence columns and tuber-symptom columns: proportion or 0/1 as recorded.
# aphid_count_: aphids in the trap.
# humidity_, rain_recorded_: weather columns whose units are not stated as percent and mm.


carob_script <- function(path) {

"
Dataset for: Potato seed degeneration in Ecuador

Vegetative multiplication of potato in the field can lead to an accumulation of pest and pathogens such as fungi, bacteria and mainly virus in the seed over successive cycles of reproduction. This process is known as seed degeneration, where the main vectors for virus infection and transmission are aphids. This study was carried out to evaluate the rate of seed degeneration in two potato cultivars, at different altitudes and considering the use of different on-farm seed management options in the Ecuadorian Andes with the aim to support the promotion of an integrated seed health strategy. The study involved the cultivars INIAP-Fripapa and Superchola (susceptible and resistant to degeneration, respectively, according to farmers' perceptions). The field trials were carried out at three altitudes ( 3400 masl) during 3 cropping cycles of successive vegetative reproduction. Twelve plots of 49 m2 were planted each year; 2 plots of each cultivar at each altitude/site, in which 3 types of seed management were carried out: positive selection, roguing and random selection. We evaluated: (1) virus incidence (PVX, PVY, PVS, PLRV, APLV and APMoV) in plants at emergence, flowering and in tubers using the DAS-ELISA; (2) abundance of aphids; (3) incidence and severity of pest damage and diseases in tubers; and (4) tuber yield. Similar data were taken from 10 farmers' fields, and separate trials were done to estimate the efficiency of auto-infection (percentage of infected progeny tubers from an infected mother tuber) of PVY at the three altitudes.
"

	uri <- "doi:10.21223/P3/3CT90C"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;seed_management_",
		response_vars = "tuber_weight_g_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 0.6
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	chr <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}
	f <- function(name) ff[basename(ff) == name]

	r1 <- carobiner::read.excel(f("Locations_Data_Potato_seed_degeneration_in_Ecuador.xls"), sheet="Locations data")
	r1$key <- paste(r1$Cycle, r1$Control, r1$Location)
	r1 <- r1[!duplicated(r1$key), ]

	r2 <- carobiner::read.excel(f("Harvest_Data_Potato_seed_degeneration_in_Ecuador.xls"), sheet="Harvest Data")
	ih <- match(paste(r2$Cycle, r2$Control, r2$Location), r1$key)
	d1 <- data.frame(
		variety = chr(r2$Variety),
		seed_management_ = chr(r2$Management),
		location = chr(r2$Location),
		elevation = num(r2$Altitude),
		cycle_ = chr(r2$Cycle),
		plot_manager_ = chr(r2$Control),
		plant_number_ = num(r2$Plantnum),
		tuber_number_ = num(r2$TubN_Ct_plant),
		tuber_weight_g_ = num(r2$TubWg_Cp_plant),
		black_scurf_ = num(r2$BScurf_Inci_0t01),
		wireworm_ = num(r2$WWorm_Inci_0to1),
		powdery_scab_ = num(r2$PScab_Inci_0to1),
		common_scab_ = num(r2$CScab_Inci_0to1),
		malformation_ = num(r2$Malform_Inci_0to1),
		pcn_ = num(r2$PCN_Inci_0to1),
		black_leg_ = num(r2$BLeg_Inci_0to1),
		physical_damage_ = num(r2$PhysDam_Inci_0to1),
		cracking_ = num(r2$Cracking_Inci_0to1),
		surface_cracking_ = num(r2$Scracking_Inci_0to1),
		insect_damage_ = num(r2$DamOtherInsects_Inci_0to1),
		mechanical_damage_ = num(r2$MechDamage_Inci_0to1),
		dry_rot_ = num(r2$DryRut_Inci_0to1),
		smut_ = num(r2$Smut_Inci_0to1),
		latitude = num(r1$Latitude[ih]),
		longitude = num(r1$Longitude[ih]),
		planting_date = as.character(as.Date(r1[["Planting date"]][ih])),
		harvest_date = as.character(as.Date(r1[["Harvest date"]][ih]))
	)

	r3 <- carobiner::read.excel(f("ELISA_Data_Potato_seed_degeneration_in_Ecuador.xls"), sheet="ELISA Data")
	ie <- match(paste(r3$Cycle, r3$Control, r3$Location), r1$key)
	d2 <- data.frame(
		variety = chr(r3$Variety),
		seed_management_ = chr(r3$Management),
		location = chr(r3$Location),
		elevation = num(r3$Altitude),
		cycle_ = chr(r3$Cycle),
		plot_manager_ = chr(r3$Control),
		growth_stage = chr(r3$Stage),
		DAP = as.integer(round(num(r3$DAP))),
		pvx_ = num(r3$PPVX_Inci_0to1),
		pvy_ = num(r3$PPVY_Inci_0to1),
		pvs_ = num(r3$PPVS_Inci_0to1),
		plrv_ = num(r3$PPLRV_Inci_0to1),
		aplv_ = num(r3$AAPLV_Inci_0to1),
		apmov_ = num(r3$AAPMoV_Inci_0to1),
		latitude = num(r1$Latitude[ie]),
		longitude = num(r1$Longitude[ie]),
		planting_date = as.character(as.Date(r1[["Planting date"]][ie])),
		harvest_date = as.character(as.Date(r1[["Harvest date"]][ie]))
	)

	r4 <- carobiner::read.excel(f("Aphids_Data_Potato_seed_degeneration_in_Ecuador.xls"), sheet="Aphids data")
	ia <- match(paste(r4$Cycle, r4$Control, r4$Location), r1$key)
	d3 <- data.frame(
		variety = chr(r4$Variety),
		seed_management_ = chr(r4$Management),
		location = chr(r4$Location),
		elevation = num(r4$Altitude),
		cycle_ = chr(r4$Cycle),
		plot_manager_ = chr(r4$Control),
		DAP = as.integer(round(num(r4$DAP))),
		aphid_count_ = num(r4$Aphids_numb),
		latitude = num(r1$Latitude[ia]),
		longitude = num(r1$Longitude[ia]),
		planting_date = as.character(as.Date(r1[["Planting date"]][ia])),
		harvest_date = as.character(as.Date(r1[["Harvest date"]][ia]))
	)

	d <- carobiner::bindr(d1, d2, d3)
	d$treatment <- trimws(paste(d$variety, d$seed_management_))
	d$crop <- "potato"
	d$country <- "Ecuador"
	d$on_farm <- tolower(d$plot_manager_) == "farmer"
	d$is_survey <- FALSE
	d$stress <- "disease"
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	d$trial_id <- paste("Ecuador_seed", d$location, d$cycle_, d$variety, d$seed_management_, seq_len(nrow(d)), sep="_")
	miss <- is.na(d$variety) | !nzchar(d$variety)
	d$variety[miss] <- paste("unlabeled", seq_len(nrow(d))[miss])
	d$seed_management_[is.na(d$seed_management_) | !nzchar(d$seed_management_)] <- "not recorded"
	d$treatment <- trimws(paste(d$variety, d$seed_management_))

	r5 <- carobiner::read.excel(f("Weather_Data_Potato_seed_degeneration_in_Ecuador_v2.xls"), sheet="Weather Data")
	mins <- round(num(r5$Time) * 24 * 60)
	time <- rep(NA_character_, length(mins))
	ok <- !is.na(mins)
	time[ok] <- sprintf("%02d:%02d", (mins[ok] %/% 60) %% 24, mins[ok] %% 60)
	wth <- data.frame(
		station_id = chr(r5$Location),
		date = as.character(as.Date(r5$Date, "%m/%d/%Y")),
		time = time,
		temp = num(r5$Temp),
		humidity_ = num(r5$HR),
		rain_recorded_ = num(r5$Rain)
	)
	wth <- wth[!is.na(wth$date), ]

	carobiner::write_files(path, meta, d, wth=wth)
}
