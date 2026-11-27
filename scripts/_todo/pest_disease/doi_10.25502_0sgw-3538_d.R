# R script for "carob"
# license: GPL (>=3)

## ISSUES
# CKAN metadata coordinates (6.4428, 2.55) fall in southern Benin ~24 km east of IITA-Benin at Abomey-Calavi (~6.421, 2.328); confirm the exact trial site.
# Fruit weight (FW, values 0.1-1.2) has no unit in the dictionary and no plot area is given, so it is kept in fruit_weight_ rather than converted to yield (kg/ha).
# Fruit class (FC, values 1 and 2; watermelon has only 1) has no definition in the data dictionary.

## NEW
# plant_number_: plant replicate index (1 or 2) on each resting host plant per block and date.
# petioles_with_exudate_: number of petioles with exudate on the resting host plant (count).
# petioles_with_aphid_: number of petioles with aphids on the resting host plant (count).
# fruit_class_: fruit size/grade class code (1 or 2; definition not in dictionary).
# fruit_weight_: weight of incubated fruit sample as recorded (unit not specified in dictionary).
# fruits_sampled_: number of fruits incubated per sample (count).
# fruits_infested_: number of incubated fruits infested by fruit flies (count).
# fruit_infestation_: percentage of incubated fruits infested (%; 100 * fruits_infested_ / fruits_sampled_).
# pupae_: total fruit fly pupae recovered from the incubated fruit sample (count).
# sex_: insect sex (male or female) for fruit-fly counts in the long table.
# pest_species values not in terminag: Dacus ciliatus, Dacus vertebratus, Dacus bivittatus, Dacus punctatifrons, Zeugodacus cucurbitae.

## MISSING
# yield, yield_moisture, N_fertilizer, P_fertilizer, K_fertilizer, irrigated, planting_date, harvest_date.

## NOTES
# On-station RCBD trial at IITA-Benin (2008-11-18 to 2009-02-01) evaluating GF-120 spinosad protein bait spray vs untreated control on border roost crops (sorghum and castor bean) to protect zucchini and watermelon (3 blocks).
# ff-counts-on-resting-plants.csv (518 rows) records plant-level adult fruit fly counts by species and sex plus petioles with exudates and aphids; fruits-incubation-data.csv (86 non-empty rows after dropping 40 all-NA rows) records 10-fruit incubation samples.
# Wide table d preserves all 604 individual observations (no plant averaging); long table dd holds species x sex adult fruit fly counts in pest_incidence linked by record_id.

## NOT PROCESSED
# ff-counts-on-resting-plants.csv: Rain, Weather, Wind — qualitative daily flags (Yes/No, Sunny/Cloudy), not quantitative weather measurements.
# data_dictionary.csv: column documentation only.


carob_script <- function(path) {

"
GF-120 and Control of Cucurbit crops in Benin

Fruit flies belong to family Tephritidae are the most destructive pests of horticultural crops in Benin, West Africa. Given their economic losses in zucchini and watermelon crops, the protein bait GF-120 was evaluated for its effectiveness to control cucurbits-infesting fruit flies The protein bait was applied to clusters of sorghum and castor bean planted in four corners alternately as a border crops. For over 2 years – 2008 to 2009, we conducted on-station trials at IITA-Benin to identify 1) the most attractive roosting hosts to fruit flies among 12 tested, and 2) evaluate the effectiveness of GF-120 bait spray on the selected roosting hosts. The benefit obtained in applying GF-120 to protect zucchini and watermelon plants was assessed. The experimental design consisted of a 'Complete Random Block Design' with two treatments – untreated and GF-120 treated plots, replicated three times. The results revealed that the castor bean and sorghum were identified as the best roosting hosts naturally attractive to fruit flies. The protein bait GF-120 was therefore applied to the underside of young leaves of castor bean and sorghum every Monday morning from 7am to 9am during the test period. The GF-120-treated castor bean, attracted more fruit flies than GF-120-treated sorghum. Infestation rates on the zucchini and watermelon were significantly lower (29% and 20%) on GF-120-treated plots than on untreated plots (73% and 77%) respectively. The use of GF-120 in integrated fruit flies management is therefore benefic and advantageous for the vegetable growers.
"

	uri <- "doi:10.25502/0sgw-3538/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "Identification of roosting hosts and their integration into bait applications for the management of cucurbit flies in Benin, West Africa",
		design = "RCBD; GF-120 bait spray vs untreated control on border roost crops (sorghum and castor bean); 3 blocks",
		data_type = "experiment",
		treatment_vars = "insecticide_used;insecticide_product",
		response_vars = "pest_incidence;fruit_infestation_",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 80,
		carob_effort = 0
	)

	f1 <- ff[basename(ff) == "ff-counts-on-resting-plants.csv"]
	f2 <- ff[basename(ff) == "fruits-incubation-data.csv"]
	r1 <- read.csv(f1, check.names=FALSE)
	r2 <- read.csv(f2, check.names=FALSE)

	# Drop 40 rows in fruits-incubation-data.csv where all measurement columns are NA
	meas2 <- c("FW", "NFS", "NFI", "TP", "D_ciliaM", "D_ciliaF", "D_verteM", "D_verteF",
		"D_bivitM", "D_bivitF", "D_punctM", "D_punctF", "Z_cucurM", "Z_cucurF")
	r2 <- r2[rowSums(!is.na(r2[, meas2])) > 0, , drop=FALSE]

	trt <- function(x) {
		x <- trimws(x)
		ifelse(tolower(x) == "control", "control", x)
	}
	crop_name <- function(x) {
		x <- tolower(trimws(x))
		x[x == "ricinus"] <- "castor bean"
		x
	}
	sex_long <- function(record_id, date, male, female, species) {
		n <- length(record_id)
		data.frame(
			record_id = rep(record_id, 2),
			date = rep(date, 2),
			pest_species = species,
			sex_ = rep(c("male", "female"), each=n),
			pest_incidence = as.integer(c(male, female))
		)
	}

	trt1 <- trt(r1$Treatment)
	d1 <- data.frame(
		record_id = seq_len(nrow(r1)),
		date = as.character(as.Date(r1$Date, format="%d/%m/%Y")),
		treatment = trt1,
		# GF-120 Naturalyte is a commercial spinosad protein bait
		insecticide_used = trt1 != "control",
		insecticide_product = ifelse(trt1 == "control", "none", "spinosad"),
		block_id = trimws(r1$Block),
		crop = crop_name(r1$RHP),
		plant_number_ = as.integer(r1[["plant number"]]),
		growth_stage = tolower(trimws(r1$PlantStage)),
		petioles_with_exudate_ = as.integer(r1$Nbpetiolew_Ex),
		petioles_with_aphid_ = as.integer(r1$Nbpetiolew_Aph),
		yield_part = "none"
	)

	trt2 <- trt(r2$Treatment)
	d2 <- data.frame(
		record_id = seq_len(nrow(r2)) + nrow(r1),
		date = as.character(as.Date(r2$Date, format="%d/%m/%Y")),
		treatment = trt2,
		insecticide_used = trt2 != "control",
		insecticide_product = ifelse(trt2 == "control", "none", "spinosad"),
		block_id = trimws(r2$Block),
		crop = crop_name(r2$RHP),
		fruit_class_ = as.integer(r2$FC),
		fruit_weight_ = as.numeric(r2$FW),
		fruits_sampled_ = as.integer(r2$NFS),
		fruits_infested_ = as.integer(r2$NFI),
		fruit_infestation_ = 100 * as.numeric(r2$NFI) / as.numeric(r2$NFS),
		pupae_ = as.integer(r2$TP),
		yield_part = "fruit"
	)

	d <- carobiner::bindr(d1, d2)
	d$country <- "Benin"
	d$location <- "IITA-Benin"
	# Coordinates from CKAN coverage_x/coverage_y (southern Benin)
	d$latitude <- 6.4428
	d$longitude <- 2.55
	d$geo_from_source <- TRUE
	d$trial_id <- "IITA-Benin_2008"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$stress <- "pests"

	dd <- rbind(
		sex_long(d1$record_id, d1$date, r1$D_ciliaM, r1$D_ciliaF, "Dacus ciliatus"),
		sex_long(d1$record_id, d1$date, r1$D_verteM, r1$D_verteF, "Dacus vertebratus"),
		sex_long(d1$record_id, d1$date, r1$D_bivitM, r1$D_bivitF, "Dacus bivittatus"),
		sex_long(d1$record_id, d1$date, r1$D_punctM, r1$D_punctF, "Dacus punctatifrons"),
		sex_long(d1$record_id, d1$date, r1$Z_cucurM, r1$Z_cucurF, "Zeugodacus cucurbitae"),
		sex_long(d2$record_id, d2$date, r2$D_ciliaM, r2$D_ciliaF, "Dacus ciliatus"),
		sex_long(d2$record_id, d2$date, r2$D_verteM, r2$D_verteF, "Dacus vertebratus"),
		sex_long(d2$record_id, d2$date, r2$D_bivitM, r2$D_bivitF, "Dacus bivittatus"),
		sex_long(d2$record_id, d2$date, r2$D_punctM, r2$D_punctF, "Dacus punctatifrons"),
		sex_long(d2$record_id, d2$date, r2$Z_cucurM, r2$Z_cucurF, "Zeugodacus cucurbitae")
	)
	dd <- dd[!is.na(dd$pest_incidence), ]

	carobiner::write_files(path, meta, d, long=dd)
}
