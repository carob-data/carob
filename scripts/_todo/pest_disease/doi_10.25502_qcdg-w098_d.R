# R script for "carob"
# license: GPL (>=3)

## NOTES
# Aphid counts on and around plantain fields in South Cameroon (Mengama, Abang Minko'o, Nlono), 2017-2018, plus a 2017 quadrat inventory of host plants.
# The files do not contain BBTD incidence, roguing dates, or yield. Musa sp is stored as plantain (the crop in the abstract; terminag also equates plantain with Musa sp.).
# Xanthosoma sagittifolium and Xanthosoma sp are tannia. Carica papaya is papaya. Other hosts stay in host_plant_ with crop left empty.
# pest_incidence is the total aphid count. Winged and wingless counts are kept beside it. Blank padding rows in the around-field file are dropped.

## ISSUES
# No coordinates in the tables. Do not copy the single CKAN point (coverage_x 02.33282, coverage_y 011.44253) onto three localities.
# 39 around-field rows have no host plant name.
# Georeference Mengama, Abang Minko'o, and Nlono (South Cameroon).
# Aphid species is not named in the files (the BBTD vector is often Pentalonia nigronervosa; that name is not applied here).
# Quadrat area is not given, so the inventory is a count per quadrat, not plants per ha.
# Percent alates is empty in the around-field file and is the ratio of the two counts in the within-field file, so it is not stored.
# No planting date, fertilizer, or irrigation. hhid is locality plus field (or quadrat), not a household code.
# write_files: missing yield, yield_moisture, latitude, longitude, irrigated, N/P/K, planting_date, harvest_date; crop empty on wild hosts; pest name "aphids" not in the vocabulary; carob_contributor empty.

## NOT PROCESSED
# aphid-around-field.csv: Percent alates. Entirely empty.
# aphid-within-field.csv: percent_alates. Repeats alate_aphids / total_aphids.
# All three files: ID. Row number.

## NEW VARIABLES
# host_plant_: name as recorded, including wild hosts.
# aphids_apterous_: wingless aphids on the sample.
# aphids_alate_: winged aphids on the sample.
# sample_where_: within field or around field.
# distance_m_: metres from the field border (around-field samples).
# border_: border number (around-field samples).
# quadrat_: quadrat number in the host inventory.
# plants_in_quadrat_: plants counted in that quadrat.


carob_script <- function(path) {

"
BBTD containment by phytosanitation of infected plants in Cameroon

Banana and plantain (Musa spp.) contribute to livelihood and income in Central Africa, they are amongst,the most important food crops. BBTDwere first reported in Cameroon in 2009, Under the BBTV Alliance for banana bunchy top disease control in Africa, works were done on (1) characterization of banana farming system in BBTD affected area, (2) assessment of disease effect on farm, (3) Evaluation of the reduction in BBTD incidence after rouging and subsequent follow up visits (4) assessment of the effect of farming system on phytosanitation of BBTV infected mats and implication on BBTD management in smallholder farms in three locations in BBTD endemic area of South Cameroon.This work showed that, three sources of seeds; existing fields, buying from neighbor fields or village, and Extension service were identified with existing field as the main source of plantain seeds without any insurance of phytosanitary quality. The most frequent or popular varieties are AAB plantains Ebang, Essong and Elat, all susceptible to BBTD. Three type of farms were identified during this work; household, fallow and forest farms. BBTD incidence is greater in Mengama than Abang Minko’o and Nlono; The reinfection rate was progressively slow down with the increase of number of follow-up visit, while the lowest reinfection rate was obtained in the forest farms in low BBTD pressure area. In general, phytosanitation has contributed to reduce BBTD incidence in all the three locations and types of fields. Reduction in BBTD incidence achieved but 100% reduction was limited to fewer fields Reduction in BBTD incidence easily achieved in low disease pressure area. Frequent follow-up visits are needed for phytosanitation to be effective for BBTD control in Cameroon.
"

	uri <- "doi:10.25502/qcdg-w098/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = "aphid counts in and around plantain fields",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "pest_incidence",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.6
	)

	f1 <- ff[basename(ff) == "aphid-around-field.csv"]
	f2 <- ff[basename(ff) == "aphid-within-field.csv"]
	f3 <- ff[basename(ff) == "host-plant-inventory.csv"]
	r1 <- read.csv(f1, check.names=FALSE, stringsAsFactors=FALSE)
	r2 <- read.csv(f2, check.names=FALSE, stringsAsFactors=FALSE)
	r3 <- read.csv(f3, check.names=FALSE, stringsAsFactors=FALSE)
	names(r1) <- trimws(names(r1))
	names(r2) <- trimws(names(r2))

	crop_of <- function(x) {
		x <- trimws(x)
		out <- rep(NA_character_, length(x))
		out[x == "Musa sp"] <- "plantain"
		out[x %in% c("Xanthosoma sagittifolium", "Xanthosoma sp")] <- "tannia"
		out[x == "Carica papaya"] <- "papaya"
		out
	}

	r1 <- r1[!is.na(r1$Locality) & trimws(r1$Locality) != "", ]
	d1 <- data.frame(
		date = as.character(r1$Year),
		location = carobiner::fix_name(trimws(r1$Locality), "title"),
		plot_id = as.character(r1$Field),
		host_plant_ = trimws(r1$Host_plant),
		crop = crop_of(r1$Host_plant),
		sample_where_ = "around field",
		border_ = as.integer(r1$Border),
		distance_m_ = as.numeric(r1$Distance_m),
		aphids_apterous_ = as.integer(r1$Apterous_aphids),
		aphids_alate_ = as.integer(r1$Alate_aphids),
		pest_incidence = as.integer(r1$Total_aphids),
		stringsAsFactors = FALSE
	)
	d2 <- data.frame(
		date = as.character(r2$Year),
		location = carobiner::fix_name(trimws(r2$Locality), "title"),
		plot_id = as.character(r2$Field),
		host_plant_ = trimws(r2$Host_plant),
		crop = crop_of(r2$Host_plant),
		sample_where_ = "within field",
		border_ = NA_integer_,
		distance_m_ = NA_real_,
		aphids_apterous_ = as.integer(r2$apterous_aphids),
		aphids_alate_ = as.integer(r2$alate_aphids),
		pest_incidence = as.integer(r2$total_aphids),
		stringsAsFactors = FALSE
	)
	d3 <- data.frame(
		date = as.character(r3$Year),
		location = carobiner::fix_name(trimws(r3$Locality), "title"),
		plot_id = NA_character_,
		host_plant_ = trimws(r3$Specie_name),
		crop = crop_of(r3$Specie_name),
		sample_where_ = "quadrat inventory",
		border_ = NA_integer_,
		distance_m_ = NA_real_,
		aphids_apterous_ = NA_integer_,
		aphids_alate_ = NA_integer_,
		pest_incidence = NA_integer_,
		quadrat_ = as.integer(r3$Num_quadrat),
		plants_in_quadrat_ = as.integer(r3$Number_of_plant_in_the_quadrat),
		stringsAsFactors = FALSE
	)
	d <- carobiner::bindr(d1, d2, d3)
	both_na <- is.na(d$aphids_apterous_) & is.na(d$aphids_alate_)
	part_sum <- rowSums(cbind(d$aphids_apterous_, d$aphids_alate_), na.rm=TRUE)
	part_sum[both_na] <- NA
	fill <- is.na(d$pest_incidence) & !both_na
	d$pest_incidence[fill] <- part_sum[fill]
	d$pest_incidence <- as.integer(d$pest_incidence)
	d$host_plant_ <- trimws(d$host_plant_)
	d$host_plant_[!is.na(d$host_plant_) & d$host_plant_ == ""] <- NA
	yp <- rep("none", nrow(d))
	yp[d$crop %in% c("plantain", "papaya")] <- "fruit"
	yp[d$crop == "tannia"] <- "roots"
	d$yield_part <- yp

	d$country <- "Cameroon"
	d$pest_species <- ifelse(d$sample_where_ == "quadrat inventory", NA_character_, "aphids")
	d$stress <- "pests"
	d$geo_from_source <- FALSE
	d$hhid <- ifelse(
		is.na(d$quadrat_),
		paste(d$location, d$plot_id, sep="_field"),
		paste(d$location, d$quadrat_, sep="_quadrat")
	)
	d$trial_id <- paste(d$location, d$date, sep="_")
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
