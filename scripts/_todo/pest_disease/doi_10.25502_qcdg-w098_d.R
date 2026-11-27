# R script for "carob"
# license: GPL (>=3)

## ISSUES
# - The dataset abstract describes BBTD containment by phytosanitation, but the uploaded CSV files contain only aphid counts on host plants within/around fields and a quadrat host-plant inventory (no BBTD incidence, roguing dates, or yield).
# - In aphid-around-field.csv, 39 rows have an empty Host_plant (38 have all aphid counts NA, likely no host plant present at that distance; ID 1 has Total_aphids = 0).
# - In aphid-within-field.csv, rows 296 and 297 (Abang Minko'o, Field 6, Costus afer) appear to have swapped total_aphids vs apterous_aphids (row 296: apterous = 0, alate = 0, total = 3; row 297: apterous = 3, alate = 0, total = 0); kept as recorded.
# - Aphid species is not named in the files (a companion IITA study notes Pentalonia nigronervosa on Musa and Pentalonia caladii on alternative hosts); stored as "aphid".
# - Quadrat area is not given in host-plant-inventory.csv, so counts cannot be converted to plant density per ha.

## NEW
# host_plant_: plant species sampled as recorded, including wild alternative hosts
# aphids_apterous_: wingless aphids counted per sample (count)
# aphids_alate_: winged aphids counted per sample (count)
# sample_where_: sampling context ("within field", "around field", or "quadrat inventory")
# distance_m_: distance from field border for around-field samples (m)
# border_: field border number for around-field samples (1-4)
# quadrat_: quadrat number in the host-plant inventory (1-5)
# plants_in_quadrat_: plants counted in the quadrat (count)

## MISSING
# yield, yield_moisture, yield_part, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, planting_date, harvest_date (not applicable to this aphid and host-plant survey)

## NOTES
# Survey of aphids on cultivated and wild host plants within and around plantain fields in three localities of Vallée-du-Ntem, Sud, Cameroon (Mengama, Abang Minko'o, Nlono; 2017-2018), plus a 2017 quadrat host-plant inventory.
# Musa sp is mapped to plantain, Xanthosoma spp. to tannia, and Carica papaya to papaya; wild alternative hosts remain in host_plant_ with crop = NA.
# Locality coordinates are georeferenced at village level (Mapcarta/GeoView; Abang Minko'o also matches the CKAN coverage point 2.333, 11.443).
# Fifteen trailing blank padding rows in aphid-around-field.csv are removed.

## NOT PROCESSED
# aphid-around-field.csv: "Percent alates" (entirely empty).
# aphid-within-field.csv: percent_alates (redundant ratio 100 * alate_aphids / total_aphids).
# data_dictionary.csv: codebook only.


carob_script <- function(path) {

"
BBTD containment by phytosanitation of infected plants in Cameroon

Banana and plantain (Musa spp.) contribute to livelihood and income in Central Africa, they are amongst,the most important food crops. BBTDwere first reported in Cameroon in 2009, Under the BBTV Alliance for banana bunchy top disease control in Africa, works were done on (1) characterization of banana farming system in BBTD affected area, (2) assessment of disease effect on farm, (3) Evaluation of the reduction in BBTD incidence after rouging and subsequent follow up visits (4) assessment of the effect of farming system on phytosanitation of BBTV infected mats and implication on BBTD management in smallholder farms in three locations in BBTD endemic area of South Cameroon.This work showed that, three sources of seeds; existing fields, buying from neighbor fields or village, and Extension service were identified with existing field as the main source of plantain seeds without any insurance of phytosanitary quality. The most frequent or popular varieties are AAB plantains Ebang, Essong and Elat, all susceptible to BBTD. Three type of farms were identified during this work; household, fallow and forest farms. BBTD incidence is greater in Mengama than Abang Minko’o and Nlono; The reinfection rate was progressively slow down with the increase of number of follow-up visit, while the lowest reinfection rate was obtained in the forest farms in low BBTD pressure area. In general, phytosanitation has contributed to reduce BBTD incidence in all the three locations and types of fields. Reduction in BBTD incidence achieved but 100% reduction was limited to fewer fields Reduction in BBTD incidence easily achieved in low disease pressure area. Frequent follow-up visits are needed for phytosanitation to be effective for BBTD control in Cameroon.
"

	uri <- "doi:10.25502/qcdg-w098/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "BBTV mitigation: Community Management in Nigeria and Screening Wild Banana Progenitors for Resistance",
		design = "survey of aphid counts within and around plantain fields and quadrat inventory of alternative host plants across 3 localities",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "pest_incidence",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f1 <- ff[basename(ff) == "aphid-around-field.csv"]
	f2 <- ff[basename(ff) == "aphid-within-field.csv"]
	f3 <- ff[basename(ff) == "host-plant-inventory.csv"]
	r1 <- read.csv(f1, check.names = FALSE, na.strings = "")
	r2 <- read.csv(f2, check.names = FALSE, na.strings = "")
	r3 <- read.csv(f3, check.names = FALSE, na.strings = "")
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

	# Drop 15 completely empty trailing padding rows
	r1 <- r1[!is.na(r1$Locality), ]
	d1 <- data.frame(
		order = as.integer(r1$ID),
		date = as.character(r1$Year),
		location = trimws(r1$Locality),
		plot_id = as.character(r1$Field),
		host_plant_ = trimws(r1$Host_plant),
		crop = crop_of(r1$Host_plant),
		sample_where_ = "around field",
		border_ = as.integer(r1$Border),
		distance_m_ = as.numeric(r1$Distance_m),
		aphids_apterous_ = as.integer(r1$Apterous_aphids),
		aphids_alate_ = as.integer(r1$Alate_aphids),
		pest_incidence = as.integer(r1$Total_aphids)
	)
	d2 <- data.frame(
		order = as.integer(r2$ID),
		date = as.character(r2$Year),
		location = trimws(r2$Locality),
		plot_id = as.character(r2$Field),
		host_plant_ = trimws(r2$Host_plant),
		crop = crop_of(r2$Host_plant),
		sample_where_ = "within field",
		aphids_apterous_ = as.integer(r2$apterous_aphids),
		aphids_alate_ = as.integer(r2$alate_aphids),
		pest_incidence = as.integer(r2$total_aphids)
	)
	d3 <- data.frame(
		order = as.integer(r3$ID),
		date = as.character(r3$Year),
		location = trimws(r3$Locality),
		host_plant_ = trimws(r3$Specie_name),
		crop = crop_of(r3$Specie_name),
		sample_where_ = "quadrat inventory",
		quadrat_ = as.integer(r3$Num_quadrat),
		plants_in_quadrat_ = as.integer(r3$Number_of_plant_in_the_quadrat)
	)

	d <- carobiner::bindr(d1, d2, d3)
	d$host_plant_[!is.na(d$host_plant_) & d$host_plant_ == ""] <- NA_character_

	# Village-level georeferences in Vallée-du-Ntem, Sud, Cameroon (Mapcarta/GeoView; ~5 km uncertainty)
	geo <- data.frame(
		location = c("Abang Minko'o", "Mengama", "Nlono"),
		adm1 = "Sud",
		adm2 = "Vallée-du-Ntem",
		latitude = c(2.3333, 2.3488, 2.3333),
		longitude = c(11.4333, 11.5572, 11.4000),
		geo_from_source = FALSE,
		geo_uncertainty = 5000,
		geo_source = "Mapcarta/GeoView"
	)
	d <- merge(d, geo, by = "location", all.x = TRUE)
	d$country <- "Cameroon"
	d$pest_species <- ifelse(d$sample_where_ == "quadrat inventory", NA_character_, "aphid")
	d$stress <- "pests"
	d$hhid <- ifelse(
		is.na(d$quadrat_),
		paste(d$location, d$plot_id, sep = "_field"),
		paste(d$location, d$quadrat_, sep = "_quadrat")
	)
	d$trial_id <- paste(d$location, d$date, sep = "_")
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
