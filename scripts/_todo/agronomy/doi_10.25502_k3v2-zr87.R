# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm soybean plots, Kole district (action site Bala), Uganda, season 2013a.
# The crop column is empty. Crop is soybean from the dataset title and Maksoy variety names.
# Harvested area is 25 m2, matching plot_size "5X5M2". weight_kg and weight_grain are the same numbers and are treated as kg.
# TSP product kg/plot to elemental P with terminag fraction 19.23% P. Control is no mineral fertilizer (N, P and K = 0).
# Manure mapped to farmyard manure; Control organic fertilizer mapped to none. Grain was shelled, yield_part seed.

## ISSUES
# GPS columns are empty. sector_ward is empty. Georeference villages in Kole (Aparangwen, Apongimalo, Apumi, Omoladyang, Omoladyang Dani).
# Season code 2013a is not in the terminag season list.
# Yield moisture and irrigation were not recorded.
# One planting and harvest date per farm, applied to every plot. Some fertilizer years are not real dates (values such as 503) and are dropped by the date check.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated; invalid season 2013a.

## NOT PROCESSED
# a_family_composition.csv, a_livestock.csv, a_technician.csv, a_inputs.csv (no rows): household and enumerator fields.
# a_general.csv: age, education, labour, farm size, legume flags.
# b_conservation_structure.csv: conservation_method.
# b_info_site_1.csv: fertility scores.
# b_info_site_2.csv: previous-season fertilizer (empty); previous crops that are not a single crop name.
# c_use_of_package_2.csv: inoculant storage. source_inoculant is Y/N and is not the inoculant product name.
# c_use_of_package_3.csv: germination class. crop_2 is empty.
# d_cropping_calendar.csv: weeding and fertilizer dates.
# f_farmer_assessment.csv: relative scores.
# g_treatments_1/2/lead_farmer.csv: no rows. g_treatments_n2africa_plot.csv: opinion text.
# variable_definitions.csv: dictionary.


carob_script <- function(path) {

"
N2Africa farm monitoring - Uganda, 2013, Season II Soybean

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/k3v2-zr87"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety;inoculated;fertilizer_type",
		response_vars = "yield",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.6
	)

	ymd <- function(dd, mm, yyyy) {
		dd <- as.numeric(dd)
		mm <- as.numeric(mm)
		yyyy <- as.numeric(yyyy)
		dim <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
		leap <- !is.na(yyyy) & (yyyy %% 4 == 0) & ((yyyy %% 100 != 0) | (yyyy %% 400 == 0))
		ok_y <- !is.na(yyyy) & yyyy > 1900 & yyyy < 2100
		ok_m <- ok_y & !is.na(mm) & mm >= 1 & mm <= 12
		maxd <- rep(NA_real_, length(mm))
		maxd[ok_m] <- dim[mm[ok_m]]
		feb <- which(ok_m & mm == 2 & leap)
		maxd[feb] <- 29
		ok <- which(ok_m & !is.na(dd) & dd >= 1 & dd <= maxd)
		out <- rep(NA_character_, length(yyyy))
		out[which(ok_y)] <- as.character(yyyy[which(ok_y)])
		out[ok] <- sprintf("%04d-%02d-%02d", yyyy[ok], mm[ok], dd[ok])
		out
	}

	f0 <- ff[basename(ff) == "a_general.csv"]
	f1 <- ff[basename(ff) == "c_use_of_package_1.csv"]
	f3 <- ff[basename(ff) == "c_use_of_package_3.csv"]
	f4 <- ff[basename(ff) == "d_cropping_calendar.csv"]
	f5 <- ff[basename(ff) == "e_harvest.csv"]
	f7 <- ff[basename(ff) == "b_info_site_2.csv"]

	r0 <- read.csv(f0, na.strings = c("", "NA"))
	r1 <- read.csv(f1, na.strings = c("", "NA"))
	r3 <- read.csv(f3, na.strings = c("", "NA"))
	r4 <- read.csv(f4, na.strings = c("", "NA"))
	r5 <- read.csv(f5, na.strings = c("", "NA"))
	r7 <- read.csv(f7, na.strings = c("", "NA"))

	loc <- carobiner::fix_name(r0$vilage, "title")
	loc[which(loc == "")] <- NA
	d0 <- data.frame(
		trial_id = as.character(r0$farm_id),
		season = as.character(r0$season),
		country = "Uganda",
		adm2 = carobiner::fix_name(r0$district, "title"),
		location = loc,
		elevation = r0$gps_altitude,
		farmer_gender = tolower(trimws(r0$gender_of_farmer))
	)

	prev <- tolower(trimws(r7$main_crop_last_season))
	prev <- carobiner::replace_values(prev,
		c("bean", "beans", "cassava", "fallow", "groundnuts", "maize", "millet", "simsim", "sunflower"),
		c("common bean", "common bean", "cassava", "fallow", "groundnut", "maize", "millet", "sesame", "sunflower"),
		FALSE)
	known <- c("common bean", "cassava", "groundnut", "maize", "millet", "sesame", "sunflower")
	prev[which(!prev %in% known)] <- NA
	d7 <- data.frame(trial_id = as.character(r7$farm_id), previous_crop = prev)

	area <- r5$area_harvested_m2
	yield <- r5$weight_kg / area * 10000
	yield[which(is.na(area) | area <= 0)] <- NA
	d5 <- data.frame(
		trial_id = as.character(r5$farm_id),
		plot_id = as.character(r5$plot_no),
		plot_area = area,
		yield = yield
	)

	om <- tolower(trimws(r1$org_fertilizer_type))
	om[is.na(om)] <- "none"
	om[which(om %in% c("control", "nil", "none"))] <- "none"
	om[which(om == "manure")] <- "farmyard manure"
	ft <- tolower(trimws(r1$min_fertilizer_type))
	ft[is.na(ft)] <- "none"
	ft[which(ft %in% c("control", "nil", "none"))] <- "none"
	ft[which(ft == "tsp")] <- "TSP"
	inoc <- toupper(trimws(r1$inoculant_used))
	d1 <- data.frame(
		trial_id = as.character(r1$farm_id),
		plot_id = as.character(r1$plot_no),
		crop = "soybean",
		variety = trimws(r1$variety),
		inoculated = ifelse(inoc == "Y", TRUE, ifelse(inoc == "N", FALSE, NA)),
		OM_type = om,
		OM_kg = as.numeric(r1$org_fertilizer_amount_kg),
		fertilizer_type = ft,
		fert_kg = as.numeric(r1$min_fertiliser_amount_kg)
	)
	d1$variety[which(d1$variety == "")] <- NA

	sp_r <- r3$crop_1_spacing_row_to_row
	sp_p <- r3$crop_1_spacing_plant_to_plant
	sp_r[which(sp_r == 0)] <- NA
	sp_p[which(sp_p == 0)] <- NA
	d3 <- data.frame(
		trial_id = as.character(r3$farm_id),
		plot_id = as.character(r3$plot_no),
		row_spacing = sp_r,
		plant_spacing = sp_p
	)

	r4 <- r4[!duplicated(r4$farm_id), ]
	d4 <- data.frame(
		trial_id = as.character(r4$farm_id),
		planting_date = ymd(r4$date_planting_dd, r4$date_planting_mm, r4$date_planting_yyyy),
		harvest_date = ymd(r4$date_harvest_dd, r4$date_harvest_mm, r4$date_harvest_yyyy)
	)

	d <- merge(d1, d5, by = c("trial_id", "plot_id"), all = TRUE)
	d <- merge(d, d3, by = c("trial_id", "plot_id"), all.x = TRUE)
	d <- merge(d, d0, by = "trial_id", all.x = TRUE)
	d <- merge(d, d4, by = "trial_id", all.x = TRUE)
	d <- merge(d, d7, by = "trial_id", all.x = TRUE)

	rate <- d$fert_kg / d$plot_area * 10000
	d$N_fertilizer <- NA_real_
	d$P_fertilizer <- NA_real_
	d$K_fertilizer <- NA_real_
	tsp <- which(d$fertilizer_type == "TSP")
	d$N_fertilizer[tsp] <- 0
	d$K_fertilizer[tsp] <- 0
	d$P_fertilizer[tsp] <- rate[tsp] * 0.1923
	none <- which(d$fertilizer_type == "none")
	d$N_fertilizer[none] <- 0
	d$P_fertilizer[none] <- 0
	d$K_fertilizer[none] <- 0

	d$OM_used <- !is.na(d$OM_type) & d$OM_type != "none"
	d$OM_amount <- d$OM_kg / d$plot_area * 10000
	d$OM_amount[which(d$OM_type == "none")] <- 0
	d$yield_part <- "seed"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE
	d$fert_kg <- NULL
	d$OM_kg <- NULL

	carobiner::write_files(path, meta, d)
}
