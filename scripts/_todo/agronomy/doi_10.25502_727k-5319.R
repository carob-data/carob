# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm climbing-bean input plots, Kapchorwa, Uganda, season 2013A (dataset season II).
# Plot records from c_use_of_package_1/3, harvest from e_harvest, dates from d_cropping_calendar, site from a_general and b_info_site_2.
# area_harvested_m2 is m2. Where weight_kg is 0 and weight_grain is positive, weight_grain is grams (when both are positive, weight_kg = weight_grain/1000).
# TSP product kg/plot to elemental P with terminag TSP fraction 19.23% P. N and K of TSP are 0. "O.17" read as 0.17 kg.
# Manure mapped to farmyard manure. Amount is kg product on the plot, converted with harvested area.
# Climbing bean stored as common bean with variety_type "climbing bean". Grain was shelled (crop_1_grain_unshelled N), yield_part seed.

## ISSUES
# gps_latitude and gps_longitude are empty. sector_ward is empty. Georeference villages in Kapchorwa (action sites Kapchesi, Kapchesombe, Kaptanya, Kiruoko, Siron).
# CKAN coverage_x/coverage_y is one dataset point, not plot coordinates, and was not used.
# Season code 2013A is not in the terminag season list. Inoculant was N on every plot. Organic fertilizer type is manure but the amount is empty.
# Yield moisture was not recorded. Irrigation was not recorded.
# One planting and harvest date per farm, applied to every plot on that farm.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated; invalid season 2013A; NA yield.

## NOT PROCESSED
# a_family_composition.csv: household age-sex counts (not plot production).
# a_general.csv: age, education, hired labour, farm_size_ha/acre, legume area flags, rainfall/soil sample flags.
# a_inputs.csv: household input purchases (input, quantity), not the trial plot.
# a_livestock.csv: livestock counts.
# a_technician.csv: enumerator name and date.
# b_conservation_structure.csv: conservation_method.
# b_info_site_1.csv: farmer soil-fertility scores.
# b_info_site_2.csv: fertilizer and inoculant used in previous seasons (type only); previous crops that are not a single crop name.
# c_use_of_package_2.csv: inoculant source, storage, adhesive, chemicals (inoculant_used is taken from package 1).
# c_use_of_package_3.csv: germination classes and crop_2 spacing (crop_2 empty).
# d_cropping_calendar.csv: land preparation, weeding, and fertilizer-application dates.
# e_harvest.csv: weight_grain kept only as the grams source for missing weight_kg.
# f_farmer_assessment.csv: relative scores for yield, labour, weeds, pests (not quantities).
# g_treatments_*.csv: training and opinion text; g_treatments_1/2/lead_farmer have no rows.
# variable_definitions.csv: data dictionary. Relative "yield" in the assessment form is not a kg/ha yield.


carob_script <- function(path) {

"
N2Africa farm monitoring - Uganda, 2013, II

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/727k-5319"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.8
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

	parse_kg <- function(x) {
		x <- trimws(tolower(as.character(x)))
		x <- gsub("^o\\.", "0.", x)
		ok <- grepl("^[0-9.]+$", x)
		v <- rep(NA_real_, length(x))
		v[ok] <- as.numeric(x[ok])
		v
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
	adm3 <- carobiner::fix_name(r0$sector_ward, "title")
	adm3[which(adm3 == "")] <- NA
	d0 <- data.frame(
		trial_id = as.character(r0$farm_id),
		season = as.character(r0$season),
		country = "Uganda",
		adm2 = carobiner::fix_name(r0$district, "title"),
		adm3 = adm3,
		location = loc,
		elevation = r0$gps_altitude,
		farmer_gender = tolower(trimws(r0$gender_of_farmer))
	)

	prev <- tolower(trimws(r7$main_crop_last_season))
	prev <- carobiner::replace_values(prev,
		c("beans", "bean", "bush beans", "climbing beans", "maize", "banana", "barley", "cowpeas", "soyabean", "sweet potatoes", "potatoes", "onions", "yams"),
		c("common bean", "common bean", "common bean", "common bean", "maize", "banana", "barley", "cowpea", "soybean", "sweetpotato", "potato", "onion", "yam"),
		FALSE)
	known <- c("common bean", "maize", "banana", "barley", "cowpea", "soybean", "sweetpotato", "potato", "onion", "yam")
	prev[which(!prev %in% known)] <- NA
	d7 <- data.frame(
		trial_id = as.character(r7$farm_id),
		previous_crop = prev
	)

	kg <- r5$weight_kg
	g <- r5$weight_grain
	use_g <- !is.na(g) & g > 0 & (is.na(kg) | kg == 0)
	kg[use_g] <- g[use_g] / 1000
	area <- r5$area_harvested_m2
	yield <- kg / area * 10000
	yield[is.na(area) | area <= 0] <- NA
	d5 <- data.frame(
		trial_id = as.character(r5$farm_id),
		plot_id = as.character(r5$plot_no),
		plot_area = area,
		yield = yield
	)

	om <- tolower(trimws(r1$org_fertilizer_type))
	om[is.na(om)] <- "none"
	om[which(om == "manure")] <- "farmyard manure"
	ft <- trimws(r1$min_fertilizer_type)
	ft[is.na(ft)] <- "none"
	ft[which(tolower(ft) == "tsp")] <- "TSP"
	am <- parse_kg(r1$min_fertiliser_amount_kg)
	inoc <- toupper(trimws(r1$inoculant_used))
	d1 <- data.frame(
		trial_id = as.character(r1$farm_id),
		plot_id = as.character(r1$plot_no),
		crop = "common bean",
		variety_type = "climbing bean",
		variety = trimws(r1$variety),
		inoculated = ifelse(inoc == "Y", TRUE, ifelse(inoc == "N", FALSE, NA)),
		OM_type = om,
		OM_kg = r1$org_fertilizer_amount_kg,
		fertilizer_type = ft,
		fert_kg = am,
		row_area = r1$plot_size
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

	area <- d$plot_area
	miss <- is.na(area) | area <= 0
	area[miss] <- d$row_area[miss]
	d$plot_area <- area

	rate <- d$fert_kg / d$plot_area * 10000
	d$N_fertilizer <- ifelse(d$fertilizer_type == "TSP", 0, NA_real_)
	d$K_fertilizer <- ifelse(d$fertilizer_type == "TSP", 0, NA_real_)
	d$P_fertilizer <- ifelse(d$fertilizer_type == "TSP", rate * 0.1923, NA_real_)
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
	d$row_area <- NULL

	carobiner::write_files(path, meta, d)
}
