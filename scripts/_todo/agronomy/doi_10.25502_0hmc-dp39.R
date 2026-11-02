# R script for "carob"
# license: GPL (>=3)

## ISSUES
# GPS lat/lon columns empty; georeference villages (Lushoto, Hai, Arumeru).
# weight_grain mixed "g"/"gm"/bare numbers; treated as grams.
# area_harvested_m2 is 0 for some plots; plot_size used when it looks like m2 (not "6m10").
# TSP amounts such as 9gm per 16 m2 give very low P rates; check N2Africa protocol.
# crop column often blank; filled as common bean from non-missing plot crops.
# harvest month sometimes missing.
# write_files: missing longitude/latitude/yield_moisture; NA yield when area unknown; NA variety/inoculated/fertilizer_type.


carob_script <- function(path) {

"
N2Africa farm monitoring - Tanzania, 2013

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/0hmc-dp39"
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
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 55,
		carob_effort = 0.9
	)

	f0 <- ff[basename(ff) == "a_general.csv"]
	f1 <- ff[basename(ff) == "c_use_of_package_1.csv"]
	f2 <- ff[basename(ff) == "c_use_of_package_3.csv"]
	f3 <- ff[basename(ff) == "d_cropping_calendar.csv"]
	f4 <- ff[basename(ff) == "e_harvest.csv"]
	f5 <- ff[basename(ff) == "b_info_site_2.csv"]

	r0 <- read.csv(f0, na.strings = c("", "NA"))
	r1 <- read.csv(f1, na.strings = c("", "NA"))
	r2 <- read.csv(f2, na.strings = c("", "NA"))
	r3 <- read.csv(f3, na.strings = c("", "NA"))
	r4 <- read.csv(f4, na.strings = c("", "NA"))
	r5 <- read.csv(f5, na.strings = c("", "NA"))

	parse_m2 <- function(x) {
		x <- trimws(tolower(x))
		x <- gsub("m2", "", x)
		x <- gsub(" ", "", x)
		# "6m10" is a plot label, not an area
		ok <- grepl("^[0-9.]+$", x)
		out <- as.numeric(NA)
		out[ok] <- as.numeric(x[ok])
		out
	}

	parse_mass_kg <- function(x) {
		x <- trimws(tolower(x))
		x[x %in% c("no", "nil", "none")] <- "0"
		is_g <- grepl("gm$|g$", x)
		x <- gsub("kg", "", x)
		x <- gsub("gm$|g$", "", x)
		x <- trimws(x)
		ok <- grepl("^[0-9.]+$", x)
		v <- rep(as.numeric(NA), length(x))
		v[ok] <- as.numeric(x[ok])
		v[is_g & ok] <- v[is_g & ok] / 1000
		v
	}

	parse_grams <- function(x) {
		x <- trimws(tolower(as.character(x)))
		x <- gsub("gm$|g$", "", x)
		x <- trimws(x)
		ok <- grepl("^[0-9.]+$", x)
		v <- rep(as.numeric(NA), length(x))
		v[ok] <- as.numeric(x[ok])
		v
	}

	d0 <- data.frame(
		trial_id = r0$farm_id,
		country = "Tanzania",
		adm2 = carobiner::fix_name(r0$district, "title"),
		location = carobiner::fix_name(r0$vilage, "title"),
		elevation = as.numeric(gsub(" meter", "", r0$gps_altitude)),
		farmer_gender = tolower(r0$gender_of_farmer)
	)
	d0$adm2[d0$adm2 == ""] <- NA
	d0$location[d0$location == ""] <- NA

	d5 <- data.frame(
		trial_id = r5$farm_id,
		previous_crop = tolower(trimws(r5$main_crop_last_season))
	)
	d5$previous_crop <- carobiner::replace_values(d5$previous_crop,
		c("cabbeag", "cabbage", "maize/beans", "beans", "beans, vegetables",
			"beans/maize", "maize and beans", "maize and sunflower", "maize/beansr"),
		c("cabbage", "cabbage", "maize", "common bean", "common bean",
			"maize", "maize", "maize", "maize"),
		FALSE
	)

	ymd <- function(dd, mm, yyyy) {
		oky <- !is.na(yyyy) & yyyy > 1900
		ok <- oky & !is.na(mm) & mm > 0 & mm <= 12 & !is.na(dd) & dd > 0 & dd <= 31
		out <- rep(NA_character_, length(yyyy))
		out[oky] <- as.character(yyyy[oky])
		out[ok] <- sprintf("%04d-%02d-%02d", yyyy[ok], mm[ok], dd[ok])
		out
	}
	d3 <- data.frame(
		trial_id = r3$farm_id,
		planting_date = ymd(r3$date_planting_dd, r3$date_planting_mm, r3$date_planting_yyyy),
		harvest_date = ymd(r3$date_harvest_dd, r3$date_harvest_mm, r3$date_harvest_yyyy)
	)

	d1 <- data.frame(
		trial_id = r1$farm_id,
		plot_id = as.character(r1$plot_no),
		plot_area = parse_m2(r1$plot_size),
		crop = tolower(trimws(r1$crop)),
		variety = trimws(r1$variety),
		inoculated = toupper(r1$inoculant_used) == "Y",
		OM_type = r1$org_fertilizer_type,
		OM_amount = parse_mass_kg(r1$org_fertilizer_amount_kg),
		fertilizer_type = r1$min_fertilizer_type,
		fert_amount = parse_mass_kg(r1$min_fertiliser_amount_kg)
	)
	d1$crop[d1$crop %in% c("bean", "beans")] <- "common bean"
	d1$crop[d1$crop == ""] <- NA
	d1$variety[d1$variety %in% c("", "NO", "No")] <- NA
	d1$OM_type <- trimws(tolower(d1$OM_type))
	d1$OM_type[d1$OM_type %in% c("fym", "fyd", "farm yard manure", "animal manure")] <- "farmyard manure"
	d1$OM_type[d1$OM_type %in% c("no", "none", "")] <- "none"
	d1$OM_used <- !is.na(d1$OM_type) & d1$OM_type != "none"
	d1$fertilizer_type <- trimws(d1$fertilizer_type)
	d1$fertilizer_type[d1$fertilizer_type %in% c("No", "NO", "Nil", "none", "None", "")] <- "none"

	d2 <- data.frame(
		trial_id = r2$farm_id,
		plot_id = as.character(r2$plot_no),
		row_spacing = r2$crop_1_spacing_row_to_row,
		plant_spacing = r2$crop_1_spacing_plant_to_plant
	)

	d4 <- data.frame(
		trial_id = r4$farm_id,
		plot_id = as.character(r4$plot_no),
		area_harvested = r4$area_harvested_m2,
		grain_g = parse_grams(r4$weight_grain)
	)

	d <- merge(d1, d4, by = c("trial_id", "plot_id"), all = TRUE)
	d <- merge(d, d2, by = c("trial_id", "plot_id"), all.x = TRUE)
	d <- merge(d, d0, by = "trial_id", all.x = TRUE)
	d <- merge(d, d3, by = "trial_id", all.x = TRUE)
	d <- merge(d, d5, by = "trial_id", all.x = TRUE)

	area <- d$area_harvested
	area[is.na(area) | area == 0] <- d$plot_area[is.na(area) | area == 0]
	d$plot_area <- area
	# grams -> kg/ha
	d$yield <- (d$grain_g / 1000) / d$plot_area * 10000

	# TSP ~ 19.23% P; control/none fertilizer = 0
	d$N_fertilizer <- 0
	d$P_fertilizer <- 0
	d$K_fertilizer <- 0
	i <- !is.na(d$fertilizer_type) & d$fertilizer_type == "TSP" & !is.na(d$fert_amount) & !is.na(d$plot_area)
	d$P_fertilizer[i] <- (d$fert_amount[i] / d$plot_area[i] * 10000) * 0.1923
	d$OM_amount <- d$OM_amount / d$plot_area * 10000

	d$crop[is.na(d$crop)] <- "common bean"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$yield_part <- "seed"
	d$geo_from_source <- FALSE

	d$grain_g <- NULL
	d$area_harvested <- NULL
	d$fert_amount <- NULL

	carobiner::write_files(path, meta, d)
}
