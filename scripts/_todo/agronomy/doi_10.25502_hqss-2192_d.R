# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm cowpea, soybean and groundnut plots in northern Ghana, 2012.
# Yield = crop_1_weight_grain_kg / crop_1_area_harvested_m2 * 10000. Shelled grain (unshelled N) is seed; unshelled Y is pod. Groundnut with unshelled N is seed.
# Mineral amount is kg product on the harvested plot. A single TSP product uses terminag P fraction 19.23%, and N and K of TSP are 0. Mixtures (TSP with urea, MOP/K, or sulphate) keep the product names and do not split one amount into nutrients.
# Amount 0, or type nil/none/no, is 0 N, P and K.
# About 2.3 kg TSP on a 20 m2 harvested area is about 221 kg P/ha. The farm-level plot_size is often 150 m2 and would give about 30 kg P/ha. The plot-level harvested area was used because the amount is recorded per plot. This needs checking.
# GPS values are kept. geo_from_source is TRUE where both coordinates are present.
# cyperdicot in the organic-fertilizer column is a pesticide and was not treated as organic matter.

## ISSUES
# Which area the fertilizer amount refers to is unresolved (harvested subplot versus farm plot_size). See NOTES.
# Eight grain weights are above 100 kg (maximum 2694) and yields from those rows are not credible.
# sector_ward mixes region names (Northern, Upper East, Upper West) and local codes. It was not stored as adm3. District spellings are not reconciled to GADM.
# plot_treatment contains many free-text codes and Excel #NAME? values (set to NA).
# Season value 2012 is not in the terminag season list.
# Yield moisture and irrigation were not recorded.
# write_files: missing carob_contributor, yield_moisture, irrigated; variety, inoculated and fertilizer_type have NA; yield up to 1347000 and P_fertilizer up to about 577; invalid season 2012; 16 harvests within 45 days of planting; high cowpea and soybean yields; duplicate coordinates; NA yield, latitude, longitude.

## NOT PROCESSED
# a_family_composition.csv, a_livestock.csv: household composition and livestock.
# a_inputs.csv: input, quantity, price, cash_or_credit for household purchases.
# a_general.csv: age, education, labour, farm size, legume flags.
# b_info_site_1.csv: fertility scores.
# b_info_site_2.csv: previous-season fertilizer and inoculant text; previous crops that are not a single crop name.
# c_use_of_package_1.csv: farm plot_size (see ISSUES; not used as the plot area).
# c_use_of_package_3.csv: inoculant source and storage. cyperdicot amount.
# c_use_of_package_4.csv: germination and the second plot_treatment string (crop_2 is empty).
# d_cropping_calendar.csv: weeding dates and fertilizer comments.
# f_treatments.csv: what the farmer says they learned.
# g_farmer_assessment.csv: relative scores.
# variable_definitions.csv: dictionary. The "yield" field there is a relative score.


carob_script <- function(path) {

"
N2Africa farm monitoring - Ghana, 2012

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/hqss-2192/d"
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
		carob_completion = 55,
		carob_effort = 1
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
	f1 <- ff[basename(ff) == "c_use_of_package_2.csv"]
	f4 <- ff[basename(ff) == "c_use_of_package_4.csv"]
	f5 <- ff[basename(ff) == "d_cropping_calendar.csv"]
	f6 <- ff[basename(ff) == "e_harvest.csv"]
	f7 <- ff[basename(ff) == "b_info_site_2.csv"]

	r0 <- read.csv(f0, na.strings = c("", "NA"))
	r1 <- read.csv(f1, na.strings = c("", "NA"))
	r4 <- read.csv(f4, na.strings = c("", "NA"))
	r5 <- read.csv(f5, na.strings = c("", "NA"))
	r6 <- read.csv(f6, na.strings = c("", "NA"))
	r7 <- read.csv(f7, na.strings = c("", "NA"))

	lat <- r0$gps_latitude
	lon <- r0$gps_longitude
	lat[which(lat == 0)] <- NA
	lon[which(lon == 0)] <- NA
	loc <- carobiner::fix_name(r0$vilage, "title")
	loc[which(loc == "")] <- NA
	d0 <- data.frame(
		trial_id = as.character(r0$farm_id),
		season = as.character(r0$season),
		country = "Ghana",
		adm2 = carobiner::fix_name(r0$district, "title"),
		location = loc,
		latitude = lat,
		longitude = lon,
		elevation = r0$gps_altitude,
		farmer_gender = tolower(trimws(r0$gender_of_farmer)),
		geo_from_source = !is.na(lat) & !is.na(lon)
	)

	prev <- tolower(trimws(r7$main_crop_last_season))
	prev <- carobiner::replace_values(prev,
		c("cowpea", "groundnut", "groundnuts", "maize", "soybean", "soyabean", "fallow", "sorghum"),
		c("cowpea", "groundnut", "groundnut", "maize", "soybean", "soybean", "fallow", "sorghum"),
		FALSE)
	known <- c("cowpea", "groundnut", "maize", "soybean", "sorghum")
	prev[which(!prev %in% known)] <- NA
	d7 <- data.frame(trial_id = as.character(r7$farm_id), previous_crop = prev)

	crop <- tolower(trimws(r1$crop_1))
	crop[which(crop == "")] <- NA
	low <- tolower(trimws(r1$mineral_fert_type))
	low[is.na(low)] <- ""
	none <- low %in% c("", "nil", "nill", "no", "none", "n")
	has_tsp <- grepl("tsp", low)
	has_urea <- grepl("urea", low)
	has_k <- grepl("mop|\\bk\\b", low)
	has_s <- grepl("sulph|sulf", low)
	ft <- rep(NA_character_, length(low))
	ft[which(none)] <- "none"
	ft[which(has_tsp & !has_urea & !has_k & !has_s)] <- "TSP"
	ft[which(has_tsp & has_urea & !has_k)] <- "TSP; urea"
	ft[which(has_tsp & has_k & !has_urea)] <- "TSP; KCl"
	ft[which(has_tsp & has_urea & has_k)] <- "TSP; urea; KCl"
	ft[which(has_tsp & has_s & !has_urea & !has_k)] <- "TSP; unknown"
	left <- which(is.na(ft) & low != "")
	ft[left] <- low[left]
	kg <- r1$mineral_fert_amount
	ft[which(!is.na(kg) & kg == 0)] <- "none"
	om <- tolower(trimws(r1$organic_fert_type))
	om[is.na(om)] <- "none"
	om[which(om %in% c("nil", "nill", "no", "none", "cyperdicot"))] <- "none"
	inoc <- toupper(trimws(r1$inoculant_used))
	trt <- trimws(r1$plot_treatment)
	trt[which(trt %in% c("#NAME?", ""))] <- NA
	d1 <- data.frame(
		trial_id = as.character(r1$farm_id),
		plot_id = as.character(r1$plot_no),
		treatment = trt,
		crop = crop,
		variety = trimws(r1$variety_1),
		inoculated = ifelse(inoc == "Y", TRUE, ifelse(inoc == "N", FALSE, NA)),
		OM_type = om,
		OM_kg = r1$organic_fert_amount,
		fertilizer_type = ft,
		fert_kg = kg
	)
	d1$variety[which(d1$variety == "")] <- NA

	sr <- r4$crop_1_spacing_row_to_row
	sp <- r4$crop_1_spacing_plant_to_plant
	sr[which(sr == 0)] <- NA
	sp[which(sp == 0)] <- NA
	d4 <- data.frame(
		trial_id = as.character(r4$farm_id),
		plot_id = as.character(r4$plot_no),
		row_spacing = sr,
		plant_spacing = sp
	)

	area <- r6$crop_1_area_harvested_m2
	y <- r6$crop_1_weight_grain_kg / area * 10000
	y[which(is.na(area) | area <= 0)] <- NA
	uns <- toupper(trimws(r6$crop_1_grain_unshelled))
	part <- ifelse(uns == "Y", "pod", "seed")
	d6 <- data.frame(
		trial_id = as.character(r6$farm_id),
		plot_id = as.character(r6$plot_no),
		plot_area = area,
		yield = y,
		yield_part = part
	)

	r5 <- r5[!duplicated(r5$farm_id), ]
	d5 <- data.frame(
		trial_id = as.character(r5$farm_id),
		planting_date = ymd(r5$date_planting_dd, r5$date_planting_mm, r5$date_planting_yyyy),
		harvest_date = ymd(r5$date_harvest_dd, r5$date_harvest_mm, r5$date_harvest_yyyy)
	)

	d <- merge(d1, d6, by = c("trial_id", "plot_id"), all = TRUE)
	d <- merge(d, d4, by = c("trial_id", "plot_id"), all.x = TRUE)
	d <- merge(d, d0, by = "trial_id", all.x = TRUE)
	d <- merge(d, d5, by = "trial_id", all.x = TRUE)
	d <- merge(d, d7, by = "trial_id", all.x = TRUE)

	rate <- d$fert_kg / d$plot_area * 10000
	d$N_fertilizer <- NA_real_
	d$P_fertilizer <- NA_real_
	d$K_fertilizer <- NA_real_
	none <- which(d$fertilizer_type == "none")
	d$N_fertilizer[none] <- 0
	d$P_fertilizer[none] <- 0
	d$K_fertilizer[none] <- 0
	tsp <- which(d$fertilizer_type == "TSP")
	d$N_fertilizer[tsp] <- 0
	d$K_fertilizer[tsp] <- 0
	d$P_fertilizer[tsp] <- rate[tsp] * 0.1923

	d$OM_used <- !is.na(d$OM_type) & d$OM_type != "none"
	d$OM_amount <- d$OM_kg / d$plot_area * 10000
	d$OM_amount[which(d$OM_type == "none")] <- 0
	d$yield_part[which(is.na(d$yield_part))] <- "seed"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$fert_kg <- NULL
	d$OM_kg <- NULL

	carobiner::write_files(path, meta, d)
}
