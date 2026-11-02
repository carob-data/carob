# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm bush bean intercropped with cassava, Kayonza, Rwanda, season 2011B. Every plot has both crops, so each plot is two rows.
# Management from c_use_of_package_1/2/4, harvest from e_harvest, dates from the long d_cropping_calendar, site from a_general.
# crop_1_area_harvested and plot_size are both 60 and are treated as m2, as in other N2Africa monitoring scripts (yield = kg / m2 * 10000).
# Grain and stover weights treated as kg. Bush bean with grain_unshelled Y is yield_part pod. Cassava weight is stored as yield of roots. Stover, when > 0, is fwy_residue.
# Mineral fertilizer is NPK. Amount 0 is 0 kg N, P and K. A positive amount is not converted because the NPK grade is not in the file.
# Organic fertilizer amount is kg on the plot, converted with harvested area. Farmyard/compost spellings mapped to terminag. "Synthetic" left as recorded.

## ISSUES
# GPS columns are empty. Georeference villages in Kayonza district, sectors Nyamirama and Rukara.
# NPK grade is unknown, so N/P/K are NA wherever mineral_fert_amount > 0.
# Area unit is not named on crop_1_area_harvested; m2 is inferred from plot_size and the series.
# Cassava "grain" weight may be fresh roots; moisture was not recorded.
# Season code 2011B is not in the terminag season list.
# One planting date per farm. Bean and cassava harvest dates are separate activities.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated; invalid season 2011B; OM_type "organic fertilizer" and "synthetic" not in terminag; row_spacing up to 200 cm; 6 cassava harvest dates more than 366 days after planting.

## NOT PROCESSED
# a_farmer_group.csv: group name and purpose.
# a_general.csv: farmer age, farm_size, area_under_* (household legume area, not the trial plot).
# a_livestock.csv: livestock counts.
# b_info_site_1.csv: distance to homestead, plot ownership, soil name, fertility score, soil-sampling dates.
# b_info_site_2.csv: 2nd-4th crops and previous-season fertilizer/inoculant types; previous crops that are not a single crop name.
# c_use_of_package_3.csv: inoculant storage (inoculant_used is N for all plots).
# c_use_of_package_5.csv: crop protection (applied = N; products empty).
# d_cropping_calendar.csv: weeding and fertilizer-application dates.
# f_treatments.csv: farmer scores for yield, labour, weeds, pests (relative, not kg/ha).
# g_visits_farmer.csv and g_visits_officer.csv: visit dates and remarks.
# variable_definitions.csv: dictionary.


carob_script <- function(path) {

"
N2Africa farm monitoring - Rwanda, 2011

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/1tnr-jv20"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety;OM_type",
		response_vars = "yield",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
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

	loc <- carobiner::fix_name(r0$vilage, "title")
	loc[which(loc == "")] <- NA
	d0 <- data.frame(
		trial_id = as.character(r0$farm_id),
		season = as.character(r0$season),
		country = "Rwanda",
		adm2 = carobiner::fix_name(r0$district, "title"),
		adm3 = carobiner::fix_name(r0$sector_ward, "title"),
		location = loc,
		elevation = r0$gps_altitude,
		farmer_gender = tolower(trimws(r0$gender_of_farmer))
	)

	prev <- tolower(trimws(r7$main_crop_last_season))
	prev <- carobiner::replace_values(prev,
		c("beans", "bean", "maize", "cassava", "sorghum", "irish potatoes", "sweet potatoes", "groundnuts", "peas"),
		c("common bean", "common bean", "maize", "cassava", "sorghum", "potato", "sweetpotato", "groundnut", "pea"),
		FALSE)
	known <- c("common bean", "maize", "cassava", "sorghum", "potato", "sweetpotato", "groundnut", "pea")
	prev[which(!prev %in% known)] <- NA
	d7 <- data.frame(trial_id = as.character(r7$farm_id), previous_crop = prev)

	om <- tolower(trimws(r1$organic_fert_type))
	om[is.na(om)] <- "none"
	om[which(om %in% c("farmyard", "farmyard manure"))] <- "farmyard manure"
	om[which(om == "compost")] <- "compost"
	ft <- trimws(r1$mineral_fert_type)
	ft[is.na(ft)] <- "none"
	ft <- toupper(ft)
	am <- r1$mineral_fert_amount
	ft[which(!is.na(am) & am == 0)] <- "none"
	inoc <- toupper(trimws(r1$inoculant_used))
	d1 <- data.frame(
		trial_id = as.character(r1$farm_id),
		plot_id = as.character(r1$plot_no),
		variety = trimws(r1$variety_1),
		variety_2 = trimws(r1$variety_2),
		crop_2 = tolower(trimws(r1$crop_2)),
		inoculated = ifelse(inoc == "Y", TRUE, ifelse(inoc == "N", FALSE, NA)),
		OM_type = om,
		OM_kg = r1$organic_fert_amount,
		fertilizer_type = ft,
		fert_kg = am
	)
	d1$crop_2[which(d1$crop_2 == "")] <- NA

	sr <- r4$crop_1_spacing_row_to_row
	sp <- r4$crop_1_spacing_plant_to_plant
	sr[which(sr == 0)] <- NA
	sp[which(sp == 0)] <- NA
	sr2 <- r4$crop_2_spacing_row_to_row
	sp2 <- r4$crop_2_spacing_plant_to_plant
	sr2[which(sr2 == 0)] <- NA
	sp2[which(sp2 == 0)] <- NA
	d4 <- data.frame(
		trial_id = as.character(r4$farm_id),
		plot_id = as.character(r4$plot_no),
		row_spacing = sr,
		plant_spacing = sp,
		row_spacing_2 = sr2,
		plant_spacing_2 = sp2
	)

	area <- r6$crop_1_area_harvested
	y1 <- r6$crop_1_weight_grain / area * 10000
	y1[is.na(area) | area <= 0] <- NA
	res <- r6$crop_1_weight_stover / area * 10000
	res[which(is.na(res) | res == 0)] <- NA
	den <- r6$crop_1_plants_no / area * 10000
	den[which(is.na(den) | den == 0)] <- NA
	area2 <- r6$crop_2_area_harvested
	y2 <- r6$crop_2_weight_grain / area2 * 10000
	y2[is.na(area2) | area2 <= 0] <- NA
	res2 <- r6$crop_2_weight_stover / area2 * 10000
	res2[which(is.na(res2) | res2 == 0)] <- NA
	den2 <- r6$crop_2_plants_no / area2 * 10000
	den2[which(is.na(den2) | den2 == 0)] <- NA
	part1 <- ifelse(toupper(r6$crop_1_grain_unshelled) == "Y", "pod", "seed")
	d6 <- data.frame(
		trial_id = as.character(r6$farm_id),
		plot_id = as.character(r6$plot_no),
		plot_area = area,
		yield = y1,
		yield_part = part1,
		fwy_residue = res,
		plant_density = den,
		plot_area_2 = area2,
		yield_2 = y2,
		fwy_residue_2 = res2,
		plant_density_2 = den2
	)

	plant <- r5[r5$activity == "Date of planting", ]
	hbean <- r5[r5$activity == "Date of harvest of beans", ]
	hcas <- r5[r5$activity == "Date of harvest of cassava", ]
	dp <- data.frame(trial_id = as.character(plant$farm_id), planting_date = ymd(plant$dd, plant$mm, plant$yyyy))
	db <- data.frame(trial_id = as.character(hbean$farm_id), harvest_bean = ymd(hbean$dd, hbean$mm, hbean$yyyy))
	dc <- data.frame(trial_id = as.character(hcas$farm_id), harvest_cassava = ymd(hcas$dd, hcas$mm, hcas$yyyy))
	dp <- dp[!duplicated(dp$trial_id), ]
	db <- db[!duplicated(db$trial_id), ]
	dc <- dc[!duplicated(dc$trial_id), ]

	d <- merge(d1, d6, by = c("trial_id", "plot_id"), all = TRUE)
	d <- merge(d, d4, by = c("trial_id", "plot_id"), all.x = TRUE)
	d <- merge(d, d0, by = "trial_id", all.x = TRUE)
	d <- merge(d, dp, by = "trial_id", all.x = TRUE)
	d <- merge(d, db, by = "trial_id", all.x = TRUE)
	d <- merge(d, dc, by = "trial_id", all.x = TRUE)
	d <- merge(d, d7, by = "trial_id", all.x = TRUE)

	bean <- data.frame(
		trial_id = d$trial_id,
		plot_id = d$plot_id,
		season = d$season,
		country = d$country,
		adm2 = d$adm2,
		adm3 = d$adm3,
		location = d$location,
		elevation = d$elevation,
		farmer_gender = d$farmer_gender,
		crop = "common bean",
		variety_type = "bush bean",
		variety = d$variety,
		previous_crop = d$previous_crop,
		inoculated = d$inoculated,
		OM_type = d$OM_type,
		OM_kg = d$OM_kg,
		fertilizer_type = d$fertilizer_type,
		fert_kg = d$fert_kg,
		plot_area = d$plot_area,
		yield = d$yield,
		yield_part = d$yield_part,
		fwy_residue = d$fwy_residue,
		plant_density = d$plant_density,
		row_spacing = d$row_spacing,
		plant_spacing = d$plant_spacing,
		planting_date = d$planting_date,
		harvest_date = d$harvest_bean
	)
	cas <- bean
	cas$crop <- "cassava"
	cas$variety_type <- NA
	cas$variety <- d$variety_2
	cas$plot_area <- d$plot_area_2
	cas$yield <- d$yield_2
	cas$yield_part <- "roots"
	cas$fwy_residue <- d$fwy_residue_2
	cas$plant_density <- d$plant_density_2
	cas$row_spacing <- d$row_spacing_2
	cas$plant_spacing <- d$plant_spacing_2
	cas$harvest_date <- d$harvest_cassava
	keep <- !is.na(d$crop_2)
	d <- rbind(bean, cas[keep, ])

	d$N_fertilizer <- 0
	d$P_fertilizer <- 0
	d$K_fertilizer <- 0
	npk <- !is.na(d$fertilizer_type) & d$fertilizer_type == "NPK" & !is.na(d$fert_kg) & d$fert_kg > 0
	d$N_fertilizer[npk] <- NA
	d$P_fertilizer[npk] <- NA
	d$K_fertilizer[npk] <- NA

	d$OM_used <- !is.na(d$OM_type) & d$OM_type != "none"
	d$OM_amount <- d$OM_kg / d$plot_area * 10000
	d$OM_amount[which(d$OM_type == "none")] <- 0

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE
	d$OM_kg <- NULL
	d$fert_kg <- NULL

	carobiner::write_files(path, meta, d)
}
