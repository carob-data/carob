# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm plots in Kayonza and Kabarondo, Rwanda, seasons 2012A and 2012B.
# Bush bean plots are intercropped with cassava (two rows per plot). Most soybean plots are sole; three are with maize.
# crop_1_area_harvested treated as m2 (plot_size is 60 m2 on the same farms). Grain and stover weights treated as kg.
# Bush bean with grain_unshelled Y is yield_part pod. Soybean is seed. Maize is grain. Cassava weight is roots. Stover above 0 is fwy_residue.
# DAP converted with terminag fractions N 18% and P 20.1%. NPK grade is not in the file, so N/P/K stay NA when the amount is positive. Amount 0 is 0 N, P and K.
# Organic amount is kg on the plot. compost+farmyard spellings mapped to terminag and joined with "; ".

## ISSUES
# GPS columns are empty. Georeference villages in Kayonza and Kabarondo (sectors Kabarondo, Nyamirama, Rukara, Rwinkwavu).
# NPK grade unknown.
# No harvest activity for soybean or maize, so those harvest dates are NA. Bean harvest uses "Date of harvest of beans" and "Date of harvest(beans)".
# Cassava harvest can be more than a year after planting.
# Season codes 2012A and 2012B are not in the terminag season list.
# OM type "synthetic" is not an organic fertilizer and was left as recorded.
# Area column is not named m2.
# Yield moisture and irrigation were not recorded. Plots with no harvest row have yield NA; yield_part is seed for bean and soybean on those rows.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated; variety and fertilizer_type have NA; yield 0 to 200000 and row_spacing 40 to 200 out of bounds; invalid season 2012A/2012B; OM_type "synthetic"; 14 harvests within 45 days of planting and 138 more than 366 days after; high cassava and common bean yields; NA yield.

## NOT PROCESSED
# a_farmer_group.csv: group name and purpose.
# a_general.csv: age, farm_size, area_under_* household legume areas.
# a_livestock.csv: livestock counts.
# b_info_site_1.csv: homestead distance, ownership, soil name, fertility score, sampling dates.
# b_info_site_2.csv: extra crops in the rotation and previous fertilizer types; previous crops that are not a single crop name.
# c_use_of_package_3.csv: inoculant storage (use is N on all plots).
# c_use_of_package_5.csv: crop protection recorded as N.
# d_cropping_calendar.csv: weeding and fertilizer dates.
# f_treatments.csv: relative scores, not kg/ha.
# g_visits_farmer.csv and g_visits_officer.csv: visit remarks.
# variable_definitions.csv: dictionary.


carob_script <- function(path) {

"
N2Africa farm monitoring - Rwanda, 2012, II

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/0msx-r973"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety;fertilizer_type",
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

	clean_om <- function(x) {
		x <- tolower(trimws(x))
		x[is.na(x)] <- "none"
		x <- gsub("faramyard", "farmyard", x)
		x <- gsub("farmyatd", "farmyard", x)
		x <- gsub("farmyard  manure", "farmyard manure", x)
		x <- gsub("\\+", "; ", x)
		x <- gsub("^farmyard$", "farmyard manure", x)
		x <- gsub("farmyard;", "farmyard manure;", x)
		x
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
		c("beans", "bean", "banana", "cassava", "groundnuts", "maize", "peas", "sorghum", "soybeans", "sweet potatoes", "irish potatoes"),
		c("common bean", "common bean", "banana", "cassava", "groundnut", "maize", "pea", "sorghum", "soybean", "sweetpotato", "potato"),
		FALSE)
	known <- c("common bean", "banana", "cassava", "groundnut", "maize", "pea", "sorghum", "soybean", "sweetpotato", "potato")
	prev[which(!prev %in% known)] <- NA
	d7 <- data.frame(trial_id = as.character(r7$farm_id), previous_crop = prev)

	raw1 <- tolower(trimws(r1$crop_1))
	vtype <- ifelse(raw1 == "bush bean", "bush bean", NA_character_)
	crop1 <- raw1
	crop1[which(crop1 == "bush bean")] <- "common bean"
	ft <- toupper(trimws(r1$mineral_fert_type))
	ft[is.na(ft)] <- "none"
	am <- r1$mineral_fert_amount
	ft[which(!is.na(am) & am == 0)] <- "none"
	inoc <- toupper(trimws(r1$inoculant_used))
	d1 <- data.frame(
		trial_id = as.character(r1$farm_id),
		plot_id = as.character(r1$plot_no),
		crop = crop1,
		variety_type = vtype,
		variety = trimws(r1$variety_1),
		crop_2 = tolower(trimws(r1$crop_2)),
		variety_2 = trimws(r1$variety_2),
		inoculated = ifelse(inoc == "Y", TRUE, ifelse(inoc == "N", FALSE, NA)),
		OM_type = clean_om(r1$organic_fert_type),
		OM_kg = r1$organic_fert_amount,
		fertilizer_type = ft,
		fert_kg = am
	)
	d1$crop_2[which(d1$crop_2 == "")] <- NA
	d1$variety[which(d1$variety == "")] <- NA
	d1$variety_2[which(d1$variety_2 == "")] <- NA

	sr <- r4$crop_1_spacing_row_to_row
	sp <- r4$crop_1_spacing_plant_to_plant
	sr2 <- r4$crop_2_spacing_row_to_row
	sp2 <- r4$crop_2_spacing_plant_to_plant
	sr[which(sr == 0)] <- NA
	sp[which(sp == 0)] <- NA
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
	y1[which(is.na(area) | area <= 0)] <- NA
	res <- r6$crop_1_weight_stover / area * 10000
	res[which(is.na(res) | res == 0)] <- NA
	den <- r6$crop_1_plants_no / area * 10000
	den[which(is.na(den) | den == 0)] <- NA
	area2 <- r6$crop_2_area_harvested
	y2 <- r6$crop_2_weight_grain / area2 * 10000
	y2[which(is.na(area2) | area2 <= 0)] <- NA
	res2 <- r6$crop_2_weight_stover / area2 * 10000
	res2[which(is.na(res2) | res2 == 0)] <- NA
	den2 <- r6$crop_2_plants_no / area2 * 10000
	den2[which(is.na(den2) | den2 == 0)] <- NA
	uns <- toupper(trimws(r6$crop_1_grain_unshelled))
	part1 <- ifelse(uns == "Y", "pod", "seed")
	part1[which(is.na(uns))] <- "seed"
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

	one_date <- function(sub) {
		z <- data.frame(trial_id = as.character(sub$farm_id), dte = ymd(sub$dd, sub$mm, sub$yyyy))
		z <- z[order(is.na(z$dte)), ]
		z[!duplicated(z$trial_id), ]
	}
	dp <- one_date(r5[r5$activity == "Date of planting", ])
	names(dp)[2] <- "planting_date"
	db <- one_date(r5[r5$activity %in% c("Date of harvest of beans", "Date of harvest(beans)"), ])
	names(db)[2] <- "harvest_bean"
	dc <- one_date(r5[r5$activity == "Date of harvest of cassava", ])
	names(dc)[2] <- "harvest_cassava"

	d <- merge(d1, d6, by = c("trial_id", "plot_id"), all = TRUE)
	d <- merge(d, d4, by = c("trial_id", "plot_id"), all.x = TRUE)
	d <- merge(d, d0, by = "trial_id", all.x = TRUE)
	d <- merge(d, dp, by = "trial_id", all.x = TRUE)
	d <- merge(d, db, by = "trial_id", all.x = TRUE)
	d <- merge(d, dc, by = "trial_id", all.x = TRUE)
	d <- merge(d, d7, by = "trial_id", all.x = TRUE)
	d$yield_part[which(is.na(d$yield_part) & d$crop %in% c("common bean", "soybean"))] <- "seed"

	leg <- data.frame(
		trial_id = d$trial_id, plot_id = d$plot_id, season = d$season,
		country = d$country, adm2 = d$adm2, adm3 = d$adm3, location = d$location,
		elevation = d$elevation, farmer_gender = d$farmer_gender,
		crop = d$crop, variety_type = d$variety_type, variety = d$variety,
		previous_crop = d$previous_crop, inoculated = d$inoculated,
		OM_type = d$OM_type, OM_kg = d$OM_kg, fertilizer_type = d$fertilizer_type, fert_kg = d$fert_kg,
		plot_area = d$plot_area, yield = d$yield, yield_part = d$yield_part,
		fwy_residue = d$fwy_residue, plant_density = d$plant_density,
		row_spacing = d$row_spacing, plant_spacing = d$plant_spacing,
		planting_date = d$planting_date, harvest_date = d$harvest_bean
	)
	leg$harvest_date[which(leg$crop != "common bean")] <- NA
	comp <- leg
	comp$crop <- d$crop_2
	comp$variety_type <- NA
	comp$variety <- d$variety_2
	comp$plot_area <- d$plot_area_2
	comp$yield <- d$yield_2
	comp$yield_part <- "seed"
	comp$yield_part[which(d$crop_2 == "cassava")] <- "roots"
	comp$yield_part[which(d$crop_2 == "maize")] <- "grain"
	comp$fwy_residue <- d$fwy_residue_2
	comp$plant_density <- d$plant_density_2
	comp$row_spacing <- d$row_spacing_2
	comp$plant_spacing <- d$plant_spacing_2
	comp$harvest_date <- ifelse(d$crop_2 == "cassava", d$harvest_cassava, NA_character_)
	keep <- which(!is.na(d$crop_2))
	d <- rbind(leg, comp[keep, ])

	d$N_fertilizer <- NA_real_
	d$P_fertilizer <- NA_real_
	d$K_fertilizer <- NA_real_
	none <- which(d$fertilizer_type == "none")
	d$N_fertilizer[none] <- 0
	d$P_fertilizer[none] <- 0
	d$K_fertilizer[none] <- 0
	rate <- d$fert_kg / d$plot_area * 10000
	dap <- which(d$fertilizer_type == "DAP")
	d$N_fertilizer[dap] <- rate[dap] * 0.18
	d$P_fertilizer[dap] <- rate[dap] * 0.201
	d$K_fertilizer[dap] <- 0

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
