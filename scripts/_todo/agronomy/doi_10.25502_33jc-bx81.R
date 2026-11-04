# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm cowpea, groundnut and soybean plots in northern Nigeria, 2011. Treatment labels are control, P fertilizer, inoculant, and P plus inoculant.
# crop_1_area_harvested is treated as m2 because it matches plot_size (often 150) and kg / m2 * 10000 gives yields around 1 t/ha. The column name does not say m2.
# crop_1_weight_grain is treated as kg for the same reason. Grain unshelled Y is pod; N is seed. Groundnut is pod.
# SSP product kg/plot to elemental P with terminag fraction 8.74%. None or amount 0 is 0 N, P and K.
# Organic types mapped where the name is clear (farmyard manure, cattle dung, poultry manure). Insecticide names and "upper cort" were not mapped to an organic fertilizer.
# No GPS. Soil n.perc treated as percent N (* 10000 = mg/kg). oc.perc mapped to soil_SOC. ecec mapped to soil_CEC. District names ending in LGA stored as adm2.

## ISSUES
# Confirm that area and grain weight are m2 and kg. They are not named with units.
# GPS and altitude are empty. Georeference LGAs and villages (Bichi, Bunkure, Dawakin Kudu, Garko, Gaya, Giwa, Igabi, Kachia, Soba, Ungogo, Zango Kataf).
# "Upper cort" / "Uper cort" and insecticide names in the organic-fertilizer column were dropped rather than guessed.
# Season text includes "2011" and "Raining season/ 2011", which are not terminag season names.
# Yield moisture and irrigation were not recorded.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated; treatment, variety and inoculated have NA; plot_area up to 1250 m2; invalid season "2011" and "Raining season/ 2011"; datespan 10 harvests within 45 days of planting; high cowpea and soybean yields; NA yield.

## NOT PROCESSED
# a_general.csv: age, education, farm size, legume flags. gps columns are empty.
# a_livestock.csv: livestock counts.
# b_info_site_1.csv: fertility scores and sampling dates.
# b_info_site_2.csv: previous-season products; previous crops that are not a single crop name. fourth_crop fields contain fertilizer notes, not crops.
# c_use_of_package_3.csv: inoculant source and storage.
# c_use_of_package_4.csv: germination. crop_2 is empty.
# d_cropping_calendar.csv: emergence, weeding, pesticide dates, and days-after-1-July.
# e_harvest.csv: crop_2 weights are 0.
# f_treatments.csv: what the farmer says they learned.
# h_soil_data.csv: total carbon, Cu, Mn, Fe, exchangeable acidity.
# variable_definitions.csv: dictionary.


carob_script <- function(path) {

"
N2Africa farm monitoring - Nigeria, 2011

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/33jc-bx81"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment;variety;inoculated",
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
	f8 <- ff[basename(ff) == "h_soil_data.csv"]

	r0 <- read.csv(f0, na.strings = c("", "NA"))
	r1 <- read.csv(f1, na.strings = c("", "NA"))
	r4 <- read.csv(f4, na.strings = c("", "NA"))
	r5 <- read.csv(f5, na.strings = c("", "NA"))
	r6 <- read.csv(f6, na.strings = c("", "NA"))
	r7 <- read.csv(f7, na.strings = c("", "NA"))
	r8 <- read.csv(f8, na.strings = c("", "NA"))

	adm <- trimws(r0$district)
	adm <- gsub(" [Ll][Gg][Aa]$", "", adm)
	loc <- carobiner::fix_name(r0$vilage, "title")
	loc[which(loc == "")] <- NA
	d0 <- data.frame(
		trial_id = as.character(r0$farm_id),
		season = as.character(r0$season),
		country = "Nigeria",
		adm2 = carobiner::fix_name(adm, "title"),
		location = loc,
		farmer_gender = tolower(trimws(r0$gender_of_farmer))
	)

	prev <- tolower(trimws(r7$main_crop_last_season))
	prev <- carobiner::replace_values(prev,
		c("cowpea", "groundnut", "maize", "soybean", "sorghum", "cassava", "millet", "rice", "bean", "beans"),
		c("cowpea", "groundnut", "maize", "soybean", "sorghum", "cassava", "millet", "rice", "common bean", "common bean"),
		FALSE)
	known <- c("cowpea", "groundnut", "maize", "soybean", "sorghum", "cassava", "millet", "rice", "common bean")
	prev[which(!prev %in% known)] <- NA
	d7 <- data.frame(trial_id = as.character(r7$farm_id), previous_crop = prev)

	crop <- tolower(trimws(r1$crop_1))
	crop[which(crop == "")] <- NA
	ft <- tolower(trimws(r1$mineral_fert_type))
	ft[is.na(ft)] <- "none"
	ft[which(ft %in% c("none", "nil", "no"))] <- "none"
	ft[which(ft == "ssp")] <- "SSP"
	om <- tolower(trimws(r1$organic_fert_type))
	om[is.na(om)] <- "none"
	om[which(grepl("poutry|poultry|chicken", om))] <- "poultry manure"
	om[which(om != "poultry manure" & grepl("dung|drop|animal waste|animal manure", om))] <- "cattle dung"
	om[which(grepl("farm yard|fym", om))] <- "farmyard manure"
	om[which(grepl("demeth|cort|insect", om))] <- "none"
	om[which(om %in% c("nil", "none", "no"))] <- "none"
	inoc <- toupper(trimws(r1$inoculant_used))
	am <- r1$mineral_fert_amount
	ft[which(!is.na(am) & am == 0)] <- "none"
	d1 <- data.frame(
		trial_id = as.character(r1$farm_id),
		plot_id = as.character(r1$plot_no),
		treatment = trimws(r1$treatment),
		crop = crop,
		variety = trimws(r1$variety_1),
		inoculated = ifelse(inoc == "Y", TRUE, ifelse(inoc == "N", FALSE, NA)),
		OM_type = om,
		OM_kg = r1$organic_fert_amount,
		fertilizer_type = ft,
		fert_kg = am
	)
	d1$variety[which(d1$variety == "")] <- NA
	d1$treatment[which(d1$treatment == "")] <- NA

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

	area <- r6$crop_1_area_harvested
	y <- r6$crop_1_weight_grain / area * 10000
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

	d8 <- data.frame(
		trial_id = as.character(r8$farm_id),
		soil_pH = r8$ph,
		soil_SOC = r8$oc.perc,
		soil_sand = r8$sand.perc,
		soil_silt = r8$silt.perc,
		soil_clay = r8$clay.perc,
		soil_N = r8$n.perc * 10000,
		soil_P = r8$p_olsen.ppm,
		soil_K_exch = r8$k.cmol_per_kg,
		soil_Ca_exch = r8$ca.cmol_per_kg,
		soil_Mg_exch = r8$mg.cmol_per_kg,
		soil_Na_exch = r8$na.cmol_per_kg,
		soil_Zn = r8$zn.ppm,
		soil_CEC = r8$ecec.cmol_per_kg
	)

	d <- merge(d1, d6, by = c("trial_id", "plot_id"), all = TRUE)
	d <- merge(d, d4, by = c("trial_id", "plot_id"), all.x = TRUE)
	d <- merge(d, d0, by = "trial_id", all.x = TRUE)
	d <- merge(d, d5, by = "trial_id", all.x = TRUE)
	d <- merge(d, d7, by = "trial_id", all.x = TRUE)
	d <- merge(d, d8, by = "trial_id", all.x = TRUE)

	rate <- d$fert_kg / d$plot_area * 10000
	d$N_fertilizer <- NA_real_
	d$P_fertilizer <- NA_real_
	d$K_fertilizer <- NA_real_
	none <- which(d$fertilizer_type == "none")
	d$N_fertilizer[none] <- 0
	d$P_fertilizer[none] <- 0
	d$K_fertilizer[none] <- 0
	ssp <- which(d$fertilizer_type == "SSP")
	d$N_fertilizer[ssp] <- 0
	d$K_fertilizer[ssp] <- 0
	d$P_fertilizer[ssp] <- rate[ssp] * 0.0874

	d$OM_used <- !is.na(d$OM_type) & d$OM_type != "none"
	d$OM_amount <- d$OM_kg / d$plot_area * 10000
	d$OM_amount[which(d$OM_type == "none")] <- 0
	gn <- which(d$crop == "groundnut")
	d$yield_part[gn] <- "pod"
	d$yield_part[which(is.na(d$yield_part))] <- "seed"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE
	d$fert_kg <- NULL
	d$OM_kg <- NULL

	carobiner::write_files(path, meta, d)
}
