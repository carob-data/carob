# R script for "carob"
# license: GPL (>=3)

#livestock name (poultry) included chicken, duck, turkey, etc
# No harvest/yield data exists in this survey (crop-management/practices
# survey only) -- response_vars = "none", no yield-related columns populated.,
# 'Chapa milli' (min_fert_type_1) is treated as a typo/local name for
# 'Chapa Mbili', a regional NPK compound fertilizer brand, consistent with
# the literal '17-17-17' NPK formulation appearing in min_fert_type_2 --
# both standardised to 'NPK'. 


carob_script <- function(path) {

"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. This dataset covers a Uganda baseline survey of crop management practices in Kapchorwa district (Chema sub-county), covering household demographics, livestock composition, and per-plot crop/fertilizer management."

	uri   <- "doi:10.25502/c2yw-2m70"
	group <- "survey"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group,
		major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "N2Africa",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		carob_completion = 100,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-07-13",
		carob_effort = 3,
		notes = NA,
		design = NA
	)

	f1 <- ff[basename(ff) == "a_general_1.csv"]
	r1 <- read.csv(f1)

	f2 <- ff[basename(ff) == "a_general_2.csv"]
	r2 <- read.csv(f2)

	f3 <- ff[basename(ff) == "a_livestock_composition.csv"]
	r3 <- read.csv(f3)

	f4 <- ff[basename(ff) == "b_crop_management.csv"]
	r4 <- read.csv(f4)

	standardise_crop <- function(x) {
		x <- trimws(tolower(x))

		x[grepl("^banana$|^bananan$", x)]                          <- "banana"
		x[grepl("^bean$|^beans$|^bush bean$|^bush beans$|^climbing bean$|^climbing beans$", x)] <- "common bean"
		x[grepl("^casava$|^cassava$", x)]                          <- "cassava"
		x[grepl("^coffee$", x)]                                    <- "coffee"
		x[grepl("^groundnut$|^groundnuts$", x)]                    <- "groundnut"
		x[grepl("^irish potato$|^irish potatoes$", x)]             <- "potato"
		x[grepl("^maize$", x)]                                     <- "maize"
		x[grepl("^sweet potato$|^sweetpotato$", x)]                <- "sweetpotato"
		x[grepl("^trees$", x)]                                     <- "fruits"
		# Non-crop land uses / soil conservation practices -- not crops
		x[grepl("^grass bunds?$|^pasture$", x)]                    <- NA
		x[x %in% c("", "na")]                                      <- NA
		x
	}

	d1 <- data.frame(
		field_id        = r1$farm_id,
		country         = "Uganda",
		adm1            = tolower(trimws(r1$district)),
		adm2            = tolower(trimws(r1$sub_county)),
		adm3            = tolower(trimws(r1$parish)),
		adm4            = tolower(trimws(r1$village)),
		latitude        = suppressWarnings(as.numeric(r1$gps_latitude)),
		longitude       = suppressWarnings(as.numeric(r1$gps_longitude)),
		elevation       = suppressWarnings(as.numeric(r1$gps_altitude)),
		date            = as.character(substr(sprintf("%08d", r1$date_interview), 5, 8)),
		geo_from_source = TRUE,
		stringsAsFactors = FALSE
	)

	# farm_size -> farmland, converted from acres to ha
	unit_clean <- tolower(trimws(r1$unit))
	farmsize_num <- suppressWarnings(as.numeric(r1$farm_size))
	d1$farmland <- ifelse(
		grepl("^acres?$", unit_clean),
		farmsize_num * 0.404686,
		farmsize_num
	)
	d1$farmland[d1$farmland <= 0] <- NA

	d2 <- data.frame(
		field_id        = r2$farm_id,
		sex             = trimws(r2$sex_farmer),
		age             = suppressWarnings(as.numeric(r2$age_farmer)),
		is_head         = trimws(r2$respondent_head_hh) == "Y",
		hh_size         = suppressWarnings(as.numeric(r2$total_number_hh)),
		hh_adult_women  = suppressWarnings(
			as.numeric(r2$females_17_35) + as.numeric(r2$females_36_60)),
		hh_adult_men    = suppressWarnings(
			as.numeric(r2$males_17_35) + as.numeric(r2$males_36_60)),
		hh_child_18     = suppressWarnings(
			as.numeric(r2$females_0_16) + as.numeric(r2$males_0_16)),
		hh_elders       = suppressWarnings(
			as.numeric(r2$females_over_60) + as.numeric(r2$males_over_60)),
		education       = trimws(tolower(r2$highest_education_hh)),
		stringsAsFactors = FALSE
	)

	d2$sex[d2$sex == "M"] <- "male"
	d2$sex[d2$sex == "F"] <- "female"
	d2$sex[d2$sex == ""]  <- NA
	d2$education[d2$education %in% c("", "none")] <- NA
	d2$hh_size[d2$hh_size > 50 | d2$hh_size < 0] <- NA

	d3 <- data.frame(
		field_id = r3$farm_id,
		animal   = trimws(tolower(r3$livestock_type)),
		heads    = suppressWarnings(as.numeric(r3$livestock_number)),
		stringsAsFactors = FALSE
	)

	d3$animal[d3$animal == ""]                       <- NA
	d3$animal[grepl("^poultry", d3$animal)]          <- "poultry"
	d3$animal[d3$animal == "cattle"]                 <- "cattle"
	d3$animal[d3$animal == "goats"]                  <- "goat"
	d3$animal[d3$animal == "sheep"]                  <- "sheep"
	d3$animal[d3$animal == "pigs"]                   <- "pig"
	d3$animal[d3$animal == "donkey"]                 <- "donkey"
	d3 <- d3[!is.na(d3$animal), ]

	get_field_size <- function(ha, m2, acres) {
		ha    <- suppressWarnings(as.numeric(ha))
		m2    <- suppressWarnings(as.numeric(m2))
		acres <- suppressWarnings(as.numeric(acres))
		out <- ifelse(!is.na(ha) & ha > 0, ha,
		       ifelse(!is.na(m2) & m2 > 0, m2 / 10000,
		       ifelse(!is.na(acres) & acres > 0, acres * 0.404686, NA_real_)))
		out
	}

	d4_list <- lapply(seq_len(nrow(r4)), function(i) {
		row   <- r4[i, ]
		field <- as.character(row$farm_id)
		plot  <- as.character(row$field)
		size  <- get_field_size(row$size_ha, row$size_m2, row$size_acres)

		crops <- character(0)
		for (j in 1:5) {
			cn <- trimws(row[[paste0("crop_", j)]])
			if (!is.na(cn) && cn != "") crops <- c(crops, cn)
		}
		if (length(crops) == 0) return(NULL)

		primary <- standardise_crop(crops[1])
		others  <- if (length(crops) > 1) standardise_crop(crops[-1]) else character(0)
		others  <- others[!is.na(others)]
		intercrop <- if (length(others) > 0) paste(unique(others), collapse = ";") else NA_character_

		# Fertilizer type: up to 2 mineral fertilizer products per plot
		f1 <- trimws(row$min_fert_type_1)
		f2 <- trimws(row$min_fert_type_2)
		f1[f1 == ""] <- NA
		f2[f2 == ""] <- NA

		recode_fert <- function(f) {
			if (is.na(f)) return(NA_character_)
			fl <- tolower(f)
			if (grepl("^dap$", fl))                     return("DAP")
			if (grepl("^urea$", fl))                    return("urea")
			if (grepl("^can$", fl))                     return("CAN")
			if (grepl("^can\\+urea$|^urea\\+can$", fl)) return("CAN;urea")
			if (grepl("^npk$", fl))                     return("NPK")
			if (grepl("^17-17-17$", fl))                return("NPK")
			if (grepl("chapa", fl))                     return("NPK") # "Chapa milli" -> local NPK brand, see header note
			f
		}
		f1 <- recode_fert(f1)
		f2 <- recode_fert(f2)
		fert_parts <- unique(c(f1, f2))
		fert_parts <- fert_parts[!is.na(fert_parts)]
		fertilizer_type <- if (length(fert_parts) > 0) paste(fert_parts, collapse = ";") else NA_character_

		data.frame(
			field_id        = field,
			plot_id         = plot,
			crop            = primary,
			intercrops      = intercrop,
			field_size      = size,
			field_distance  = suppressWarnings(as.numeric(row$walking_distance_min)),
			fertilizer_type = fertilizer_type,
			OM_used         = trimws(row$organic_input_applied) == "Y",
			inoculated      = trimws(row$inoculant_applied) == "Y",
			soil_fertility  = tolower(trimws(row$fertility_field)),  #raw data showed ranking of soil fertility ranging from poor, moderate, good
			stringsAsFactors = FALSE
		)
	})

	d4 <- do.call(rbind, d4_list)
	d4$soil_fertility[d4$soil_fertility == ""] <- NA

	d <- d4

	# Household/farm-level merges
	d <- merge(d, d1, by = "field_id", all = TRUE)
	d <- merge(d, d2, by = "field_id", all = TRUE)
	d <- merge(d, d3, by = "field_id", all = TRUE)

	d$trial_id       <- as.character(as.integer(as.factor(1)))
	d$on_farm        <- TRUE
	d$is_survey      <- TRUE
	d$irrigated      <- FALSE
	d$yield_part     <- NA_character_
	d$yield_isfresh  <- TRUE
	d$treatment      <- NA_character_
	d$planting_date  <- NA_character_
	d$harvest_date   <- NA_character_
	d$yield          <- NA_real_
	d$yield_moisture <- NA_real_
	d$N_fertilizer   <- NA_real_
	d$P_fertilizer   <- NA_real_
	d$K_fertilizer   <- NA_real_

	d$yield_part[d$crop %in% c("maize")]                          <- "grain"
	d$yield_part[d$crop %in% c("common bean", "groundnut")]       <- "seed"
	d$yield_part[d$crop %in% c("cassava", "sweetpotato", "potato")] <- "roots"
	d$yield_part[d$crop %in% c("banana")]                          <- "fruit"
	d$yield_part[d$crop %in% c("coffee")]                          <- "seed"

	d$field_id       <- as.character(d$field_id)
	d$plot_id        <- as.character(d$plot_id)
	d$hh_size        <- as.integer(d$hh_size)
	d$hh_adult_women <- as.integer(d$hh_adult_women)
	d$hh_adult_men   <- as.integer(d$hh_adult_men)
	d$hh_child_18    <- as.integer(d$hh_child_18)
	d$hh_elders      <- as.integer(d$hh_elders)
	d$inoculated     <- as.logical(d$inoculated)
	d$OM_used        <- as.logical(d$OM_used)
	d$is_head        <- as.logical(d$is_head)
	d$geo_from_source <- as.logical(d$geo_from_source)
	
	char_cols <- sapply(d, is.character)
	d[char_cols] <- lapply(d[char_cols], trimws)

	d <- unique(d)

	carobiner::write_files(path, meta, d)
}
