# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {

"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. This dataset covers the Togo baseline survey conducted in 2012 across two action sites in the Plateaux region: Kpalimé (farm IDs: TGKL) and a second adjacent site (farm IDs: TGRM)."

	uri   <- "doi:10.25502/wcbr-ck09/d"
	group <- "survey"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "N2Africa",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		carob_completion = 100,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-06-24",
		carob_effort = 4,
		notes = NA,
		design = NA
	)

	f1 <- ff[basename(ff) == "a1_a3_demographic.csv"]
	r1 <- read.csv(f1)

	f2 <- ff[basename(ff) == "a4_demographic.csv"]
	r2 <- read.csv(f2)

	f3 <- ff[basename(ff) == "c_labour.csv"]
	r3 <- read.csv(f3)

	f4 <- ff[basename(ff) == "d_livestock_ownership.csv"]
	r4 <- read.csv(f4)

	f5 <- ff[basename(ff) == "e_land_use.csv"]
	r5 <- read.csv(f5)

	f6 <- ff[basename(ff) == "e3_land_use_rotation.csv"]
	r6 <- read.csv(f6)

	f7 <- ff[basename(ff) == "f_crop_production.csv"]
	r7 <- read.csv(f7)

	f8 <- ff[basename(ff) == "g1_legume_utilisation.csv"]
	r8 <- read.csv(f8)

	f9 <- ff[basename(ff) == "g2_legume_utilisation.csv"]
	r9 <- read.csv(f9)

	standardise_crop <- function(x) {
		x <- trimws(tolower(x))
		x[grepl("^maize$|^corn$|^mais$", x)]                          <- "maize"
		x[grepl("^cassava$|^manioc$", x)]                             <- "cassava"
		x[grepl("^yam$|^igname$|^kratchi|^katara|^lotossou", x)]      <- "yam"
		x[grepl("^cowpea$", x)]                                       <- "cowpea"
		x[grepl("^soybean$|^soya$", x)]                               <- "soybean"
		x[grepl("^groundnut$|^arachide$", x)]                         <- "groundnut"
		x[grepl("^rice$", x)]                                         <- "rice"
		x[grepl("cajanus|pigeon pea", x)]                             <- "pigeonpea"
		x[grepl("mucuna", x)]                                         <- "mucuna"
		x[grepl("^sweet pot|^sweet pat", x)]                          <- "sweetpotato"
		x[grepl("^taro$|^cocoyam$", x)]                               <- "cocoyam"
		x[grepl("^banana$|^plantain$", x)]                            <- "banana"
		x[grepl("^tomato$|^tomate$|^tomoto$", x)]                     <- "tomato"
		x[grepl("^pepper$|^pipper$", x)]                              <- "pepper"
		x[grepl("^eggplant$|^garden egg$|garden eggs", x)]           <- "eggplant"
		x[grepl("^okra$|^gombo$|^okro$|^occro$", x)]                 <- "okra"
		x[grepl("^cabbage$", x)]                                      <- "cabbage"
		x[grepl("^lettuce$", x)]                                      <- "lettuce"
		x[grepl("^cocoa$", x)]                                        <- "cocoa"
		x[grepl("^coffee$|^cofee$", x)]                               <- "coffee"
		x[grepl("^palm oil$|^oil palm$", x)]                          <- NA
		x[grepl("^rubber$", x)]                                       <- NA
		x[grepl("^sugarcane$|^sugar cane", x)]                        <- "sugarcane"
		x[grepl("eucalyptus|teck|techtona|techtonas", x)]             <- "eucalyptus"
		x[grepl("^orange", x)]                                        <- "orange"
		x[grepl("^avocado$", x)]                                      <- "avocado"
		x[grepl("^sorghum$", x)]                                      <- "sorghum"
		x[grepl("^fallow$", x)]                                       <- NA
		x[x %in% c("", "na", "NA")]                                   <- NA
		x
	}


	d1 <- data.frame(
		field_id  = r1$farm_id,
		# household head gender: m/f
		sex      = trimws(tolower(r1$gender_household_head)),
		hh_size  = suppressWarnings(
			as.numeric(r1$household_number_of_adults) +
			as.numeric(r1$household_number_of_children)),
		hh_adult_men   = suppressWarnings(NA_integer_),
		hh_adult_women = suppressWarnings(NA_integer_),
		hh_child_18    = suppressWarnings(as.integer(r1$household_number_of_children)),
		stringsAsFactors = FALSE
	)

	d1$sex[d1$sex == "m"] <- "male"
	d1$sex[d1$sex == "f"] <- "female"
	d1$sex[d1$sex == ""] <- NA
	d1$hh_size[d1$hh_size < 0 | d1$hh_size > 50] <- NA

	d2 <- data.frame(
	  field_id = r2$farm_id,
	  age      = suppressWarnings(as.numeric(r2$age)),
	  sex      = r2$gender,
	  education = trimws(tolower(r2$highest_education_level))
	)
	
	d2$is_head <- ifelse(r2$relation_to_household_member=="household head",TRUE,FALSE)
	d2$sex <- ifelse(d2$sex=="M","male","female")
 
	d3 <- data.frame(
		field_id           = r3$farm_id,
		farm_labour_hired = trimws(r3$labour_hired) == "y",
		stringsAsFactors  = FALSE
	)
  
	d4 <- data.frame(
		field_id   = r4$farm_id,
		animal = trimws(tolower(r4$livestock)),
		heads = r4$number_owned,
		stringsAsFactors = FALSE
	)

	d4$animal[d4$animal == ""]             <- NA
	# Standardise to terminag vocabulary
	d4$animal[grepl("^goats?$", d4$animal)]          <- "goat"
	d4$animal[d4$livestock == "pigs"]                   <- "pig"
	d4$animal[d4$livestock == "sheep"]                  <- "sheep"
	d4$animal[grepl("^ducks?$", d4$animal)]          <- "duck"
	d4$animal[d4$livestock == "chicken"]                <- "chicken"
	d4$animal[d4$livestock == "rabbits"]                <- "rabbit"
	d4$animal[grepl("^dogs?$", d4$animal)]           <- "dog"
	d4$animal[grepl("guinea fowls?", d4$animal)]     <- "guinea fowl"
	d4$animal[d4$livestock == "guinea pigs"]            <- "guinea pig"
	d4$animal[d4$livestock == "pigeon"]                 <- "pigeon"
	d4$animal[d4$livestock == "oxen"]                   <- "cattle"
	d4$animal[grepl("agouti|rat\\(zool\\)", d4$animal)] <- NA
	d4$animal[grepl("^cats?$", d4$animal)]           <- "cat"

	d5 <- data.frame(
		field_id      = r5$farm_id,
		land_fallow  = r5$is_land_left_fallow_during_cropping_season == "y",
		stringsAsFactors = FALSE
	)

	# Each row = one season in a rotation; up to 3 crops per season.
	# Reconstruct crop_rotation as unique standardised crops per farm.
	r6_long <- do.call(rbind, lapply(seq_len(nrow(r6)), function(i) {
		row    <- r6[i, ]
		crops  <- c(row$principle_crop, row$second_crop, row$third_crop)
		crops  <- unique(trimws(crops[crops != "" & !is.na(crops)]))
		crops  <- standardise_crop(crops)
		crops  <- crops[!is.na(crops)]
		if (length(crops) == 0) return(NULL)
		data.frame(field_id = row$farm_id, crop = crops, stringsAsFactors = FALSE)
	}))

	if (!is.null(r6_long)) {
		d6 <- aggregate(
			crop ~ field_id,
			data = r6_long,
			FUN  = function(x) paste(unique(x), collapse = ";")
		)
		names(d6)[2] <- "crop_rotation"
	} else {
		d6 <- data.frame(farm_id = character(0), crop_rotation = character(0))
	}

	# No yield column in this file; provides crop identity, field size, and inputs.
	d7 <- data.frame(
		field_id         = r7$farm_id,
		crop            = standardise_crop(r7$crop),
		field_size      = suppressWarnings(as.numeric(r7$area_ha)),
		OM_used         = trimws(r7$animal_manure_applied) == "y" |
		                  trimws(r7$other_organic_input) == "y",
		OM_type         = trimws(r7$other_organic_input_type),
		OM_type_manure  = trimws(r7$animal_manure_type),
		fertilizer_type = trimws(r7$mineral_fert_type),
		stringsAsFactors = FALSE
	)

	# field_size: area_ha is populated for all rows (no need for acre fallback here)
	d7$field_size[d7$field_size <= 0] <- NA

	# Combine OM types into a single field
	d7$OM_type <- ifelse(
		d7$OM_type != "" & d7$OM_type_manure != "",
		paste(d7$OM_type, d7$OM_type_manure, sep = ";"),
		ifelse(d7$OM_type != "", d7$OM_type, d7$OM_type_manure)
	)
	d7$OM_type_manure <- NULL
	d7$OM_type[d7$OM_type == ""] <- NA

	# Standardise OM type
	d7$OM_type[grepl("^none$|^no manure$|^no organic matter$|not applied",d7$OM_type)] <- "none"
	d7$OM_type <- tolower(d7$OM_type)
	d7$OM_type[grepl("crop residue|harvest residue", d7$OM_type)] <- "crop residue"
	d7$OM_type[grepl("domestic waste|household waste|waste manager", d7$OM_type)] <- "domestic waste"
	d7$OM_type[grepl("cajanus|fertilizing plant", d7$OM_type)] <- "green manure"
	d7$OM_type[grepl("goat|sheep|pig", d7$OM_type)] <- "animal dung"
	d7$OM_type[grepl("chicken|poultry|duck|guinea",d7$OM_type)] <- "poultry manure"

	# Standardise fertilizer type
	d7$fertilizer_type <- tolower(trimws(d7$fertilizer_type))
	d7$fertilizer_type[d7$fertilizer_type %in% c("", "n", "s")] <- NA
	# All variants are NPK 15-15-15 with or without urea
	d7$fertilizer_type[grepl("npk.*urea|urea.*npk", d7$fertilizer_type)] <- "NPK;urea"
	d7$fertilizer_type[grepl("^npk", d7$fertilizer_type) &
		!grepl("urea", d7$fertilizer_type)]                               <- "NPK"
	d7$fertilizer_type[grepl("^urea$", d7$fertilizer_type)]              <- "urea"

	# Identify the legume crop grown per farm from f_crop_production
	legume_crops <- c("cowpea", "soybean", "groundnut", "bean", "pigeonpea",
	                  "mucuna", "pea")
	farm_legume_crop <- d7[d7$crop %in% legume_crops & !is.na(d7$crop),
		c("field_id", "crop", "field_size")]
	# Use mean field size across legume plots per farm
	farm_legume_size <- aggregate(
		field_size ~ field_id,
		data = farm_legume_crop[!is.na(farm_legume_crop$field_size), ],
		FUN  = mean
	)
	# Dominant legume crop per farm (first if multiple)
	farm_legume_name <- farm_legume_crop[!duplicated(farm_legume_crop$field_id),
		c("field_id", "crop")]

	d8 <- data.frame(
		field_id        = r8$farm_id,
		yield = suppressWarnings(as.numeric(r8$total_prod_most_recent_season_kg)),
		amount_sold_kg = suppressWarnings(as.numeric(r8$amount_used_for_sale)),
		stringsAsFactors = FALSE
	)

	# Merge crop name and field size
	d8 <- merge(d8, farm_legume_name, by = "field_id", all.x = TRUE)
	d8 <- merge(d8, farm_legume_size, by = "field_id", all.x = TRUE)

	# Convert total kg to kg/ha
	d8$yield <- ifelse(
		!is.na(d8$yield) & !is.na(d8$field_size) & d8$field_size > 0,
		d8$yield / d8$field_size,
		NA_real_
	)

	# Out-of-bounds: 0 and 150000 are clearly invalid (boundary artefacts)
	d8$yield[d8$yield <= 0 | d8$yield > 100000] <- NA

	d8$field_size     <- NULL

	d9 <- data.frame(
		field_id  = r9$farm_id,
		previous_crop_residue_management = trimws(r9$haulms_usage),
		stringsAsFactors = FALSE
	)
	d9$previous_crop_residue_management[
		d9$previous_crop_residue_management == ""] <- NA

	# Standardise haulm usage to CAROB conventions
	d9$previous_crop_residue_management <- tolower(
		d9$previous_crop_residue_management)
	d9$previous_crop_residue_management[
		grepl("incorporated", d9$previous_crop_residue_management)] <- "incorporated"
	d9$previous_crop_residue_management[
		grepl("^burnt$|^burned", d9$previous_crop_residue_management)] <- "burned"
	d9$previous_crop_residue_management[
		grepl("burned in field|cutting.*burn|burn.*cutting",
		d9$previous_crop_residue_management)] <- "burned"
	d9$previous_crop_residue_management[
		grepl("composted", d9$previous_crop_residue_management)] <- "incorporated as compost"
	d9$previous_crop_residue_management[
		grepl("feed for own livestock", d9$previous_crop_residue_management)] <- "fed to livestock"
	d9$previous_crop_residue_management[
		grepl("left in the field", d9$previous_crop_residue_management)] <- "left on field"
	d9$previous_crop_residue_management[
		grepl("give away|gift|donation", d9$previous_crop_residue_management)] <- "given away"
	# Remaining categories (firewood, cutting, wood heater, soup) = removed
	d9$previous_crop_residue_management[
		grepl("firewood|cutting|wood heater|slip|soup",
		d9$previous_crop_residue_management)] <- "removed (other use)"

	# Keep one record per farm_id
	#d9 <- d9[!duplicated(d9$farm_id), ]

	unique_farms <- unique(c(r1$farm_id))
	loc <- data.frame(
		field_id   = unique_farms,
		country   = "Togo",
		location  = ifelse(grepl("^TGKL", unique_farms), "Kpalimé", "Plateaux region"),
		latitude  = ifelse(grepl("^TGKL", unique_farms), 6.897, 6.950),
		longitude = ifelse(grepl("^TGKL", unique_farms), 0.626, 0.750),
		elevation = ifelse(grepl("^TGKL", unique_farms), 310, 300),
		adm1      = "Plateaux",
		geo_from_source = FALSE,
		stringsAsFactors = FALSE
	)

	d <- d7

	# Merge legume yield into base (match on farm_id + crop)
	d <- merge(d, d8[, c("field_id", "crop", "yield", "amount_sold_kg")],
		by = c("field_id", "crop"), all = TRUE)

	d <- merge(d, d9, by = "field_id", all = TRUE)

	# Household-level merges
	d <- merge(d, d1,  by = "field_id", all = TRUE)
	d <- merge(d, d2, by = c("field_id","sex"), all = TRUE)
	d <- merge(d, d3,  by = "field_id", all = TRUE)
	d <- merge(d, d4, by = "field_id", all = TRUE)
	d <- merge(d, d5,  by = "field_id", all = TRUE)
	d <- merge(d, d6,  by = "field_id", all = TRUE)
	d <- merge(d, loc, by = "field_id", all = TRUE)

	d$on_farm        <- TRUE
	d$is_survey      <- TRUE
	d$irrigated      <- FALSE
	d$yield_part     <- NA_character_
	d$yield_isfresh  <- TRUE
	d$treatment      <- NA_character_
	d$planting_date  <- NA_character_
	d$harvest_date   <- NA_character_
	d$yield_moisture <- NA_real_
	d$year           <- 2012L
	d$N_fertilizer   <- NA_real_
	d$P_fertilizer   <- NA_real_
	d$K_fertilizer   <- NA_real_

	d$yield_part[d$crop %in% c("maize", "rice", "sorghum")]                     <- "grain"
	d$yield_part[d$crop %in% c("cowpea", "soybean", "groundnut", "bean",
		"pigeonpea")]                                                             <- "seed"
	d$yield_part[d$crop %in% c("cassava", "sweetpotato", "taro", "yam")]        <- "roots"
	d$yield_part[d$crop %in% c("tomato", "pepper", "eggplant", "okra",
		"cabbage", "lettuce")]                                                    <- "fruit"
	d$yield_part[d$crop %in% c("banana", "avocado", "orange")]                  <- "fruit"
	d$yield_part[d$crop %in% c("cocoa", "coffee")]                              <- "seed"
	d$yield_part[d$crop == "oilpalm"]                                           <- "fruit"
	d$yield_part[d$crop %in% c("mucuna")]                                       <- "seed"

	#fix crop names
	#other names cocoyam, eucalyptus, pigeonpea are part of the dataset
	d$crop[d$crop=="mucuna"] <- "velvet bean"
	d$crop[d$crop %in% c("yam(kratchi,katara )","yam: kratchi,katara and lotossou")] <- "yam"
	d$crop[d$crop %in% c("tomato and pepper","pepper and tomato","cocoa and orange","toothpick")] <- NA
	#fix OM_type
	d$OM_type[d$OM_type %in% c("crop residue","crops residues","domestic wates and crops residues",
	                           "domestid wates and crops residues","domesyic wastes and crops residues")] <- "farmyard manure"
	#fix crop_rotation
	#other names pigeonpea, plantain banana, plantian are part of the dataset
	d$crop_rotation <- gsub("\\bgoundnut\\b", "groundnut", d$crop_rotation)
	d$crop_rotation <- gsub("\\bgroudnut\\b", "groundnut", d$crop_rotation)
	d$crop_rotation <- gsub("\\bgroungnut\\b", "groundnut", d$crop_rotation)
	
	d$crop_rotation <- gsub("\\bmucuna\\b", "velvet bean", d$crop_rotation)
	d$crop_rotation <- gsub("\\bpeanut\\b", "rhizoma peanut", d$crop_rotation)
	
	invalid_terms <- c(
	  "aubergine",
	  "cassia",
	  "cocoyam",
	  "garding eggs",
	  "gboma"
	)
	
	for(term in invalid_terms) {
	  d$crop_rotation <- gsub(
	    paste0("(^|;)", term, "(;|$)"),
	    ";",
	    d$crop_rotation
	  )
	}
	
	d$crop_rotation <- gsub(";+", ";", d$crop_rotation)
	d$crop_rotation <- gsub("^;|;$", "", d$crop_rotation)
	d$crop_rotation[d$crop_rotation == ""] <- NA
	
	d$hh_size         <- as.integer(d$hh_size)
	d$hh_child_18     <- as.integer(d$hh_child_18)
	d$farm_labour_hired <- as.logical(d$farm_labour_hired)
	d$geo_from_source <- as.logical(d$geo_from_source)
	d$OM_used         <- as.logical(d$OM_used)
  
	d$trial_id <- 1
	
  char_cols <- sapply(d, is.character)
	d[char_cols] <- lapply(d[char_cols], trimws)
	
	d <- unique(d)

	carobiner::write_files(path, meta, d)
}
