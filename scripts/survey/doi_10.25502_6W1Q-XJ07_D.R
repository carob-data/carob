# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {

"N2Africa is to contribute to increasing biological nitrogen fixation and
productivity of grain legumes among African smallholder farmers which will
contribute to enhancing soil fertility, improving household nutrition and
increasing income levels of smallholder farmers. As a vision of success,
N2Africa will build sustainable, long-term partnerships to enable African
smallholder farmers to benefit from symbiotic N2-fixation by grain legumes
through effective production technologies including inoculants and fertilizers
adapted to local settings. A strong national expertise in grain legume
production and N2-fixation research and development will be the legacy of the
project. This dataset covers the Tanzania baseline survey conducted in 2013
across four districts in the Northern Zone: Moshi Rural (Kilimanjaro),
Lushoto (Tanga), Arumeru (Arusha), and Hai (Kilimanjaro)."

	uri   <- "doi:10.25502/6W1Q-XJ07/D"
	group <- "survey"
  carobiner::draft(uri,path,group)

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group,
		major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "N2Africa",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		carob_completion = 100,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-06-24",
		carob_effort = NA,
		notes = paste(
			"No GPS latitude/longitude in source data; altitude only.",
			"District-level centroids used (WGS84); geo_from_source = FALSE.",
			"Yield = harvest_amount / field_size (kg/ha); field_size derived from",
			"size_ha where > 0, else size_m2 / 10000.",
			"Non-numeric harvest entries ('100(rete)', '50 cofee', '200kg', '4000kg'",
			"cleaned by numeric extraction.",
			"g1 weight_unit values '115' and '345' treated as kg (data entry errors).",
			"c_labour.csv contains Togo farm IDs and is not used for this dataset."),
		design = NA
	)

	f1 <- ff[basename(ff) == "a_d_general_1.csv"]
	r1 <- read.csv(f1)

	f2 <- ff[basename(ff) == "a_d_general_2.csv"]
	r2 <- read.csv(f2)

	f4 <- ff[basename(ff) == "a_d_general_4.csv"]
	r4 <- read.csv(f4)

	f5 <- ff[basename(ff) == "e_land_use_1.csv"]
	r5 <- read.csv(f5)

	f6 <- ff[basename(ff) == "e_land_use_2.csv"]
	r6 <- read.csv(f6)

	f7 <- ff[basename(ff) == "f_crop_production.csv"]
	r7 <- read.csv(f7)

	f8 <- ff[basename(ff) == "g_legume_utilisation_1.csv"]
	r8 <- read.csv(f8)

	f9 <- ff[basename(ff) == "g_legume_utilisation_2.csv"]
	r9 <- read.csv(f9)

    standardise_crop <- function(x) {
		x <- trimws(tolower(x))
		# Remove slash-separated combos for single-crop contexts
		x <- sub("/.*$", "", x)
		x <- trimws(x)

		x[grepl("^maize$", x)]                                         <- "maize"
		x[grepl("^beans?$|^common bean$", x)]                          <- "common bean"
		x[grepl("^cowpeas?$|^cow peas?$", x)]                          <- "cowpea"
		x[grepl("^pigeon peas?$|^pegion peas?$|^pegeon peas?$|^pegeonpea$|^pepeion pea$|^pigeonpea$", x)] <- "pigeon pea"
		x[grepl("^soybean$|^soya$", x)]                                 <- "soybean"
		x[grepl("^groundnuts?$|^ground nuts?$", x)]                     <- "groundnut"
		x[grepl("^bambara nut$|^bambaranati$|^bambara$", x)]            <- "bambara nut"
		x[grepl("^chickpea$", x)]                                       <- "chickpea"
		x[grepl("^green gram$|^greengram$|^green harm$", x)]            <- "mung bean"
		x[grepl("^sunflower$|^sunflowes$|^sanflower$", x)]              <- "sunflower"
		x[grepl("^sorghum$|^soughum$", x)]                              <- "sorghum"
		x[grepl("^rice$|^raice$", x)]                                   <- "rice"
		x[grepl("^millet$|^fingermillet$|^finger millet$", x)]         <- "millet"
		x[grepl("^cassava$", x)]                                        <- "cassava"
		x[grepl("^sweetpotato$|^sweet potato$|^sweet potatoes$", x)]   <- "sweetpotato"
		x[grepl("^irish potato$|^irish potatoes$|^irishpotatoes$|^potatoes?$|^pottato$", x)] <- "potato"
		x[grepl("^banana$", x)]                                         <- "banana"
		x[grepl("^coffee$", x)]                                         <- "coffee"
		x[grepl("^cotton$", x)]                                         <- "cotton"
		x[grepl("^tomato$|^tomatoes$|^tomatto$", x)]                    <- "tomato"
		x[grepl("^cabbage$", x)]                                        <- "cabbage"
		x[grepl("^onion$", x)]                                          <- "onion"
		x[grepl("^tea$", x)]                                            <- "tea"
		x[grepl("^yam$|^yarms$", x)]                                    <- "yam"
		x[grepl("^pepper$|^pili pili hoho$|^hoho$|^sweet pepper$|^chill$|^onex$", x)] <- "pepper"
		x[grepl("^vegetable$|^vegetables$|^small veges$|^vegetable/tomato$", x)] <- NA
		x[grepl("^various$|^fruits.*avoc|^trees$|^grasses$|^grazing$", x)] <- NA
		x[x %in% c("", "na")]                                           <- NA
		x
	}

	# Extract district (first segment) and ward (third segment)
	parse_site <- function(site) {
		parts <- strsplit(trimws(site), "/")[[1]]
		parts <- trimws(parts)
		list(
			district = if (length(parts) >= 1) parts[1] else NA_character_,
			ward     = if (length(parts) >= 3) parts[3] else NA_character_
		)
	}

	site_parsed <- lapply(r1$action_site, parse_site)
	district_raw <- sapply(site_parsed, `[[`, "district")
	ward_raw     <- sapply(site_parsed, `[[`, "ward")

	# Normalise district names
	district <- tolower(trimws(district_raw))
	district[grepl("moshi", district)]   <- "Moshi Rural"
	district[grepl("lushoto", district)] <- "Lushoto"
	district[grepl("arumeru", district)] <- "Arumeru"
	district[grepl("^hai$", district)]   <- "Hai"

	# District centroid coordinates (WGS84) — no per-household GPS available
	lat_map  <- c("Moshi Rural" = -3.350, "Lushoto" = -4.790,
	              "Arumeru"     = -3.370, "Hai"     = -3.570)
	lon_map  <- c("Moshi Rural" =  37.330, "Lushoto" =  38.290,
	              "Arumeru"     =  36.830, "Hai"     =  37.150)

	d1 <- data.frame(
		field_id         = r1$farm_id,
		country         = "Tanzania",
		adm1            = "Northern Zone",
		adm2            = district,
		location        = tolower(trimws(ward_raw)),
		adm3         = tolower(trimws(r1$village)),
		latitude        = lat_map[district],
		longitude       = lon_map[district],
		elevation       = suppressWarnings(as.numeric(
			ifelse(r1$gps_altitude_dec != "" & r1$gps_altitude_dec != "0",
			       r1$gps_altitude_dec, r1$gps_altitude))),
		year            = as.integer(r1$date_interview_yyyy),
		geo_from_source = FALSE,
		stringsAsFactors = FALSE
	)

 d2 <- data.frame(
		field_id        = r2$farm_id,
		sex            = trimws(r2$sex_farmer),
		age            = suppressWarnings(as.numeric(r2$age_farmer)),
		is_head        = trimws(r2$farmer_head_hh) == "Y",
		hh_size        = suppressWarnings(as.numeric(r2$total_number_hh)),
		hh_adult_women = suppressWarnings(
			as.numeric(r2$females_17_35) + as.numeric(r2$females_36_60)),
		hh_adult_men   = suppressWarnings(
			as.numeric(r2$males_17_35) + as.numeric(r2$males_36_60)),
		hh_child_18    = suppressWarnings(as.numeric(r2$children_0_16)),
		hh_elders      = suppressWarnings(
			as.numeric(r2$females_over_60) + as.numeric(r2$males_over_60)),
		education      = trimws(tolower(r2$highest_education_hh)),
		farm_labour_hired = trimws(r2$hired_labour_outside_hh) == "Y",
		stringsAsFactors = FALSE
	)

	d2$sex[d2$sex == "M"] <- "male"
	d2$sex[d2$sex == "F"] <- "female"
	d2$sex[d2$sex == ""]  <- NA
	d2$education[d2$education %in% c("", "none")] <- NA
	d2$hh_size[d2$hh_size > 50 | d2$hh_size < 0] <- NA

  d4 <- data.frame(
		field_id   = r4$farm_id,
		animal = trimws(tolower(r4$livestock_type)),
		heads = r4$livestock_number,
		stringsAsFactors = FALSE
	)

	d4$animal[d4$animal == ""]                 <- NA
	d4$animal[d4$animal == "cattle"]           <- "cattle"
	d4$animal[d4$animal == "goats"]            <- "goat"
	d4$animal[d4$animal == "sheep"]            <- "sheep"
	d4$animal[d4$animal == "pigs"]             <- "pig"
	d4$animal[d4$animal == "chicken"]          <- "chicken"
	d4$animal[grepl("^ducks?$", d4$animal)]   <- "duck"
	d4$animal[d4$animal == "rabbits"]          <- "rabbit"
	d4$animal[d4$animal == "donkey"]           <- "donkey"

	d5 <- data.frame(
		field_id  = r5$farm_id,
		farmland = suppressWarnings(as.numeric(r5$total_area_ha)),
		stringsAsFactors = FALSE
	)
	d5$farmland[d5$farmland <= 0] <- NA

	split_season_crops <- function(x) {
		# Split on / and standardise each part
		parts <- unlist(strsplit(x, "/"))
		parts <- trimws(parts)
		parts <- standardise_crop(parts)
		parts[!is.na(parts) & parts != ""]
	}

	r6_long <- do.call(rbind, lapply(seq_len(nrow(r6)), function(i) {
		row <- r6[i, ]
		all_crops <- unique(c(
			split_season_crops(row$season_2012a),
			split_season_crops(row$season_2012b),
			split_season_crops(row$season_2013a),
			split_season_crops(row$season_2013b)
		))
		all_crops <- all_crops[!is.na(all_crops) & all_crops != ""]
		if (length(all_crops) == 0) return(NULL)
		data.frame(field_id = row$farm_id, crop = all_crops,
			stringsAsFactors = FALSE)
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

	# Clean crop_rotation: fix typos/aliases and remove non-crop noise terms.
	# crop_rotation is a ";" separated string; each element is checked individually.
	# Strategy: split each rotation into tokens, recode or drop each token,
	# then rejoin unique valid terms.
	clean_rotation <- function(rot) {
	  if (is.na(rot) || rot == "") return(NA_character_)
	  tokens <- trimws(unlist(strsplit(rot, ";")))
	  tokens <- tolower(tokens)
	  
	  # Fixable typos and aliases → map to terminag names
	  tokens[tokens == "maze"]                         <- "maize"
	  tokens[tokens == "soghum"]                       <- "sorghum"
	  tokens[tokens == "sseet potato"]                 <- "sweetpotato"
	  tokens[tokens == "yams"]                         <- "yam"
	  tokens[tokens == "onions"]                       <- "onion"
	  tokens[tokens == "greengrams"]                   <- "mung bean"
	  tokens[tokens == "cabbaage"]                     <- "cabbage"
	  tokens[tokens == "irish potato(red type)"]       <- "potato"
	  # Compound crop entries not yet split — recode to component crops
	  tokens[tokens == "maize and beans"]              <- "maize;common bean"
	  tokens[tokens == "maize beans"]                  <- "maize;common bean"
	  tokens[tokens == "beans and sunflower"]          <- "common bean;sunflower"
	  # Local variety name — identifiable as maize
	  tokens[tokens == "lyamungo maize"]               <- "maize"
	  
	  # Non-crop noise, season labels, and unclassifiable terms → remove (set NA)
	  remove_terms <- c(
	    "fallow", "grazing area", "graizing", "grazing",
	    "main", "rains", "short",                   # season label fragments
	    "legron pea",                                # unresolvable typo
	    "majani ya malisho",                         # Swahili for "animal fodder grass"
	    "trees+grasses for animals+potatoes",        # composite non-crop description
	    "vergetables", "fruits",                     # non-specific, not in terminag
	    "v"                                          # single-letter data entry error
	  )
	  tokens[tokens %in% remove_terms] <- NA
	  
	  # Re-split any tokens that were recoded to compound strings
	  tokens <- trimws(unlist(strsplit(paste(tokens, collapse = ";"), ";")))
	  tokens <- tokens[!is.na(tokens) & tokens != ""]
	  tokens <- unique(tokens)
	  
	  if (length(tokens) == 0) return(NA_character_)
	  paste(tokens, collapse = ";")
	}
	
	d6$crop_rotation <- sapply(d6$crop_rotation, clean_rotation)
	
	
	get_field_size <- function(ha, m2) {
		ha  <- suppressWarnings(as.numeric(ha))
		m2  <- suppressWarnings(as.numeric(m2))
		out <- ifelse(!is.na(ha) & ha > 0, ha,
		       ifelse(!is.na(m2) & m2 > 0, m2 / 10000, NA_real_))
		out
	}

	parse_harvest <- function(x) {
		x <- trimws(x)
		# Strip trailing units and text, extract leading number
		x <- gsub("kg$|kgs$|\\(rete\\)|cofee.*$", "", x, ignore.case = TRUE)
		x <- trimws(x)
		suppressWarnings(as.numeric(x))
	}

	d7_list <- lapply(seq_len(nrow(r7)), function(i) {
		row <- r7[i, ]
		farm  <- row$farm_id
		field <- as.character(row$field)
		size  <- get_field_size(row$size_ha, row$size_m2)

		# Primary crop = crop_1; remaining crops = intercrop
		crops <- character(0)
		for (j in 1:4) {
			cn <- trimws(row[[paste0("crop_", j)]])
			if (!is.na(cn) && cn != "") crops <- c(crops, cn)
		}
		if (length(crops) == 0) return(NULL)

		# Handle "MAIZE/BEANS" entries in crop_1 as intercrop specification
		primary_raw <- crops[1]
		if (grepl("/", primary_raw)) {
			parts   <- trimws(strsplit(primary_raw, "/")[[1]])
			primary <- standardise_crop(parts[1])
			others  <- standardise_crop(parts[-1])
		} else {
			primary <- standardise_crop(primary_raw)
			others  <- if (length(crops) > 1) standardise_crop(crops[-1]) else character(0)
		}

		intercrop <- if (length(others) > 0) {
			others <- others[!is.na(others)]
			if (length(others) > 0) paste(others, collapse = ";") else NA_character_
		} else NA_character_

		# Yield in kg/ha
		harvest_kg <- parse_harvest(row$harvest_amount)
		yield_ha   <- if (!is.na(harvest_kg) && !is.na(size) && size > 0) {
			harvest_kg / size
		} else NA_real_

		data.frame(
			farm_id         = farm,
			field_id        = field,
			crop            = primary,
			intercrops       = intercrop,
			field_size      = size,
			yield           = yield_ha,
			field_distance  = suppressWarnings(as.numeric(row$walking_distance_min)),
			fertilizer_type = trimws(row$min_fert_type),
			OM_used         = trimws(row$organic_input_applied) == "Y",
			inoculated      = trimws(row$inoculant_applied) == "Y",
			stringsAsFactors = FALSE
		)
	})

	d7 <- do.call(rbind, d7_list)

	# Out-of-bounds yield
	d7$yield[d7$yield <= 0 | d7$yield > 150000] <- NA

	# Standardise fertilizer type
	d7$fertilizer_type <- trimws(tolower(d7$fertilizer_type))
	d7$fertilizer_type[d7$fertilizer_type %in% c("", "none", "not used", "n")] <- NA

	d7$fertilizer_type[grepl("minjingu", d7$fertilizer_type) &
		!grepl("urea|npk|dap", d7$fertilizer_type)]              <- "MOHP"         #Minjingu known as Minjingu Organic Hyper Phosphate (MOHP).
	d7$fertilizer_type[grepl("^dap$", d7$fertilizer_type)]      <- "DAP"
	d7$fertilizer_type[grepl("^urea$", d7$fertilizer_type)]     <- "urea"
	d7$fertilizer_type[grepl("^can$", d7$fertilizer_type)]      <- "CAN"
	d7$fertilizer_type[grepl("^npk$|^mpk$", d7$fertilizer_type)] <- "NPK"
	d7$fertilizer_type[grepl("urea.*dap|dap.*urea", d7$fertilizer_type)]  <- "DAP;urea"
	d7$fertilizer_type[grepl("urea.*minjingu|minjingu.*urea", d7$fertilizer_type)] <- "MOHP;urea"
	d7$fertilizer_type[grepl("dap.*minjingu|minjingu.*dap", d7$fertilizer_type)]   <- "DAP;MOHP"
	d7$fertilizer_type[grepl("npk.*urea|urea.*npk", d7$fertilizer_type)]   <- "NPK;urea"
	d7$fertilizer_type[grepl("npk.*dap|dap.*npk", d7$fertilizer_type)]     <- "NPK;DAP"
	d7$fertilizer_type[grepl("urea.*can|can.*urea", d7$fertilizer_type)]   <- "CAN;urea"
	d7$fertilizer_type[grepl("urea.*boost|boost.*urea|busta", d7$fertilizer_type)] <- "urea"
	d7$fertilizer_type[grepl("so4|dap.*so4|so4.*dap", d7$fertilizer_type)] <- "DAP;SO4"
	# Complex multi-product entries: keep as compound with ";"
	d7$fertilizer_type[grepl("minjingu.*urea.*npk|urea.*npk.*dap", d7$fertilizer_type)] <-
		"MOHP;Urea;NPK;DAP"
	d7$fertilizer_type[grepl("npk.*organic", d7$fertilizer_type)] <- "NPK"

## --- d8: legume yield (g_legume_utilisation_1) --------------------------------
	# All production values are numeric; weight units are kg variants
	# weight_unit values '115' and '345' are data entry errors — treat as kg

	d8 <- data.frame(
		field_id        = r8$farm_id,
		crop           = trimws(r8$crop),
		yield = suppressWarnings(as.numeric(r8$total_production_farm)),
		amount_sold_kg = suppressWarnings(as.numeric(r8$amount_for_sale)),
		stringsAsFactors = FALSE
	)

	# Standardise legume crop names to terminag vocabulary
	d8$crop[d8$crop == "Common bean"]  <- "common bean"
	d8$crop[d8$crop == "Cowpea"]       <- "cowpea"
	d8$crop[d8$crop == "Pigeon pea"]   <- "pigeon pea"
	d8$crop[d8$crop == "Soybean"]      <- "soybean"
	d8$crop[d8$crop == "Groundnut"]    <- "groundnut"
	d8$crop[d8$crop == "Bambara nut"]  <- "bambara nut"
	d8$crop[d8$crop == "Chickpea"]     <- "chickpea"

	d8$yield[d8$yield <= 0] <- NA

	# Convert total kg to kg/ha using field size from d7 (mean legume plot size per farm)
	legume_crops <- c("common bean", "cowpea", "pigeon pea", "soybean",
		"groundnut", "bambara nut", "chickpea", "mung bean")
	field_sizes <- d7[d7$crop %in% legume_crops & !is.na(d7$field_size),
		c("field_id", "crop", "field_size")]
	field_sizes_agg <- aggregate(
		field_size ~ field_id + crop,
		data = field_sizes,
		FUN  = mean
	)

	d8 <- merge(d8, field_sizes_agg, by = c("field_id", "crop"), all.x = TRUE)
	d8$yield <- ifelse(
		!is.na(d8$yield) & !is.na(d8$field_size) & d8$field_size > 0,
		d8$yield / d8$field_size,
		NA_real_
	)
	#d8$yield_g1_total <- NULL
	d8$field_size     <- NULL
	#d8$yield[d8$yield > 150000] <- NA

d9 <- data.frame(
		field_id  = r9$farm_id,
		crop     = trimws(r9$legume_type),
		previous_crop_residue_management = trimws(r9$haulm_use),
		stringsAsFactors = FALSE
	)

	d9$crop[d9$crop == "Common bean"] <- "common bean"
	d9$crop[d9$crop == "Pigeon pea"]  <- "pigeon pea"
	d9$crop[d9$crop == "Soybean"]     <- "soybean"
	d9$crop[d9$crop == "Groundnut"]   <- "groundnut"
	d9$crop[d9$crop == "Bambara nut"] <- "bambara nut"

	d9$previous_crop_residue_management[
		d9$previous_crop_residue_management == ""] <- NA
	mgmt <- tolower(d9$previous_crop_residue_management)
	d9$previous_crop_residue_management[grepl(
		"feed|feeds|animal|livestock|zero grazing", mgmt)]       <- "fed to livestock"
	d9$previous_crop_residue_management[grepl(
		"incorporat|incooperat|incoperat|incooprat", mgmt)]      <- "incorporated"
	d9$previous_crop_residue_management[grepl(
		"burn|burned", mgmt)]                                    <- "burned"
	d9$previous_crop_residue_management[grepl(
		"sale|sell", mgmt)]                                      <- "sold"
	d9$previous_crop_residue_management[grepl(
		"donate|give|friend|neighbour", mgmt)]                   <- "given away"
	# Dual use: fed to livestock AND incorporated — use dominant/first mentioned
	d9$previous_crop_residue_management[grepl(
		"feed.*incorporat|incorporat.*feed", mgmt)]              <- "fed to livestock"
	#d9 <- d9[!duplicated(paste(d9$farm_id, d9$crop)), ]
  
	d <- d7

	# Merge g1 legume yield — prefer g1 (cleaner) over f_crop yield for legumes
	d <- merge(d, d8[, c("field_id", "crop", "yield", "amount_sold_kg")],
		by = c("field_id", "crop","yield"), all = TRUE)

	# Merge residue management
	d <- merge(d, d9, by = c("field_id", "crop"), all = TRUE)

	# Household-level merges
	d <- merge(d, d1,  by = "field_id", all = TRUE)
	d <- merge(d, d2,  by = "field_id", all = TRUE)
	d <- merge(d, d4, by = "field_id", all = TRUE)
	d <- merge(d, d5,  by = "field_id", all = TRUE)
	d <- merge(d, d6,  by = "field_id", all = TRUE)

	d$trial_id <- as.character(as.integer(as.factor(1)))
	d$on_farm        <- TRUE
	d$is_survey      <- TRUE
	d$irrigated      <- FALSE
	d$yield_part     <- NA_character_
	d$yield_isfresh  <- TRUE
	d$treatment      <- NA_character_
	d$planting_date  <- NA_character_
	d$harvest_date   <- NA_character_
	d$yield_moisture <- NA_real_
	d$N_fertilizer   <- NA_real_
	d$P_fertilizer   <- NA_real_
	d$K_fertilizer   <- NA_real_

	d$yield_part[d$crop %in% c("maize", "sorghum", "rice", "millet")]    <- "grain"
	d$yield_part[d$crop %in% c("common bean", "cowpea", "pigeon pea",
		"soybean", "groundnut", "bambara nut", "chickpea", "mung bean")]  <- "seed"
	d$yield_part[d$crop %in% c("cassava", "sweetpotato", "potato", "yam")] <- "roots"
	d$yield_part[d$crop %in% c("tomato", "pepper", "cabbage", "onion")]  <- "fruit"
	d$yield_part[d$crop == "sunflower"]                                   <- "seed"
	d$yield_part[d$crop == "cotton"]                                      <- "seed"
	d$yield_part[d$crop %in% c("banana")]                                 <- "fruit"
	d$yield_part[d$crop %in% c("coffee", "tea")]                          <- "seed"

	d$field_id          <- as.character(d$field_id)
	d$hh_size           <- as.integer(d$hh_size)
	d$hh_adult_women    <- as.integer(d$hh_adult_women)
	d$hh_adult_men      <- as.integer(d$hh_adult_men)
	d$hh_child_18       <- as.integer(d$hh_child_18)
	d$hh_elders         <- as.integer(d$hh_elders)
	d$inoculated        <- as.logical(d$inoculated)
	d$OM_used           <- as.logical(d$OM_used)
	d$is_head           <- as.logical(d$is_head)
	d$farm_labour_hired <- as.logical(d$farm_labour_hired)
	d$geo_from_source   <- as.logical(d$geo_from_source)

	#fix fertilizer_type
	d$fertilizer_type[d$fertilizer_type %in% c("booster","SO4")] <- NA
	d$fertilizer_type[d$fertilizer_type == "npk/minjungu"] <- "NPK;MOHP"
	d$fertilizer_type[d$fertilizer_type == "urea/boosier"] <- "urea"
	d$fertilizer_type[d$fertilizer_type == "DAP;SO4"] <- "DAP"
	
	#fix intercrops
	d$intercrops[d$intercrops=="fruits(eg ovacado,sandarose0"] <- "none"
	d$intercrops[d$intercrops=="yams"] <- "yam"
	d$intercrops[d$intercrops=="coffee;yams;maize"] <- "coffee;yam;maize"
	d$intercrops[d$intercrops=="bambara nut"] <- "bambara groundnut"
	d$crop[d$crop=="bambara nut"] <- "bambara groundnut"
	d$farm_id <- NULL
	
	char_cols <- sapply(d, is.character)
	d[char_cols] <- lapply(d[char_cols], trimws)
  
	d <- unique(d)
	
	carobiner::write_files(path, meta, d)
}
