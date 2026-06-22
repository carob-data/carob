# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {

"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."


# notes = "No per-household GPS coordinates in source data; dataset centroid (lat 6.42806, lon -9.42950) used for all records. Yield field in f_crop_production is a free-text string requiring parsing; non-kg units set to NA.",


	uri   <- "doi:10.25502/4NQ1-KV83/D"
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
		carob_date = "2026-06-22",
		carob_effort = NA,
		design = NA
	)

	f1  <- ff[basename(ff) == "a_demographic.csv"]
	r1  <- read.csv(f1)
	f2  <- ff[basename(ff) == "c_labour.csv"]
	r2  <- read.csv(f2)
	f3  <- ff[basename(ff) == "d_livestock_ownership.csv"]
	r3  <- read.csv(f3)
	f4  <- ff[basename(ff) == "e4_e6_land_use.csv"]
	r4  <- read.csv(f4)
	f5  <- ff[basename(ff) == "f_crop_production.csv"]
	r5  <- read.csv(f5)
	f6  <- ff[basename(ff) == "g1_legume_utilisation.csv"]
	r6  <- read.csv(f6)
	f7  <- ff[basename(ff) == "g2_legume_utilisation.csv"]
	r7  <- read.csv(f7)
	f8  <- ff[basename(ff) == "h1_access_to_markets.csv"]
	r8  <- read.csv(f8)
	f9  <- ff[basename(ff) == "h2_h3_access_to_markets.csv"]
	r9  <- read.csv(f9)

	d1 <- data.frame(
		hhid      = r1$id,
		field_id  = r1$farm_id,
		sex       = r1$gender,
		age       = as.numeric(r1$age),
		hh_size   = as.numeric(r1$number_adults_children),
		education = r1$highest_education_level
	)

	d1$sex[d1$sex == ""]  <- NA
	d1$education <- trimws(tolower(d1$education))
	d1$education[d1$education == ""] <- NA

	d2 <- data.frame(
		hhid                  = r2$id,
		field_id              = r2$farm_id,
		farm_labour_hired     = r2$labour_hired == "y"
	)

	d3 <- data.frame(
		hhid     = r3$id,
		field_id = r3$farm_id,
		animal = gsub("s$", "", trimws(tolower(r3$livestock)))
	)
	d3$animal[d3$animal == ""] <- NA
	
	#aggfun <- function(x) paste(unique(na.omit(x)), collapse = ";")
	aggfun <- function(x) paste(unique(na.omit(x[x != ""])), collapse = ";")
	d3a <- aggregate(animal ~ hhid + field_id, d3, aggfun)
	d3a$animal[d3a$animal == ""] <- NA

#	d4 <- data.frame(
	#	hhid       = r4$id,
	#	field_id   = r4$farm_id,
		# land with plantation trees in ha — kept as contextual land info
		#land_owned_with_plantation_trees_ha = as.numeric(r4$land_owned_with_plantation_trees_ha)
	#)

	d5 <- data.frame(
		hhid             = r5$id,
		field_id         = r5$farm_id,
		crop             = r5$crop,
		field_size       = as.numeric(r5$area_ha),
		# sole_crop_or_intercrop: S = sole, I = intercrop
		is_intercropped  = r5$sole_crop_or_intercrop == "I",
		OM_used          = r5$animal_manure_applied == "y" | r5$other_organic_input == "y",
		OM_type          = r5$other_organic_input_type
	)

	# Mineral fertiliser type
	d5$fertilizer_type <- r5$mineral_fert_type
	d5$fertilizer_type[!is.na(r5$mineral_fert_applied) & r5$mineral_fert_applied == "n"] <- NA

	parse_yield_kg <- function(s) {
		s   <- trimws(tolower(s))
		out <- rep(NA_real_, length(s))
		# plain kg: e.g. "600kg", "1500 kg"
		m_kg   <- regmatches(s, regexpr("^([0-9]+\\.?[0-9]*)\\s*kg$", s, perl = TRUE))
		idx_kg <- grep("^([0-9]+\\.?[0-9]*)\\s*kg$", s, perl = TRUE)
		out[idx_kg] <- as.numeric(sub("kg", "", m_kg))
		# typos close to kg: "300kh", "100kf", "75kf", "350kgw"
		idx_ktypo <- grep("^([0-9]+)k[gfhw]", s, perl = TRUE)
		out[idx_ktypo] <- as.numeric(sub("k[gfhw].*$", "", s[idx_ktypo]))
		# tonnes: "1 ton", "1.5 ton", ".5ton"
		idx_ton <- grep("^\\.?[0-9]+\\.?[0-9]*\\s*tons?$", s, perl = TRUE)
		out[idx_ton] <- as.numeric(sub("tons?", "", gsub(" ", "", s[idx_ton]))) * 1000
		out
	}

	d5$yield <- parse_yield_kg(r5$yield)

	d6 <- data.frame(
		hhid     = r6$id,
		field_id = r6$farm_id,
		crop     = r6$legume_type,
		yield    = as.numeric(r6$total_prod_most_recent_season_kg)
	)
	d6$crop_amount_sold <- r6$amount_used_for_sale

	d7 <- data.frame(
		hhid     = r7$id,
		field_id = r7$farm_id,
		crop     = r7$legume_type
	)
	
	pcm <- trimws(r7$haulms_usage)
	pcm[pcm == ""] <- NA

	# Standardise haulm usage descriptions
	pcm <- tolower(pcm)
	pcm[grepl("nothing|throw|thrown|dump|wasted|unused|no use|nothing", pcm)] <- "discarded"
	pcm[grepl("composted|compost", pcm)] <- "incorporated as compost"
	pcm[grepl("incorporated in the soil", pcm)] <- "incorporated"
	pcm[grepl("feed|fodder|livestock", pcm)] <- "fed to livestock"
	pcm[grepl("burnt|burned", pcm)] <- "burned"
	pcm[grepl("sold", pcm)] <- "sold"
	pcm[grepl("left out on field", pcm)] <- "left on field"
	# Local soda / soap — biomass used for traditional ash extraction; treated as removal
	pcm[grepl("soda|soap", pcm)] <- "removed (other use)"
	pcm[grepl("give away", pcm)] <- "given away"

	d7$previous_crop_residue_management <- pcm

	d8 <- data.frame(
		hhid            = r8$id,
		field_id        = r8$farm_id,
		market_access   = trimws(r8$market),
		market_type     = trimws(toupper(r8$how_f_mc_c))
	)

	# Normalise market_type codes: MC = market centre, C = cooperative, F = farmer group
	d8$market_type <- trimws(toupper(d8$market_type))
	d8$market_type[d8$market_type %in% c("MC", " MC")]  <- "market centre"
	d8$market_type[d8$market_type == "C"]               <- "cooperative"
	d8$market_type[d8$market_type == "F"]               <- "farmer group"
	d8$market_type[d8$market_type == "V"]               <- "village market"
	# Non-standard entries
	d8$market_type[d8$market_type %in% c("WHEELBARROW", "BICYCLE")] <- "unknown"
	d8$market_type[d8$market_type == ""] <- NA

	# Parse market time to minutes (for market_distance proxy)
	parse_minutes <- function(x) {
		x <- trimws(tolower(x))
		out <- rep(NA_real_, length(x))
		# e.g. "5mins", "25min", "45mina", "45min"
		m_min <- grep("^([0-9]+\\.?[0-9]*)\\s*min", x, perl = TRUE)
		out[m_min] <- as.numeric(sub("\\s*min.*$", "", x[m_min]))
		# e.g. "1.5hrs", "4hrs", "1hr 30mins", "2.5hrs"
		m_hr <- grep("^([0-9]+\\.?[0-9]*)\\s*hr", x, perl = TRUE)
		out[m_hr] <- as.numeric(sub("\\s*hr.*$", "", x[m_hr])) * 60
		# "1hr 30mins" — add the minutes part
		m_hrmin <- grep("^([0-9]+)\\s*hr.*([0-9]+)\\s*min", x, perl = TRUE)
		hrs  <- as.numeric(sub("hr.*$", "", x[m_hrmin]))
		mins <- as.numeric(sub("^.*hr[s]?\\s*", "", sub("min.*$", "", x[m_hrmin])))
		out[m_hrmin] <- hrs * 60 + mins
		out
	}

	d8$market_timeto <- parse_minutes(r8$time)
	d8$market_costs    <- as.numeric(r8$costs)

	d8a <- aggregate(cbind(market_timeto, market_costs) ~ hhid + field_id, d8, mean, na.rm = TRUE)
	tmp_mkt <- aggregate(market_type ~ hhid + field_id, d8, FUN = aggfun)
	d8a <- merge(d8a, tmp_mkt, by = c("hhid", "field_id"), all = TRUE)

	d9 <- data.frame(
		hhid              = r9$id,
		field_id          = r9$farm_id,
		sell_at_farm_gate = trimws(r9$sell_at_farm_gate) == "y",
		sell_at_home      = r9$sell_at_home == "y"
	)

	# Start from crop production (one row per crop per household)
	d <- d5

	standardise_crop <- function(crop) {
		x <- trimws(tolower(crop))
### this does not do anything!
### x[grepl("^cassava$", x)]                                   <- "cassava"
		x[x=="corn"]                    <- "maize"
		x[grepl("country bean|big bean|brown bean|long bean", x)] <- "common bean"
		x[grepl("mung beans", x)]       <- "mung bean"
		x[grepl("^pepper$|^garden egg$|^bitterball$", x)]         <- "pepper"
		x[x=="oranges"]                 <- "orange"
		x[grepl("eddo?", x)]            <- "eddo"
		x[grepl("potato?", x)]          <- "sweetpotato" ## assumption because this is Liberia
		x[x=="sugar cane"]              <- "sugarcane"
		x[x=="peppet"]                  <- "pepper"
		x[x=="bitterbal"]               <- "bitterball"
		x[x=="tomatoes"]                <- "tomato"
		x[x=="other non-legume crop (specify)"] <- NA
		x
	}

  ## need to keep upload /lowland distinction. It is lke a different crop
  d$crop[d$crop %in% c("rice - lowland","rice - upland")] <- "rice"
  
	d$crop  <- standardise_crop(d$crop)
	d6$crop <- standardise_crop(d6$crop)
	d7$crop <- standardise_crop(d7$crop)

	# Merge legume production (g1) — crop-level merge
	d <- merge(d, d6[, c("hhid", "field_id", "crop", "yield", "crop_amount_sold")],
		by = c("hhid", "field_id", "crop"), all = TRUE)

	# Resolve duplicate yield columns: prefer g1 (clean numeric) for legumes
	d$yield <- ifelse(!is.na(d$yield.y), d$yield.y, d$yield.x)
	d$yield.x <- NULL
	d$yield.y <- NULL


	# Merge haulm management (g2)
	d <- merge(d, d7[, c("hhid", "field_id", "crop", "previous_crop_residue_management")],
		by = c("hhid", "field_id", "crop"), all = TRUE)

	# Household-level merges
	d <- merge(d, d1, by = c("hhid", "field_id"), all = TRUE)
	d <- merge(d, d2, by = c("hhid", "field_id"), all = TRUE)
	d <- merge(d, d3a, by = c("hhid", "field_id"), all = TRUE)
	d <- merge(d, d8a, by = c("hhid", "field_id"), all = TRUE)
	d <- merge(d, d9, by = c("hhid", "field_id"), all = TRUE)


	d$country   <- "Liberia"
	d$location  <- NA_character_
	# No per-household GPS; dataset centroid from IITA portal metadata
	d$latitude  <- 6.42806
	d$longitude <- -9.42950
	d$elevation <- NA_real_
	d$on_farm    <- TRUE
	d$is_survey  <- TRUE
	d$irrigated  <- FALSE
	d$geo_from_source <- FALSE

	d$trial_id   <- "1" ## d$trial_id <- as.character(as.integer(as.factor(1))) ????
	d$treatment  <- d$planting_date <- d$harvest_date  <- NA_character_
	d$yield_moisture<- NA_real_

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- d$S_fertilizer <- d$lime <- NA_real_

	d$fertilizer_type <- trimws(tolower(d$fertilizer_type))
	d$fertilizer_type[d$fertilizer_type %in% c("", "no", "n/a")] <- NA

	d$fertilizer_type[grepl("^npk|nkp", d$fertilizer_type)] <- "NPK"
	d$fertilizer_type[grepl("nkp",  d$fertilizer_type)] <- "NPK"
	#d$fertilizer_type[grepl("npk 15/15|15/15", d$fertilizer_type)] <- "NPK"
	# "grass" entered as fertilizer type — reassign to OM
	d$OM_type[grepl("^grass$", d$fertilizer_type) & is.na(d$OM_type)] <- "foliage"
	d$fertilizer_type[grepl("^grass$", d$fertilizer_type)] <- NA

	d$OM_type <- trimws(tolower(d$OM_type))
	d$OM_type[d$OM_type %in% c("", "no", "n/a", "0")] <- NA
	d$OM_type[grepl("compost",    d$OM_type)] <- "compost"
	d$OM_type[grepl("chicken|poultry|chiken manure", d$OM_type)] <- "poultry manure"
	d$OM_type[grepl("^grass$|rotten grass|bush trash", d$OM_type)] <- "foliage"

	d$yield_part <- "grain"
	d$yield_part[d$crop %in% c("cowpea", "soybean", "groundnut", "mung bean", "bean")] <- "seed"
	d$yield_part[d$crop %in% c("cassava", "sweetpotato")]   <- "roots"
	d$yield_part[d$crop %in% c("tomato", "pepper", "okra", "pumpkin", "cucumber", "garden egg")] <- "fruit"
	d$yield_part[d$crop %in% c("banana", "orange", "pineapple")]   <- "fruit"
	d$yield_part[d$crop == "sugarcane"]                            <- "stems"
	d$yield_part[d$crop %in% c("coffee", "cocoa")]                 <- "seed"

	d$hhid                    <- as.character(d$hhid)
	d$field_id                <- as.character(d$field_id)
	d$sex[d$sex == "M"]       <- "male"
	d$sex[d$sex == "F"]       <- "female"
	d$hh_size                 <- as.integer(d$hh_size)

	char_cols <- sapply(d, is.character)
	d[char_cols] <- lapply(d[char_cols], trimws)
	
#	d$market_costs <- d$sell_at_farm_gate <- d$sell_at_home <- NULL

	d$yield_isfresh <- TRUE
    
	carobiner::write_files(path, meta, d)
}
