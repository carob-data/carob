# R script for "carob"
# license: GPL (>=3)

## NOTES
# Cassava neem-oil and cutting-dip trials at the place named Dar es Salaam in the dictionary coverage field, 2023 and 2024.
# 2023-whitefly-virus.csv: Block is the block (1-4), Treatment is the product, Rep is the plant (1-12), Month is 1-6.
# 2024-whitefly-virus.csv: those roles are swapped. Block holds the product name, Treatment holds the block number 1-4, Rep is still the plant, Month is 1MAP-6MAP.
# 2023 product names are aligned to the yield file: control, TopNeem -> TopNeem Ultra, EthaNeem -> EthanolNeem.
# 2024 names are aligned to the yield file: Ethanol-Neem -> Ethanol Neem, Mandiplus+Ethanol-Neem -> Mandiplus & Ethanol Neem.
# Root_weight is grams per plant (dictionary). Not converted to kg/ha. CMD and CBSD scores are the dictionary 1-5 scale (1 = no symptoms).
# Keele University is an author affiliation. The draft organization "Yale" is not in the metadata.
# CKAN coverage_x/y are the same point as the other Tanzania whitefly dataset and are not used, because the dictionary places this trial at Dar es Salaam.

## ISSUES
# 2023 Month is 1-6 with no MAP label. It is stored as month 1-6, not as months after planting. 2024 Month is 1MAP-6MAP.
# No coordinates. Georeference Dar es Salaam, Tanzania, and confirm it is the field rather than the author's office.
# data_organization includes Keele University (author affiliation). That name is not in the organization vocabulary yet.
# No planting date, variety, fertilizer, irrigation, or product rates. Root weight stays in grams per plant.
# Rep in the whitefly files is the plant number, not a block replicate. The replicate is the block.
# write_files: missing latitude, longitude, yield, yield_moisture, N/P/K, irrigated, planting_date, harvest_date; carob_contributor empty.

## NOT PROCESSED
# All four data files: ID. Row number.
# data_dictionary.csv: documentation or a codebook, not an observation table.

## NEW VARIABLES
# plant_number_: plant 1-12 (Rep in the whitefly files, Plant in the yield files).
# months_after_planting_: 2024 whitefly file only (1-6).
# observation_month_: 2023 whitefly file, month index 1-6, not labeled MAP.
# stage_: adult or nymph.
# root_weight_g_: fresh root weight per plant, grams.
# root_number_: roots harvested per plant.
# trial_year_: 2023 or 2024.


carob_script <- function(path) {

"
Efficacy of botanical oils and cutting dipping in insecticides for control of cassava Bemisia tabaci whitefly

The project evaluated efficacy of crude formulation of neem oil, commercial neem oil in combination with cutting dipping in insecticides solutions for control of whiteflies in cassava.
"

	uri <- "doi:10.25502/34f2-0h91/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; Keele University",
		publication = NA,
		project = NA,
		design = "4 blocks; neem oils and cutting dips",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "pest_incidence;disease_severity",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.7
	)

	f1 <- ff[basename(ff) == "2023-whitefly-virus.csv"]
	f2 <- ff[basename(ff) == "2023-root-yield.csv"]
	f3 <- ff[basename(ff) == "2024-whitefly-virus.csv"]
	f4 <- ff[basename(ff) == "2024-root-yield.csv"]
	r1 <- read.csv(f1, check.names=FALSE)
	r2 <- read.csv(f2, check.names=FALSE)
	r3 <- read.csv(f3, check.names=FALSE)
	r4 <- read.csv(f4, check.names=FALSE)

	align23 <- function(x) {
		x <- trimws(x)
		x[tolower(x) == "control"] <- "control"
		x[x == "TopNeem"] <- "TopNeem Ultra"
		x[x == "EthaNeem"] <- "EthanolNeem"
		x
	}
	align24 <- function(x) {
		x <- trimws(x)
		x[tolower(x) == "control"] <- "control"
		x[x == "Ethanol-Neem"] <- "Ethanol Neem"
		x[x == "Mandiplus+Ethanol-Neem"] <- "Mandiplus & Ethanol Neem"
		x
	}

	r1$treatment <- align23(r1$Treatment)
	r1$block_id <- as.character(r1$Block)
	r1$plant_number_ <- as.integer(r1$Rep)
	r1$observation_month_ <- as.integer(r1$Month)
	r1$months_after_planting_ <- NA_integer_
	r2$treatment <- align23(r2$Treatment)
	r2$block_id <- as.character(r2$Block)
	r2$plant_number_ <- as.integer(r2$Plant)

	# 2024 whitefly file: product is in Block, block number is in Treatment.
	r3$treatment <- align24(r3$Block)
	r3$block_id <- as.character(r3$Treatment)
	r3$plant_number_ <- as.integer(r3$Rep)
	r3$observation_month_ <- NA_integer_
	r3$months_after_planting_ <- as.integer(sub("MAP", "", r3$Month))
	r4$treatment <- align24(r4$Treatment)
	r4$block_id <- as.character(r4$Block)
	r4$plant_number_ <- as.integer(r4$Plant)

	plants <- function(w, y, year) {
		id <- unique(data.frame(
			trial_year_ = year,
			block_id = w$block_id,
			treatment = w$treatment,
			plant_number_ = w$plant_number_
		))
		yy <- data.frame(
			block_id = y$block_id,
			treatment = y$treatment,
			plant_number_ = y$plant_number_,
			root_number_ = as.integer(y$Root_numbers),
			root_weight_g_ = as.numeric(y$Root_weight)
		)
		m <- merge(id, yy, by=c("block_id", "treatment", "plant_number_"), all=TRUE)
		m
	}
	d <- rbind(plants(r1, r2, "2023"), plants(r3, r4, "2024"))
	d <- d[order(d$trial_year_, d$block_id, d$treatment, d$plant_number_), ]
	d$record_id <- seq_len(nrow(d))
	d$crop <- "cassava"
	d$yield_part <- "roots"
	d$country <- "Tanzania"
	d$location <- "Dar es Salaam"
	d$geo_from_source <- FALSE
	d$trial_id <- paste(d$location, d$trial_year_, sep="_")
	d$insecticide_used <- d$treatment != "control"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$stress <- "pests;disease"
	d$key <- paste(d$trial_year_, d$block_id, d$treatment, d$plant_number_, sep="|")

	attach_id <- function(w, year) {
		w$record_id <- d$record_id[match(
			paste(year, w$block_id, w$treatment, w$plant_number_, sep="|"),
			d$key
		)]
		stopifnot(!any(is.na(w$record_id)))
		w
	}
	r1 <- attach_id(r1, "2023")
	r3 <- attach_id(r3, "2024")

	piece <- function(w) {
		base <- data.frame(
			record_id = w$record_id,
			months_after_planting_ = w$months_after_planting_,
			observation_month_ = w$observation_month_
		)
		a <- base
		a$pest_species <- "Bemisia tabaci"
		a$stage_ <- "adult"
		a$pest_incidence <- as.integer(w$Whitefly_number)
		a$disease <- NA_character_
		a$disease_severity <- NA_character_
		n <- base
		n$pest_species <- "Bemisia tabaci"
		n$stage_ <- "nymph"
		n$pest_incidence <- as.integer(w$Nymph_number)
		n$disease <- NA_character_
		n$disease_severity <- NA_character_
		cmd <- base
		cmd$pest_species <- NA_character_
		cmd$stage_ <- NA_character_
		cmd$pest_incidence <- NA_integer_
		cmd$disease <- "cassava mosaic"
		cmd$disease_severity <- as.character(w$CMD_severity)
		cbsd <- base
		cbsd$pest_species <- NA_character_
		cbsd$stage_ <- NA_character_
		cbsd$pest_incidence <- NA_integer_
		cbsd$disease <- "cassava brown streak"
		cbsd$disease_severity <- as.character(w$CBSD_severity)
		rbind(a, n, cmd, cbsd)
	}
	dd <- rbind(piece(r1), piece(r3))
	dd$severity_scale <- ifelse(is.na(dd$disease), NA_character_, "1-5")
	d$key <- NULL

	carobiner::write_files(path, meta, d, dd)
}
