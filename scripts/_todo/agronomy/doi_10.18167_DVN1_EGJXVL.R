# R script for "carob"
# license: GPL (>=3)

## NOTES
# Measured grain yields are Trials_yield_observation.csv in the calibration folder. The definition file gives the unit as t dry matter per ha, so yield is times 1000 and yield_moisture is 0. Model outputs and the virtual-experiment files are not used.
# The yield file is one quoted field per line. It is unquoted before reading. id_treatment +C+N is the fertilizer code C1N1, and the same for the other three combinations.
# fe_n, fe_p, and fe_k are kg/ha of the elements, summed over the applications on that row. Blank cells on a matched fertilizer row are treated as 0. Organic amount is om_amount_DW in t/ha, times 1000.
# Countries are the ones on the yield rows: Côte d'Ivoire, Kenya, and Zimbabwe. site_general_info spells Ivory Cost for the Gagnoa site. Coordinates and elevation are that site table.

## ISSUES
# Some yield cells are empty and are kept.
# write_files: missing irrigated, harvest_date. invalid terms: fertilizer_type: ammonium nitrate, CaNH4NO2, CaNH4NO3, PK fertiliser, super-phosphate. NA detected: yield.

## NOT PROCESSED
# Trials_models_yield_simulations.csv, Trials_models_SOC_simulations.csv, and Models_ouputs_simulation_scenarios.csv.
# Virtual_experiments_models_input_outputs.
# Soil-layer files, residue_initial, soil_preparation, and the DSSAT weather files.
# time_from_start on the yield file.


carob_script <- function(path) {

"
Data for 'Beyond climate change: the role of integrated soil fertility management for sustaining future maize yield in sub-Saharan Africa'

The dataset contains 15 soil crop model i) input and outputs on long-term experimental data on maize growth and soil organic carbon and ii) climate change and integrated soil fertility management simulation outputs at four sites in sub-Saharan Africa (2026)
"

	uri <- "doi:10.18167/DVN1/EGJXVL"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;OM_amount",
		response_vars = "yield",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 45,
		carob_effort = 0.6
	)

	read_semi <- function(f) {
		read.csv(f, sep=";", check.names=FALSE, na.strings=c("", "NA", "na"))
	}
	read_yield <- function(f) {
		raw <- readChar(f, file.info(f)$size, useBytes=TRUE)
		raw <- iconv(raw, from="latin1", to="UTF-8")
		lines <- strsplit(raw, "\r\n|\n")[[1]]
		lines <- lines[nzchar(lines)]
		lines <- sub("^\"", "", lines)
		lines <- sub("\"$", "", lines)
		lines <- gsub("\"\"", "\"", lines, fixed=TRUE)
		read.csv(text=paste(lines, collapse="\n"), check.names=FALSE, na.strings=c("", "NA", "na"))
	}
	sum3 <- function(a, b, c) {
		a <- as.numeric(a)
		b <- as.numeric(b)
		c <- as.numeric(c)
		a[is.na(a)] <- 0
		b[is.na(b)] <- 0
		c[is.na(c)] <- 0
		a + b + c
	}

	fy <- ff[basename(ff) == "Trials_yield_observation.csv"]
	y <- read_yield(fy)
	y$site_code <- substr(y$id_season, 1, 4)
	y$fert_code <- y$id_treatment
	y$fert_code <- gsub("+C", "C1", y$fert_code, fixed=TRUE)
	y$fert_code <- gsub("-C", "C0", y$fert_code, fixed=TRUE)
	y$fert_code <- gsub("+N", "N1", y$fert_code, fixed=TRUE)
	y$fert_code <- gsub("-N", "N0", y$fert_code, fixed=TRUE)

	fp <- ff[basename(ff) == "planting.csv"]
	plant <- read_semi(fp)
	fc <- ff[basename(ff) == "crop_sequence.csv"]
	seq <- read.csv(fc, check.names=FALSE)
	fs <- ff[basename(ff) == "site_general_info.csv"]
	site <- read.csv(fs, check.names=FALSE)
	ffert <- ff[basename(ff) == "mineral_fertilisation.csv"]
	fert <- read_semi(ffert)
	fo <- ff[basename(ff) == "organic_amendment.csv"]
	om <- read_semi(fo)

	fert$N_fertilizer <- sum3(fert$fe_n1, fert$fe_n2, fert$fe_n3)
	fert$P_fertilizer <- sum3(fert$fe_p1, fert$fe_p2, fert$fe_p3)
	fert$K_fertilizer <- sum3(fert$fe_k1, fert$fe_k2, fert$fe_k3)
	pick_type <- function(a, b, c) {
		mapply(function(x, y, z) {
			v <- unique(trimws(c(x, y, z)))
			v <- v[!is.na(v) & nzchar(v)]
			if (length(v) == 0) "none" else paste(v, collapse=";")
		}, as.character(a), as.character(b), as.character(c), USE.NAMES=FALSE)
	}
	fert$fertilizer_type <- pick_type(fert$fe_type1, fert$fe_type2, fert$fe_type3)
	fert <- fert[, c("id_season", "id_treatment", "N_fertilizer", "P_fertilizer", "K_fertilizer", "fertilizer_type")]
	names(fert)[names(fert) == "id_treatment"] <- "fert_code"

	om$OM_amount <- as.numeric(om$om_amount_DW) * 1000
	om$OM_amount[is.na(om$OM_amount)] <- 0
	om <- om[, c("id_season", "id_treatment", "OM_amount")]
	names(om)[names(om) == "id_treatment"] <- "fert_code"

	crop <- seq[, c("id_season", "crop")]
	crop$crop[crop$crop == "fallow"] <- "none"
	plant <- plant[, c("id_season", "pdate", "plant_population", "row_spacing")]

	d <- merge(y, crop, by="id_season", all.x=TRUE)
	d <- merge(d, plant, by="id_season", all.x=TRUE)
	d <- merge(d, fert, by=c("id_season", "fert_code"), all.x=TRUE)
	d <- merge(d, om, by=c("id_season", "fert_code"), all.x=TRUE)
	d <- merge(d, site[, c("id_site", "site_name", "latitude", "longitude", "elevation")], by.x="site_code", by.y="id_site", all.x=TRUE)

	country <- c(ICGA="Côte d'Ivoire", KEEM="Kenya", KEMA="Kenya", ZIMU="Zimbabwe")
	part <- c(maize="grain", soybean="seed", none="none")
	when <- as.Date(d$pdate, "%m/%d/%Y")

	d <- data.frame(
		trial_id = d$id_season,
		country = unname(country[d$site_code]),
		location = as.character(d$site_name),
		latitude = as.numeric(d$latitude),
		longitude = as.numeric(d$longitude),
		elevation = as.numeric(d$elevation),
		geo_from_source = TRUE,
		crop = d$crop,
		treatment = d$id_treatment,
		yield = as.numeric(d$Yield) * 1000,
		yield_part = unname(part[d$crop]),
		yield_moisture = 0,
		planting_date = as.character(when),
		N_fertilizer = d$N_fertilizer,
		P_fertilizer = d$P_fertilizer,
		K_fertilizer = d$K_fertilizer,
		fertilizer_type = d$fertilizer_type,
		OM_amount = d$OM_amount,
		OM_used = d$OM_amount > 0,
		OM_type = ifelse(d$OM_amount > 0, "compost", "none"),
		plant_density = as.numeric(d$plant_population) * 10000,
		row_spacing = as.numeric(d$row_spacing),
		on_farm = FALSE,
		is_survey = FALSE
	)
	d$OM_type[is.na(d$OM_amount)] <- NA_character_
	d$OM_used[is.na(d$OM_amount)] <- NA
	d$fertilizer_type[is.na(d$fertilizer_type)] <- NA_character_

	carobiner::write_files(path, meta, d)
}
