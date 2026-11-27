# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Dictionary coding mismatch in 3.1_dictionary.xls: for 2005 and 2007, Category 5 is written as "6=control", but the 2005 data use codes 1-5 (no code 6). Code 5 in 2005 has the lowest yield (26.5 t/ha vs 37.2-54.2 t/ha) and highest 90-DAP bottom-leaf damage (99.0%) and is mapped as the untreated control. In 2007, only codes 1-4 appear in the data; they are mapped per the dictionary (abamectin, cyromazine, spinosad, neem), but verify against Mujica & Kroschel (2013) whether 2007 code 4 was actually an untreated control.
# Check Mujica & Kroschel (2013, doi:10.1016/j.cropro.2012.12.019; paywalled) for exact harvest dates, fertilizer rates, spray counts in 2005/2007, and whether the "neem 0.1%" product was azadirachtin or neem oil.

## NEW
# insecticide_regime_: insecticide product or 2008 application-frequency schedule (including control / no application).
# leaf_stratum_: canopy stratum of the evaluated leaf (bottom, medium, or top).
# plant_: evaluated plant number within the plot (1-10).
# insecticide_product: neem (active ingredient not specified in dictionary; possibly azadirachtin).

## MISSING
# harvest_date, yield_moisture, N_fertilizer, P_fertilizer, K_fertilizer.

## NOTES
# Three RCBD field trials (2005 in Quilmaná, 2007 and 2008 in Imperial; Cañete valley, Lima, Peru) evaluating Liriomyza huidobrensis foliar damage and potato tuber yield across 1-6 varieties and 4-5 insecticide regimes.
# Coordinates are taken from the Dataverse metadata geographicBoundingBox (single-corner points matching Quilmaná and Imperial within ~2 km).
# Yield columns (Ha-Total and Ha-First quality in t/ha) are converted to kg/ha for yield and yield_marketable. Variety "Tambeña" in 2008 sheets is standardized to "Maria Tambeña" per the dataset description.
# Leaf-level foliar damage (% leaf area mined) by plant, canopy stratum, and days after planting (DAP) is stored in the long table (23,040 observations).

## NOT PROCESSED
# 2005/2007/2008 yield sheets: P-First quality, P-Second quality, P-Total (plot-sample weights already scaled to per-hectare yields) and Ha-Second quality (equals yield - yield_marketable).
# 3.1_dictionary.xls: insecticide spray concentrations (0.15%, 0.035%, 0.1%) cannot be converted to kg a.i./ha without spray volume.


carob_script <- function(path) {

"
Replication data for: Pest intensity-crop loss relationships for the leafminer fly Liriomyza huidobrensis (Blanchard) in different potato (Solanum tuberosum L.)

In the years 2005, 2007 and 2008, three trials were carried out in the Cañete valley (central coast of Peru) at two localities (Quilmana and Imperial, 91 and 161 m.a.s.l., respectively) during the potato cropping season from June to November. Field experiments were planted on 14th July (2005) in Quilmana, and on 9th July (2007) and 24th June (2008) in Imperial. The experiments were set up in a randomized complete block design with four (2005 and 2007) and three repetitions (2008). The experimental plots had six rows of 6 m (32.4 m2) in 2005 and of 7 m (37.8 m2) in 2007 and 2008. Row spacing was 0.9 m and potato seed was spaced 0.3 m within rows. A 1-m buffer area was used to separate plots. Crop management (irrigation, fertilization, etc.) followed traditional methods commonly employed in the region. Potato varieties varied over the years in the present study. The Canchan variety was planted in 2005 as it is the most common variety in the Cañete valley and is considered highly susceptible to L. huidobrensis. In 2007, three new varieties were included with different maturation times: Revolucion (early, susceptible), Tomasa (middle-late, tolerant) and Capiro (late, unknown susceptibility). In 2008, another early variety, Desiree, was included as well as Maria Tambeña, as a L. huidobrensis resistant variety. Capiro was replaced by Yungay because of the availability of seed.
"

	uri <- "doi:10.21223/P3/EGNLAF"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = "doi:10.1016/j.cropro.2012.12.019",
		project = "Tackling Liriomyza leafmining flies: invasive pests of global proportions",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety;insecticide_product;insecticide_regime_",
		response_vars = "yield;yield_marketable;pest_severity",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f1 <- ff[basename(ff) == "subchapter 3.1_2005 crop loss trial.xls"]
	f2 <- ff[basename(ff) == "subchapter 3.1_2007crop loss trial.xls"]
	f3 <- ff[basename(ff) == "subchapter 3.1_2008 crop loss trial.xls"]

	read_headered <- function(f, sheet) {
		r <- carobiner::read.excel(f, sheet=sheet)
		hdr <- trimws(as.character(unlist(r[1, ])))
		dat <- r[-1, , drop=FALSE]
		blank <- is.na(hdr) | !nzchar(hdr)
		hdr[blank] <- paste0("blank", seq_along(hdr))[blank]
		names(dat) <- make.unique(hdr)
		dat
	}

	traits_map <- c(
		"Canchan" = "highly susceptible to Liriomyza huidobrensis",
		"Revolucion" = "early; susceptible to Liriomyza huidobrensis",
		"Tomasa" = "middle-late; tolerant to Liriomyza huidobrensis",
		"Capiro" = "late",
		"Desiree" = "early",
		"Maria Tambeña" = "resistant to Liriomyza huidobrensis",
		"Yungay" = NA_character_
	)

	one_yield <- function(f, sheet, year) {
		dat <- read_headered(f, sheet)
		if ("Potato variety" %in% names(dat)) {
			variety <- trimws(as.character(dat[["Potato variety"]]))
		} else {
			variety <- rep("Canchan", nrow(dat))
		}
		variety[variety == "Tambeña"] <- "Maria Tambeña"
		code <- as.character(as.integer(as.numeric(dat$Treatment)))
		if (year == 2008) {
			regime <- c(
				"1" = "four applications of abamectin and cyromazine",
				"2" = "two applications of abamectin and cyromazine",
				"3" = "one application of abamectin",
				"4" = "no application"
			)
			product <- c("1"="abamectin;cyromazine", "2"="abamectin;cyromazine", "3"="abamectin", "4"="none")
			used <- c("1"=TRUE, "2"=TRUE, "3"=TRUE, "4"=FALSE)
			times <- c("1"=4L, "2"=2L, "3"=1L, "4"=0L)
			location <- "Imperial"
			longitude <- -76.3778
			latitude <- -13.0101
			elevation <- 161
			planting_date <- "2008-06-24"
			plot_length <- 7
			plot_area <- 37.8
		} else if (year == 2007) {
			regime <- c("1"="abamectin", "2"="cyromazine", "3"="spinosad", "4"="neem")
			product <- regime
			used <- c("1"=TRUE, "2"=TRUE, "3"=TRUE, "4"=TRUE)
			times <- c("1"=NA_integer_, "2"=NA_integer_, "3"=NA_integer_, "4"=NA_integer_)
			location <- "Imperial"
			longitude <- -76.3778
			latitude <- -13.0101
			elevation <- 161
			planting_date <- "2007-07-09"
			plot_length <- 7
			plot_area <- 37.8
		} else {
			# Code 5 is the untreated control (dictionary typo says "6=control")
			regime <- c("1"="abamectin", "2"="cyromazine", "3"="spinosad", "4"="neem", "5"="control")
			product <- c("1"="abamectin", "2"="cyromazine", "3"="spinosad", "4"="neem", "5"="none")
			used <- c("1"=TRUE, "2"=TRUE, "3"=TRUE, "4"=TRUE, "5"=FALSE)
			times <- c("1"=NA_integer_, "2"=NA_integer_, "3"=NA_integer_, "4"=NA_integer_, "5"=0L)
			location <- "Quilmaná"
			longitude <- -76.3985
			latitude <- -12.9407
			elevation <- 91
			planting_date <- "2005-07-14"
			plot_length <- 6
			plot_area <- 32.4
		}
		firsts <- grep("First quality", names(dat))
		totals <- grep("Total", names(dat))
		rep_num <- as.integer(as.numeric(dat$Repetition))
		trial_id <- paste(location, year, sep="_")
		data.frame(
			year = year,
			trial_id = trial_id,
			plot_id = paste(trial_id, variety, code, rep_num, sep="_"),
			country = "Peru",
			adm1 = "Lima",
			adm2 = "Cañete",
			location = location,
			longitude = longitude,
			latitude = latitude,
			elevation = elevation,
			geo_uncertainty = 2000,
			geo_source = "Dataverse metadata (westLongitude/southLatitude)",
			geo_from_source = TRUE,
			planting_date = planting_date,
			crop = "potato",
			variety = variety,
			variety_traits = unname(traits_map[variety]),
			treatment = unname(regime[code]),
			insecticide_regime_ = unname(regime[code]),
			insecticide_product = unname(product[code]),
			insecticide_used = unname(used[code]),
			insecticide_times = unname(times[code]),
			code = code,
			rep = rep_num,
			plot_length = plot_length,
			plot_width = 5.4,
			plot_area = plot_area,
			row_spacing = 90,
			plant_spacing = 30,
			seed_density = round(10000 / (0.9 * 0.3)),
			# Per-hectare columns are in t/ha -> kg/ha
			yield = as.numeric(dat[[totals[length(totals)]]]) * 1000,
			yield_marketable = as.numeric(dat[[firsts[length(firsts)]]]) * 1000,
			yield_part = "tubers",
			yield_isfresh = TRUE,
			pest_species = "Liriomyza huidobrensis",
			stress = "pests",
			severity_scale = "0-100",
			irrigated = TRUE,
			on_farm = FALSE,
			is_survey = FALSE
		)
	}

	d <- rbind(
		one_yield(f1, "2005 yield data", 2005),
		one_yield(f2, "2007 yield data", 2007),
		one_yield(f3, "2008_YieldData", 2008)
	)
	d$record_id <- seq_len(nrow(d))
	key <- paste(d$year, tolower(d$variety), d$code, d$rep, sep="|")

	damage_long <- function(f, sheet, year) {
		dat <- read_headered(f, sheet)
		nms <- names(dat)
		variety <- if ("Variety" %in% nms) {
			trimws(as.character(dat$Variety))
		} else {
			trimws(as.character(dat$variety))
		}
		variety[variety == "Tambeña"] <- "Maria Tambeña"
		code <- as.character(as.integer(as.numeric(dat$Treatment)))
		rep <- as.integer(as.numeric(dat$Repetition))
		plant_ <- as.integer(as.numeric(dat$Plant))
		hit <- nms[grepl("^[0-9]+(bottom|medium|top)$", nms)]
		parts <- lapply(hit, function(nm) {
			data.frame(
				year = year,
				variety = variety,
				code = code,
				rep = rep,
				plant_ = plant_,
				DAP = as.integer(sub("[A-Za-z].*$", "", nm)),
				leaf_stratum_ = sub("^[0-9]+", "", nm),
				pest_severity = as.character(as.numeric(dat[[nm]]))
			)
		})
		do.call(rbind, parts)
	}

	sheets07 <- c("2007Revolucion damage data", "2007Canchan damage data", "2007 Tomasa damage data", "2007_CapiroDamageData")
	sheets08 <- c("2008_DesireeDamageData", "2008_RevolucionDamageData", "2008_CanchanDamageData", "2008_TambeñaDamageData", "2008_TomasaDamageData", "2008_YungayDamageData")
	z <- rbind(
		damage_long(f1, "2005 damage data", 2005),
		do.call(rbind, lapply(sheets07, function(s) damage_long(f2, s, 2007))),
		do.call(rbind, lapply(sheets08, function(s) damage_long(f3, s, 2008)))
	)
	zkey <- paste(z$year, tolower(z$variety), z$code, z$rep, sep="|")
	rid <- d$record_id[match(zkey, key)]
	if (anyNA(rid)) stop("unmatched damage records in long table")

	x <- data.frame(
		record_id = rid,
		DAP = z$DAP,
		leaf_stratum_ = z$leaf_stratum_,
		plant_ = z$plant_,
		pest_severity = z$pest_severity
	)
	d$year <- NULL
	d$code <- NULL
	rownames(d) <- NULL
	rownames(x) <- NULL

	carobiner::write_files(path, meta, d, long=x)
}
