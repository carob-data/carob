# R script for "carob"
# license: GPL (>=3)

## NOTES
# Three randomized complete block trials of Liriomyza huidobrensis on potato in the Cañete valley. 2005 in Quilmana (91 m), planted 2005-07-14. 2007 and 2008 in Imperial (161 m), planted 2007-07-09 and 2008-06-24.
# Row spacing 0.9 m and within-row spacing 0.3 m. The experimental plot was 32.4 m2 in 2005 (6 rows of 6 m) and 37.8 m2 in 2007 and 2008 (6 rows of 7 m).
# Yield is the per-hectare total in the sheet. On the first 2005 row, 120.571 kg from 21.6 m2 is 55.820 t/ha, which is the Ha-Total cell, so that column is t/ha and is multiplied by 1000. First quality is stored as yield_marketable.
# 2005 and 2007 treatment codes: 1 abamectin 0.15%, 2 cyromazine 0.035%, 3 spinosad 0.1%, 4 neem 0.1%. The dictionary's control is code 6. The 2005 sheet uses code 5 and does not use code 6, so code 5 is left unnamed.
# 2008 codes are application counts: 1 is four (abamectin twice and cyromazine twice), 2 is two (one of each), 3 is one abamectin, 4 is no application. No application is insecticide_times 0.
# Foliar damage is the percent of one leaf mined, by stratum and days after planting, on individual plants.
# Irrigation and fertilization followed local practice. Rates are not stated. Irrigation is recorded as done.

## ISSUES
# 2005 treatment 5 is not defined. Dictionary control code 6 is not in the table.
# No coordinates. The bounding boxes give only a west longitude and a south latitude for Quilmana and Imperial.
# No harvest date and no fertilizer rate.
# No yield moisture. Tuber weights are the harvested sample.
# neem is the dictionary name. It is not mapped to an active ingredient.
# missing variables: yield_moisture, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date
# unknown variable: insecticide_regime_ (insecticide_price?)
# invalid terms: insecticide_product: neem
# invalid terms: pest_species: Liriomyza huidobrensis
# unknown variable: leaf_stratum_ (leaf_S?)
# unknown variable: plant_ (plant_height?)

## NOT PROCESSED
# Plot-sample weights (P-First quality, P-Second quality, P-Total). The per-hectare columns are the yield.
# 2007 and 2008 insecticide concentration is not a rate in kg/ha.
# 3.1_dictionary.xls: documentation or a codebook, not an observation table.


## NEW VARIABLES
# insecticide_regime_: the insecticide or the 2008 application schedule, including no application.
# leaf_stratum_: bottom, medium, or top leaf.
# plant_: plant number within the plot.


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
		design = "Randomized Complete Block Design (RCBD)",
		data_type = "experiment",
		treatment_vars = "variety;insecticide_regime_",
		response_vars = "yield;pest_severity",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.8
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

	one_yield <- function(f, sheet, year) {
		dat <- read_headered(f, sheet)
		if ("Potato variety" %in% names(dat)) {
			variety <- trimws(as.character(dat[["Potato variety"]]))
		} else {
			variety <- rep("Canchan", nrow(dat))
		}
		code <- as.character(as.integer(as.numeric(dat$Treatment)))
		if (year == 2008) {
			regime <- c("1"="four applications of abamectin and cyromazine", "2"="two applications of abamectin and cyromazine", "3"="one application of abamectin", "4"="no application")
			product <- c("1"="abamectin;cyromazine", "2"="abamectin;cyromazine", "3"="abamectin", "4"=NA_character_)
			used <- c("1"=TRUE, "2"=TRUE, "3"=TRUE, "4"=FALSE)
			times <- c("1"=4L, "2"=2L, "3"=1L, "4"=0L)
			location <- "Imperial"
			elevation <- 161
			planting_date <- "2008-06-24"
			plot_area <- 37.8
		} else if (year == 2007) {
			regime <- c("1"="abamectin", "2"="cyromazine", "3"="spinosad", "4"="neem")
			product <- regime
			used <- c("1"=TRUE, "2"=TRUE, "3"=TRUE, "4"=TRUE)
			times <- c("1"=NA_integer_, "2"=NA_integer_, "3"=NA_integer_, "4"=NA_integer_)
			location <- "Imperial"
			elevation <- 161
			planting_date <- "2007-07-09"
			plot_area <- 37.8
		} else {
			regime <- c("1"="abamectin", "2"="cyromazine", "3"="spinosad", "4"="neem", "5"="treatment 5")
			product <- c("1"="abamectin", "2"="cyromazine", "3"="spinosad", "4"="neem", "5"=NA_character_)
			used <- c("1"=TRUE, "2"=TRUE, "3"=TRUE, "4"=TRUE, "5"=NA)
			times <- c("1"=NA_integer_, "2"=NA_integer_, "3"=NA_integer_, "4"=NA_integer_, "5"=NA_integer_)
			location <- "Quilmana"
			elevation <- 91
			planting_date <- "2005-07-14"
			plot_area <- 32.4
		}
		firsts <- grep("First quality", names(dat))
		totals <- grep("Total", names(dat))
		data.frame(
			year = year,
			trial_id = paste(location, year, sep="_"),
			country = "Peru",
			adm1 = "Lima",
			adm2 = "Cañete",
			location = location,
			elevation = elevation,
			planting_date = planting_date,
			crop = "potato",
			variety = variety,
			treatment = unname(regime[code]),
			insecticide_regime_ = unname(regime[code]),
			insecticide_product = unname(product[code]),
			insecticide_used = unname(used[code]),
			insecticide_times = unname(times[code]),
			code = code,
			rep = as.integer(as.numeric(dat$Repetition)),
			plot_area = plot_area,
			row_spacing = 90,
			plant_spacing = 30,
			plant_density = 10000 / (0.9 * 0.3),
			yield = as.numeric(dat[[totals[length(totals)]]]) * 1000,
			yield_marketable = as.numeric(dat[[firsts[length(firsts)]]]) * 1000,
			yield_part = "tubers",
			yield_isfresh = TRUE,
			pest_species = "Liriomyza huidobrensis",
			stress = "pests",
			irrigated = TRUE,
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = FALSE
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
		if ("Variety" %in% nms) {
			variety <- trimws(as.character(dat$Variety))
		} else {
			variety <- trimws(as.character(dat$variety))
		}
		code <- as.character(as.integer(as.numeric(dat$Treatment)))
		rep <- as.integer(as.numeric(dat$Repetition))
		plant_ <- as.integer(as.numeric(dat$Plant))
		hit <- nms[grepl("^[0-9]+(bottom|medium|top)$", nms)]
		parts <- vector("list", length(hit))
		for (j in seq_along(hit)) {
			nm <- hit[j]
			parts[[j]] <- data.frame(
				year = year,
				variety = variety,
				code = code,
				rep = rep,
				plant_ = plant_,
				DAP = as.integer(sub("[A-Za-z].*$", "", nm)),
				leaf_stratum_ = sub("^[0-9]+", "", nm),
				pest_severity = as.character(as.numeric(dat[[nm]]))
			)
		}
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
	x <- data.frame(
		record_id = d$record_id[match(zkey, key)],
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
