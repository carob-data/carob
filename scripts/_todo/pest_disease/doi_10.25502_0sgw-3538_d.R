# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-station CRBD at IITA-Benin (2008-11-18 to 2009-01-19): GF-120 bait vs untreated, 3 blocks.
# ff-counts-on-resting-plants.csv: fruit-fly and aphid counts on border roosts (sorghum, castor bean). The two roost plants are averaged. Fruit incubation rows are not.
# fruits-incubation-data.csv: zucchini and watermelon samples (10 fruits), infestation, fruit weight, emerged adults.
# pest_incidence on each row is the sum of the species counts in the long table (males and females kept separate there).
# CKAN coverage_x 6.4428 and coverage_y 2.55 are used as latitude and longitude (that pair is in southern Benin).

## ISSUES
# Fruit weight (FW) has no unit (values 0.1-1.2). It is not converted to yield. No plot area, so no kg/ha yield.
# Fruit class (FC) is 1 or 2 with no key. Watermelon is only class 1.
# Planting and harvest dates, irrigation, and fertilizer are not in the files. First count date is not treated as planting.
# Rain, Weather, and Wind are Yes/No or Sunny/Cloudy flags, not a weather series.
# write_files will flag pest names that are not yet in the pest vocabulary, and missing yield, yield_moisture, N/P/K, irrigated, planting_date, harvest_date, and carob_contributor.

## NOT PROCESSED
# ff-counts-on-resting-plants.csv: Rain, Weather, Wind. Qualitative conditions on the count day, not weather records.
# Both count files: ID. Row number.
# data_dictionary.csv: documentation or a codebook, not an observation table.

## NEW VARIABLES
# fruit_class_: fruit class code (1 or 2); meaning not in the dictionary.
# fruit_weight_: fruit weight as recorded; unit not stated.
# fruits_sampled_: number of fruits incubated (10).
# fruits_infested_: number of those fruits infested.
# fruit_infestation_: 100 * fruits_infested / fruits_sampled.
# pupae_: total pupae from the incubated sample.
# petiole_exudate_: mean number of petioles with exudate on the roost plants.
# sex_: male or female for a fruit-fly count in the long table.


carob_script <- function(path) {

"
GF-120 and Control of Cucurbit crops in Benin

Fruit flies belong to family Tephritidae are the most destructive pests of horticultural crops in Benin, West Africa. Given their economic losses in zucchini and watermelon crops, the protein bait GF-120 was evaluated for its effectiveness to control cucurbits-infesting fruit flies The protein bait was applied to clusters of sorghum and castor bean planted in four corners alternately as a border crops. For over 2 years – 2008 to 2009, we conducted on-station trials at IITA-Benin to identify 1) the most attractive roosting hosts to fruit flies among 12 tested, and 2) evaluate the effectiveness of GF-120 bait spray on the selected roosting hosts. The benefit obtained in applying GF-120 to protect zucchini and watermelon plants was assessed. The experimental design consisted of a 'Complete Random Block Design' with two treatments – untreated and GF-120 treated plots, replicated three times. The results revealed that the castor bean and sorghum were identified as the best roosting hosts naturally attractive to fruit flies. The protein bait GF-120 was therefore applied to the underside of young leaves of castor bean and sorghum every Monday morning from 7am to 9am during the test period. The GF-120-treated castor bean, attracted more fruit flies than GF-120-treated sorghum. Infestation rates on the zucchini and watermelon were significantly lower (29% and 20%) on GF-120-treated plots than on untreated plots (73% and 77%) respectively. The use of GF-120 in integrated fruit flies management is therefore benefic and advantageous for the vegetable growers.
"

	uri <- "doi:10.25502/0sgw-3538/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = "CRBD; GF-120 vs untreated; 3 blocks; sorghum and castor bean as border roosts",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "pest_incidence;fruit_infestation_",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.7
	)

	f1 <- ff[basename(ff) == "ff-counts-on-resting-plants.csv"]
	f2 <- ff[basename(ff) == "fruits-incubation-data.csv"]
	r1 <- read.csv(f1, check.names=FALSE)
	r2 <- read.csv(f2, check.names=FALSE)

	trt <- function(x) {
		x <- trimws(x)
		ifelse(tolower(x) == "control", "control", x)
	}
	crop_name <- function(x) {
		x <- tolower(trimws(x))
		x[x == "ricinus"] <- "castor bean"
		x
	}
	row_sum <- function(m) {
		s <- rowSums(m, na.rm=TRUE)
		s[apply(is.na(m), 1, all)] <- NA
		as.integer(s)
	}
	sex_long <- function(record_id, male, female, species) {
		n <- length(record_id)
		data.frame(
			record_id = rep(record_id, 2),
			pest_species = species,
			sex_ = rep(c("male", "female"), each=n),
			pest_incidence = as.integer(c(male, female))
		)
	}

	fly_m <- cbind(
		r1$D_ciliaM, r1$D_ciliaF, r1$D_verteM, r1$D_verteF,
		r1$D_bivitM, r1$D_bivitF, r1$D_punctM, r1$D_punctF,
		r1$Z_cucurM, r1$Z_cucurF, r1$Nbpetiolew_Aph
	)
	d1 <- data.frame(
		record_id = seq_len(nrow(r1)),
		date = as.character(as.Date(r1$Date, format="%d/%m/%Y")),
		treatment = trt(r1$Treatment),
		block_id = r1$Block,
		crop = crop_name(r1$RHP),
		plant_number_ = as.integer(r1[["plant number"]]),
		growth_stage = tolower(trimws(r1$PlantStage)),
		petiole_exudate_ = as.integer(r1$Nbpetiolew_Ex),
		pest_incidence = row_sum(fly_m),
		insecticide_used = trt(r1$Treatment) != "control",
		yield_part = "none"
	)

	fruit_m <- cbind(
		r2$D_ciliaM, r2$D_ciliaF, r2$D_verteM, r2$D_verteF,
		r2$D_bivitM, r2$D_bivitF, r2$D_punctM, r2$D_punctF,
		r2$Z_cucurM, r2$Z_cucurF
	)
	d2 <- data.frame(
		record_id = seq_len(nrow(r2)) + nrow(r1),
		date = as.character(as.Date(r2$Date, format="%d/%m/%Y")),
		treatment = trt(r2$Treatment),
		block_id = r2$Block,
		crop = crop_name(r2$RHP),
		fruit_class_ = as.integer(r2$FC),
		fruit_weight_ = as.numeric(r2$FW),
		fruits_sampled_ = as.integer(r2$NFS),
		fruits_infested_ = as.integer(r2$NFI),
		fruit_infestation_ = 100 * as.numeric(r2$NFI) / as.numeric(r2$NFS),
		pupae_ = as.integer(r2$TP),
		pest_incidence = row_sum(fruit_m),
		insecticide_used = trt(r2$Treatment) != "control",
		yield_part = "fruit"
	)

	d <- carobiner::bindr(d1, d2)
	d$country <- "Benin"
	d$location <- "IITA-Benin"
	d$latitude <- 6.4428
	d$longitude <- 2.55
	d$geo_from_source <- TRUE
	d$trial_id <- "IITA-Benin_2008"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$stress <- "pests"

	dd <- rbind(
		sex_long(d1$record_id, r1$D_ciliaM, r1$D_ciliaF, "Dacus ciliatus"),
		sex_long(d1$record_id, r1$D_verteM, r1$D_verteF, "Dacus vertebratus"),
		sex_long(d1$record_id, r1$D_bivitM, r1$D_bivitF, "Dacus bivittatus"),
		sex_long(d1$record_id, r1$D_punctM, r1$D_punctF, "Dacus punctatifrons"),
		sex_long(d1$record_id, r1$Z_cucurM, r1$Z_cucurF, "Zeugodacus cucurbitae"),
		sex_long(d2$record_id, r2$D_ciliaM, r2$D_ciliaF, "Dacus ciliatus"),
		sex_long(d2$record_id, r2$D_verteM, r2$D_verteF, "Dacus vertebratus"),
		sex_long(d2$record_id, r2$D_bivitM, r2$D_bivitF, "Dacus bivittatus"),
		sex_long(d2$record_id, r2$D_punctM, r2$D_punctF, "Dacus punctatifrons"),
		sex_long(d2$record_id, r2$Z_cucurM, r2$Z_cucurF, "Zeugodacus cucurbitae")
	)
	aph <- data.frame(
		record_id = d1$record_id,
		pest_species = "aphids",
		sex_ = NA_character_,
		pest_incidence = as.integer(r1$Nbpetiolew_Aph)
	)
	dd <- carobiner::bindr(dd, aph)
	dd <- dd[!is.na(dd$pest_incidence), ]
	dd$date <- d$date[match(dd$record_id, d$record_id)]

	# Average one row per plant to one row per group in `by`.
	# Numbers are means. A yes/no column stays yes/no when the plants agree, otherwise it is the proportion of plants.
	# Text that is all numbers is averaged. Other text that differs is joined with "; ". Full dates are averaged.
	mean_by <- function(x, by) {
		if (!nrow(x)) return(x)
		if ("plant_number_" %in% names(x)) x$plant_number_ <- NULL
		if ("record_id" %in% names(x)) x$record_id <- NULL
		by <- intersect(by, names(x))
		key <- do.call(paste, c(lapply(by, function(v) ifelse(is.na(x[[v]]), "", as.character(x[[v]]))), list(sep = "\r")))
		mean_na <- function(z) {
			z <- as.numeric(z)
			if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
		}
		keep <- !duplicated(key)
		out <- x[keep, , drop = FALSE]
		num <- vapply(x, is.numeric, TRUE)
		logi <- vapply(x, is.logical, TRUE)
		num[names(num) %in% by] <- FALSE
		logi[names(logi) %in% by] <- FALSE
		for (v in names(x)[num]) {
			m <- tapply(x[[v]], key, mean_na)
			m <- unname(m[key[keep]])
			if (is.integer(x[[v]]) && all(is.na(m) | abs(m - round(m)) < 1e-8)) {
				m <- as.integer(round(m))
			}
			out[[v]] <- m
		}
		for (v in names(x)[logi]) {
			same <- tapply(x[[v]], key, function(z) {
				z <- z[!is.na(z)]
				!length(z) || all(z == z[1])
			})
			if (isTRUE(all(same))) {
				m <- tapply(x[[v]], key, function(z) {
					z <- z[!is.na(z)]
					if (!length(z)) NA else z[1]
				})
				out[[v]] <- unname(m[key[keep]])
			} else {
				m <- tapply(as.numeric(x[[v]]), key, mean_na)
				out[[v]] <- unname(m[key[keep]])
			}
		}
		for (v in names(x)) {
			if (v %in% by || num[v] || logi[v]) next
			u <- tapply(as.character(x[[v]]), key, function(z) {
				z <- z[!is.na(z)]
				z <- trimws(z)
				z <- z[nzchar(z)]
				if (!length(z)) return(NA_character_)
				if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", z))) {
					return(as.character(mean(as.Date(z))))
				}
				isn <- grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
				parts <- character()
				if (any(isn)) parts <- c(parts, as.character(mean(as.numeric(z[isn]))))
				codes <- unique(z[!isn])
				parts <- c(parts, codes)
				if (length(parts) == 1) parts else paste(parts, collapse = "; ")
			})
			out[[v]] <- unname(u[key[keep]])
		}
		rownames(out) <- NULL
		out
	}

	plant <- !is.na(d$plant_number_)
	d$plot_key <- ifelse(plant, paste(d$date, d$treatment, d$block_id, d$crop, d$growth_stage, sep = "\r"), paste0("row", d$record_id))
	dd$plot_key <- d$plot_key[match(dd$record_id, d$record_id)]
	d <- mean_by(d, "plot_key")
	d$record_id <- seq_len(nrow(d))
	dd <- mean_by(dd, c("plot_key", "pest_species", "sex_", "date"))
	dd$record_id <- d$record_id[match(dd$plot_key, d$plot_key)]
	stopifnot(!any(is.na(dd$record_id)))
	dd$plot_key <- NULL
	d$plot_key <- NULL

	carobiner::write_files(path, meta, d, dd)
}
