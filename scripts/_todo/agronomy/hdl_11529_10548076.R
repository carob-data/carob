# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm long-term trial at Rangpur, Bangladesh. The dataset title names the place. The latitude and longitude columns in the site sheet are empty.
# Treatment is the short code from the field book. The protocol that expands those codes is not in the files.
# Grain yield columns are sun-dry t/ha, stored as kg/ha. Where Moisture.pct is present, that is yield_moisture.
# Maize files label potassium as K2O, so P and K are converted from the oxides. Wheat files label potassium as K.kg.ha, so that column is stored as K without an oxide conversion.
# Boric acid is converted to elemental B (10.81/61.83). Zinc sulfate is not stored, because the hydrate is not stated.
# The 2014-15 and 2015-16 maize yield columns are the same sequence of numbers. The sites and treatments in those two files are not the same.
# Some farmers have the same treatment code twice, with different yields. Those rows are kept as separate plots.
# Row spacing 0 is empty. One maize harvest date is 2026-05-18 and is not stored.

## ISSUES
# Treatment codes are not expanded. Zinc sulfate is not stored. Biomass is not stated as fresh or dry.
# Mohonpur site 27 is in the maize phenology sheets and has no grain-harvest row.
# write_files: missing variables: latitude, longitude, irrigated. unknown variables: biomass_kg_ha_. NA detected: yield.

## NEW VARIABLES
# biomass_kg_ha_: biomass column in t/ha, stored as kg/ha. Fresh or dry is not stated.

## NOT PROCESSED
# Irrigation, ponding, weeds, pesticide applications, biomass subsamples, soil analysis, and prices.
# Fertilizer product names.


carob_script <- function(path) {

"
On-farm long-term maize and wheat trials at Rangpur, Bangladesh, rabi seasons 2014-15, 2015-16, and 2016-17. Treatments are the field-book codes.
"

	uri <- "hdl:11529/10548076"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Rabi (winter) crops long-term trial, Rangpur, Bangladesh",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.7
	)

	one <- function(f) {
		ph <- as.data.frame(carobiner::read.excel.hdr(f, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
		gh <- as.data.frame(carobiner::read.excel.hdr(f, sheet="14 - Grain Harvest ", skip=4, hdr=2))
		ft <- as.data.frame(carobiner::read.excel.hdr(f, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
		ph <- ph[!is.na(ph$Tmnt) & !is.na(ph$Crop) & !is.na(ph$Node), ]
		gh <- gh[!is.na(gh$Tmnt), ]
		ft <- ft[!is.na(ft$Tmnt), ]
		# The same treatment code is entered twice for some farmers, with different yields.
		add_seq <- function(x) ave(seq_len(nrow(x)), x$Site.No, x$Tmnt, FUN=seq_along)
		ph$seq <- add_seq(ph)
		gh$seq <- add_seq(gh)
		ft$seq <- add_seq(ft)

		key <- function(x) paste(x$Site.No, x$Tmnt, x$seq)
		ig <- match(key(ph), key(gh))
		ifr <- match(key(ph), key(ft))

		take <- function(df, idx, nms) {
			n <- nms[nms %in% names(df)]
			if (length(n) == 0) return(rep(NA_real_, length(idx)))
			as.numeric(df[[n[1]]][idx])
		}
		take_c <- function(df, idx, nms) {
			n <- nms[nms %in% names(df)]
			if (length(n) == 0) return(rep(NA_character_, length(idx)))
			as.character(df[[n[1]]][idx])
		}

		y <- take(gh, ig, c("Sun.dry.grain.yield.t.ha", "Calculation_Sun.dry.grain.yield.t.ha", "Sun.dry.grain.t.ha"))
		bio <- take(gh, ig, c("Total.Biomass.t.ha", "Biomass.t.ha"))
		hi <- take(gh, ig, "HI")
		hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
		moist <- take(gh, ig, "Moisture.pct")

		K <- take(ft, ifr, "K2O.kg.ha")
		k_plain <- take(ft, ifr, "K.kg.ha")
		K <- ifelse(is.na(K), k_plain, K / 1.2051)

		bname <- c("Boric.acid.kg.ha", "Boric.Acid.kg.ha")
		B <- take(ft, ifr, bname)

		sp <- as.character(ph$Row.spacing.cm)
		sp[sp %in% c("30-40", "40-50", "0")] <- NA
		hd <- as.character(as.Date(take_c(ph, seq_len(nrow(ph)), c("Date.of.harvest.mm.dd.yy", "Datw.of.harvest.dd.mm.yy"))))
		# One maize harvest is recorded as 2026-05-18. The dataset was published in 2018.
		hd[!is.na(hd) & hd > "2018-01-01"] <- NA

		data.frame(
			country = "Bangladesh",
			adm2 = "Rangpur",
			location = ph$Node,
			trial_id = paste(ph$Year, ph$Node),
			plot_id = paste(ph$Site.No, ph$Tmnt, ph$seq),
			treatment = ph$Tmnt,
			crop = tolower(ph$Crop),
			variety = ph$Variety,
			yield_part = "grain",
			planting_date = as.character(as.Date(ph$Date.of.seeding.mm.dd.yy)),
			harvest_date = hd,
			row_spacing = as.numeric(sp),
			yield = y * 1000,
			yield_moisture = moist,
			biomass_kg_ha_ = bio * 1000,
			harvest_index = hi,
			N_fertilizer = take(ft, ifr, "N.kg.ha"),
			P_fertilizer = take(ft, ifr, "P2O5.kg.ha") / 2.29,
			K_fertilizer = K,
			gypsum = take(ft, ifr, c("Gypsum.kg.ha", "Gypsum.Kg.ha")),
			B_fertilizer = B * 10.81 / 61.83,
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	ff <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	d <- lapply(ff, one)
	d <- do.call(rbind, d)
	carobiner::write_files(path, meta, d)
}
