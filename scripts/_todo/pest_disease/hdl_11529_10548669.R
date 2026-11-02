# R script for "carob"
# license: GPL (>=3)

## NOTES
# Herbicide trial in common bean at El Batan, 2016. geographicCoverage is Mexico, Estado de Mexico, Texcoco. The protocol places the trial in lote G6.
# Design is RCBD with two replications. Variety is Pinto Saltillo. Beds are permanent and 0.75 m. The crop was rainfed (Temporal).
# Fertilizer in the protocol is 50 kg N/ha as urea and 40 kg P2O5/ha as triple superphosphate, on every plot. P is elemental. K is not stated.
# Yield_Dry is kg/ha of dry seed. Subsample moisture in %Humidity is a fraction and is stored as a percent.
# Plot codes are 100 * rep + treatment. Weed counts are one number per species per plot. The protocol counts two quadrats of 0.50 m by the bed width and does not say whether the sheet is one quadrat or the sum, so the counts are not converted to plants/ha.
# Weed dry weight is the sheet value in grams, not kg/ha. Dates are the Excel serials above the PARC header.
# Doses in the protocol are g or ml of product per ha, not kg of active ingredient. Treatment 1 is an unweeded check and treatment 2 is weed free.

## ISSUES
# No coordinates.
# Fusilade is stored as fluazifop-p-butyl. Basagran is stored as bentazon, sodium salt. Fomesafan is stored as fomesafen. Those names follow the product, not a concentration assay.
# write_files: missing variables: latitude, longitude, K_fertilizer, planting_date, harvest_date. unknown variables: herbicide_dose_g_or_ml_ha_. invalid terms: herbicide_product: fomesafen, imazethapyr. unknown variables: weed_count_, weed_dry_g_.

## NEW VARIABLES
# herbicide_dose_g_or_ml_ha_: product dose from the protocol, g or ml per ha. Zero on the unweeded and weed-free plots.
# weed_count_: weed count as entered for that species and date.
# weed_dry_g_: weed dry weight (g) on that date. The quadrat area is not applied.

## NOT PROCESSED
# Grain-yield components (grains per stem, thousand-grain weight, test weight, straw).
# Phytotoxicity scores. The protocol lists them and this workbook does not have that column.
# Species counts are the field-book abbreviations (Verd, Golon, H.ANCHA, H.ANGOSTA), not botanical names.


carob_script <- function(path) {

"
Evaluation of herbicides for weed control in beans in El Batan, Mexico (2016)

Effectiveness of herbicides were compared during 2016 in El Batan, Mexico. The results will be used to make recommendations to farmers for weed control.
"

	uri <- "hdl:11529/10548669"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Herbicide evaluation in common bean, El Batan, 2016",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.6
	)

	key <- data.frame(
		trt = 1:10,
		treatment = c(
			"unweeded check", "weed free", "Prawl (pendimethalin)",
			"Flex BIW (fomesafen)", "Fusilade (fluazifop-p-butyl)",
			"Fusiflex (fomesafen;fluazifop-p-butyl)", "Pivot (imazethapyr) 500",
			"Pivot (imazethapyr) 400", "Pivot (imazethapyr) 300",
			"Basagran (bentazon, sodium salt)"
		),
		herbicide_product = c(
			"none", "none", "pendimethalin", "fomesafen", "fluazifop-p-butyl",
			"fomesafen;fluazifop-p-butyl", "imazethapyr", "imazethapyr",
			"imazethapyr", "bentazon, sodium salt"
		),
		herbicide_dose_g_or_ml_ha_ = c(0, 0, 2631, 1000, 1000, 1000, 500, 400, 300, 2000),
		herbicide_used = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
		weeding_done = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
	)

	numcol <- function(r, pat) {
		n <- grep(pat, names(r), value=TRUE)
		if (!length(n)) return(rep(NA_real_, nrow(r)))
		as.numeric(r[[n[1]]])
	}

	f <- ff[basename(ff) == "DAT-BV101_BEANS-16.xlsx"]
	r <- carobiner::read.excel(f, sheet="Calculations")
	r <- r[!is.na(r$Rep) & !is.na(r$Trt), ]
	moist <- numcol(r, "Humidity")
	moist <- ifelse(!is.na(moist) & moist <= 1.5, moist * 100, moist)
	hi <- numcol(r, "^HI$")
	hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
	d <- data.frame(
		country = "Mexico",
		adm1 = "Estado de Mexico",
		adm2 = "Texcoco",
		location = "El Batan",
		trial_id = "El Batan 2016",
		plot_id = sprintf("%d%02d", as.integer(r$Rep), as.integer(r$Trt)),
		rep = as.integer(r$Rep),
		crop = "common bean",
		variety = "Pinto Saltillo",
		yield_part = "seed",
		yield = numcol(r, "^Yield_Dry$"),
		yield_moisture = moist,
		yield_isfresh = FALSE,
		dmy_total = numcol(r, "^Biomass$"),
		harvest_index = hi,
		maturity_days = numcol(r, "^Maturity$"),
		plot_area = numcol(r, "^Area$"),
		row_spacing = 75,
		seed_rate = 50,
		N_fertilizer = 50,
		P_fertilizer = 40 / 2.29,
		land_prep_method = "permanent beds",
		irrigated = FALSE,
		stress = "weeds",
		trt = as.integer(r$Trt)
	)
	d <- merge(d, key, by="trt", sort=FALSE)
	d$trt <- NULL
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE

	sheet_date <- function(s) {
		p <- strsplit(s, "-")[[1]]
		mon <- tolower(p[2])
		if (grepl("^agost", mon)) mon <- "ago"
		m <- c(ene=1, jan=1, feb=2, mar=3, abr=4, apr=4, may=5, jun=6, jul=7, ago=8, aug=8, sep=9, oct=10, nov=11, dic=12, dec=12)
		sprintf("20%s-%02d-%02d", p[3], as.integer(m[mon]), as.integer(p[1]))
	}
	as_num <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		x[grepl("[A-Za-z]", x)] <- NA_character_
		as.numeric(x)
	}

	fc <- ff[basename(ff) == "DAT-BV101-16-WeedCounts_Bean.xlsx"]
	sh <- c("01-Agost-16", "19-Ago-16")
	counts <- list()
	for (s in sh) {
		w <- carobiner::read.excel(fc, sheet=s, col_names=FALSE, na=c(".", ""))
		hdr <- which(apply(w, 1, function(z) any(toupper(trimws(as.character(z))) == "PARC")))[1]
		nms <- trimws(as.character(unlist(w[hdr, ])))
		body <- w[(hdr + 1):nrow(w), ]
		parc <- as_num(body[[1]])
		ok <- !is.na(parc)
		body <- body[ok, ]
		parc <- parc[ok]
		for (j in seq_len(ncol(body))[-1]) {
			if (is.na(nms[j]) || !nzchar(nms[j])) next
			val <- as_num(body[[j]])
			keep <- !is.na(val)
			if (!any(keep)) next
			counts[[length(counts) + 1]] <- data.frame(
				plot_id = sprintf("%03d", as.integer(parc[keep])),
				date = sheet_date(s),
				weed_species = nms[j],
				weed_count_ = val[keep],
				weed_dry_g_ = NA_real_
			)
		}
	}

	fd <- ff[basename(ff) == "DAT-BV101-16-WeedDryWeight_Bean.xlsx"]
	w <- carobiner::read.excel(fd, sheet="WeedDryWeight", col_names=FALSE, na=c(".", ""))
	hdr <- which(apply(w, 1, function(z) any(toupper(trimws(as.character(z))) == "PARC")))[1]
	nms <- trimws(as.character(unlist(w[hdr, ])))
	above <- as_num(unlist(w[hdr - 1, ]))
	body <- w[(hdr + 1):nrow(w), ]
	parc_j <- which(toupper(nms) == "PARC")[1]
	parc <- as_num(body[[parc_j]])
	ok <- !is.na(parc)
	for (j in which(grepl("seco|peso", nms, ignore.case=TRUE))) {
		val <- as_num(body[[j]])
		keep <- ok & !is.na(val)
		if (!any(keep) || is.na(above[j])) next
		counts[[length(counts) + 1]] <- data.frame(
			plot_id = sprintf("%03d", as.integer(parc[keep])),
			date = as.character(as.Date(above[j], origin="1899-12-30")),
			weed_species = "all weeds",
			weed_count_ = NA_real_,
			weed_dry_g_ = val[keep]
		)
	}
	long <- do.call(rbind, counts)
	long <- long[long$plot_id %in% d$plot_id, ]
	long$trial_id <- "El Batan 2016"

	carobiner::write_files(path, meta, d, long=long)
}
