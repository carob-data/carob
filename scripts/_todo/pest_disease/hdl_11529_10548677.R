# R script for "carob"
# license: GPL (>=3)

## NOTES
# Herbicide trial in wheat at El Batan, 2017. geographicCoverage is Mexico, Estado de Mexico, Texcoco. The protocol places the trial in lote J8.
# Design is RCBD with two replications. Variety is BV232-2016 entry 13. Beds are permanent and 1.5 m wide, with 20 cm rows. The crop was rainfed (Temporal).
# Fertilizer on every plot is 150 kg N/ha as urea, 40 kg P2O5/ha as triple superphosphate, and 1.5 kg/ha borax. The protocol says the borax is about 180 g B/ha. K is not stated.
# Yield_Dry is kg/ha of dry grain. Subsample moisture is a fraction and is stored as a percent. Spike counts per m2 are stored per ha.
# Plot codes are 100 * rep + treatment. Weed counts stay as entered. The protocol uses two quadrats and does not say whether the cell is one quadrat or the sum.
# Peak+Amber is 26.3 + 6.66 g or ml/ha, and Everest+Topik is 30 + 750. Those pairs are not stored in the single dose column.
# Doses are g or ml of product per ha. Treatment 1 is an unweeded check and treatment 2 is weed free.

## ISSUES
# No coordinates.
# Product names are mapped to the active ingredient: Puma to fenoxaprop-p-ethyl, Topik to clodinafop, Axial (Pinoxoden) to pinoxaden, Esteron to 2,4-D, Banvel 12-24 to dicamba and 2,4-D, Sigma to mesosulfuron-methyl and iodosulfuron-methyl-sodium.
# write_files: missing variables: latitude, longitude, K_fertilizer, planting_date, harvest_date. unknown variables: herbicide_dose_g_or_ml_ha_. invalid terms: herbicide_product: flucarbazone-sodium, prosulfuron, triasulfuron. unknown variables: weed_count_, weed_dry_g_.

## NEW VARIABLES
# herbicide_dose_g_or_ml_ha_: product dose from the protocol, g or ml per ha. Zero on the unweeded and weed-free plots. NA where two products have separate doses (Peak+Amber 26.3+6.66 and Everest+Topik 30+750). Those doses are in the treatment string.
# weed_count_: weed count as entered for that species and date.
# weed_dry_g_: weed dry weight (g) on that date.

## NOT PROCESSED
# Grain-yield components other than spike density (thousand-grain weight, grains per spike, grain-filling rates).
# Lodging, plant height is kept. Phytotoxicity is the Scoring column and is not a standard score with a stated scale.
# Weed-species abbreviations are the field-book codes.


carob_script <- function(path) {

"
Evaluation of herbicides for weed control in wheat in El Batan, Mexico (2017)

Effectiveness of 14 herbicides were compared during 2017 in El Batan, Mexico. The results will be used to make recommendations to wheat farmers for weed control.
"

	uri <- "hdl:11529/10548677"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Herbicide evaluation in wheat, El Batan, 2017",
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
		trt = 1:14,
		treatment = c(
			"unweeded check", "weed free", "Peak (prosulfuron)", "Amber (triasulfuron)",
			"Peak+Amber (prosulfuron;triasulfuron) 26.3+6.66",
			"Banvel 12-24 (dicamba;2,4-D)", "Esteron (2,4-D)",
			"Puma (fenoxaprop-p-ethyl)", "Starane (fluroxypyr)",
			"Everest+Topik (flucarbazone-sodium;clodinafop) 30+750",
			"Topik (clodinafop)", "Axial (pinoxaden)",
			"Sigma (mesosulfuron-methyl;iodosulfuron-methyl-sodium)",
			"Veloz (carfentrazone)"
		),
		herbicide_product = c(
			"none", "none", "prosulfuron", "triasulfuron", "prosulfuron;triasulfuron",
			"dicamba;2,4-D", "2,4-D", "fenoxaprop-p-ethyl", "fluroxypyr",
			"flucarbazone-sodium;clodinafop", "clodinafop", "pinoxaden",
			"mesosulfuron-methyl;iodosulfuron-methyl-sodium", "carfentrazone"
		),
		herbicide_dose_g_or_ml_ha_ = c(0, 0, 30, 10, NA, 1000, 500, 1000, 400, NA, 750, 600, 500, 1000),
		herbicide_used = c(FALSE, FALSE, rep(TRUE, 12)),
		weeding_done = c(FALSE, TRUE, rep(FALSE, 12))
	)

	numcol <- function(r, pat) {
		n <- grep(pat, names(r), value=TRUE)
		if (!length(n)) return(rep(NA_real_, nrow(r)))
		as.numeric(r[[n[1]]])
	}

	f <- ff[basename(ff) == "DAT-BV101-17-Yield_Wheat.xlsx"]
	r <- carobiner::read.excel(f, sheet="Calculations")
	r <- r[!is.na(r$Rep) & !is.na(r$Trt), ]
	moist <- numcol(r, "Humidity")
	moist <- ifelse(!is.na(moist) & moist <= 1.5, moist * 100, moist)
	d <- data.frame(
		country = "Mexico",
		adm1 = "Estado de Mexico",
		adm2 = "Texcoco",
		location = "El Batan",
		trial_id = "El Batan 2017",
		plot_id = sprintf("%d%02d", as.integer(r$Rep), as.integer(r$Trt)),
		rep = as.integer(r$Rep),
		crop = "wheat",
		variety = "BV232-2016 entry 13",
		yield_part = "grain",
		yield = numcol(r, "Yield_Dry"),
		yield_moisture = moist,
		yield_isfresh = FALSE,
		plant_height = numcol(r, "^Height$"),
		flowering_days = numcol(r, "^Flowering$"),
		maturity_days = numcol(r, "^Maturity$"),
		spike_density = numcol(r, "Spikes") * 10000,
		plot_area = numcol(r, "^Area$"),
		row_spacing = 20,
		seed_rate = 110,
		N_fertilizer = 150,
		P_fertilizer = 40 / 2.29,
		B_fertilizer = 0.18,
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

	fc <- ff[basename(ff) == "DAT-BV101-17-WeedCounts_Wheat.xlsx"]
	sh <- c("20-Jun-17", "03-Jul-17", "07-Ago-17")
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

	date_cell <- function(x) {
		x <- trimws(as.character(x))
		if (!nzchar(x) || is.na(x)) return(NA_character_)
		if (grepl("[A-Za-z]", x)) {
			p <- strsplit(x, "-")[[1]]
			if (length(p) != 3) return(NA_character_)
			mon <- tolower(p[2])
			if (grepl("^agos", mon)) mon <- "ago"
			m <- c(ene=1, jan=1, feb=2, mar=3, abr=4, apr=4, may=5, jun=6, jul=7, ago=8, aug=8, sep=9, oct=10, nov=11, dic=12, dec=12)
			if (is.na(m[mon])) return(NA_character_)
			return(sprintf("20%s-%02d-%02d", p[3], as.integer(m[mon]), as.integer(p[1])))
		}
		v <- as.numeric(x)
		if (is.na(v) || v < 20000) return(NA_character_)
		as.character(as.Date(v, origin="1899-12-30"))
	}

	fd <- ff[basename(ff) == "DAT-BV101-17-WeedDryWeight_Wheat.xlsx"]
	w <- carobiner::read.excel(fd, sheet="WeedDryWeight", col_names=FALSE, na=c(".", ""))
	hdrs <- which(apply(w, 1, function(z) any(toupper(trimws(as.character(z))) == "PARC")))
	for (h in hdrs) {
		nms <- trimws(as.character(unlist(w[h, ])))
		above <- trimws(as.character(unlist(w[h - 1, ])))
		parc_cols <- which(toupper(nms) == "PARC")
		for (pc in parc_cols) {
			body <- w[(h + 1):nrow(w), ]
			parc <- as_num(body[[pc]])
			peso <- which(tolower(nms) == "peso seco")
			peso <- peso[peso > pc]
			if (pc < max(parc_cols)) peso <- peso[peso < min(parc_cols[parc_cols > pc])]
			peso <- peso[seq_len(min(3, length(peso)))]
			for (k in seq_along(peso)) {
				val <- as_num(body[[peso[k]]])
				keep <- !is.na(parc) & !is.na(val)
				if (!any(keep)) next
				counts[[length(counts) + 1]] <- data.frame(
					plot_id = sprintf("%03d", as.integer(parc[keep])),
					date = date_cell(above[peso[k]]),
					weed_species = "all weeds",
					weed_count_ = NA_real_,
					weed_dry_g_ = val[keep]
				)
			}
		}
	}
	long <- do.call(rbind, counts)
	long <- long[long$plot_id %in% d$plot_id, ]
	long$trial_id <- "El Batan 2017"

	carobiner::write_files(path, meta, d, long=long)
}
