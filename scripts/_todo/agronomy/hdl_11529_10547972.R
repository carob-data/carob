# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm long-term jute trials at Rangpur, Bangladesh, kharif 2015 and 2016. The site sheet says Country: Bangladesh and District: Rangpur.
# Fiber and jute stick are t/ha and are stored as kg/ha. Dry fiber plus dry stick equals the dry biomass column. yield_part is fiber. Stick is stem dry matter.
# K2O and P2O5 summary columns are oxides. Zinc sulfate is not stored. The protocol dose is not in these files; the sheet's N, P2O5, and K2O totals are used.
# Row spacing 0 is empty.

## ISSUES
# Harvest height is empty.
# write_files: missing variables: yield_moisture, latitude, longitude, irrigated. out of bounds: plot_area (400, 400). invalid terms: yield_part: fiber. NA detected: yield.

## NEW VARIABLES
# None.

## NOT PROCESSED
# Irrigation, ponding, weeds, pesticides, prices, and fertilizer product names.
# Daily rainfall. Farmer names. Zinc sulfate.


carob_script <- function(path) {

"
On-farm long-term jute trials at Rangpur, Bangladesh, 2015 and 2016.
"

	uri <- "hdl:11529/10547972"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Pre-kharif long-term trial, Rangpur, Bangladesh",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.5
	)

	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		if (is.numeric(x)) return(as.character(as.Date(x, origin="1899-12-30")))
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		out <- rep(NA_character_, length(x))
		iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
		if (any(iso)) out[iso] <- substr(x[iso], 1, 10)
		out
	}

	one <- function(f) {
		r1 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
		r2 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="13 -  jute harvest", skip=4, hdr=2))
		r3 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
		r1 <- r1[!is.na(r1$Tmnt) & !is.na(r1$Crop) & !is.na(r1$Node), ]
		r2 <- r2[!is.na(r2$Tmnt), ]
		r3 <- r3[!is.na(r3$Tmnt), ]
		add_seq <- function(x) ave(seq_len(nrow(x)), x$Site.No, x$Tmnt, FUN=seq_along)
		r1$seq <- add_seq(r1)
		r2$seq <- add_seq(r2)
		r3$seq <- add_seq(r3)
		key <- function(x) paste(x$Site.No, x$Tmnt, x$seq)
		ij <- match(key(r1), key(r2))
		ifr <- match(key(r1), key(r3))
		take <- function(df, idx, nms) {
			n <- nms[nms %in% names(df)]
			if (length(n) == 0) return(rep(NA_real_, length(idx)))
			as.numeric(df[[n[1]]][idx])
		}
		sp <- as.character(r1$Row.spacing.cm)
		sp[sp %in% c("0", "30-40", "40-50")] <- NA
		hi <- take(r2, ij, "HI")
		hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
		hd <- as_date(r1$Date.of.harvest.mm.dd.yy)
		hd[!is.na(hd) & hd > "2018-06-05"] <- NA_character_
		data.frame(
			country = "Bangladesh",
			adm2 = "Rangpur",
			location = r1$Node,
			trial_id = paste(r1$Year, r1$Node),
			plot_id = paste(r1$Site.No, r1$Tmnt, r1$seq),
			treatment = r1$Tmnt,
			crop = "jute",
			variety = r1$Variety,
			yield_part = "fiber",
			planting_date = as_date(r1$Date.of.seeding.mm.dd.yy),
			harvest_date = hd,
			row_spacing = as.numeric(sp),
			seed_rate = as.numeric(r1$Seedrate.kg.ha),
			plot_area = as.numeric(r1$Plot.size.m2),
			yield = take(r2, ij, "Dry.Fiber") * 1000,
			dmy_stems = take(r2, ij, "Dry.jute.steak") * 1000,
			dmy_total = take(r2, ij, "Dry.biomass.t.ha") * 1000,
			harvest_index = hi,
			N_fertilizer = take(r3, ifr, "N.kg.ha"),
			P_fertilizer = take(r3, ifr, "P2O5.kg.ha") / 2.29,
			K_fertilizer = take(r3, ifr, "K2O.kg.ha") / 1.2051,
			gypsum = take(r3, ifr, c("Gypsum.Kg.ha.1", "Gypsum.Kg.ha")),
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE,
			yield_isfresh = FALSE
		)
	}

	ff <- ff[basename(ff) %in% c("Kharif-I Jute  2015-LT-Kolkondo-Rangpur.xlsx", "Kharif-I Jute  2016 -LT-Kolkondo & Mohonpur-Rangpur.xlsx")]
	d <- do.call(rbind, lapply(ff, one))
	carobiner::write_files(path, meta, d)
}
