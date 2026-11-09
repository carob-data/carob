# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm validation trials at Rangpur, Bangladesh. The file names and the validation workbook name the district. Country is Bangladesh.
# Four field books (maize 2015-16 and 2016-17, wheat 2014-15 and 2015-16) use the site, phenology, fertilizer, and grain-harvest sheets. The 2016-17 validation workbook is a separate set of demonstration plots.
# The field-book latitude and longitude columns are empty. Coordinates are taken only from the validation workbook. On the mustard, wheat, maize, jute, and boro sheets the headers are reversed: the value near 25 is latitude and the value near 89 is longitude. The intercrop and kharif-maize sheets are already in the right order.
# Node spellings are aligned: Lakkhhitari, Lakkhatari, Lakkhiitari, and Lakkhitari to Lakkhitari; Mohonpur to Mohanpur.
# CT is conventional tillage. ZT is zero tillage (land_prep_method none). ST is strip tillage. BP is bed planting, stored as raised beds.
# In the field books, K.kg.ha equals 60 percent of the MOP rate, which is the K2O fraction of muriate of potash. It is converted to elemental K with K2O / 1.2051, including the column that is already named K2O. P2O5 is converted with / 2.29. S.kg.ha and Zn.kg.ha are stored as elemental. Zinc sulfate is not stored.
# The validation dose is the string N-P-K-S-Zn in kg/ha. The header also says "/33 deci". That phrase is not used as a conversion, because the same header says kg/ha.
# Grain yield in t/ha is stored as kg/ha. On the validation sheets the straw column says sun-dry, and biomass equals grain plus straw, so those are dry matter. Field-book biomass is not labeled fresh or dry.
# Harvest index is a ratio and is stored as a percent. A harvest date of 2007-07-25 on one jute row is not stored.
# R-M, R-W, R-W-J, R-Mu-Boro, R-Mu-M, and R-P-M are rice rotations. P is stored as potato.

## ISSUES
# Field-book sites have no coordinates. The validation plots do.
# Field-book plot size is 400 m2, above the vocabulary maximum of 350.
# yield_part fiber is not in the vocabulary. Jute yield is the fibre.
# write_files: unknown variable biomass_kg_ha_. out of bounds plot_area (400, 400). invalid terms yield_part: fiber. NA detected: latitude, longitude.

## NOT PROCESSED
# Rjshahi sheet: a two-cell note, not plot records.
# Field-book irrigation, ponding, weeds, pesticides, prices, rainfall, soil analysis, and fertilizer product names.
# Zinc sulfate and, where an elemental S column is present, the gypsum product amount.
# Roundup and Affinity. Affinity is not mapped to an active ingredient.
# Farmer names.
# Field-book straw yield. The column is not labeled fresh or dry.

## NEW VARIABLES
# biomass_kg_ha_: field-book biomass in t/ha, stored as kg/ha. Fresh or dry is not stated.


carob_script <- function(path) {

"
Farmer participatory on-farm trials with CA technologies comparing with farmers' practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory validation trials were conducted comparing to existing practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners.
"

	uri <- "hdl:11529/10548008"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Rabi validation trials, Rangpur, Bangladesh",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment;variety",
		response_vars = "yield",
		carob_contributor = "Fredy Chimire",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.8,
		notes = "Field-book coordinates are empty. Validation-sheet latitude and longitude headers are swapped on most sheets. K.kg.ha is K2O."
	)

	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) {
			out <- as.character(as.Date(x))
		} else if (is.numeric(x)) {
			out <- as.character(as.Date(x, origin="1899-12-30"))
		} else {
			x <- trimws(as.character(x))
			x[x %in% c("", "0", "NA")] <- NA_character_
			out <- rep(NA_character_, length(x))
			iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
			out[iso] <- substr(x[iso], 1, 10)
		}
		out[!is.na(out) & (out < "2014-01-01" | out > "2018-01-01")] <- NA_character_
		out
	}

	place <- function(x) {
		low <- tolower(trimws(as.character(x)))
		low[low %in% c("lakkhhitari", "lakkhatari", "lakkhiitari", "lakkhitari")] <- "lakkhitari"
		low[low %in% c("mohonpur", "mohanpur")] <- "mohanpur"
		carobiner::fix_name(low, "title")
	}

	prep_from <- function(x) {
		x <- toupper(trimws(as.character(x)))
		out <- rep(NA_character_, length(x))
		out[x == "CT"] <- "conventional"
		out[x == "ZT"] <- "none"
		out[x == "ST"] <- "strip tillage"
		out[x == "BP"] <- "raised beds"
		out
	}

	rotation <- function(x) {
		x <- trimws(as.character(x))
		c(
			"R-M" = "rice;maize",
			"R-W" = "rice;wheat",
			"R-W-J" = "rice;wheat;jute",
			"R-Mu-Boro" = "rice;mustard;rice",
			"R-Mu-M" = "rice;mustard;maize",
			"R-P-M" = "rice;potato;maize"
		)[x]
	}

	take <- function(df, idx, nms) {
		n <- nms[nms %in% names(df)]
		if (length(n) == 0) return(rep(NA_real_, length(idx)))
		as.numeric(df[[n[1]]][idx])
	}

	col_like <- function(df, pattern) {
		n <- grep(pattern, names(df), value=TRUE)
		if (length(n) == 0) return(rep(NA_real_, nrow(df)))
		as.numeric(df[[n[1]]])
	}

	col_date <- function(df, pattern) {
		n <- grep(pattern, names(df), value=TRUE)
		if (length(n) == 0) return(rep(NA_character_, nrow(df)))
		as_date(df[[n[1]]])
	}

	fieldbook <- function(f) {
		r1 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
		r2 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="14 - Grain Harvest ", skip=4, hdr=2))
		r3 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
		r4 <- as.data.frame(carobiner::read.excel.hdr(f, sheet="2 - Site information", skip=5, hdr=3))
		r1 <- r1[!is.na(r1$Tmnt) & !is.na(r1$Crop), ]
		r2 <- r2[!is.na(r2$Tmnt), ]
		r3 <- r3[!is.na(r3$Tmnt), ]
		key <- function(x) paste(x$Site.No, x$Tmnt)
		ig <- match(key(r1), key(r2))
		ifr <- match(key(r1), key(r3))
		sk <- paste(r4$Site.No.Unique.farmer.ID, r4$Tmnt.Short.abbreviation.as.mentioned.in.protocol)
		isite <- match(key(r1), sk)

		K <- take(r3, ifr, "K2O.kg.ha")
		k_named <- take(r3, ifr, "K.kg.ha")
		K <- ifelse(is.na(K), k_named, K) / 1.2051
		sp <- as.character(r1$Row.spacing.cm)
		sp[sp %in% c("0")] <- NA
		hi <- take(r2, ig, "HI")
		hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
		loc <- place(r1$Node)
		crop <- tolower(trimws(r1$Crop))
		prep <- prep_from(r1$Tmnt)
		hd <- rep(NA_character_, nrow(r1))
		if ("Datw.of.harvest.dd.mm.yy" %in% names(r1)) hd <- as_date(r1$Datw.of.harvest.dd.mm.yy)
		if ("Date.of.harvest.dd.mm.yy" %in% names(r1)) {
			alt <- as_date(r1$Date.of.harvest.dd.mm.yy)
			hd <- ifelse(is.na(hd), alt, hd)
		}

		data.frame(
			country = "Bangladesh",
			adm2 = "Rangpur",
			location = loc,
			latitude = NA_real_,
			longitude = NA_real_,
			geo_from_source = FALSE,
			trial_id = paste(r1$Year, loc, crop),
			plot_id = paste(r1$Year, loc, r1$Site.No, r1$Tmnt),
			treatment = prep,
			land_prep_method = prep,
			crop = crop,
			variety = trimws(as.character(r1$Variety)),
			intercrops = NA_character_,
			intercropped = FALSE,
			crop_rotation = unname(rotation(r1$Cropping.System)),
			season = "rabi",
			yield_part = "grain",
			planting_date = as_date(r1$Date.of.seeding.dd.mm.yy),
			harvest_date = hd,
			yield = take(r2, ig, c("Calculation_Grain.yield.t.ha", "Grain.yield.t.ha")) * 1000,
			yield_moisture = NA_real_,
			seed_weight = take(r2, ig, "TGW.g"),
			dmy_residue = NA_real_,
			dmy_total = NA_real_,
			dmy_stems = NA_real_,
			biomass_kg_ha_ = take(r2, ig, "Biomass.t.ha") * 1000,
			harvest_index = hi,
			N_fertilizer = take(r3, ifr, "N.kg.ha"),
			P_fertilizer = take(r3, ifr, "P2O5.kg.ha") / 2.29,
			K_fertilizer = K,
			S_fertilizer = take(r3, ifr, "S.kg.ha"),
			Zn_fertilizer = take(r3, ifr, "Zn.kg.ha"),
			gypsum = take(r3, ifr, c("Gypsum.Kg.ha", "Gypsum.kg.ha")),
			row_spacing = as.numeric(sp),
			seed_rate = take(r1, seq_len(nrow(r1)), c("Seedrate.kg.ha", "Seed.rate")),
			plot_area = as.numeric(r1$Plot.size.m2),
			soil_type = as.character(r4$Soil.texture.sand.silt.clay.etc[isite]),
			irrigation_number = NA_integer_,
			irrigated = NA,
			on_farm = TRUE,
			is_survey = FALSE
		)
	}

	# Where S.kg.ha is already stored, the gypsum column is the product and is not kept.
	drop_gypsum_if_s <- function(d) {
		d$gypsum[!is.na(d$S_fertilizer)] <- NA_real_
		d
	}

	validation <- function(f) {
		sheets <- readxl::excel_sheets(f)
		sheets <- sheets[sheets != "Rjshahi"]
		one <- function(s) {
			r <- as.data.frame(carobiner::read.excel(f, sheet=s))
			r <- r[!is.na(r$Crop) & trimws(as.character(r$Crop)) != "", ]
			lat <- col_like(r, "^Latitude$")
			lon <- col_like(r, "^Longitude$")
			swap <- !is.na(lat) & !is.na(lon) & lat > 40 & lon < 40
			lat2 <- lat
			lat2[swap] <- lon[swap]
			lon[swap] <- lat[swap]
			lat <- lat2

			crop_raw <- trimws(as.character(r$Crop))
			crop <- tolower(crop_raw)
			other <- rep(NA_character_, nrow(r))
			both <- grepl("&", crop_raw)
			if (any(both)) {
				bits <- strsplit(crop_raw[both], "&")
				crop[both] <- tolower(trimws(vapply(bits, `[`, "", 1)))
				other[both] <- tolower(trimws(vapply(bits, `[`, "", 2)))
			}
			crop[crop == "rice"] <- "rice"
			part <- ifelse(crop == "mustard", "seed", ifelse(crop == "jute", "fiber", "grain"))
			prep <- if ("Tillage" %in% names(r)) prep_from(r$Tillage) else rep(NA_character_, nrow(r))
			seas <- tolower(trimws(as.character(r$Season)))
			seas[grepl("kharif|khaif", seas)] <- "kharif"
			seas[seas == "rabi"] <- "rabi"
			seas[seas == "boro"] <- "boro"

			dose <- r[["Dose of fertilizer (N-P-K-S-Zn, kg/ha)/33 deci"]]
			parts <- strsplit(as.character(dose), "-", fixed=TRUE)
			grab <- function(i) vapply(parts, function(z) {
				if (length(z) < i || is.na(z[i]) || !nzchar(z[i])) return(NA_real_)
				as.numeric(z[i])
			}, numeric(1))

			y <- col_like(r, "^(Grain yield|Jute fibre yield) \\(t/ha\\)$")
			area <- col_like(r, "^Sampling area")
			kg <- col_like(r, "yield or")
			y <- ifelse(is.na(y), kg / area * 10, y)
			hi <- col_like(r, "^HI$")
			hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
			sown <- col_date(r, "Date of sowing")
			planted <- col_date(r, "transplanting")
			planted <- ifelse(is.na(planted), sown, planted)
			loc <- place(r[["Site/Location/Node"]])
			n_irri <- as.integer(col_like(r, "^Number of irrigation"))

			data.frame(
				country = "Bangladesh",
				adm2 = carobiner::fix_name(r$District, "title"),
				location = loc,
				latitude = lat,
				longitude = lon,
				geo_from_source = !is.na(lat) & !is.na(lon),
				trial_id = paste(r$Year, seas, loc, crop),
				plot_id = paste(s, loc, seq_len(nrow(r))),
				treatment = ifelse(is.na(prep), paste(crop, other), prep),
				land_prep_method = prep,
				crop = crop,
				variety = trimws(as.character(r$Variety)),
				intercrops = other,
				intercropped = !is.na(other),
				crop_rotation = unname(rotation(r[["Cropping pattern"]])),
				season = seas,
				yield_part = part,
				planting_date = planted,
				harvest_date = col_date(r, "^Date of harvesting"),
				yield = y * 1000,
				yield_moisture = col_like(r, "^Moisture"),
				seed_weight = NA_real_,
				dmy_residue = col_like(r, "^Straw yield \\(t/ha\\)") * 1000,
				dmy_total = col_like(r, "^Biomass \\(t/ha\\)") * 1000,
				dmy_stems = col_like(r, "Jute stick yield \\(t/ha\\)") * 1000,
				biomass_kg_ha_ = NA_real_,
				harvest_index = hi,
				N_fertilizer = grab(1),
				P_fertilizer = grab(2),
				K_fertilizer = grab(3),
				S_fertilizer = grab(4),
				Zn_fertilizer = grab(5),
				gypsum = NA_real_,
				row_spacing = NA_real_,
				seed_rate = NA_real_,
				plot_area = NA_real_,
				soil_type = NA_character_,
				irrigation_number = n_irri,
				irrigated = !is.na(n_irri) & n_irri > 0,
				on_farm = TRUE,
				is_survey = FALSE
			)
		}
		do.call(rbind, lapply(sheets, one))
	}

	f1 <- ff[basename(ff) %in% c(
		"Rabi 2016-17-validation trials-all nodes-Rangpur.xlsx",
		"Rabi maize 2015-16-DS&BP-all nodes-Rangpur.xlsx",
		"Rabi maize 2016-17-DS&BP-all nodes-Rangpur.xlsx",
		"Wheat 2014-15-DS&BP-all nodes Rangpur.xlsx",
		"Wheat 2015-16-DS&BP-all nodes-Rangpur.xlsx"
	)]
	val <- f1[grepl("validation", basename(f1), ignore.case=TRUE)]
	f1 <- f1[!grepl("validation", basename(f1), ignore.case=TRUE)]

	d <- lapply(f1, fieldbook)
	d <- lapply(d, drop_gypsum_if_s)
	d <- do.call(rbind, c(d, list(validation(val))))
	d$record_id <- as.integer(seq_len(nrow(d)))
	carobiner::write_files(path, meta, d)
}
