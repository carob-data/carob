# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI farm survey of 60 rice farmers in the Upper Pampanga River Irrigation System (UPRIIS), Nueva Ecija, Philippines, dry season 2000.
# Files are Lotus WK1 exports with multi-row headers; the row with the variable codes (first cell "COUNTRY") is used as header.
# Records are household x parcel (HHCODE 1xx/2xx/3xx = upper/middle/lower part of the system, coding guide in the .doc).
# Production (PRODN.XLS, kg) is reported per disposal lot, so lots are summed by household x parcel; crop_price is the quantity-weighted mean price (PHP/kg).
# yield = production / AREAPLT (PARCPLOT.XLS, area planted, ha).
# Seed (QSEEDTP, kg) divided by AREAPLT to get seed_rate.
# Method codes: TPR transplanted, DDS dry direct seeded, WDS wet direct seeded; mixed codes (TPR/DDS etc.) kept as "transplanting;direct seeding".
# Water source codes from the coding guide: 1 canal gravity, 2 canal pumping, 3 shallow tubewell pumping, 4 river/creek pumping, 5 others.
# Household head age, years of education, household size and total cultivated area from DEMOG.xlsx.

## ISSUES
# Area basis of yield is uncertain: AREAPLT in PARCPLOT often equals the total cultivated area of the farm (DEMOG TCULAREA) and differs from the parcel area in FARMCHAR (e.g. HH 102: 15.1 vs 3 ha; HH 114: 5 vs 0.5 ha). Yields range 521-8521 kg/ha. Check which area the production refers to.
# Fertilizer (CHEMFERT.XLS) is mostly in bags; bag mass is not documented, so N/P/K are not computed. BRAND holds grades (14-14-14, 16-20, 18-46, 0-0-60, ...) and foliar products (CropGiant, Agruel, Wocosim, in liters). Only fertilizer_type is set; human must decide bag mass and add nutrient totals. Application timing (DAT/DAS/DAB) is not processed.
# fertilizer_type values "16-20-0", "0-17-0", "17-0-0", "20-0-0", "CropGiant", "Agruel", "Wocosim" are kept as given (not in vocabulary).
# No coordinates. Locations: province Nueva Ecija (adm1), municipalities Talavera, Sto. Domingo, Muñoz (GADM "Science City of Muñoz"), San Jose (adm2), sitio/barangay names in location. HH 318 has no village. Human must georeference.
# Planting/establishment dates (CEDATE) are empty; planting_date not set. Season is DS 2000 (one CROPESTA row says 2001).
# Not mapped: H2OSKED1/2 (irrigation event schedules), IRRIG fees and water delivery, land preparation files (LP*), HANDWEED, HERB, INS, MOLL, RODENT, PESTCTRL, HAULDRCL, LABORUSE, INCOME, OTNUTINP, FARMCHAR (tenure codes undocumented, wet season variety and non-rice crops), labour columns in all files.
# write_files: missing longitude/latitude/geo_from_source; invalid fertilizer_type terms listed above; NA yield for HH 210 (no AREAPLT in PARCPLOT; FARMCHAR gives 1.06 ha); new variables.

## NEW VARIABLES
# field_position_: location of the farm along the lateral (Upper, Middle, Lower)
# distance_lateral_: distance of the parcel from the irrigation lateral (m)
# education_years_: years of education of the household head


carob_script <- function(path) {

"
The Effects of Pumping on Water Use and Profitability in dry season rice:  A case study in Upper Pampanga River Irrigation System (UPRIIS), Philippines

To compare economic performance of rice production and water management practices of farmers in different water delivery systems in UPRIIS.
"

	uri <- "doi:10.7910/DVN/26298"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 35,
		carob_effort = 0.8
	)

	read_wk1 <- function(name) {
		r <- carobiner::read.excel(ff[basename(ff) == name], col_names=FALSE)
		h <- which(r[[1]] == "COUNTRY")[1]
		nms <- as.character(unlist(r[h, ]))
		nms[is.na(nms)] <- paste0("V", which(is.na(nms)))
		x <- r[-(1:h), ]
		names(x) <- make.unique(nms)
		x <- x[!is.na(x[[1]]) & x[[1]] != "-", ]
		x$id <- paste(x$HHCODE, x$PARNO)
		x
	}

	r1 <- read_wk1("PARCPLOT.XLS")
	r2 <- read_wk1("PRODN.XLS")
	r3 <- read_wk1("CROPESTA.XLS")
	r4 <- read_wk1("HARVTHRE.XLS")
	r5 <- read_wk1("IRRIG.XLS")
	r6 <- read_wk1("CHEMFERT.XLS")
	r7 <- read_wk1("DEMOG.xlsx")

	r2$PRODN <- as.numeric(r2$PRODN)
	r2$PRICE <- as.numeric(r2$PRICE)
	prod <- tapply(r2$PRODN, r2$id, sum)
	price <- tapply(r2$PRODN * r2$PRICE, r2$id, sum) / prod

	meth <- r3$METHOD
	meth <- gsub("TPR", "transplanting", meth)
	meth <- gsub("DDS|WDS", "direct seeding", meth)
	meth <- gsub("/", ";", meth)

	i3 <- match(r1$id, r3$id)
	i4 <- match(r1$id, r4$id)
	i5 <- match(r1$id, r5$id)
	i7 <- match(r1$HHCODE, r7$HHCODE)
	src <- c("canal (gravity)", "canal (pumping)", "shallow tubewell (pumping)", "river or creek (pumping)", "other")

	d <- data.frame(
		hhid = r1$HHCODE,
		field_id = r1$PARNO,
		country = "Philippines",
		adm1 = r1$PROVINCE,
		adm2 = r1$VILLAGE,
		location = r1$SITIO,
		season = ifelse(r1$SEASON == "DS", "dry", NA),
		crop = "rice",
		field_size = as.numeric(r1$AREAPLT),
		variety = gsub(", ", ";", gsub(",", ";", r3$VARIETY[i3])),
		planting_method = meth[i3],
		seed_rate = as.numeric(r3$QSEEDTP[i3]) / as.numeric(r1$AREAPLT),
		transplanting_days = as.numeric(r3$SEEDAGE[i3]),
		harvest_date = as.character(as.Date(as.numeric(r4$HDATE[i4]), origin="1899-12-30")),
		irrigated = TRUE,
		irrigation_source = src[as.integer(r5$SOURCEWAT[i5])],
		field_position_ = r5$POSITION[i5],
		distance_lateral_ = as.numeric(r5$DISTLAT[i5]),
		yield = as.vector(prod[r1$id]) / as.numeric(r1$AREAPLT),
		crop_price = as.vector(price[r1$id]),
		currency = "PHP",
		age = as.numeric(r7$HHAGE[i7]),
		education_years_ = as.numeric(r7$V12[i7]),
		hh_size = as.numeric(r7$HHSIZE[i7]),
		cropland = as.numeric(r7$TCULAREA[i7])
	)

	ftype <- trimws(r6$BRAND)
	ftype[ftype %in% c("Urea", "46-0-0")] <- "urea"
	ftype[ftype %in% c("14-14-14", "12-12-12", "17-0-17", "17-017", "11-7-11")] <- "NPK"
	ftype[ftype == "18-46"] <- "DAP"
	ftype[ftype == "0-0-60"] <- "KCl"
	ftype[ftype == "16-20"] <- "16-20-0"
	fts <- tapply(ftype, r6$id, function(x) paste(sort(unique(x[!is.na(x)])), collapse="; "))
	d$fertilizer_type <- as.vector(fts[r1$id])
	d$fertilizer_type[d$fertilizer_type == ""] <- "none"
	d$fertilizer_used <- d$fertilizer_type != "none"

	d$trial_id <- paste(d$hhid, d$field_id, sep="-")
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
