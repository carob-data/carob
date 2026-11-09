# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm alternative cropping-system trial at Malda, West Bengal. The grain-harvest sheet header says Country: WB (India) and District: Malda.
# Latitude and longitude columns on the site sheet are empty.
# In the 2015-16 fertilizer sheet, N.kg.ha matches urea and DAP nitrogen. P.kg.ha equals DAP times 0.46 and K.kg.ha equals MOP times 0.60, so those two columns are oxides and are converted to elemental P and K.
# Zinc sulphate is recorded, and the zinc column repeats the product rate. The hydrate is not stated, so zinc is not stored.
# 2016-17 grain yield is quintals per hectare.

## ISSUES
# Coordinates are not in the files.
# 2016-17 fertilizer, pesticide, and irrigation blocks are repeated across the sheet and are not read.
# Kharif and pre-kharif yields on the 2016-17 stat sheet are not in the rabi crop rows.
# Biomass is not labeled fresh or dry. One 2016-17 lentil harvest serial is outside 2014-2018 and is not stored.
# One harvest cell is the text 29-02-2017. 2017 is not a leap year, so that date is not stored.
# Plot size in 2016-17 is 666.5 m2.
# write_files: missing variables: yield_moisture, latitude, longitude. out of bounds: plot_area (666.5, 666.5). invalid terms: crop: black cumin. invalid terms: crop_rotation: black cumin.

## NOT PROCESSED
# 2015-16: rainfall, land preparation, crop management operations, irrigation amounts, ponding, pesticides, weeds, biomass, soil analysis, prices.
# 2016-17: Crop Management, Land Preparation, Summary costs, pivot table, and the stat sheet except the node name.
# Zinc sulphate rate.


carob_script <- function(path) {

"
4.6-Rabi (winter) crops-all nodes-Alternative cropping systems trial -Malda-West Bengal

Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory alternative cropping systems trials were conducted comparing to exiting systems to find out suitable and more profitable cropping systems, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitored and acquired data. Where possible, collaborating farmers were selected by the community, and the project worked with already existed farmer groups, with groups of both men and women farmers.
"

	uri <- "hdl:11529/10548383"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = "SRFSI alternative cropping systems, Malda",
		design = NA,
		data_type = "experiment",
		treatment_vars = "crop_rotation",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.6
	)

	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) {
			y <- as.character(as.Date(x))
		} else if (is.numeric(x)) {
			y <- as.character(as.Date(x, origin="1899-12-30"))
		} else {
			y <- trimws(as.character(x))
			y[y %in% c("", "NA")] <- NA_character_
		}
		yr <- as.integer(substr(y, 1, 4))
		y[!is.na(yr) & (yr < 2014 | yr > 2018)] <- NA_character_
		y
	}

	crop_name <- function(x) {
		x <- tolower(trimws(as.character(x)))
		x[x %in% c("balck cumin", "black cumin")] <- "black cumin"
		x[x == "corriander"] <- "coriander"
		x
	}

	rotation <- function(x) {
		x <- trimws(as.character(x))
		m <- c(
			"R-W-Mun" = "rice;wheat;mung bean",
			"R-L-Mun" = "rice;lentil;mung bean",
			"R-C-Mun" = "rice;coriander;mung bean",
			"R-BC-Mun" = "rice;black cumin;mung bean",
			"R-L-R" = "rice;lentil;rice",
			"BG-W-J" = "black gram;wheat;jute",
			"BG-L-J" = "black gram;lentil;jute",
			"Rice-Wheat" = "rice;wheat",
			"RICE- WHEAT" = "rice;wheat",
			"RICE-LENTIL-MUNGBEAN" = "rice;lentil;mung bean",
			"RICE-LENTIL-MUNG BEAN" = "rice;lentil;mung bean",
			"Rice-Lentil-Mungbean" = "rice;lentil;mung bean",
			"RICE-CORRIANDER" = "rice;coriander;mung bean",
			"Rice-Coriander" = "rice;coriander",
			"RICE-BLACK CUMIN" = "rice;black cumin;mung bean",
			"Rice-Black Cumin" = "rice;black cumin",
			"RICE-MUSTARD-BORO RICE" = "rice;mustard;rice",
			"Rice-Mustard-Boro" = "rice;mustard;rice",
			"BLACK GRAM-WHEAT-JUTE" = "black gram;wheat;jute",
			"BLACK GRAM-LENTIL-JUTE" = "black gram;lentil;jute"
		)
		out <- unname(m[x])
		miss <- is.na(out)
		out[miss] <- x[miss]
		out
	}

	f1 <- ff[basename(ff) == "Rabi crops  2015-16-ACST-All node -Malda.xlsx"]
	r1 <- as.data.frame(carobiner::read.excel.hdr(f1, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
	r2 <- as.data.frame(carobiner::read.excel.hdr(f1, sheet="14 - Grain Harvest ", skip=4, hdr=2))
	r3 <- as.data.frame(carobiner::read.excel.hdr(f1, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
	r4 <- as.data.frame(carobiner::read.excel.hdr(f1, sheet="7 - Irrigation ", skip=4, hdr=2))
	r1 <- r1[!is.na(r1$Year) & !is.na(r1$Crop), ]
	r2 <- r2[!is.na(r2$Year), ]
	r3 <- r3[!is.na(r3$Year), ]

	key <- function(x) {
		paste(
			trimws(as.character(x$Year)),
			trimws(as.character(x$Node)),
			trimws(as.character(x$Site.No)),
			trimws(toupper(as.character(x$Farmer.s.name))),
			trimws(toupper(as.character(x$Tmnt)))
		)
	}
	ig <- match(key(r1), key(r2))
	ifr <- match(key(r1), key(r3))

	prod <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA_character_
		x[toupper(x) == "MURIATE"] <- "KCl"
		x[toupper(x) == "UREA"] <- "urea"
		x[toupper(x) == "DAP"] <- "DAP"
		x[toupper(x) == "ZINC SULPHATE"] <- "ZnSO4"
		x
	}
	p1 <- prod(r3$Rabi_Fertilizer.1.Application_Product.used)[ifr]
	p2 <- prod(r3$Fertilizer.2.Application_Product.used)[ifr]
	p3 <- prod(r3$Fertilizer.3.Application_Product.used)[ifr]
	p4 <- prod(r3$Fertilizer.4.Application_Product.used)[ifr]
	p5 <- prod(r3$Fertilizer.5.Application_Product.used)[ifr]
	fertilizer_type <- apply(cbind(p1, p2, p3, p4, p5), 1, function(z) {
		z <- unique(z[!is.na(z)])
		if (length(z) == 0) NA_character_ else paste(z, collapse=";")
	})

	yn <- grep("Year$", names(r4), value=TRUE)[1]
	r4 <- r4[!is.na(r4[[yn]]), ]
	r4$Year <- r4[[yn]]
	datecols <- grep("Date.of.irrigation", names(r4), value=TRUE)
	r4$irrigated <- FALSE
	for (dc in datecols) {
		r4$irrigated <- r4$irrigated | !is.na(r4[[dc]])
	}
	ii <- match(key(r1), key(r4))

	variety <- trimws(as.character(r1$Variety))
	variety[toupper(variety) == "LOCAL"] <- "local"

	d1 <- data.frame(
		crop = crop_name(r1$Crop),
		variety = variety,
		treatment = trimws(as.character(r1$Cropping.System)),
		crop_rotation = rotation(r1$Cropping.System),
		location = carobiner::fix_name(r1$Node, "title"),
		plot_id = trimws(as.character(r1$Site.No)),
		rep = as.integer(as.numeric(r1$Site.No)),
		planting_date = as_date(r1$Date.of.seeding.dd.mm.yy),
		harvest_date = as_date(r1$Datw.of.harvest.dd.mm.yy),
		yield = as.numeric(r2$GY.ton.ha[ig]) * 1000,
		seed_weight = as.numeric(r2$TGW[ig]),
		N_fertilizer = as.numeric(r3$N.kg.ha[ifr]),
		P_fertilizer = as.numeric(r3$P.kg.ha[ifr]) / 2.29,
		K_fertilizer = as.numeric(r3$K.kg.ha[ifr]) / 1.2051,
		fertilizer_type = fertilizer_type,
		irrigated = r4$irrigated[ii],
		season_label = trimws(as.character(r1$Year))
	)

	f2 <- ff[basename(ff) == "Rabi crops ACST 2016-17-All node-Malda.xlsx"]
	r5 <- carobiner::read.excel(f2, sheet="Crop Parameter", fix_names=TRUE)
	r5 <- r5[!is.na(r5$Crop), ]
	r6 <- carobiner::read.excel(f2, sheet="Crop Management", fix_names=TRUE)
	r6 <- r6[!is.na(r6$Crop), ]
	r7 <- carobiner::read.excel(f2, sheet="stat", fix_names=TRUE)
	r7 <- r7[!is.na(r7$year), ]
	k2 <- paste(trimws(toupper(as.character(r5$Farmer.s.name))), trimws(toupper(as.character(r5$Tmnt))), trimws(as.character(r5$Year)))
	ks <- paste(trimws(toupper(as.character(r7$far))), trimws(toupper(as.character(r7$sys))), trimws(as.character(r7$year)))
	isn <- match(k2, ks)
	kcm <- paste(trimws(toupper(as.character(r6$Farmer.s.name))), trimws(toupper(as.character(r6$Tmnt))), trimws(as.character(r6$Year)), tolower(trimws(as.character(r6$Crop))))
	kcp <- paste(k2, tolower(trimws(as.character(r5$Crop))))
	icm <- match(kcp, kcm)

	# Crop Management stores harvest day as an Excel serial in a text column.
	# 29-02-2017 is not a real date.
	doh <- trimws(as.character(r6$DOH))
	doh[doh == "29-02-2017"] <- NA_character_
	variety2 <- trimws(as.character(r5$Variety))
	variety2[toupper(variety2) == "LOCAL"] <- "local"

	d2 <- data.frame(
		crop = crop_name(r5$Crop),
		variety = variety2,
		treatment = trimws(as.character(r5$Tmnt)),
		crop_rotation = rotation(r5$Tmnt),
		location = carobiner::fix_name(r7$Node[isn], "title"),
		plot_id = trimws(as.character(r5$Site.No)),
		rep = as.integer(as.numeric(r5$Site.No)),
		planting_date = as_date(r5$DOS),
		harvest_date = as_date(as.numeric(doh[icm])),
		yield = as.numeric(r5$Yield.q.ha) * 100,
		seed_weight = as.numeric(NA),
		N_fertilizer = as.numeric(NA),
		P_fertilizer = as.numeric(NA),
		K_fertilizer = as.numeric(NA),
		fertilizer_type = as.character(NA),
		irrigated = as.logical(NA),
		season_label = trimws(as.character(r5$Year)),
		row_spacing = as.numeric(r5$Spacing.m) * 100,
		plot_area = as.numeric(r6$Plot.size.m2[icm])
	)

	d <- carobiner::bindr(d1, d2)
	d$country <- "India"
	d$adm1 <- "West Bengal"
	d$adm2 <- "Malda"
	d$trial_id <- as.character(as.integer(as.factor(paste(d$location, d$season_label))))
	d$season_label <- NULL
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE
	d$yield_part <- ifelse(d$crop %in% c("wheat", "rice"), "grain", "seed")
	d$yield_part[d$crop == "black cumin"] <- "seed"

	carobiner::write_files(path, meta, d)
}
