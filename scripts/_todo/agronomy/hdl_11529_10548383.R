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
	ph <- as.data.frame(carobiner::read.excel.hdr(f1, sheet="4- Stand counts & Phenology", skip=4, hdr=2))
	gh <- as.data.frame(carobiner::read.excel.hdr(f1, sheet="14 - Grain Harvest ", skip=4, hdr=2))
	ft <- as.data.frame(carobiner::read.excel.hdr(f1, sheet="6 - Fertilizer amounts ", skip=4, hdr=2))
	ir <- as.data.frame(carobiner::read.excel.hdr(f1, sheet="7 - Irrigation ", skip=4, hdr=2))
	ph <- ph[!is.na(ph$Year) & !is.na(ph$Crop), ]
	gh <- gh[!is.na(gh$Year), ]
	ft <- ft[!is.na(ft$Year), ]

	key <- function(x) {
		paste(
			trimws(as.character(x$Year)),
			trimws(as.character(x$Node)),
			trimws(as.character(x$Site.No)),
			trimws(toupper(as.character(x$Farmer.s.name))),
			trimws(toupper(as.character(x$Tmnt)))
		)
	}
	ig <- match(key(ph), key(gh))
	ifr <- match(key(ph), key(ft))

	prod <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA_character_
		x[toupper(x) == "MURIATE"] <- "KCl"
		x[toupper(x) == "UREA"] <- "urea"
		x[toupper(x) == "DAP"] <- "DAP"
		x[toupper(x) == "ZINC SULPHATE"] <- "ZnSO4"
		x
	}
	p1 <- prod(ft$Rabi_Fertilizer.1.Application_Product.used)[ifr]
	p2 <- prod(ft$Fertilizer.2.Application_Product.used)[ifr]
	p3 <- prod(ft$Fertilizer.3.Application_Product.used)[ifr]
	p4 <- prod(ft$Fertilizer.4.Application_Product.used)[ifr]
	p5 <- prod(ft$Fertilizer.5.Application_Product.used)[ifr]
	fertilizer_type <- apply(cbind(p1, p2, p3, p4, p5), 1, function(z) {
		z <- unique(z[!is.na(z)])
		if (length(z) == 0) NA_character_ else paste(z, collapse=";")
	})

	yn <- grep("Year$", names(ir), value=TRUE)[1]
	ir <- ir[!is.na(ir[[yn]]), ]
	ir$Year <- ir[[yn]]
	datecols <- grep("Date.of.irrigation", names(ir), value=TRUE)
	ir$irrigated <- FALSE
	for (dc in datecols) {
		ir$irrigated <- ir$irrigated | !is.na(ir[[dc]])
	}
	ii <- match(key(ph), key(ir))

	variety <- trimws(as.character(ph$Variety))
	variety[toupper(variety) == "LOCAL"] <- "local"

	d1 <- data.frame(
		crop = crop_name(ph$Crop),
		variety = variety,
		treatment = trimws(as.character(ph$Cropping.System)),
		crop_rotation = rotation(ph$Cropping.System),
		location = carobiner::fix_name(ph$Node, "title"),
		plot_id = trimws(as.character(ph$Site.No)),
		rep = as.integer(as.numeric(ph$Site.No)),
		planting_date = as_date(ph$Date.of.seeding.dd.mm.yy),
		harvest_date = as_date(ph$Datw.of.harvest.dd.mm.yy),
		yield = as.numeric(gh$GY.ton.ha[ig]) * 1000,
		seed_weight = as.numeric(gh$TGW[ig]),
		N_fertilizer = as.numeric(ft$N.kg.ha[ifr]),
		P_fertilizer = as.numeric(ft$P.kg.ha[ifr]) / 2.29,
		K_fertilizer = as.numeric(ft$K.kg.ha[ifr]) / 1.2051,
		fertilizer_type = fertilizer_type,
		irrigated = ir$irrigated[ii],
		season_label = trimws(as.character(ph$Year))
	)

	f2 <- ff[basename(ff) == "Rabi crops ACST 2016-17-All node-Malda.xlsx"]
	cp <- carobiner::read.excel(f2, sheet="Crop Parameter", fix_names=TRUE)
	cp <- cp[!is.na(cp$Crop), ]
	cm <- carobiner::read.excel(f2, sheet="Crop Management", fix_names=TRUE)
	cm <- cm[!is.na(cm$Crop), ]
	st <- carobiner::read.excel(f2, sheet="stat", fix_names=TRUE)
	st <- st[!is.na(st$year), ]
	k2 <- paste(trimws(toupper(as.character(cp$Farmer.s.name))), trimws(toupper(as.character(cp$Tmnt))), trimws(as.character(cp$Year)))
	ks <- paste(trimws(toupper(as.character(st$far))), trimws(toupper(as.character(st$sys))), trimws(as.character(st$year)))
	isn <- match(k2, ks)
	kcm <- paste(trimws(toupper(as.character(cm$Farmer.s.name))), trimws(toupper(as.character(cm$Tmnt))), trimws(as.character(cm$Year)), tolower(trimws(as.character(cm$Crop))))
	kcp <- paste(k2, tolower(trimws(as.character(cp$Crop))))
	icm <- match(kcp, kcm)

	# Crop Management stores harvest day as an Excel serial in a text column.
	# 29-02-2017 is not a real date.
	doh <- trimws(as.character(cm$DOH))
	doh[doh == "29-02-2017"] <- NA_character_
	variety2 <- trimws(as.character(cp$Variety))
	variety2[toupper(variety2) == "LOCAL"] <- "local"

	d2 <- data.frame(
		crop = crop_name(cp$Crop),
		variety = variety2,
		treatment = trimws(as.character(cp$Tmnt)),
		crop_rotation = rotation(cp$Tmnt),
		location = carobiner::fix_name(st$Node[isn], "title"),
		plot_id = trimws(as.character(cp$Site.No)),
		rep = as.integer(as.numeric(cp$Site.No)),
		planting_date = as_date(cp$DOS),
		harvest_date = as_date(as.numeric(doh[icm])),
		yield = as.numeric(cp$Yield.q.ha) * 100,
		seed_weight = as.numeric(NA),
		N_fertilizer = as.numeric(NA),
		P_fertilizer = as.numeric(NA),
		K_fertilizer = as.numeric(NA),
		fertilizer_type = as.character(NA),
		irrigated = as.logical(NA),
		season_label = trimws(as.character(cp$Year)),
		row_spacing = as.numeric(cp$Spacing.m) * 100,
		plot_area = as.numeric(cm$Plot.size.m2[icm])
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
