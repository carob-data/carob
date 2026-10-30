# R script for "carob"

## NOTES
# Three Maricopa cotton experiments: FACE (CO2 x irrigation, 1989-91), AgIIS (N x water, 1999), FISE (irrigation method x density x N, 2002-03).
# Lint yield at 0% moisture used as yield (kg/ha). Replicate 0 rows are treatment means and were dropped.
# Coordinates 33.0628, -111.9826 from FACE Treatment_Metadata. Extra package: readODS.

## ISSUES
# Requires readODS.
# Weather+Maricopa+AZMET.ods not processed (wth=). In-season growth, ET, reflectance, canopy temperature not processed.
# FACE harvest dates not in Plant_summary; harvest_date left missing except FISE (DATE column) and AgIIS (1999-11-12 from management notes).
# Irrigation amounts not summed from Irrigation sheets (AgIIS 929 vs 993 mm in metadata not mapped).
# FACE seed (GWAH) not stored separately; yield is lint (LIWAH).
# FISE N includes ~2 ppm N in irrigation water as recorded in FEAMN.
# CO2_concentration_ is FACE-only (NA for AgIIS/FISE). All plots irrigated so irrigated is not a treatment contrast.
# write_files: UofAZ not in org vocab; fertilizer_type mapped to urea (UAN in source).

## NEW VARIABLES
# CO2_concentration_: daytime canopy CO2 (µmol/mol) in FACE treatments

carob_script <- function(path) {

"
Cotton Response to CO2, Water, Nitrogen and Plant Density - A Repository of FACE, AgIIS and FISE Experiment Data

Several cotton experiments have been conducted at the University of Arizona’s Maricopa Agricultural Center from which datasets have been obtained documenting cotton responses to elevated CO2 concentrations, water supply, nitrogen fertilizer, and planting density. In particular, these experiments included FACE (free-air CO2 enrichment; CO2, water; 10 treatment-years), AgIIS (Agricultural Irrigation Imaging System, pronounced Ag Eyes; nitrogen fertilizer, water supply; 4 treatment-years), and FISE (FAO-56 Irrigation Scheduling Experiments; irrigation scheduling method, planting density, nitrogen fertilizer; 24 treatment-years). Besides achieving the experimental objectives of determining cotton’s response to the several variables, as well as testing remote sensing techniques, the comprehensive datasets are suitable for validating plant growth models because they include weather, soils, management, growth, yield and other data.
"

	uri <- "doi:10.7910/DVN/ZGNU0O"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "USDA-ARS;UofAZ",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;seed_density;CO2_concentration_",
		response_vars = "yield",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 55,
		carob_effort = 2.5
	)

	bn <- basename(ff)
	fface <- ff[grepl("FACE\\+Cotton\\+Growth", bn)]
	fagiis <- ff[grepl("AgIIS\\+Cotton\\+Growth", bn)]
	ffise <- ff[grepl("FISE\\+Cotton\\+Growth", bn)]

	read_ods <- function(f, sheet, skip) {
		as.data.frame(readODS::read_ods(f, sheet=sheet, skip=skip, col_names=TRUE),
			stringsAsFactors=FALSE)
	}

	# FACE yield (skip description rows; header is TRNO, R#, ...)
	yf <- read_ods(fface, "Plant_summary", 2)
	yf <- yf[!is.na(yf$TRNO) & yf[["R#"]] != 0, ]
	tf <- read_ods(fface, "Treatment_Metadata", 2)
	tf <- tf[!is.na(tf$TRNO), ]
	pf <- read_ods(fface, "Plantings", 2)
	pf <- pf[!is.na(pf$TRNO), ]
	nf <- read_ods(fface, "Fertilizations", 2)
	nf <- nf[!is.na(nf$TRNO), ]
	nface <- aggregate(list(N_fertilizer=as.numeric(nf$FEAMN)), list(TRNO=nf$TRNO), sum, na.rm=TRUE)

	t1 <- tf[match(yf$TRNO, tf$TRNO), ]
	p1 <- pf[match(yf$TRNO, pf$TRNO), ]
	n1 <- nface$N_fertilizer[match(yf$TRNO, nface$TRNO)]

	d1 <- data.frame(
		trial_id = paste0("FACE_", yf$DAYR),
		rep = as.integer(yf[["R#"]]),
		treatment = t1$Trt_name,
		CO2_concentration_ = as.numeric(t1$CO2_day),
		irrigated = TRUE,
		N_fertilizer = n1,
		P_fertilizer = 0,
		K_fertilizer = 0,
		planting_date = as.character(p1$PDATE),
		emergence_date = as.character(p1$PLDOE),
		variety = p1$Cultivar_ID,
		seed_density = as.numeric(p1$PLPOP) * 10000,
		plant_density = as.numeric(p1$PLPOE) * 10000,
		row_spacing = as.numeric(p1$PLRS),
		yield = as.numeric(yf$LIWAH),
		dmy_total = as.numeric(yf$CWAH),
		LAI = as.numeric(yf$LAIX),
		latitude = as.numeric(t1$fl_lat),
		longitude = as.numeric(t1$fl_long),
		elevation = as.numeric(t1$flele),
		location = "Maricopa",
		site = t1$Site_name,
		country = "United States",
		crop = "cotton",
		yield_part = "lint",
		yield_moisture = 0,
		geo_from_source = TRUE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	# AgIIS 1999 (header is on row 4)
	ya <- read_ods(fagiis, "Plant_summary", 3)
	# drop means if R# present
	if ("R#" %in% names(ya)) ya <- ya[!is.na(ya$TRNO) & ya[["R#"]] != 0, ] else ya <- ya[!is.na(ya$TRNO), ]
	ta <- read_ods(fagiis, "Treatment_Metadata", 2)
	ta <- ta[!is.na(ta$TRNO), ]
	pa <- read_ods(fagiis, "Plantings", 2)
	pa <- pa[!is.na(pa$TRNO), ]
	na <- read_ods(fagiis, "Fertilizations", 2)
	na <- na[!is.na(na$TRNO), ]
	if ("R#" %in% names(na)) na <- na[na[["R#"]] != 0, ]
	nagi <- aggregate(list(N_fertilizer=as.numeric(na$FEAMN)),
		list(TRNO=na$TRNO, R=na[["R#"]]), sum, na.rm=TRUE)

	t2 <- ta[match(ya$TRNO, ta$TRNO), ]
	p2 <- pa[match(ya$TRNO, pa$TRNO), ]
	n2 <- nagi$N_fertilizer[match(paste(ya$TRNO, ya[["R#"]]), paste(nagi$TRNO, nagi$R))]
	lintcol <- intersect(c("LIWAH", "lint"), names(ya))
	if (length(lintcol) == 0) lintcol <- names(ya)[grepl("LIWAH|Lint", names(ya))][1]

	d2 <- data.frame(
		trial_id = "AgIIS_1999",
		rep = if ("R#" %in% names(ya)) as.integer(ya[["R#"]]) else 1L,
		treatment = if ("Trt_name" %in% names(t2)) t2$Trt_name else as.character(ya$TRNO),
		CO2_concentration_ = as.numeric(NA),
		irrigated = TRUE,
		N_fertilizer = n2,
		P_fertilizer = 0,
		K_fertilizer = 0,
		planting_date = as.character(p2$PDATE),
		emergence_date = as.character(p2$PLDOE),
		variety = p2$Cultivar_ID,
		seed_density = as.numeric(p2$PLPOP) * 10000,
		plant_density = as.numeric(p2$PLPOE) * 10000,
		row_spacing = as.numeric(p2$PLRS),
		yield = as.numeric(ya[[lintcol[1]]]),
		dmy_total = if ("CWAH" %in% names(ya)) as.numeric(ya$CWAH) else as.numeric(NA),
		LAI = if ("LAIX" %in% names(ya)) as.numeric(ya$LAIX) else as.numeric(NA),
		latitude = 33.0628,
		longitude = -111.9826,
		elevation = 361,
		location = "Maricopa",
		site = "Maricopa Agricultural Center",
		country = "United States",
		crop = "cotton",
		yield_part = "lint",
		yield_moisture = 0,
		geo_from_source = TRUE,
		on_farm = FALSE,
		is_survey = FALSE,
		harvest_date = "1999-11-12"
	)
	d1$harvest_date <- NA_character_

	# FISE
	ys <- read_ods(ffise, "Plant_Summary", 3)
	ys <- ys[!is.na(ys$TRNO), ]
	ts <- read_ods(ffise, "Treatment_Metadata", 2)
	ts <- ts[!is.na(ts$TRNO), ]
	ps <- read_ods(ffise, "Plantings", 2)
	ps <- ps[!is.na(ps$TRNO), ]
	ns <- read_ods(ffise, "Fertilizations", 2)
	ns <- ns[!is.na(ns$TRNO), ]
	# one row per irrigation/fert event x plot; sum by treatment and plot
	nfise <- aggregate(
		list(N_fertilizer=as.numeric(ns$FEAMN), P_fertilizer=as.numeric(ns$FEAMP)),
		list(TRNO=ns$TRNO, plot=ns[[3]]),
		sum, na.rm=TRUE
	)

	# unique treatments (drop plot-level duplicates in plantings/metadata)
	ts <- ts[!duplicated(ts$TRNO), ]
	ps <- ps[!duplicated(ps$TRNO), ]

	t3 <- ts[match(ys$TRNO, ts$TRNO), ]
	p3 <- ps[match(ys$TRNO, ps$TRNO), ]
	n3 <- nfise[match(paste(ys$TRNO, ys[[3]]), paste(nfise$TRNO, nfise$plot)), ]

	trtname <- if ("Trt_name" %in% names(t3)) t3$Trt_name else ys[[2]]
	d3 <- data.frame(
		trial_id = paste0("FISE_", substr(as.character(ys$DATE), 1, 4)),
		rep = as.integer(ys[[3]]),
		treatment = as.character(trtname),
		CO2_concentration_ = as.numeric(NA),
		irrigated = TRUE,
		N_fertilizer = n3$N_fertilizer,
		P_fertilizer = n3$P_fertilizer,
		K_fertilizer = 0,
		planting_date = as.character(p3$PDATE),
		emergence_date = as.character(p3$PLDOE),
		variety = if ("Cultivar_ID" %in% names(p3)) p3$Cultivar_ID else p3[[5]],
		seed_density = as.numeric(p3$PLPOP) * 10000,
		plant_density = as.numeric(p3$PLPOE) * 10000,
		row_spacing = as.numeric(p3$PLRS),
		yield = as.numeric(ys$LIWAH),
		dmy_total = as.numeric(NA),
		LAI = as.numeric(NA),
		latitude = 33.0628,
		longitude = -111.9826,
		elevation = 361,
		location = "Maricopa",
		site = "Maricopa Agricultural Center",
		country = "United States",
		crop = "cotton",
		yield_part = "lint",
		yield_moisture = 0,
		geo_from_source = TRUE,
		on_farm = FALSE,
		is_survey = FALSE,
		harvest_date = as.character(ys$DATE)
	)

	d <- rbind(d1, d2, d3)
	d$fertilizer_type <- "urea"

	carobiner::write_files(path, meta, d)
}
