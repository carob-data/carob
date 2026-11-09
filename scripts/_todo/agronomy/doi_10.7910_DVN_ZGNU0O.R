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


## NOT PROCESSED
# AgIIS+Cotton+Reflectance+SoilMoisture+ET.ods: not read. .ods file. Observation records are missing from d until this file is standardized.
# FACE+Cotton+Carbon+Isotopes+and+Tracing.ods: not read. .ods file. Observation records are missing from d until this file is standardized.
# FISE+Cotton+Midday+Canopy+Temperatures.ods: not read. .ods file. Observation records are missing from d until this file is standardized.
# FISE+Cotton+Reflectance+Factors+and+Multispectral+Vegetation+Indices.ods: not read. .ods file. Observation records are missing from d until this file is standardized.

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
	f1 <- ff[basename(ff) == "FACE+Cotton+Growth+Management+Soil+ET.ods"]
	f2 <- ff[basename(ff) == "AgIIS+Cotton+Growth+Management+Soil.ods"]
	f3 <- ff[basename(ff) == "FISE+Cotton+Growth+Management+Soil+ET.ods"]

	read_ods <- function(f, sheet, skip) {
		as.data.frame(readODS::read_ods(f, sheet=sheet, skip=skip, col_names=TRUE))
	}

	# FACE yield (skip description rows; header is TRNO, R#, ...)
	r1 <- read_ods(f1, "Plant_summary", 2)
	r1 <- r1[!is.na(r1$TRNO) & r1[["R#"]] != 0, ]
	r2 <- read_ods(f1, "Treatment_Metadata", 2)
	r2 <- r2[!is.na(r2$TRNO), ]
	r3 <- read_ods(f1, "Plantings", 2)
	r3 <- r3[!is.na(r3$TRNO), ]
	r4 <- read_ods(f1, "Fertilizations", 2)
	r4 <- r4[!is.na(r4$TRNO), ]
	nface <- aggregate(list(N_fertilizer=as.numeric(r4$FEAMN)), list(TRNO=r4$TRNO), sum, na.rm=TRUE)

	t1 <- r2[match(r1$TRNO, r2$TRNO), ]
	p1 <- r3[match(r1$TRNO, r3$TRNO), ]
	n1 <- nface$N_fertilizer[match(r1$TRNO, nface$TRNO)]

	d1 <- data.frame(
		trial_id = paste0("FACE_", r1$DAYR),
		rep = as.integer(r1[["R#"]]),
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
		yield = as.numeric(r1$LIWAH),
		dmy_total = as.numeric(r1$CWAH),
		LAI = as.numeric(r1$LAIX),
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
	r5 <- read_ods(f2, "Plant_summary", 3)
	# drop means if R# present
	if ("R#" %in% names(r5)) r5 <- r5[!is.na(r5$TRNO) & r5[["R#"]] != 0, ] else r5 <- r5[!is.na(r5$TRNO), ]
	r6 <- read_ods(f2, "Treatment_Metadata", 2)
	r6 <- r6[!is.na(r6$TRNO), ]
	r7 <- read_ods(f2, "Plantings", 2)
	r7 <- r7[!is.na(r7$TRNO), ]
	r8 <- read_ods(f2, "Fertilizations", 2)
	r8 <- r8[!is.na(r8$TRNO), ]
	if ("R#" %in% names(r8)) r8 <- r8[r8[["R#"]] != 0, ]
	nagi <- aggregate(list(N_fertilizer=as.numeric(r8$FEAMN)),
		list(TRNO=r8$TRNO, R=r8[["R#"]]), sum, na.rm=TRUE)

	t2 <- r6[match(r5$TRNO, r6$TRNO), ]
	p2 <- r7[match(r5$TRNO, r7$TRNO), ]
	n2 <- nagi$N_fertilizer[match(paste(r5$TRNO, r5[["R#"]]), paste(nagi$TRNO, nagi$R))]
	lintcol <- intersect(c("LIWAH", "lint"), names(r5))
	if (length(lintcol) == 0) lintcol <- names(r5)[grepl("LIWAH|Lint", names(r5))][1]

	d2 <- data.frame(
		trial_id = "AgIIS_1999",
		rep = if ("R#" %in% names(r5)) as.integer(r5[["R#"]]) else 1L,
		treatment = if ("Trt_name" %in% names(t2)) t2$Trt_name else as.character(r5$TRNO),
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
		yield = as.numeric(r5[[lintcol[1]]]),
		dmy_total = if ("CWAH" %in% names(r5)) as.numeric(r5$CWAH) else as.numeric(NA),
		LAI = if ("LAIX" %in% names(r5)) as.numeric(r5$LAIX) else as.numeric(NA),
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
	r9 <- read_ods(f3, "Plant_Summary", 3)
	r9 <- r9[!is.na(r9$TRNO), ]
	r10 <- read_ods(f3, "Treatment_Metadata", 2)
	r10 <- r10[!is.na(r10$TRNO), ]
	r11 <- read_ods(f3, "Plantings", 2)
	r11 <- r11[!is.na(r11$TRNO), ]
	r12 <- read_ods(f3, "Fertilizations", 2)
	r12 <- r12[!is.na(r12$TRNO), ]
	# one row per irrigation/fert event x plot; sum by treatment and plot
	nfise <- aggregate(
		list(N_fertilizer=as.numeric(r12$FEAMN), P_fertilizer=as.numeric(r12$FEAMP)),
		list(TRNO=r12$TRNO, plot=r12[[3]]),
		sum, na.rm=TRUE
	)

	# unique treatments (drop plot-level duplicates in plantings/metadata)
	r10 <- r10[!duplicated(r10$TRNO), ]
	r11 <- r11[!duplicated(r11$TRNO), ]

	t3 <- r10[match(r9$TRNO, r10$TRNO), ]
	p3 <- r11[match(r9$TRNO, r11$TRNO), ]
	n3 <- nfise[match(paste(r9$TRNO, r9[[3]]), paste(nfise$TRNO, nfise$plot)), ]

	trtname <- if ("Trt_name" %in% names(t3)) t3$Trt_name else r9[[2]]
	d3 <- data.frame(
		trial_id = paste0("FISE_", substr(as.character(r9$DATE), 1, 4)),
		rep = as.integer(r9[[3]]),
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
		yield = as.numeric(r9$LIWAH),
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
		harvest_date = as.character(r9$DATE)
	)

	d <- rbind(d1, d2, d3)
	d$fertilizer_type <- "urea"

	carobiner::write_files(path, meta, d)
}
