# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. plant.csv (per-plant repeated growth measurements) not processed: finer-grained than the
#    plot-level yield data used here, and skipped by the sibling AfSIS scripts too.
# 2. Raw date columns are internally inconsistent (Excel 1904/1900 epoch mismatch), so
#    planting_date is set to "2010" from Kihara et al. (2016) and harvest_date is left NA.
# 3. FType1/FType2/FertMet (farmer's own background fertilizer practice) left unprocessed --
#    distinct from the trial's own N/P/K/lime/Zn/S/Ca/Mg treatment, captured separately below.
# 4. GrainFWbp/GrainDWbp ("100/1000 grain weight") left unprocessed: observed values (0.05-0.78 g)
#    are implausibly small for that field.
# 5. dmy_residue (not fwy_residue) used for stover: codebook describes TStoverYld as dry weight.
# 6. yield uses unadjusted TGrainYld, not TGrainYld_adj (matches sibling Tuchila S2 script);
#    TGrainYld_adj has far more NAs (57 vs 2 of 290).
# 7. Damage/lodging/sub-sample weight columns are not central to this trial, left unprocessed.
# 8. adm1 = "Southern" (GADM has no region tier for Malawi); adm2/adm3 reverse-geocoded via GADM 4.1.

carob_script <- function(path) {

"Africa Soil Information System - Phase 1, Tuchila S1

The AFSIS project aimed to establish an Africa Soil Information system. Data was collected in
sentinel sites across sub-Saharan Africa using the Land Degradation Surveillance framework and
included also multi-location diagnostic trials in selected sentinel sites to determine nutrient
limitations and response to improved soil management practices (soil amendments). This dataset
covers the maize nutrient omission trial (season 1) at the Thuchila sentinel site, Malawi."

	uri <- "doi:10.25502/20180814/1514/HJ"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project = "AfSIS",
		publication = "doi:10.1016/j.agee.2016.05.012",
		data_organization = "IITA;ICRISAT;ABC",
		design = "nutrient omission trial",
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;lime;Zn_fertilizer;S_fertilizer;Ca_fertilizer;Mg_fertilizer",
		response_vars = "yield;dmy_residue",
		carob_contributor = "AI agronomy writer agent",
		carob_date = "2026-07-30",
		carob_completion = 75,
		carob_effort = 0.6
	)

## read data
	f1 <- ff[tolower(basename(ff)) == "tuchila_s1_field.csv"]
	f2 <- ff[tolower(basename(ff)) == "tuchila_s1_plot.csv"]
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)

	p <- tolower(gsub("\\s*-\\s*", ";", trimws(r1$PCrop1)))
	p <- gsub("sweet potatoes", "sweetpotato", p)
	p <- gsub("ground ?nuts?", "groundnut", p)
	r1$previous_crop <- p

## no plot in this site/season received the manure ("+MA") treatment (see ISSUES)
	om1 <- tolower(r1$MType1)
	om1[om1 %in% c("goat manure", "manure")] <- "farmyard manure" # not distinguished in terminag's OM value list
	om <- paste(om1, tolower(r1$MType2), sep=";")
	om <- gsub(";none", "", om, fixed=TRUE)
	om <- gsub("^none;", "", om)
	om[om == "none"] <- NA
	r1$OM_type <- om
	r1$OM_used <- !is.na(r1$OM_type)

## reverse-geocode against GADM for district/Traditional Authority
	pts <- terra::vect(r1[, c("Flong", "Flat")], geom=c("Flong", "Flat"), crs="EPSG:4326")
	gadm <- geodata::gadm("Malawi", level=2, path=file.path(path, "data", "gadm"))
	adm <- terra::extract(gadm, pts)
	r1$adm2 <- carobiner::fix_name(adm$NAME_1, "title")
	r1$adm3 <- carobiner::fix_name(adm$NAME_2, "title")

	d1 <- data.frame(
		trial_id = r1$FieldID,
		site = ifelse(trimws(r1$Village) == "", NA, carobiner::fix_name(r1$Village, "title")),
		location = carobiner::fix_name(r1$Site, "title"),
		latitude = r1$Flat,
		longitude = r1$Flong,
		crop = tolower(r1$TCrop),
		variety = r1$TCVariety,
		previous_crop = r1$previous_crop,
		OM_type = r1$OM_type,
		OM_used = r1$OM_used,
		field_topography = tolower(r1$Postn), # "upland"/"lowland" toposequence position, no terminag term
		adm2 = r1$adm2,
		adm3 = r1$adm3
	)

## nutrient omission rates (kg/ha), per Kihara et al. (2016); merge against a lookup table (avoids nested ifelse)
	trt <- data.frame(
		treatment     = c("Control", "NPK", "NP",  "NK",  "PK",  "NPK+Lime", "NPK+MN"),
		N_fertilizer  = c(0,         100,   100,   100,   0,     100,        100),
		P_fertilizer  = c(0,         30,    30,    0,     30,    30,         30),
		K_fertilizer  = c(0,         60,    0,     60,    60,    60,         60),
		lime          = c(0,         0,     0,     0,     0,     500,        0),
		Zn_fertilizer = c(0,         0,     0,     0,     0,     0,          3),
		S_fertilizer  = c(0,         0,     0,     0,     0,     0,          5),
		Ca_fertilizer = c(0,         0,     0,     0,     0,     0,          10),
		Mg_fertilizer = c(0,         0,     0,     0,     0,     0,          5)
	)

	# PNoHvst==0 co-occurs with AdjHarea==NA and plausible non-zero yields elsewhere; it marks
	# "not recorded", not a true zero-plant harvest, so treat as NA
	pno <- r2$PNoHvst
	pno[pno == 0] <- NA

	d2 <- data.frame(
		trial_id = r2$FieldID,
		plot_id = r2$PlotID,
		rep = as.integer(r2$Rep),
		treatment = r2$TrtDesc,
		yield = r2$TGrainYld * 1000,          # t/ha -> kg/ha, dry weight (see ISSUES)
		dmy_residue = r2$TStoverYld * 1000,   # t/ha -> kg/ha, dry weight (see ISSUES)
		plot_area = r2$Harea,                 # m2
		plant_density = 10000 * pno / r2$Harea
	)
	d2 <- merge(d2, trt, by="treatment")

	d <- merge(d1, d2, by="trial_id")
	d <- d[!is.na(d$yield), ] # 2 of 290 plots had no recorded grain yield

## housekeeping variables
	d$country <- "Malawi"
	d$adm1 <- "Southern" # see ISSUES
	d$geo_from_source <- TRUE

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$season <- "wet"
	d$planting_date <- "2010" # see ISSUES
	d$harvest_date <- as.character(NA)

	d$yield_part <- "grain"
	d$yield_isfresh <- FALSE
	d$yield_moisture <- as.numeric(NA)

	d$record_id <- 1:nrow(d)

	carobiner::write_files(path, meta, d)
}
