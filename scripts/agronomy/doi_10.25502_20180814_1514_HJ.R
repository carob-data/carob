# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. plant.csv (repeated per-plant growth measurements) not processed; finer-grained than the plot-level data used here.
# 2. Raw dates inconsistent (Excel 1904/1900 epoch mismatch); planting_date set to "2010" from Kihara et al. (2016), harvest_date left NA.
# 3. FType1/FType2/FertMet left unprocessed: values are fertilizer product/dosing-method names, not "fallow" as the codebook (miscopied from FalTyp) claims. Unclear if farmer background practice (cf. MType1/2 -> OM_type) or the trial's own application method -- unresolved.
# 4. GrainFWbp/GrainDWbp ("100/1000 grain weight") left unprocessed: values (0.05-0.78 g) implausibly small.
# 5. dmy_residue (not fwy_residue): codebook labels TStoverYld as dry weight.
# 6. yield uses unadjusted TGrainYld, not TGrainYld_adj (matches sibling Tuchila S2 script; TGrainYld_adj has far more NAs: 57 vs 2 of 290).
# 7. Damage/lodging/sub-sample weight columns not central to this trial, left unprocessed.
# 8. adm1 = "Southern" (GADM has no region tier for Malawi); adm2/adm3 hard-coded from an interactive GADM 4.1 lookup (see "geo" table below).
# 9. The 2 of 290 plots dropped for NA TGrainYld (FieldID/PlotID Thuc200901109, Thuc200916201) still had valid TStoverYld (1.574/1.862 t/ha), CobNo, PNoHvst etc, checked directly against tuchila_s1_plot.csv; dropped anyway because yield is a required, NA-disallowed field (carobiner required_variables.csv) with no way to retain a partial row -- same convention as every other agronomy script in this repo.

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
		# verbatim from Kihara et al. (2016) Methods: "field trials were implemented using a
		# modified nutrient omission trial design"; no formal statistical block design
		# (e.g. RCBD) or randomization is stated -- only site-level blocking (16 blocks/site,
		# 1 field/block replicate) already captured via trial_id
		design = "modified nutrient omission trial design",
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;lime;Zn_fertilizer;S_fertilizer;Ca_fertilizer;Mg_fertilizer",
		response_vars = "yield;dmy_residue",
		carob_contributor = "Oscar Bautista",
		carob_LLM = "Claude Sonnet 5",
		carob_date = "2026-07-30",
		carob_completion = 75,
		carob_effort = 0.6
	)

	f1 <- ff[tolower(basename(ff)) == "tuchila_s1_field.csv"]
	f2 <- ff[tolower(basename(ff)) == "tuchila_s1_plot.csv"]
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)

	p <- tolower(gsub("\\s*-\\s*", ";", trimws(r1$PCrop1)))
	p <- gsub("sweet potatoes", "sweetpotato", p)
	p <- gsub("ground ?nuts?", "groundnut", p)
	r1$previous_crop <- p

## no plot in this site/season received the manure ("+MA") treatment
	om1 <- tolower(r1$MType1)
	om1[om1 %in% c("goat manure", "manure")] <- "farmyard manure" # not distinguished in terminag's OM value list
	om <- paste(om1, tolower(r1$MType2), sep=";")
	om <- gsub(";none", "", om, fixed=TRUE)
	om <- gsub("^none;", "", om)
	om[om == "none"] <- NA
	r1$OM_type <- om
	r1$OM_used <- !is.na(r1$OM_type)

	geo <- data.frame(
		FieldID = c("Thuc2009011", "Thuc2009012", "Thuc2009021", "Thuc2009022", "Thuc2009031", "Thuc2009032", "Thuc2009041", "Thuc2009042", "Thuc2009051", "Thuc2009052", "Thuc2009061", "Thuc2009062", "Thuc2009071", "Thuc2009072", "Thuc2009081", "Thuc2009082", "Thuc2009091", "Thuc2009092", "Thuc2009101", "Thuc2009102", "Thuc2009111", "Thuc2009112", "Thuc2009121", "Thuc2009122", "Thuc2009131", "Thuc2009132", "Thuc2009141", "Thuc2009142", "Thuc2009151", "Thuc2009152", "Thuc2009161", "Thuc2009162"),
		adm2 = c("Chiradzulu", "Chiradzulu", "Chiradzulu", "Chiradzulu", "Chiradzulu", "Chiradzulu", "Chiradzulu", "Chiradzulu", "Chiradzulu", "Mulanje", "Chiradzulu", "Chiradzulu", "Chiradzulu", "Chiradzulu", "Chiradzulu", "Chiradzulu", "Mulanje", "Mulanje", "Mulanje", "Mulanje", "Mulanje", "Mulanje", "Mulanje", "Mulanje", "Mulanje", "Mulanje", "Mulanje", "Mulanje", "Mulanje", "Mulanje", "Mulanje", "Mulanje"),
		adm3 = c("Ta Nkalo", "Ta Nkalo", "Ta Nkalo", "Ta Nkalo", "Ta Nkalo", "Ta Nkalo", "Ta Nkalo", "Ta Nkalo", "Ta Nkalo", "Ta Nthiramanja", "Ta Nkalo", "Ta Nkalo", "Ta Nkalo", "Ta Nkalo", "Ta Nkalo", "Ta Nkalo", "Ta Nthiramanja", "Ta Nthiramanja", "Ta Nthiramanja", "Ta Nthiramanja", "Ta Nthiramanja", "Ta Nthiramanja", "Sc Juma", "Sc Juma", "Ta Nthiramanja", "Ta Nthiramanja", "Ta Nthiramanja", "Ta Nthiramanja", "Sc Juma", "Ta Nthiramanja", "Sc Juma", "Sc Juma")
	)
	stopifnot(all(r1$FieldID %in% geo$FieldID)) # confirm every field has a matched lookup row
	r1 <- merge(r1, geo, by="FieldID")

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
		plot_area = r2$Harea,                 # m2 (terminag's plot_area is defined in m2,
		                                       # range 1-350, distinct from ha-based field_size)
		plant_density = 10000 * pno / r2$Harea
	)
	stopifnot(all(d2$treatment %in% trt$treatment)) # confirm every TrtDesc has a matched lookup row
	d2 <- merge(d2, trt, by="treatment")

	d <- merge(d1, d2, by="trial_id")
	# 2 of 290 plots had NA TGrainYld; verified they still carry other real measurements
	# (TStoverYld, CobNo, PNoHvst, etc) but are dropped anyway since yield is a hard-required,
	# NA-disallowed field with no partial-row option (see ISSUES)
	d <- d[!is.na(d$yield), ]

	d$country <- "Malawi"
	d$adm1 <- "Southern"
	d$geo_from_source <- TRUE

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$season <- "wet"
	d$planting_date <- "2010"
	d$harvest_date <- as.character(NA)

	d$yield_part <- "grain"
	d$yield_isfresh <- FALSE
	d$yield_moisture <- as.numeric(NA)

	d$record_id <- 1:nrow(d)

	carobiner::write_files(path, meta, d)
}
