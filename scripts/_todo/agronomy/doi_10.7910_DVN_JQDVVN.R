# R script for "carob"
# license: GPL (>=3)

## NOTES
# qDTY introgression lines in Tainan 11 (japonica) background, their donors and the recurrent parent.
# Four sources are combined:
# 1. "DTFYieldMeans05162024.csv": IRRI trials 2015WS, 2016WS, 2017DS; per-rep (2 reps) DTF, grain yield (kg/ha),
#    height and tiller number (TN), under well-watered (WW) or reproductive-stage drought under rainout shelter (RSDS (ROS)).
#    Reshaped to one row per rep.
# 2. "10_Taiwan lines-Data 2018DS.xlsx" (IRRI Expt 10, 2018DS, lowland field UI3, alpha lattice): plot-level GY14
#    (kg/ha, sheet "Harvest") merged with DTF and mean plant height (sheet "DTF & Ht") by plot.
#    Sowing 2017-12-21, transplanting 2018-01-14 (sheet "Description").
# 3. "8_Taiwanese Lines_ AliceB.xlsx" (IRRI Expt 8, 2019DS, field UI2b described as "Upland", RCBD): plot-level
#    GY (g/m2 at 14%), straw biomass (g/m2), HI, DTF, height and harvest date (sheet "Harvest").
#    Sowing 2018-12-11, transplanting 2018-12-28, draining 2019-02-15 (sheet "Description").
# 4. "TARI Tn11 with IR designations replicated.csv": per-rep yields (g/m2) from TARI drought trials in Taiwan,
#    2017-2019 first (I) and second (II) crop seasons.
# g/m2 converted to kg/ha (x10).

## ISSUES
# No coordinates or site names in the data. IRRI trials assumed at IRRI, Los Banos (authors' institute);
#   TARI trial site in Taiwan unknown. Georeference after checking the paper (doi:10.1080/15427528.2025.2515047, not read).
# TARI file: drought treatment management unknown; "2017-I"/"2017-II" mapped to season "first"/"second", year as
#   planting_date. Cross names "Tainan 11 x IR 91648-B-89-B-82" to "-90" look like spreadsheet auto-increment
#   errors of "-81"; kept as recorded. Column "selected" (yes for TN 11 only) and "order" not mapped.
# DTFYieldMeans: 20 records have "Mean TN" but no per-rep TN; these means are not mapped. Generation (BC3F2 etc.)
#   and "Parental combination" kept as new variables. WS15 has only drought, WS16 only WW plots.
# Expt 10 Description says 4 replicates, data have 3. Expt 8 is labelled "Upland" but was transplanted and
#   drained; flooded status of the WW plots unclear.
# Expt 10 plot 114 has MC 126% and therefore a negative GY14 (-42.7 kg/ha); left as recorded (out of bounds).
# Expt 8 QTL codes read from Excel as numbers (e.g. "4.0999999999999996" for 4.1); not fixed.
# Genotype names differ between files (abbreviated in the csv, full in the xlsx); not harmonized.
# Not mapped: tensiometer (Exp 8/10 tensio.csv, "Tensio"), Diviner soil moisture, Licor gas exchange, per-hill heights,
#   "Computation" and "DRI" summary sheets of the xlsx files.
# Fertilizer and planting dates for the IRRI csv trials and TARI trials not given.
# Taiwan Agricultural Research Institute is not in the data_organization vocabulary ("TARI" there is Tanzania);
#   only IRRI used.
# write_files remaining: missing coordinates, N/P/K fertilizer; NA yield; yield out of bounds (negative);
#   missing values in stress (TARI Tainan 11 rows); unknown (new) variables.

## NEW VARIABLES
# qtl_: qDTY QTL(s) introgressed in the line (e.g. "1.1, 2.1, 3.1")
# generation_: breeding generation (e.g. BC3F5) or Donor / RP (recurrent parent)
# parental_combination_: cross group code (A-D) of the csv file
# tillers_plant_: tiller number per plant (hill)


carob_script <- function(path) {

"
qDTY introgression to improve the drought tolerance of Taiwanese japonica rice variety Tainan 11

Whereas many indica and rainfed rice-growing regions have historically been considered drought-prone, lowland japonica growing regions have typically not been considered to be affected by drought. However, the increasing incidence of drought in japonica rice-growing regions necessitates breeding for future climate scenarios. Several major-effect drought tolerance yield QTLs (qDTYs) have been introgressed into indica backgrounds, but further improvement of japonica varieties is now needed. In this study, we introgressed seven qDTYs and their combinations through crossing four qDTY donor lines with the Taiwanese japonica variety Tainan 11 (TN 11). BC3F6 - BC3F8 progeny were evaluated in agronomic trials under drought. Although no single group of introgression lines with the same qDTY stood out as best performing, we observed that the higher yield under drought appeared to correspond with earlier time to flowering. Selected lines for yield under drought showed similar height and grain type to TN 11. We identified three lines that could be evaluated for deployment in Taiwan or as pre-breeding material for developing other drought tolerant japonica varieties.
"

	uri <- "doi:10.7910/DVN/JQDVVN"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = "doi:10.1080/15427528.2025.2515047",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;stress",
		response_vars = "yield;flowering_days;plant_height",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 65,
		carob_effort = 0.8
	)

## 1. IRRI 2015WS-2017DS, two reps in columns
	f1 <- ff[basename(ff) == "DTFYieldMeans05162024.csv"]
	r1 <- read.csv(f1, check.names=FALSE, na.strings=c("", "no data"))

	d1 <- lapply(1:2, function(i) {
		data.frame(
			trial_id = r1$Expt,
			season = ifelse(grepl("DS", r1$Season), "dry", "wet"),
			planting_date = paste0("20", substr(r1$Season, 3, 4)),
			parental_combination_ = r1[["Parental combination"]],
			variety_pedigree = r1$Cross,
			generation_ = r1$Generation,
			variety_code = r1[["Entry code"]],
			variety = trimws(r1$DESIGNATION),
			qtl_ = trimws(r1$QTL),
			treatment = r1$Treatment,
			rep = i,
			flowering_days = r1[[paste("DTF Rep", i)]],
			yield = as.numeric(r1[[paste("YieldKgHa Rep", i)]]),
			plant_height = r1[[paste("Height Rep", i)]],
			tillers_plant_ = r1[[paste("TN Rep", i)]],
			location = "IRRI, Los Banos",
			country = "Philippines"
		)
	})
	d1 <- do.call(rbind, d1)

## 2. IRRI Expt 10, 2018DS
	f2 <- ff[basename(ff) == "10_Taiwan lines-Data 2018DS.xlsx"]
	r2a <- carobiner::read.excel(f2, sheet="Harvest", skip=1)
	r2b <- carobiner::read.excel(f2, sheet="DTF & Ht", skip=1)
	r2b <- r2b[!is.na(r2b[["Ent#"]]), ]
	r2b <- data.frame(Plot=as.numeric(r2b$Plot), flowering_days=r2b[["DTF*"]], plant_height=r2b[["Ave ht"]])

	d2 <- data.frame(
		Plot = r2a$Plot,
		trial_id = paste0("2018DS_Expt10_", r2a$Trt),
		season = "dry",
		planting_date = "2017-12-21",
		transplanting_date = "2018-01-14",
		variety_code = r2a[["Ent#"]],
		variety = trimws(r2a$Genotype),
		treatment = r2a$Trt,
		rep = as.integer(r2a$Rep),
		plot_id = as.character(r2a$Plot),
		yield = r2a$GY14,
		location = "IRRI, Los Banos",
		country = "Philippines"
	)
	d2 <- merge(d2, r2b, by="Plot", all.x=TRUE)
	d2$Plot <- NULL

## 3. IRRI Expt 8, 2019DS
	f3 <- ff[basename(ff) == "8_Taiwanese Lines_ AliceB.xlsx"]
	r3 <- carobiner::read.excel(f3, sheet="Harvest")

	d3 <- data.frame(
		trial_id = paste0("2019DS_Expt8_", r3$Trt),
		season = "dry",
		planting_date = "2018-12-11",
		transplanting_date = "2018-12-28",
		harvest_date = as.character(as.Date(r3[["date harvest"]])),
		variety_code = r3[["Ent#"]],
		variety = trimws(r3$Genotype),
		qtl_ = trimws(r3$QTL),
		treatment = r3$Trt,
		rep = as.integer(r3$Rep),
		plot_id = as.character(r3$Plot),
		flowering_days = r3$DTF,
		plant_height = r3[["Ave ht"]],
		yield = r3$GY * 10,
		dmy_residue = r3$Biomass * 10,
		harvest_index = r3$HI * 100,
		location = "IRRI, Los Banos",
		country = "Philippines"
	)

## 4. TARI, Taiwan
	f4 <- ff[basename(ff) == "TARI Tn11 with IR designations replicated.csv"]
	r4 <- read.csv(f4, check.names=FALSE)

	d4 <- data.frame(
		trial_id = paste0("TARI_", r4$Season),
		season = ifelse(grepl("-II$", r4$Season), "second", "first"),
		planting_date = substr(r4$Season, 1, 4),
		variety_pedigree = r4[["Cross conbination"]],
		variety_code = r4[["No. IRRI"]],
		variety = trimws(r4[["IR designation"]]),
		qtl_ = trimws(r4$QTL),
		treatment = tolower(r4$Trait),
		rep = r4$Rep,
		yield = r4[["Yield\n(g/m2)"]] * 10,
		country = "Taiwan"
	)
	d4$variety_code[d4$variety_code == "n.a."] <- NA
	d4$qtl_[d4$qtl_ == "TN 11"] <- NA
	# Trait is "n.a." for the recurrent parent Tainan 11; all TARI trials appear to be drought trials
	d4$treatment[d4$treatment == "n.a."] <- NA

	d <- carobiner::bindr(d1, d2, d3, d4)

	d$stress <- ifelse(d$treatment %in% c("S", "RSDS (ROS)", "drought"), "drought",
		ifelse(d$treatment %in% c("WW", "W"), "none", NA))
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$planting_method <- ifelse(d$country == "Philippines" & !is.na(d$transplanting_date), "transplanting", NA)

	carobiner::write_files(path, meta, d)
}
