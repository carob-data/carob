# R script for "carob"
# license: GPL (>=3)

## ISSUES
## - The dataset description says 3 replications, but the Fieldbook, the "Installation" sheet and
##   the "Croquis" sheet all show 4 replications (RCBD). The data (4 reps) are used.
## - Plot size is inconsistent: "Installation" gives 10 plants/plot and 2.625 m2 (38095 plants/ha),
##   while the Fieldbook has NTP = 30 tubers planted per plot and the "Croquis" sheet says 40 plants/rep.
##   TTYNA (t/ha) in the source is computed from TTWP with a 2.625 m2 plot. Yield is taken as given
##   (TTYNA), but seed_density and plant_density were not computed.
## - Fertilizer (NPK 180-180-160; diammonium phosphate, ammonium nitrate, potassium chloride) comes
##   only from a note in the "Croquis" sheet. It is assumed to be kg/ha of N-P2O5-K2O.
## - "Vine cutting / killing" date (2004-09-01) is before planting (2004-09-24); not used.
## - Irrigation is not reported (Nazca is an arid coastal site, so the crop was most likely irrigated);
## - Two plots have no harvest data in the source (plot 69, CIP302448 rep 3; plot 92, CIP302331 rep 4),
##   so yield is NA there ("NA detected: yield"). The records are kept.
## - Entries are progenies (CIP3xxxxx family numbers) from crosses listed in the Material List;
##   the "Original_Elisa" sheet refers to "evaluation of families". Planting material (tubers vs.
##   transplanted seedlings, see "Croquis") is not fully clear.
## - Male parent CIP392745.7 has code "92.187" in the source (without the "C" prefix used for the
##   other CIP codes, e.g. C93.154); kept as in the source.

## NOT PROCESSED
## Plant_Unif, Plant_Vigor, Flower, SE (senescence), Tub_Unif (1-9 scales): no terminag variables.
## TTYA (adjusted yield) is not used; the non-adjusted yield (TTYNA) is reported.

carob_script <- function(path) {

"
Dataset for: Assessment of Potato Clones during Spring in Nazca, 2004

An evaluation of 23 clones was conducted in the Poroma area of Nazca during the spring of 2004, employing a randomized complete block design (RCBD) with 3 replications.
"

	uri <- "doi:10.21223/ZOYDHA"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "randomized complete block design",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = "Yield is the non-adjusted total tuber yield (TTYNA) reported in the source.",
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-05",
		carob_completion = 80,
		carob_effort = 3
	)

	f <- ff[basename(ff) == "PTYL200409_NAZCA.xls"]

	r1 <- carobiner::read.excel(f, sheet="Minimal")
	mi <- stats::setNames(as.list(r1$Value), r1$Factor)
	r2 <- carobiner::read.excel(f, sheet="Crop_management")
	# material list read from the text file as character; in the Excel sheet the
	# male code "92.187" is stored as a number and is read as 92.186999999999998
	f3 <- ff[basename(ff) == "02_Material_list.txt"]
	r3 <- utils::read.delim(f3, colClasses="character", na.strings="")
	r <- carobiner::read.excel(f, sheet="Fieldbook", na=c("", "#N/A", "NA"))

	# pedigree from the material list (female x male; name if no code)
	fem <- ifelse(is.na(r3[["Female.code"]]), r3[["Female.Institutional.number"]], r3[["Female.code"]])
	mal <- ifelse(is.na(r3[["Male.code"]]), r3[["Male.Institutional.number"]], r3[["Male.code"]])
	ped <- paste(fem, mal, sep=" x ")

	d <- data.frame(
		trial_id = "PTYL200409_NAZCA",
		plot_id = as.character(r$PLOT),
		rep = as.integer(r$REP),
		variety = r$INSTN,
		treatment = r$INSTN,
		variety_pedigree = ped[match(r$INSTN, r3[["Institutional.number"]])],
		crop = "potato",
		country = mi$Country,
		adm1 = mi$Admin1,
		adm2 = mi$Admin2,
		adm3 = mi$Admin3,
		location = mi$Locality,
		latitude = as.numeric(mi$Latitude),
		longitude = as.numeric(mi$Longitude),
		elevation = as.numeric(mi$Elevation),
		geo_from_source = TRUE,
		planting_date = as.character(as.Date(r2$Date[r2[["Intervention type"]] == "Planting"])),
		harvest_date = as.character(as.Date(r2$Date[r2[["Intervention type"]] == "Harvest"])),
		on_farm = FALSE,
		is_survey = FALSE,
		yield_part = "tubers",
		yield = as.numeric(r$TTYNA) * 1000,
		yield_moisture = as.numeric(NA),
		yield_isfresh = TRUE
	)

	# from the "Croquis" sheet: NPK 180-180-160, assumed kg/ha N, P2O5, K2O
	d$N_fertilizer <- 180
	d$P_fertilizer <- 180 / 2.29
	d$K_fertilizer <- 160 / 1.2051
	d$fertilizer_type <- "DAP;AN;KCl"
    d$irrigated <- NA # not reported, but certainly irrigated on the Peruvian desert coast.
	carobiner::write_files(path, meta, d)
}
