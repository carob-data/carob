# R script for "carob"
# license: GPL (>=3)

## ISSUES
## - The dataset description says 3 replications, but the Fieldbook, the "Installation" sheet and
##   the "Croquis" sheet all show 4 replications (RCBD). The data (4 reps) are used.
## - Plot size is inconsistent: "Installation" gives 10 plants/plot and 2.625 m2, while the Fieldbook
##   has NTP = 30 plants per plot and up to 28 plants harvested (NPH), so all plants of a 30-plant plot
##   were harvested. The "Croquis" sheet gives one 14 m row per entry and rep, 0.35 m between plants
##   and 0.75 m between rows ("No. plts/rep: 40"). TTYNA in the source (TTWP / 2.625 m2) therefore
##   overestimates yield about three times. Yield is computed as TTWP / (30 x 0.35 m x 0.75 m = 7.875 m2),
##   not adjusted for missing plants; TTYNA and TTYA are not used (yield_adj_ is computed instead). Plot of 30 plants confirmed by
##   V. Hualla (CIP).
## - Fertilizer (NPK 180-180-160; diammonium phosphate, ammonium nitrate, potassium chloride) comes
##   only from a note in the "Croquis" sheet. It is assumed to be kg/ha of N-P2O5-K2O.
## - "Vine cutting / killing" date (2004-09-01) is before planting (2004-09-24); not used.
## - Irrigation is not reported (Nazca is an arid coastal site, so the crop was most likely irrigated);
## - Two plots have no harvest data in the source (plot 69, CIP302448 rep 3; plot 92, CIP302331 rep 4),
##   so yield is NA there ("NA detected: yield"). The records are kept.
## - Entries are progenies (CIP3xxxxx family numbers) from crosses listed in the Material List;
##   the "Original_Elisa" sheet refers to "evaluation of families". Planting material (tubers vs.
##   transplanted seedlings, see "Croquis") is not fully clear.
## - Male parent CIP392745.7 has code "92.187" in the source; changed to the standard CIP code
##   "C92.187" (as C93.154, C91.612).

## NEW VARIABLES (scores per plot, CIP 1-9 scales; the meaning of the scale values is not
## documented in the data dictionary)
## plant_uniformity_: plant uniformity score (Plant_Unif), 45 days after planting
## plant_vigor_: plant vigor score (Plant_Vigor), 45 days after planting
## flowering_score_: flowering degree score (Flower; values 0-5), 45 days after planting
## senescence_score_: senescence score (SE), 75 days after planting ("Precoc 75 dias")
## tuber_uniformity_: tuber uniformity score at harvest (Tub_Unif)
## (days after planting from the column names in the "Original_Elisa" sheet)
## yield_adj_: total tuber yield adjusted for missing plants (kg/ha), TTWP / NPH x 38095 plants/ha
##   (30 plants per 7.875 m2); equivalent to TTYA in the source (t/ha) x 1000

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
		response_vars = "yield;yield_adj_",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-09",
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
	# standard CIP code: "92.187" -> "C92.187" (ISSUES)
	fem <- gsub("^([0-9]{2}\\.[0-9]+)$", "C\\1", fem)
	mal <- gsub("^([0-9]{2}\\.[0-9]+)$", "C\\1", mal)
	ped <- paste(fem, mal, sep=" x ")

	plot_area <- 30 * 0.35 * 0.75
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
		# all plants of a 30-plant plot harvested, 0.35 x 0.75 m (ISSUES)
		yield = as.numeric(r$TTWP) * 10000 / plot_area,
		seed_density = 10000 / (0.35 * 0.75),
		plant_density = as.numeric(r$NPH) * 10000 / plot_area,
		plot_area = plot_area,
		# yield adjusted for missing plants: weight per harvested plant x plants planted per ha
		yield_adj_ = as.numeric(r$TTWP) / as.numeric(r$NPH) * 10000 / (0.35 * 0.75),
		row_spacing = 75,
		plant_spacing = 35,
		plant_uniformity_ = as.integer(r$Plant_Unif),
		plant_vigor_ = as.integer(r$Plant_Vigor),
		flowering_score_ = as.integer(r$Flower),
		senescence_score_ = as.integer(r$SE),
		tuber_uniformity_ = as.integer(r$Tub_Unif),
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
