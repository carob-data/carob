# R script for "carob"
# license: GPL (>=3)

## NOTES
# Grain yield is t/ha (at 14% moisture per the bundled paper Rana et al. 2022, Field Crops Research, doi:10.1016/j.fcr.2022.108519). Each replicate is a row.
# Site, soil, plot size, variety, and management details come from FCR-Rana-Hydrus-2022.pdf: BISA-CIMMYT research farm, Ladhowal, Ludhiana, Punjab, India (30.99 N, 75.44 E, 229 m), sandy loam (pH 8.3, EC 0.51 dS/m), RCBD, plot 25 x 5.4 m (135 m2), cultivar PR-126, previous crop wheat, ZTDSR sown 2019-06-10 at 20 kg/ha and 22.5 cm row spacing with wheat residue retained, PTR transplanted 2019-07-10 without residue.
# ZTDSR is zero-till direct-seeded rice with flood irrigation. SSD is the same establishment with subsurface drip. PTR is puddled transplanted rice with flood irrigation.
# N rates in the treatment names are kg N/ha as urea. N0 is 0. PTR received 120 kg N/ha.

## ISSUES
# Duplicate deposit: hdl:11529/10548769 and hdl:11529/10548771 contain identical raw files for the same Rana et al. (2022) experiment.
# P and K rates are not specified in the grain sheet (P and K were applied uniformly; check paper for exact blanket doses if needed).

## NOT PROCESSED
# HYDRUS-2D water-balance output. The grain sheet is the measured yield.
# Figures and the paper PDF.


carob_script <- function(path) {

"
Water budgeting in conservation agriculture-based sub-surface drip irrigation using HYDRUS-2D in rice under annual rotation with wheat in Western Indo-Gangetic Plains

Measured rice grain yield for zero-till direct-seeded rice and puddled transplanted rice under flood irrigation and subsurface drip, with several nitrogen rates.
"

	uri <- "hdl:11529/10548769"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = "doi:10.1016/j.fcr.2022.108519",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "N_fertilizer;land_prep_method;planting_method;irrigation_method",
		response_vars = "yield",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "CIMMYT Data-Grain.xlsx"]
	r <- carobiner::read.excel(f, sheet="Grain", col_names=FALSE, col_types="text")
	code <- trimws(as.character(r[4:nrow(r), 1]))
	nrate <- rep(NA_real_, length(code))
	nrate[grepl("N0$", code)] <- 0
	nrate[grepl("N75$", code)] <- 75
	nrate[grepl("N112.5$", code)] <- 112.5
	nrate[grepl("N150$", code)] <- 150
	# Dataset description: PTR received 120 kg N/ha.
	nrate[code == "PTR"] <- 120
	prep <- ifelse(grepl("^PTR", code), "puddled", "none")
	plant <- ifelse(grepl("^PTR", code), "transplanting", "direct seeding")
	water <- ifelse(grepl("^SSD", code), "subsurface drip", "flood")

	one <- function(col, rep) {
		data.frame(
			country = "India",
			adm1 = "Punjab",
			adm2 = "Ludhiana",
			location = "Ladhowal",
			site = "BISA-CIMMYT farm",
			latitude = 30.99,
			longitude = 75.44,
			elevation = 229,
			trial_id = "Ladhowal_2019",
			rep = rep,
			treatment = code,
			crop = "rice",
			variety = "PR-126",
			previous_crop = "wheat",
			planting_date = "2019-06-10",
			transplanting_date = ifelse(grepl("^PTR", code), "2019-07-10", NA_character_),
			row_spacing = ifelse(grepl("^PTR", code), NA_real_, 22.5),
			seed_rate = ifelse(grepl("^PTR", code), NA_real_, 20),
			plot_area = 135,
			plot_length = 25,
			plot_width = 5.4,
			yield = as.numeric(r[4:nrow(r), col]) * 1000,
			yield_moisture = 14,
			yield_part = "grain",
			N_fertilizer = nrate,
			fertilizer_type = ifelse(nrate == 0, "none", "urea"),
			land_prep_method = prep,
			planting_method = plant,
			residue_prevcrop_used = !grepl("^PTR", code),
			irrigation_method = water,
			soil_texture = "sandy loam",
			soil_pH = 8.3,
			soil_EC = 0.51,
			irrigated = TRUE,
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = TRUE
		)
	}
	d <- rbind(one(3, 1L), one(4, 2L), one(5, 3L))
	d$plot_id <- paste(d$treatment, d$rep, sep="_")

	carobiner::write_files(path, meta, d)
}
