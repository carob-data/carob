# R script for "carob"
# license: GPL (>=3)

## NOTES
# Maize yield-component records for FAW-control technologies at two districts in Centre Region,
#   Cameroon: Batchenga (fields Ndjie, Okongolo; transition forest-savannah) and Mbalmayo
#   (fields Avebe, Memiam; forest). Season code 2 is 2020 only (160 rows); code 1 is 2021 only (152 rows).
# Block is used as rep. Variety is taken from the treatment name where a genotype is named
#   (ACRO6TZCOP4F2, ACRO6TZCOMP4F2, Local white). fertilized_ is TRUE/FALSE only when the
#   treatment name says so. "unfertilized" is tested before "fertilized".
# Cobwt and GWT have no unit. Values are hundreds for about 7-9 cobs, so they are the sample
#   totals, not kg/ha. Ear length is about 10-14 (unit not stated).
# planting_date is the year only.

## ISSUES
# 1. No plot area, so Cobwt and GWT cannot be converted to kg/ha. Do not treat them as t/ha.
# 2. No coordinates. Dataset coverage (2.3232, 11.43653) is not Batchenga or Mbalmayo.
#    Georeference: Batchenga (Ndjie, Okongolo) and Mbalmayo (Avebe, Memiam), Centre, Cameroon.
# 3. N/P/K rates are not given for the fertilized treatments. Insecticide "GREMEC", OMO detergent,
#    Tithonia leaves, wood ash and soil treatment have no product rate or active ingredient.
# 4. Damage and pest counts are not in this file (see doi:10.25502/h02h-bq18/d, same sites and treatments).
# 5. Companion file codes the genotype ACRO6TZCOMP4-C4F2; this file says ACRO6TZCOMP4F2. Not reconciled.
# 6. write_files: missing yield, yield_moisture, N/P/K, harvest_date, longitude, latitude.
# 7. adm2 not set: confirm GADM for Batchenga (Lekié) and Mbalmayo (Nyong-et-So'o).

## NOT PROCESSED
# ID: row number.
# data_dictionary.csv: column definitions used above; no plot measurements.

## NEW VARIABLES
# agroecosystem_: forest or transition forest-savannah, as recorded
# fertilized_: TRUE/FALSE from the treatment name; NA when the name does not say
# cob_number_: cobs in the sample (NumCobs)
# cob_weight_: Cobwt, unit not stated
# grain_weight_: GWT, unit not stated
# ear_length_: EL, unit not stated
# kernel_rows_: rows per ear
# kernels_per_row_: kernels per row

carob_script <- function(path) {

"
Impact of different technologies against Spodoptera frugiperda (Lepidoptera: Noctuidae) on the maize yield in the Tropical humid forest of Cameroon

Assess the link between damage, abundance and yield
"

	uri <- "doi:10.25502/zsnk-kk83/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Coordinating elucidation of science and effectiveness of Local Innovations for Managing Fall armyworm in Africa",
		design = "Blocked comparison of FAW control treatments and fertilized versus unfertilized maize at two districts",
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "grain_weight_",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 58,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "yield-data.csv"]
	r <- read.csv(f, check.names = FALSE)

	tr <- trimws(r$Treatment)
	variety <- rep(NA_character_, nrow(r))
	variety[grepl("ACRO6TZCOMP4F2", tr)] <- "ACRO6TZCOMP4F2"
	variety[grepl("ACRO6TZCOP4F2", tr)] <- "ACRO6TZCOP4F2"
	variety[grepl("Local white", tr)] <- "Local white"

	fert <- rep(NA, nrow(r))
	fert[grepl("unfertilized|without fertilizer", tr, ignore.case = TRUE)] <- FALSE
	fert[is.na(fert) & grepl("fertilized|with fertilizer", tr, ignore.case = TRUE)] <- TRUE

	loc <- carobiner::fix_name(trimws(r$District), "title")
	site <- carobiner::fix_name(trimws(r$Field), "title")
	year <- as.character(r[[7]])
	season <- ifelse(r[[3]] == 1, "first", ifelse(r[[3]] == 2, "second", NA_character_))

	d <- data.frame(
		country = "Cameroon",
		adm1 = "Centre",
		location = loc,
		geo_from_source = FALSE,
		site = site,
		agroecosystem_ = trimws(r$Agroecosystem),
		planting_date = year,
		season = season,
		treatment = tr,
		variety = variety,
		fertilized_ = as.logical(fert),
		rep = as.integer(r$Block),
		block_id = as.character(r$Block),
		crop = "maize",
		stress = "pests",
		pest_species = "Spodoptera frugiperda",
		cob_number_ = r$NumCobs,
		cob_weight_ = r$Cobwt,
		grain_weight_ = r$GWT,
		ear_length_ = r$EL,
		kernel_rows_ = r[["Row Per ear"]],
		kernels_per_row_ = r[["Kernnels Per row"]],
		trial_id = paste(loc, site, year, season, sep = "_"),
		on_farm = TRUE,
		is_survey = FALSE,
		yield_part = "grain"
	)

	carobiner::write_files(path, meta, d)
}
