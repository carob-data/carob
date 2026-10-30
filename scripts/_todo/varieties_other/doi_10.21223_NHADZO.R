# R script for "carob"
# license: GPL (>=3)

## NOTES
# Seed production from rooted cuttings in nine districts of Uganda, season 2019A. The abstract names the districts and the varieties Kachpot 1 and Victoria.
# The sheet records tuber counts and tuber weights. Yield is the column Tons/ha times 1000.
# Weights in the size-class columns are kg per bed, as the headers say. Counts are tuber numbers.
# Sex M and F are male and female. The value Research is not a sex.
# Individual seed businesses are on-farm. Government plots are not.

## ISSUES
# The sheet does not say the crop name. The product is tubers of Kachpot 1 and Victoria, stored as potato.
# No coordinates. The villages need georeferencing.
# No fertilizer rates.
# Survival rate is left as recorded. The summary sheet shows values such as 0.70, so it is not converted to a percentage.
# write_files: missing variables: carob_contributor, yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variable: season_ (season?). unknown variable: dehaulming_date_ (date?). unknown variables: cuttings_planted_, cuttings_harvested_, survival_rate_, and the tuber size-class columns. NA detected: yield.

## NOT PROCESSED
# Sheet1, Sheet2, Sheet3, Sheet4, and Sheet8: pivot summaries of the RAC 2019A sheet.
# Average tuber counts and average weights. The size-class totals and Tons/ha are kept.
# Off Type, rows in a bed, and supplier.


carob_script <- function(path) {

"
Dataset for: Data production of seed from cuttings 2019

Data production of seed from cuttings 2019.  This estudy was evaluated  in 9 districts of Uganda (Bugizardi, Kabale, Kanungu, Kapchorwa, Kazardi, Kisoro, Kween, Mbale and Rubanda ) in two varieties (Kachpot1  and Victoria).
"

	uri <- "doi:10.21223/NHADZO"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Data_production_of_seed_out_of_cuttings_2019_A1.xlsx"]
	r <- carobiner::read.excel(f, sheet="RAC 2019A", na=c("", "NA"))
	names(r) <- trimws(names(r))
	sex <- trimws(as.character(r[["Gender (M, F, Research)"]]))
	sex[sex == "M"] <- "male"
	sex[sex == "F"] <- "female"
	sex[sex == "Research"] <- NA_character_
	biz <- trimws(as.character(r[["Seed business type (individual, group, government)"]]))
	variety <- trimws(as.character(r[["Variety planted"]]))
	iso <- function(x) {
		x <- as.Date(x)
		out <- rep(NA_character_, length(x))
		ok <- !is.na(x)
		out[ok] <- as.character(x[ok])
		out
	}

	d <- data.frame(
		trial_id = "Uganda_cuttings_2019A",
		country = "Uganda",
		adm1 = trimws(as.character(r$Region)),
		adm2 = trimws(as.character(r$District)),
		adm3 = trimws(as.character(r[["Sub-county"]])),
		location = trimws(as.character(r$Village)),
		season_ = "2019A",
		crop = "potato",
		variety = variety,
		treatment = variety,
		plot_id = paste(r[["Plot No"]], r[["Bed No"]], sep="_"),
		planting_date = iso(r[["Date of planting"]]),
		harvest_date = iso(r[["Date of harvesting"]]),
		dehaulming_date_ = iso(r[["Date of dehaulming"]]),
		sex = sex,
		on_farm = biz == "Individual",
		is_survey = FALSE,
		geo_from_source = FALSE,
		yield_part = "tubers",
		yield = as.numeric(r[["Tons/ha"]]) * 1000,
		cuttings_planted_ = as.numeric(r[["# cuttings planted"]]),
		cuttings_harvested_ = as.numeric(r[["# cuttings harvested"]]),
		survival_rate_ = as.numeric(r[["Survival rate"]]),
		tuber_n_lt_20mm_ = as.numeric(r[["# tubers < 20mm"]]),
		tuber_wt_kg_lt_20mm_ = as.numeric(r[["Weight tubers < 20mm (Kg)"]]),
		tuber_n_20_30mm_ = as.numeric(r[["# tubers 20-30mm"]]),
		tuber_wt_kg_20_30mm_ = as.numeric(r[["Weight tubers 20-30mm (Kg)"]]),
		tuber_n_31_45mm_ = as.numeric(r[["# tubers 31-45mm"]]),
		tuber_wt_kg_31_45mm_ = as.numeric(r[["Weight tubers 31-45mm (Kg)"]]),
		tuber_n_46_60mm_ = as.numeric(r[["# tubers 46-60mm"]]),
		tuber_wt_kg_46_60mm_ = as.numeric(r[["Weight tubers 46-60mm (Kg)"]]),
		tuber_n_gt_60mm_ = as.numeric(r[["# tubers > 60mm"]]),
		tuber_wt_kg_gt_60mm_ = as.numeric(r[["Weight tubers > 60mm (Kg)"]]),
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
