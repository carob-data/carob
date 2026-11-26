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
# Coordinates are approximate district/ZARDI centroids (geo_from_source = FALSE).
# No fertilizer rates or irrigation information.
# Survival rate is left as recorded (0-1 fraction).

## NEW VARIABLES
# dehaulming_date_: date of dehaulming (YYYY-MM-DD).
# cuttings_planted_, cuttings_harvested_: number of rooted apical cuttings planted and harvested per bed.
# survival_rate_: proportion of cuttings surviving to harvest (0-1).
# tuber_count_class1_..class5_: tuber counts per bed by diameter class (<20 mm, 20-30 mm, 31-45 mm, 46-60 mm, >60 mm).
# tuber_weight_class1_..class5_: tuber fresh weights (kg/bed) by diameter class (<20 mm, 20-30 mm, 31-45 mm, 46-60 mm, >60 mm).

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
	group <- "varieties_potato"
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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 65,
		carob_effort = 0
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
		season = "first",
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
		yield_part = "tubers",
		yield_isfresh = TRUE,
		yield = as.numeric(r[["Tons/ha"]]) * 1000,
		cuttings_planted_ = as.numeric(r[["# cuttings planted"]]),
		cuttings_harvested_ = as.numeric(r[["# cuttings harvested"]]),
		survival_rate_ = as.numeric(r[["Survival rate"]]),
		tuber_count_class1_ = as.numeric(r[["# tubers < 20mm"]]),
		tuber_weight_class1_ = as.numeric(r[["Weight tubers < 20mm (Kg)"]]),
		tuber_count_class2_ = as.numeric(r[["# tubers 20-30mm"]]),
		tuber_weight_class2_ = as.numeric(r[["Weight tubers 20-30mm (Kg)"]]),
		tuber_count_class3_ = as.numeric(r[["# tubers 31-45mm"]]),
		tuber_weight_class3_ = as.numeric(r[["Weight tubers 31-45mm (Kg)"]]),
		tuber_count_class4_ = as.numeric(r[["# tubers 46-60mm"]]),
		tuber_weight_class4_ = as.numeric(r[["Weight tubers 46-60mm (Kg)"]]),
		tuber_count_class5_ = as.numeric(r[["# tubers > 60mm"]]),
		tuber_weight_class5_ = as.numeric(r[["Weight tubers > 60mm (Kg)"]])
	)
	geo <- data.frame(
		adm2 = c("BugiZARDI", "Kabale", "Kanungu", "Kapchorwa", "KaZARDI", "Kisoro", "Kween", "Mbale", "Rubanda"),
		latitude = c(1.070, -1.249, -0.958, 1.401, -1.221, -1.285, 1.420, 1.082, -1.190),
		longitude = c(34.380, 29.989, 29.790, 34.450, 29.976, 29.685, 34.530, 34.175, 29.850),
		geo_from_source = FALSE
	)
	d <- merge(d, geo, by="adm2", all.x=TRUE)

	carobiner::write_files(path, meta, d)
}
