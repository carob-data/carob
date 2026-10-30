# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are 01-General Non-Roster Dataset.xlsx.
# Country Bangladesh is in the dataset description. District, upazilla, union, and village are in the sheet.
# Interview date is day-month-year.
# Gender Male/Female is stored as male/female.
# The sheet records potato and sweetpotato seed receipt. It is not a plot yield.

## ISSUES
# No coordinates and no yield.
# write_files: missing variables: carob_contributor; geo_from_source, latitude, longitude.

## NOT PROCESSED
# Roster workbooks 02 through 11 (demography, land, inputs, assets, livestock, income).
# Seed and vine quantities. The unit column is not a yield unit.

carob_script <- function(path) {

"
Dataset for: Adoption and Impact of Salt Tolerant Potato and Sweetpotato Varieties in Khulna and Satkhira Districts of Bangladesh in 2020

Roots and tuber crops (RTCs) play an important role in diverse Asian agri-food systems and contribute to their resilience and capacity to adapt to different challenging environments.The study is a quantitative assessment to better understand the current farming practice including potato and sweetpotato farming in salt prone areas to finally increase the potato and sweetpotato yield in those areas. The information collected was divided in sixteen sections: A) Basic information and consent, (B) Household Roster, (C) Roster of land and pond/water bodies owned or under operation, (D1) Agriculture Plot Utilization, (D2) Irrigation method (in the last 12 months), (D3) Usage of Agricultural Chemicals, Fertilizers and Pesticides (in the last 12 months), (D4) Rental Cost of Tools, Machinery and Draft Animal (in the last 12 months), (D5) Labor Usage by Gender for Crop Plantation and Harvesting, (D6) Post Harvest Labor, Animal and Tools/ Machinery Usage, (E) Household Assets, (F) Household Access to Resources, (G) Household Access to Extension and Credit, (H) Livestock and Aquaculture, (I) Household annual off-farm Income, (J) Household annual Expenditures, (K) Social networks, (L) Migration, (M) Household Food Security, (N) Constraints to POTATO AND SWEETPOTATO production and Marketing, (O) Risk Perception, (P) Nutrition and Hygiene, (Q) Knowledge, exposure and behavioral responses to Covid-19, (R) Labour and other inputs use in the last harvest and transplant, (S) Potato and Sweet potato and last cyclone (Amphan) related information.
"

	uri <- "doi:10.21223/NOJ9R4"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 25,
		carob_effort = 0.3
	)

	f <- ff[grepl("^01-", basename(ff))]
	r <- carobiner::read.excel(f)
	sex <- tolower(trimws(as.character(r$Gender)))
	sex[sex == "male"] <- "male"
	sex[sex == "female"] <- "female"
	dt <- r[["Interview date (DD-MM-YYYY)"]]
	if (inherits(dt, "Date") || inherits(dt, "POSIXt")) {
		date <- as.character(as.Date(dt))
	} else {
		date <- as.character(as.Date(trimws(as.character(dt)), format="%d-%m-%Y"))
	}

	d <- data.frame(
		hhid = trimws(as.character(r[["Household ID"]])),
		country = "Bangladesh",
		adm2 = trimws(as.character(r[["District / province"]])),
		adm3 = trimws(as.character(r[["Upazilla / Subdistrict"]])),
		location = trimws(as.character(r$Village)),
		sex = sex,
		date = date,
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated = NA,
		trial_id = "Bangladesh_potato_sweetpotato"
	)
	carobiner::write_files(path, meta, d)
}
