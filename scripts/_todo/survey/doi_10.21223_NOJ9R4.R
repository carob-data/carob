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
# write_files: ; geo_from_source, latitude, longitude.

## NOT PROCESSED
# Roster workbooks 02 through 11 (demography, land, inputs, assets, livestock, income).
# Seed and vine quantities. The unit column is not a yield unit.
# 02-Household Demographics, Roster.xlsx: not read. sheet Sheet1: Household ID, Baseline Household ID, Interview date (DD-MM-YYYY), Gender, B1. Total households members, B2a. Adults (18yrs and Above), B2b. Adolescents (6-17yrs), B2c.  Children (3-5yrs), B2d.  Infants (0-2yrs), B3.1 HH Member ID, B3.1. Still Member, B3.2 Relation with the HH, .... Observation records are missing from d until this file is standardized.
# 03-Land And Water Bodies Owned, Roster.xlsx: not read. sheet Sheet1: Household ID, Baseline Household ID, Interview date (DD-MM-YYYY), Gender, C1.1 Plot ID, C1.2 Plot Description, C1.3 Plot type, C1.3 Plot type (OTHER), C1.4 Area of the plot, C1.4I- Crop 1, C1.4I- Crop 2, C1.4I- Crop 3, .... Observation records are missing from d until this file is standardized.
# 04-Plot Utilization and Irrigation Methods, Roster.xlsx: not read. sheet Sheet1: Household ID, Baseline Household ID, Interview date (DD-MM-YYYY), Gender, D2.2 Crop season, D2.3 Crop irrigated, D2.3O. Crop (OTHER) irrigated, D2.4 Cultivated area, D2.5 Main variety, D2.5 Other variety, D2.6 Seed source, D2.6 Other seed source, .... Observation records are missing from d until this file is standardized.
# 05-Inputs, tools-animal rentals and labour costs, Roster.xlsx: not read. sheet Sheet1: Household ID, Baseline Household ID, Interview date (DD-MM-YYYY), Gender, D3.1 CROP no., D3.1 Crop OTHER, D3.2 Inputs used, D3.3 Inputs use frecuency, D3.4 Amount of input use, D3.5 Unit of inputs used, D3.6 Costs of inputs use, D4.2.1.Rent tools_machinery, .... Observation records are missing from d until this file is standardized.
# 06-Assets, Roster.xlsx: not read. sheet Sheet1: Household ID, Baseline Household ID, Interview date (DD-MM-YYYY), Gender, E2. Assets, E2.1 Assets (OTHER), E2.2 Asset number, E2.3 Purchase price (Tk), District / province, Upazilla / Subdistrict, Union / commune, Village, .... Observation records are missing from d until this file is standardized.
# 07-Livestock And Aquaculture, Roster.xlsx: not read. sheet Sheet1: Household ID, Baseline Household ID, Interview date (DD-MM-YYYY), Gender, H1. Farm animal, H1. Farm animal (other), H2.1 Total Value of all Livestock/Aquaculture, H2.2 Total rearing costs (Tk) farm animals, H2.3 Total revenue from selling (Tk) farm animals, H2.4 Total value of household consumption (Tk), District / province, Upazilla / Subdistrict, .... Observation records are missing from d until this file is standardized.
# 08-Off Farm Income, Roster.xlsx: not read. sheet Sheet1: Household ID, Baseline Household ID, Interview date (DD-MM-YYYY), Gender, I3.1 Source of Income, I3.3 Income per month (Tk.), I3.4 How many months husband receive income, I3.5 Income per year (Tk.) for man/husband, I3.6 Contribution to total income for man/husband, I3.7 Income per month (Tk.) for woman/spouse, I3.8 Months woman/spouse receive income, I3.9 Income per year (Tk.) for woman/spouse, .... Observation records are missing from d until this file is standardized.
# 09-Household Expenditures, Roster.xlsx: not read. sheet Sheet1: Household ID, Baseline Household ID, Interview date (DD-MM-YYYY), Gender, J2.1. Item spent, J2.1. Item spent  (OTHER), J2.2. Annual expenditure (Tk), District / province, Upazilla / Subdistrict, Union / commune, Village, Remarks by Enumerator. Observation records are missing from d until this file is standardized.
# 10-Household Food Consumption, Roster.xlsx: not read. sheet Sheet1: Household ID, Baseline Household ID, Interview date (DD-MM-YYYY), Gender, M2.1 Food item - past 24 hour, M2. How often consume these items, M2.2 Who consumed the food item, District / province, Upazilla / Subdistrict, Union / commune, Village, Remarks by Enumerator. Observation records are missing from d until this file is standardized.
# 11_Datadictionary_Salt Tolerant Bangladesh_2020.xlsx: documentation or a codebook, not an observation table.

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

	f <- ff[basename(ff) == "01-General Non-Roster Dataset.xlsx"]
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
