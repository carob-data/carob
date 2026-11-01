# R script for "carob"
# license: GPL (>=3)

## NOTES
# Sweetpotato roots from La Molina and San Ramon. The description gives beta-carotene as mg/100 g fresh weight and iron and zinc as mg/100 g fresh weight. BCFW, FeFW and ZnFW are those figures.
# Coordinates are the Dataverse points: La Molina -12.076289, -76.948417 and San Ramon -11.1275, -75.356389.
# Cooking TYPE is Raw, Boiled or Baked. Storage codes T0 to T3 are not defined in the workbook.
# A 2017 paper is cited (hdl:10568/83093) without a DOI.

## ISSUES
# DM has no unit. It is kept as recorded.
# Cooking and storage rows have no site, so they have no coordinates.
# No yield, fertilizer, or dates.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: dm_, tuber_Fe_, tuber_Zn_, storage_code_. NA detected: latitude, longitude.

## NOT PROCESSED
# Material_List_genotypes: pedigree columns. The accession number is the variety.
# Cooking TREATMENT (T1-T3): the same information as TYPE.

## NEW VARIABLES
# tuber_Fe_: iron in the root, mg/100 g fresh weight.
# tuber_Zn_: zinc in the root, mg/100 g fresh weight.
# dm_: dry-matter column as recorded. Unit is not stated.
# storage_code_: storage treatment code (T0-T3). Meaning is not in the file.


carob_script <- function(path) {

"
Replication data for: B-Carotene, Iron and Zinc Content, Effect of Storage and Type of Cooking in Sweet Potato (Ipomoea batatas L.) Genotypes

Sweet potato (Ipomoea batatas L) is the main component in the diet of millions of people living in tropical countries, and an improvement in β-carotene content could increase the supply of vitamin A to meet a greater part of the daily requirements of people. In the present study, the content of β-carotene (BC), iron (Fe) and zinc (Zn) were determined in 25 genotypes of sweet potato from two localities: La Molina and San Ramón. Furthermore, the changes in BC content in seven sweet potato genotypes were investigated using different cooking methods and storage times. The content of BC was measured via absorbance at 450 nm and identified through HPLC while Fe and Zn content were determined through ICP-OES techniques. The results show significant variation between genotype, environment and in the genotype-environment interaction. BC content in freshly harvested raw root ranged from 7.62 to 18.93 mg /100 g in fresh weight (PF), iron content ranged from 0.40 to 0.9 6mg/100 g PF and the zinc content ranged from 0.25 to 0.51 mg/100 mg PF. Genotypes 440442 and 440518 showed the highest levels of BC in both localities. Differences in BC content were found among cooking methods and storage times. BC concentration in boiled roots (15.15 mg/100 g PF) evaluated was higher than in baked roots (14.29 mg/100 g PF); however, without differences between genotypes 440442 and 420081 due to the cooking methods. BC content of root increased with storage time, where genotype 440442 showed the highest levels of BC during storage time, while genotypes 440413 and 440513 did not differ during storage time.
"

	uri <- "doi:10.21223/P3/TMFHKL"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;treatment",
		response_vars = "beta_carotene",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "Data_storage_cooking_sweetpotato.xls"]
	g <- carobiner::read.excel(f, sheet="Genotypes (SR-LM)")
	ck <- carobiner::read.excel(f, sheet="Cooking")
	st <- carobiner::read.excel(f, sheet="Storage")

	sites <- data.frame(
		environment = c("La Molina", "San Ramon"),
		adm1 = c("Lima", "Junin"),
		adm2 = c(NA_character_, "Chanchamayo"),
		location = c("La Molina", "San Ramon"),
		latitude = c(-12.076289, -11.1275),
		longitude = c(-76.948417, -75.356389)
	)

	d1 <- data.frame(
		environment = trimws(as.character(g$ENVIRONMENT)),
		variety = trimws(as.character(g$INSTN)),
		treatment = trimws(as.character(g$INSTN)),
		rep = as.integer(g$REP),
		beta_carotene = as.numeric(g$BCFW),
		tuber_Fe_ = as.numeric(g$FeFW),
		tuber_Zn_ = as.numeric(g$ZnFW),
		dm_ = as.numeric(g$DM),
		trial_id = paste(trimws(as.character(g$ENVIRONMENT)), "composition")
	)
	d1 <- merge(d1, sites, by="environment", all.x=TRUE)
	d1$environment <- NULL
	d1$geo_from_source <- TRUE
	d1$on_farm <- FALSE

	d2 <- data.frame(
		variety = trimws(as.character(ck$INSTN)),
		treatment = trimws(as.character(ck$TYPE)),
		rep = as.integer(ck$REP),
		beta_carotene = as.numeric(ck$BCFW),
		vitamin_A = as.numeric(ck[["Pro-vitamin A (ug RAE/100g FW)"]]),
		dm_ = as.numeric(ck$DM),
		trial_id = "cooking",
		country = "Peru",
		geo_from_source = NA,
		on_farm = NA
	)

	d3 <- data.frame(
		variety = trimws(as.character(st$INSTN)),
		treatment = trimws(as.character(st$TREATMENT)),
		storage_code_ = trimws(as.character(st$TREATMENT)),
		rep = as.integer(st$REP),
		beta_carotene = as.numeric(st$BCFW),
		dm_ = as.numeric(st$DM),
		trial_id = "storage",
		country = "Peru",
		geo_from_source = NA,
		on_farm = NA
	)

	d <- carobiner::bindr(d1, d2, d3)
	d$crop <- "sweetpotato"
	d$country <- "Peru"
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
