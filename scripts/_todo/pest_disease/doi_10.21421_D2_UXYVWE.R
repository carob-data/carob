# R script for "carob"
# license: GPL (>=3)

## NOTES
# Medium-duration disease-resistant pigeonpea hybrid trial MLT-MDDRPHT-1 at ICRISAT Patancheru. The dataset title says 2016-17, so planting_date is 2016.
# Net plot 4 rows x 3.4 m x 0.75 m = 10.2 m2 (gross 12 m2), from the metadata text. Grain weight is kg/plot. Yield (kg/ha) is kg/plot x 10000 / 10.2.
# 100-seed weight is grams; seed_weight is that value x 10. Plant height is centimeters and plant stand is plants, from the metadata text.
# Plot numbers 1xx and 2xx are the two replications. Names with "(C)" are checks. Sterility is 0 on every row.
# Coordinates are the midpoint of the Dataverse bounding box for field BP 14A (78.268877–78.270929 E, 17.507420–17.507494 N).

## ISSUES
# No disease scores are in the workbook. Fertilizer, inoculation, and irrigation are not reported.
# The 100-seed column name ends with a non-breaking space.
# Final plant stand and "Total plants" differ. Density uses plant stand with the net area; the column is not labeled net or gross.
# Seeds per pod includes 0.6, which is very low; left as recorded.
# No publication DOI in the Dataverse record.
# write_files: missing carob_contributor, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, harvest_date.
# unknown variables: fertility_plants_, seeds_per_pod_.

## NOT PROCESSED
# xlsx, Entry no: one entry per hybrid name.
# xlsx, Total fertiles and Total steriles: every row is fully fertile.
# xlsx, Fertile percentage: used for sterility_index.

## NEW VARIABLES
# seeds_per_pod_: seeds per pod
# fertility_plants_: plants counted for fertility (Total plants), not the field stand

carob_script <- function(path) {

"
Phenotypic evaluation data of medium duration disease resistant Pigeonpea hybrids trial for year 2016-17

This dataset contains phenotypic evaluation data of medium duration disease resistant Pigeonpea hybrids trials. Research work was conducted at ICRISAT Center, Patancheru . Pigeonpea is a very important grain legume crop for food and other uses in Asia and Africa. It is often cross-pollinated species with a diploid number of 2n= 2x22 and genome size of 858Mbp. Every year 50 to 100 and above new crosses (and also CMS hybrids) will be made evaluated in nurseries to develop new high yielding cultivars with adaptability to different climatic/agronomic zones. Based on their agronomic performance in nurseries for maturity time, branching pattern and number of branches, pod color, pod yield and other pest and diseases tolerance characters etc, the superior progenies will be selected and advanced to further generations. Progenies selected based on preliminary/nursery data will be evaluated along with controls in replicated (twice or thrice) trials every year for further agronomic evaluation and selection. The agronomic data (days to 50% flowering and/or maturity, plant height, grain yield, grain size and color etc) of the progenies evaluated were presented herewith. The trial details and plot sizes were given. This data helps us to select and advance further. Finally the few best progenies among them will be evaluated in on-farm trials (OFTs). The best performed progenies will be considered to promote/release in respective agronomic zones.
"

	uri <- "doi:10.21421/D2/UXYVWE"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 2, minor = 2,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;flowering_days;maturity_days;plant_height",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Multi loction trial - medium duration disease resistant Pigeonpea hybrids trial (MLT-MDDRPHT-1.xlsx"]
	r <- carobiner::read.excel(f)
	seed_col <- grep("^Weight of 100 seeds", names(r), value = TRUE)

	variety <- trimws(r[["ICPH No"]])
	vtype <- ifelse(grepl("\\(C\\)", variety), "check", NA_character_)
	ped <- gsub("\\s+", " ", trimws(r$Pedigree))

	d <- data.frame(
		country = "India",
		adm1 = "Telangana",
		location = "ICRISAT Patancheru",
		crop = "pigeon pea",
		variety = variety,
		variety_type = vtype,
		variety_pedigree = ped,
		treatment = variety,
		rep = as.integer(r[["Plot number"]] %/% 100),
		plot_id = as.character(r[["Plot number"]]),
		flowering_days = as.numeric(r[["Days to flowering"]]),
		maturity_days = as.numeric(r[["Days to maturity"]]),
		plant_height = as.numeric(r[["Plant height"]]),
		plant_density = as.numeric(r[["Plant stand"]]) * 10000 / 10.2,
		fertility_plants_ = as.numeric(r[["Total plants"]]),
		sterility_index = 100 - as.numeric(r[["Fertile percentage"]]),
		seed_weight = as.numeric(r[[seed_col]]) * 10,
		seeds_per_pod_ = as.numeric(r[["Seeds per pod"]]),
		grain_color = trimws(r[["Seed base color"]]),
		yield = as.numeric(r[["Grain weight"]]) * 10000 / 10.2,
		yield_part = "grain",
		stress = "disease",
		planting_date = "2016",
		row_spacing = 75,
		plot_length = 3.4,
		plot_width = 3,
		plot_area = 10.2,
		trial_id = "1",
		on_farm = FALSE,
		is_survey = FALSE,
		longitude = (78.268877 + 78.270929) / 2,
		latitude = (17.507420 + 17.507494) / 2,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
