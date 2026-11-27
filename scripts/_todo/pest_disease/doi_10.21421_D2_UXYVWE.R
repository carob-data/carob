# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Despite "disease resistant" in the trial title, no disease scores or disease stress data are in the workbook; this dataset may belong in the varieties group.
# Seeds per pod in plot 110 (ICPH 3496, rep 1) is 0.6 vs 3.4-4.2 on all other plots (likely a data-entry typo for 3.6); left as recorded.
# Fertilizer, irrigation, moisture basis, and exact planting/harvest dates are not reported.

## NEW
# seeds_per_pod_: seeds per pod (count).

## MISSING
# yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, harvest_date

## NOTES
# Medium-duration pigeonpea hybrid trial MLT-MDDRPHT-1 at ICRISAT Patancheru (field BP 14A), 2016-17: 12 ICPH hybrids + 2 check varieties (Asha, Maruti) in 2 replications.
# Net plot is 4 rows x 3.4 m x 0.75 m = 10.2 m2 (gross 12 m2); grain weight (kg/plot) and net plant stand are scaled to kg/ha and plants/ha using 10.2 m2.
# Check tag "(C)" was stripped from variety names and marked in variety_type; non-pedigree check labels (including a raw copy-paste error "Maruti (Check)" on Asha in plot 214) were set to NA in variety_pedigree.
# Coordinates are the midpoint of the ICRISAT Dataverse bounding box for field BP 14A.

## NOT PROCESSED
# xlsx, Entry no: trial entry index (1-to-1 with ICPH No).
# xlsx, Total plants, Total fertiles, Total steriles: gross-plot plant count (always exactly Plant stand + 8 border plants across 4 rows) with 0 steriles on every row; captured by plant_density and sterility_index.

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
		response_vars = "yield;flowering_days;maturity_days;plant_height;seed_weight;sterility_index",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Multi loction trial - medium duration disease resistant Pigeonpea hybrids trial (MLT-MDDRPHT-1.xlsx"]
	r <- carobiner::read.excel(f)
	# Column header ends with a non-breaking space
	seed_col <- grep("^Weight of 100 seeds", names(r), value = TRUE)

	raw_var <- trimws(r[["ICPH No"]])
	is_check <- grepl("\\(C\\)", raw_var)
	variety <- trimws(gsub("\\(C\\)", "", raw_var))
	ped <- ifelse(is_check, NA_character_, gsub("\\s+", " ", trimws(r$Pedigree)))

	d <- data.frame(
		country = "India",
		adm1 = "Telangana",
		location = "ICRISAT Patancheru",
		site = "BP 14A",
		crop = "pigeon pea",
		variety = variety,
		variety_type = ifelse(is_check, "check", "hybrid"),
		variety_pedigree = ped,
		treatment = variety,
		rep = as.integer(r[["Plot number"]] %/% 100),
		plot_id = as.character(r[["Plot number"]]),
		flowering_days = as.numeric(r[["Days to flowering"]]),
		maturity_days = as.numeric(r[["Days to maturity"]]),
		plant_height = as.numeric(r[["Plant height"]]),
		# Net plot area = 4 rows x 3.4 m x 0.75 m = 10.2 m2 (from metadata txt)
		plant_density = as.numeric(r[["Plant stand"]]) * 10000 / 10.2,
		sterility_index = 100 - as.numeric(r[["Fertile percentage"]]),
		seed_weight = as.numeric(r[[seed_col]]) * 10,
		seeds_per_pod_ = as.numeric(r[["Seeds per pod"]]),
		grain_color = tolower(trimws(r[["Seed base color"]])),
		yield = as.numeric(r[["Grain weight"]]) * 10000 / 10.2,
		yield_part = "seed",
		planting_date = "2016",
		row_spacing = 75,
		plot_length = 3.4,
		plot_width = 3,
		plot_area = 10.2,
		trial_id = "1",
		trial_name = "MLT-MDDRPHT-1",
		on_farm = FALSE,
		is_survey = FALSE,
		longitude = (78.268877 + 78.270929) / 2,
		latitude = (17.507420 + 17.507494) / 2,
		geo_uncertainty = 109,
		geo_source = "ICRISAT Dataverse bounding box",
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
