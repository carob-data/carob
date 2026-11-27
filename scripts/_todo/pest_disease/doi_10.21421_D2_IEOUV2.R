# R script for "carob"
# license: GPL (>=3)

## ISSUES
# No disease scores are in the workbook despite the trial name ("disease resistant Pigeonpea hybrids trial"); this dataset may belong in the varieties group.
# Final plant stand and "Total plants" differ by exactly 8 plants on every row of Rep 1 (plots 101-110) and are identical on every row of Rep 2 (plots 201-210); plant_density uses Final plant stand on the net plot (10.2 m2).
# Abstract coordinate (17°30'N, 78°16'46"E) is the ICRISAT campus point, whereas the Dataverse bounding box used here locates field BP-14A.

## NEW
# seeds_per_pod_: average number of seeds per pod (count)
# fertility_plants_: total plants scored for pollen fertility per plot (count; equals Final plant stand + 8 in Rep 1 and Final plant stand in Rep 2)

## MISSING
# yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, harvest_date

## NOTES
# Medium-duration disease-resistant pigeonpea hybrid trial MLT-MDDRPHT-2 (Test No 24) at ICRISAT Patancheru (field BP-14A), 2015.
# Plot size from metadata txt: gross 4 rows x 4 m x 0.75 m = 12 m2; net 4 rows x 3.4 m x 0.75 m = 10.2 m2. Yield and plant_density use the net plot area (10.2 m2).
# Plot numbers 101-110 and 201-210 indicate two replications. Entries marked "(C)" (Maruti, Asha, ICPH 2671) are checks; "(C)" is stripped from variety and recorded in variety_type.
# All plots had 100% fertile plants (sterility_index = 0). Coordinates are the midpoint of the Dataverse bounding box for field BP-14A.

## NOT PROCESSED
# Multi location trial ... (MLT-MDDRPHT).xlsx: Entry number (1:1 with hybrid name); Total fertiles and Total steriles (redundant with Total plants and Fertile percentage, as all plots were 100% fertile).

carob_script <- function(path) {

"
Phenotypic evaluation data of medium duration disease resistant Pigeonpea hybrids trail

This database includes the research work carried out on development of medium duration disease resistant Pigeonpea hybrids trail including elite hybrids at ICRISAT Center, Patancheru (17°30'N 78°16'46E). Pigeonpea is a very important grain legume crop for food other uses in Asia and Africa. It is often cross-pollinated species with a diploid number of 2n= 2x22 and genome size of 858Mbp.  Every year 50 to 100 and above new crosses (and also CMS hybrids) will be made  evaluated in nurseries to develop new high yielding cultivars with adaptability to different climatic/agronomic zones. Based on their agronomic performance in nurseries for maturity time, branching pattern and number of branches,  pod color, pod yield and other pest and diseases tolerance characters etc, the superior progenies will be selected and advanced to further generations (to F5s). The F5 progenies selected based on preliminary/nursery data will be evaluated along with controls in replicated (twice or thrice) trials every year for further agronomic evaluation and selection. The agronomic data (days to 50% flowering and/or maturity, plant height, grain yield, grain size and color etc) of the progenies evaluated in years 2015 were presented herewith. The trial details and plot sizes were given. This data helps us to select and advance further. Finally the few best progenies among them will be evaluated in on-farm trials (OFTs) and in multi-location trials. The best performed progenies will be considered to promote/release in respective agronomic zones.
"

	uri <- "doi:10.21421/D2/IEOUV2"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 2,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;flowering_days;maturity_days;plant_height;seed_weight",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Multi location trial - medium duration disease resistant Pigeonpea hybrids trial (MLT-MDDRPHT).xlsx"]
	r <- carobiner::read.excel(f)

	raw_var <- trimws(r[["ICPH No."]])
	is_check <- grepl("\\(C\\)", raw_var)
	variety <- trimws(sub("\\s*\\(C\\)", "", raw_var))
	vtype <- ifelse(is_check, "check", "hybrid")
	ped <- gsub("\\s+", " ", trimws(r$Pedigree))
	ped[grepl("Check", ped, ignore.case = TRUE)] <- NA_character_

	d <- data.frame(
		country = "India",
		adm1 = "Telangana",
		location = "ICRISAT Patancheru",
		site = "BP-14A",
		crop = "pigeon pea",
		variety = variety,
		variety_type = vtype,
		variety_pedigree = ped,
		treatment = variety,
		rep = as.integer(r[["Plot number"]] %/% 100),
		plot_id = as.character(r[["Plot number"]]),
		flowering_days = as.numeric(r[["Days to flowering"]]),
		maturity_days = as.numeric(r[["Days to maturity"]]),
		plant_height = as.numeric(r[["Plant height in cm"]]),
		# Net plot area: 4 rows x 3.4 m x 0.75 m = 10.2 m2 (from metadata txt)
		plant_density = as.numeric(r[["Final plant stand"]]) * 10000 / 10.2,
		fertility_plants_ = as.numeric(r[["Total plants"]]),
		sterility_index = 100 - as.numeric(r[["Fertile percentage"]]),
		seed_weight = as.numeric(r[["Hundred Seed weight in g"]]) * 10,
		seeds_per_pod_ = as.numeric(r[["Seed per Pod"]]),
		grain_color = tolower(trimws(r[["Seed color"]])),
		yield = as.numeric(r[["Grain weight in kg per plot"]]) * 10000 / 10.2,
		yield_part = "grain",
		planting_date = "2015",
		row_spacing = 75,
		plot_length = 3.4,
		plot_width = 3,
		plot_area = 10.2,
		trial_id = "1",
		trial_name = "MLT-MDDRPHT-2",
		on_farm = FALSE,
		is_survey = FALSE,
		# Midpoint of Dataverse bounding box for field BP-14A (~212 m E-W)
		longitude = (78.268847 + 78.270848) / 2,
		latitude = (17.507456 + 17.507471) / 2,
		geo_uncertainty = 110,
		geo_source = "ICRISAT Dataverse bounding box",
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
