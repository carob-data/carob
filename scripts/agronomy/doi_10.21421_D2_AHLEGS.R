# R script for "carob"
# license: GPL (>=3)

## NOTES
# Three diazotrophic bacteria isolated from chickpea nodules (IC-59, IC-76A, IC-2002) tested on
#   five chickpea cultivars (BG256, RSG888, Subhra, K850, ICCV2) at ICRISAT, Patancheru, India.
# "Culture" is the bacterial inoculant -> inoculant; inoculated is FALSE for an uninoculated control.
# Nodule number per plant -> nodule_plant.
# Shoot, root and nodule weights are in mg per plant; 

## NEW VARIABLES
# shoot_weight_plant_: shoot weight (mg/plant).
# root_weight_plant_: root weight (mg/plant).
# nodule_weight_plant_: nodule weight (mg/plant).
# chlorophyll_total_: total leaf chlorophyll (mg/L).

carob_script <- function(path) {

"
Nodulation and N2 fixation traits of the three diazotrophic bacteria on five cultivars of chickpea.

Three bacteria, IC-59, IC-76A and IC-2002, isolated from the nodules of chickpea, were characterized
for nodulation, nitrogen fixation, plant growth-promoting (PGP) and yield traits in five cultivars of
chickpea such as BG256, RSG888, Subhra, K850 and ICCV2. Factorial RBD with 3 replications. 
"

	uri <- "doi:10.21421/D2/AHLEGS"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = "Factorial RBD with 3 replications",
		data_type = "experiment",
		treatment_vars = "inoculant;variety",
		response_vars = "nodule_plant;shoot_weight_plant_;root_weight_plant_;nodule_weight_plant_;chlorophyll_total_",
		notes = NA,
		carob_date = "2026-11-10",
		carob_contributor = "Ajith S",
		carob_completion = 80,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Data file of Nodulation and N2 fixation traits of the three diazotrophic bacteria on five cultivars of chickpea.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data", na=c("", "NA", "-"))
	names(r) <- trimws(names(r))

	culture <- trimws(r$Culture)

	d <- data.frame(
		trial_id = "ICRISAT_chickpea_inoculation",
		country = "India",
		adm1 = "Telangana",
		location = "ICRISAT, Patancheru",
		latitude = 17.511,
		longitude = 78.275,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		crop = "chickpea",
		planting_date = as.character(NA),
		harvest_date = as.character(NA),
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = as.logical(NA),
		rep = as.integer(r[["Replication number"]]),
		variety = trimws(r$Cultivars),
		inoculant = culture,
		inoculated = !grepl("control|uninoc", culture, ignore.case=TRUE),
		treatment = paste(culture, trimws(r$Cultivars), sep="_"),
		N_fertilizer = as.numeric(NA),
		P_fertilizer = as.numeric(NA),
		K_fertilizer = as.numeric(NA),
		nodule_plant = as.numeric(r[["Nodule number plant"]]),
		shoot_weight_plant_ = as.numeric(r[["Shoot weight"]]),
		root_weight_plant_ = as.numeric(r[["Root weight"]]),
		nodule_weight_plant_ = as.numeric(r[["Nodule weight"]]),
		chlorophyll_total_ = as.numeric(r[["Total chlorophyll"]]),
		yield = as.numeric(NA),
		yield_part = "grain",
		yield_moisture = as.numeric(NA),
		yield_isfresh = NA
	)

	carobiner::write_files(path, meta, d)
}
