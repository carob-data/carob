# R script for "carob"
# license: GPL (>=3)

## NOTES
# Sorghum landrace accessions scored for grain Fe and Zn in 2016 and 2017. There is no yield and no management treatment, so this is a variety trial and the script is in varieties_sorghum.
# One row per accession per year. variety is the cultivar name (local names repeat); variety_code is the collection serial number.
# Fe and Zn are ppm (mg/kg). terminag grain_Fe and grain_Zn are mg/g. Values were left in mg/kg.
# Country is Nigeria and the place is Kano, from the Dataverse geospatial block. Coordinates are the midpoint of the bounding box (about 8.591 E, 11.998 N). The box is about 3 km east-west, so geo_uncertainty is 1500 m.
# No publication DOI is given in the metadata.

## ISSUES
# No grain yield, fertilizer, irrigation or planting detail beyond the year in the column name.
# grain_Zn is above the terminag maximum because the values are mg/kg. grain_Fe was not converted to mg/g; that would put every value below the terminag minimum of 1.
# The north and south bounds in the Dataverse record are labeled as longitudes and are slightly reversed; the midpoint was used.
# Whether the trial was on a station or in farmers' fields is not stated. It is marked on_farm FALSE because the geographic unit says "Research location".
# write_files: missing carob_contributor; missing yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; out of bounds grain_Zn (15.5-47.2).

## NOT PROCESSED
# None. Both years of Fe and Zn and the accession identifiers are mapped. There is no other measurement column.

## NEW VARIABLES
# None.

carob_script <- function(path) {

"
Exploration of Sorghum Landrace Resources for Micronutrient Fe and Zn Bio-fortification

Identification of sorghum landrace germplasm for grain Iron (Fe) and Zinc (Zn) improvement to directly benefit the poor who depend on sorghum as staples.        Experimental location on Google Maps
"

	uri <- "doi:10.21421/D2/EP8AP4"
	group <- "varieties_sorghum"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "grain_Fe;grain_Zn",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Data file of sorghum landrace resources for micronutrient Fe and Zn biofortification.xlsx"]
	r <- carobiner::read.excel(f, na="NA")

	code <- trimws(r$`Collection serial number`)
	name <- trimws(r$`Cultivar name`)
	d16 <- data.frame(
		variety_code = code,
		variety = name,
		treatment = name,
		grain_Fe = as.numeric(r$Fe_2016),
		grain_Zn = as.numeric(r$Zn_2016),
		planting_date = "2016",
		trial_id = "Kano_2016"
	)
	d17 <- data.frame(
		variety_code = code,
		variety = name,
		treatment = name,
		grain_Fe = as.numeric(r$Fe_2017),
		grain_Zn = as.numeric(r$Zn_2017),
		planting_date = "2017",
		trial_id = "Kano_2017"
	)
	d <- rbind(d16, d17)
	d$crop <- "sorghum"
	d$variety_type <- "landrace"
	d$country <- "Nigeria"
	d$adm1 <- "Kano"
	d$location <- "Kano"
	d$longitude <- 8.590855
	d$latitude <- 11.998441
	d$geo_uncertainty <- 1500
	d$geo_from_source <- TRUE
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
