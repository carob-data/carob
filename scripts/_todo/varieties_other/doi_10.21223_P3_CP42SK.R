# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field biomass at Kiboko, Kenya, 2010, one row per accession, replicate, and DAP. The dictionary gives FML, FMS, FMR, DML, DMS, and DMR in t ha-1, and CC in percent. Values are multiplied by 1000 to kg/ha. Fresh root biomass is fwy_storage, not a single harvest yield.
# Country is Kenya and adm1 is Makueni from geographicCoverage. The abstract names the Kiboko Crop Research Station. geographicCoverage also names the city Makindu.
# Coordinates are the Kenya point in the Dataverse bounding box, southLatitude -2.42 and eastLongitude 38.25. They are not a column in Data.xls.
# The string NA in the fresh and dry root columns is missing.

## ISSUES
# No planting date, fertilizer, or irrigation in the biomass sheet. Root biomass is missing on the early harvests.
# write_files: missing variables: yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.

## NOT PROCESSED
# Meteorology Data - Kiboko. Daily temperature and solar radiation, not joined to the plots.
# Leaf Traits Data - La Molina and Light Curves Data - La Molina. Potted plants in Peru, including a light-response curve, not the Kiboko field trial.
# Material_list.xls parentage. Jewel is the only row with parents.
# Data_dictionary.xls: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Replication data for: Characterizing the diversity of sweetpotato through growth parameters and leaf traits: Precocity and light use efficiency as important ordination factors

The objective of this study was to assess the main factors that explain the functional diversity of eight representative sweetpotato genotypes through a multivariate ordination based on growth parameters and physiological leaf traits. Growth parameters were determined from the biomass (leaves, stems, and storage roots), dry matter fraction and canopy cover assessed in a field experiment at the Kiboko Crop Research Station (Kenya, during 2010). Physiological leaf traits were determined by leaf mass per area (LMA), N and P content in leaves, light response curve parameters and carbon discrimination assessed in potted plants at the CIP-La Molina Experimental Station (Peru, during 2016)
"

	uri <- "doi:10.21223/P3/CP42SK"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "fwy_leaves;fwy_stems;fwy_storage;dmy_leaves;dmy_stems;dmy_storage;ground_cover",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Data.xls"]
	r <- carobiner::read.excel(f, sheet="Biomass & Coverage - Kiboko", na="NA")
	variety <- trimws(as.character(r$Accession_Name))

	d <- data.frame(
		crop = "sweetpotato",
		variety = variety,
		variety_code = trimws(as.character(r$INSTN)),
		treatment = variety,
		rep = as.integer(r$REP),
		DAP = as.integer(r$DAP),
		fwy_leaves = as.numeric(r$FML) * 1000,
		fwy_stems = as.numeric(r$FMS) * 1000,
		fwy_storage = as.numeric(r$FMR) * 1000,
		dmy_leaves = as.numeric(r$DML) * 1000,
		dmy_stems = as.numeric(r$DMS) * 1000,
		dmy_storage = as.numeric(r$DMR) * 1000,
		ground_cover = as.numeric(r$CC),
		country = "Kenya",
		adm1 = "Makueni",
		location = "Kiboko",
		latitude = -2.42,
		longitude = 38.25,
		geo_from_source = FALSE,
		trial_id = "Kiboko_2010",
		on_farm = FALSE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
