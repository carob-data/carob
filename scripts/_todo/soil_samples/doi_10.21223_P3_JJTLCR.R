# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil organic carbon under maca and pasture in the Peruvian high-Andean puna. The title places the samples in Peru. The table has no place names.
# X and Y are UTM. The file does not give the zone or the datum, so they are not stored as longitude and latitude.
# SOC is g/kg. soil_SOC is that value divided by 10, in percent. SOC stock is Mg/ha. Bulk density is g/cm3.

## ISSUES
# No longitude, latitude, or site name.
# Some sample ids and case-study labels are blank in the file.
# write_files: missing latitude, longitude. unknown variables case_study_, soc_stock_. NA in sample_id.

## NOT PROCESSED
# Carbonates, and the isotope values (d13SOC and d13C of the size fractions).
# Aggregate and particulate fractions (WSA, POC, and C in the 2 mm, 250 µm, and 53 µm classes) and microbial biomass C. They are size classes of one sample and are not a single soil concentration.
# Data_dictionary.xls: documentation or a codebook, not an observation table.


## NEW VARIABLES
# case_study_: maca cropping system or cultivated pasture, as named in the dictionary.
# soc_stock_: soil organic carbon stock, Mg/ha.


carob_script <- function(path) {

"
Replication data for: Soil organic carbon stocks and fractionation under different land uses in the Peruvian high-Andean Puna
"

	uri <- "doi:10.21223/P3/JJTLCR"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "Database_carbon.csv"]
	r <- read.csv(f, check.names=FALSE)
	land <- trimws(as.character(r$"Land use"))
	land[!nzchar(land)] <- NA_character_
	case <- trimws(as.character(r$"Case Study"))
	case[!nzchar(case)] <- NA_character_

	d <- data.frame(
		trial_id = "Puna_SOC",
		sample_id = as.character(r$ID),
		country = "Peru",
		land_use = land,
		case_study_ = case,
		soil_bd = as.numeric(r$"Bulk density"),
		soil_SOC = as.numeric(r$SOC) / 10,
		soc_stock_ = as.numeric(r$"SOC stock"),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
