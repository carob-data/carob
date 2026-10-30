# R script for "carob"
# license: GPL (>=3)

## NOTES
# One row per field. Coordinates, elevation, and the laboratory values are in 01_Data of Soil Physical and Chemical Properties.xlsx.
# pH is a 1:1 soil-water suspension. EC is a 1:1 extract in dS/m, which is the same number as mS/cm.
# P is Olsen (mg/kg). K is buffered-acetate extraction (mg/kg). CEC and exchangeable cations are ammonium acetate at pH 7, atomic absorption.
# Sand, silt, and clay are Bouyoucos hydrometer percentages. The dictionary leaves those units blank.
# The dictionary unit for exchangeable Ca is printed as "cmolc kg−2". The other cations are cmolc kg−1. Ca is stored on that same scale.
# Location Chugay is the Dataverse geographic coverage (city). The coordinates are the sample points.

## ISSUES
# The description says soil organic carbon by Walkley-Black. The file and dictionary name that column organic matter (OM) and do not print a unit. The number is stored as soil_SOM, not converted.
# Bulk density was measured on cores at 15 cm and 30 cm. The table has one Bd value per field, kept on the 0-24 cm chemistry row.
# Dictionary does not give a unit for δ15N. The column is kept as soil_d15N_.
# Some Olsen P values are above 200 mg/kg.
# write_files: missing carob_contributor. out of bounds: soil_P where values exceed 200.

## NEW VARIABLES
# soil_d15N_: nitrogen isotope ratio δ15N of dried soil. Unit is not stated in the dictionary.

## NOT PROCESSED
# 02_Data_dictionary.xlsx: variable definitions, not observations.
# Crop-rotation and management variables named in the description are not in the workbook.


carob_script <- function(path) {

"
Replication Data for: Dataset of Soil Properties for Soil Organic Carbon (SOC) Estimation in Andean Agroecosystems

This dataset was generated to support the estimation of soil organic carbon (SOC) by integrating climatic data, soil physicochemical properties, and management variables derived from crop rotation histories in farmer-managed fields. In August 2022, 189 agricultural fields were sampled using a composite sampling approach consisting of five subsamples collected from the center of each field. Soil samples were taken at a depth of 0–24 cm for physicochemical analysis, and at 15 cm for dry bulk density (Bd) determination using the core method. This involved extracting two samples from the center of each field (at 0.15 and 0.30 m depth) using volumetric cylinders with diameters of 50 mm and heights of 50.8 mm.

The measured variables included pH (1:1 soil–water suspension), electrical conductivity (EC) (1:1 extract), available phosphorus (P) (Olsen method), available potassium (K) (buffered acetate extraction), soil organic carbon (SOC) determined using the Walkley and Black method, and cation exchange capacity (CEC) measured with buffered ammonium acetate at pH 7.0. Exchangeable cations—calcium (eCa), magnesium (eMg), potassium (eK), and sodium (eNa)—as well as exchangeable acidity (eAC), were analyzed by atomic absorption spectrophotometry. Particle size distribution (sand, silt, and clay) was determined using the Bouyoucos hydrometer method. Additionally, the nitrogen isotopic composition (δ¹⁵N) of dried soil samples was analyzed at the Stable Isotope Facility, University of California, Davis.
"

	uri <- "doi:10.21223/P3/RHE9QI"
	group <- "soil_samples"
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
		carob_completion = 85,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "01_Data of Soil Physical and Chemical Properties.xlsx"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
		sample_id = as.character(r$ID),
		country = "Peru",
		location = "Chugay",
		latitude = as.numeric(r$Lat),
		longitude = as.numeric(r$Lon),
		elevation = as.numeric(r$Elv),
		depth_top = 0,
		depth_bottom = 24,
		soil_pH = as.numeric(r$pH),
		soil_EC = as.numeric(r$EC),
		soil_SOM = as.numeric(r$OM),
		soil_P = as.numeric(r$P),
		soil_P_method = "Olsen",
		soil_K = as.numeric(r$K),
		soil_sand = as.numeric(r$Sa),
		soil_silt = as.numeric(r$Si),
		soil_clay = as.numeric(r$Cy),
		soil_CEC = as.numeric(r$CEC),
		soil_Ca_exch = as.numeric(r$eCa),
		soil_Mg_exch = as.numeric(r$eMg),
		soil_K_exch = as.numeric(r$eK),
		soil_Na_exch = as.numeric(r$eNa),
		soil_ex_acidity = as.numeric(r$eAC),
		soil_bd = as.numeric(r$Bd),
		soil_d15N_ = as.numeric(r$N15),
		geo_from_source = TRUE,
		on_farm = TRUE,
		is_survey = TRUE
	)

	carobiner::write_files(path, meta, d)
}
