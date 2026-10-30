# R script for "carob"
# license: GPL (>=3)

## NOTES
# Farmer-field sorghum biomass samples in eastern Uganda. Grain, chaff and straw yields are dry weight in t/ha, stored as kg/ha. yield_moisture is 0 because the grain yield is dry weight.
# Plant height is in metres in the file and in cm here. Plant density is per m2 in the file and per ha here.
# CKAN coverage_x is 1.37333 and coverage_y is 32.29028. With the country given as Uganda, those are latitude and longitude. One point is stored for every field.
# The file has no household id. hhid is the field code (Plot_ID).

## ISSUES
# No planting date, harvest date, or fertilizer. The coordinate is one point for the dataset.
# write_files: missing variables: carob_contributor. unknown variables: chaff_yield_kg_ha_.

## NEW VARIABLES
# chaff_yield_kg_ha_: dry chaff yield, kg/ha.


## NOT PROCESSED
# Plot area, quadrant area, and the height standard deviation.
# Sorghum_biomass_sampl_metadata.xlsx and the R script in the deposit.


carob_script <- function(path) {

"
IITA-ADC biochar study Uganda on Sorghum

Increasing organic matter/carbon contents of soils is one option from a basket of strategies being proposed to offset climate change inducing greenhouse gas (GHG) emissions, under the auspices of the Paris-COP 4 per mille initiative. Herein we assessed the productivity and usage of biomass waste from maize, sorghum, rice, millet and groundnut crops, based on measurements from multiple farmer fields and census/surveys in eastern Uganda.
"

	uri <- "doi:10.25502/FBGW-1M42/D"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		response_vars = "yield",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "sorghum_biomass_sampling.csv"]
	r <- read.csv(f, check.names=FALSE)

	d <- data.frame(
		trial_id = paste0("season", r$Season),
		country = "Uganda",
		hhid = r$Plot_ID,
		plot_id = r$Qudrant_ID,
		crop = "sorghum",
		yield = as.numeric(r$Yield_grain_dry_ton_ha_1) * 1000,
		yield_moisture = 0,
		yield_part = "grain",
		dmy_residue = as.numeric(r$Yield_straw_dry_ton_ha_1) * 1000,
		chaff_yield_kg_ha_ = as.numeric(r$Yield_chaff_dry_ton_ha_1) * 1000,
		plant_height = as.numeric(r$Avg_Hgt_in_quadrant_m) * 100,
		plant_density = as.numeric(r$Density_plant_m_2) * 10000,
		latitude = 1.37333,
		longitude = 32.29028,
		geo_from_source = TRUE,
		on_farm = TRUE,
		is_survey = TRUE
	)

	carobiner::write_files(path, meta, d)
}
