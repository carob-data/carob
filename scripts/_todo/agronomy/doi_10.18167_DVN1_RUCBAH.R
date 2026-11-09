# R script for "carob"
# license: GPL (>=3)

## NOTES
# Litter bags in banana plantations, Martinique, 2020-2021. Country is Martinique from geographicCoverage. France is also listed there and is not used.
# Farming is organic or conventional. Mesh S and L are small and large (label file). G- is litter without glyphosate and G+ is litter with glyphosate.
# decomp in the sheet is a fraction (about 0.14 to 0.98). The label calls it a percent, but the values are mass loss divided by initial ash-free mass. mass_loss is grams.
# Soil codes in this sheet are verti and niti. The label says vertisol or ferrisol, and the temperature sheet says Ferrisol.
# Plot temperature is a daily series with an explicit mean, so it is in the weather table. It is a plot sensor, not a climate station.

## ISSUES
# missing variables: yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variables: farming_, mesh_, glyphosate_, litter_mass_loss_, decomposition_, faunal_contribution_

## NOT PROCESSED
# Earthworm and macroarthropod counts.
# Plant cover, shade, and species richness.
# Temperature range. It is the difference of the daily minimum and maximum.
# Decomposer's community_labels.csv: not read. columns: code_field;The code of the field among the six fields selected in this study. Observation records are missing from d until this file is standardized.
# Decomposition_labels.csv: not read. columns: agriculture;Plot's agricultural practices : either organic or conventional . Observation records are missing from d until this file is standardized.
# Earthworm's community.csv: not read. columns: code_field;code_sample;abundance;species_name. Observation records are missing from d until this file is standardized.
# Macroarthropod's community.csv: not read. columns: code_field;code_sample;class;order;family;abundance;species. Observation records are missing from d until this file is standardized.
# Plant variables.csv: not read. columns: agriculture;plantation;code_field;code_sample;%soil_cover;%shadow. Observation records are missing from d until this file is standardized.
# Plant variables_labels.csv: not read. columns: agriculture;Agricultural practices in the fields,  either organic or conventional. Observation records are missing from d until this file is standardized.
# Temperature & humidity_labels.csv: not read. columns: agriculture;Agricultural practices in the plots,  either organic or conventional. Observation records are missing from d until this file is standardized.


## NEW VARIABLES
# decomposition_: ash-free mass loss as a fraction of the initial ash-free mass.
# farming_: organic or conventional, as practiced on the plantation.
# mesh_: litter-bag mesh, small or large.
# glyphosate_: TRUE when the litter was glyphosate-contaminated. G- is FALSE.
# litter_mass_loss_: ash-free litter mass loss, grams per bag.
# faunal_contribution_: large-mesh decomposition minus the plot mean of small-mesh decomposition, as stored.


carob_script <- function(path) {

"
Dataset on the effect of organic farming on microbial and macrofaunal litter decomposition in banana fields

Data from a field experiment in the banana agroecosystems of Martinique (Lesser Antilles). Six organic and six conventional banana plantations located in two bioclimatic zones of the island were studied. To measure the microbial and macrofaunal contribution to decomposition, 264 bags of small- and large-mesh litter were placed and left in the field for three months. The chemical composition and decomposition of glyphosate-contaminated and glyphosate-free banana leaf litter were studied. Biotic (plant cover, abundance and composition of detritivorous macrofauna) and abiotic (microclimatic variables) characteristics were also measured.
"

	uri <- "doi:10.18167/DVN1/RUCBAH"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.apsoil.2024.105697",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "farming_;mesh_;glyphosate_",
		response_vars = "decomposition_",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.5
	)

	f1 <- ff[basename(ff) == "Decomposition.csv"]
	f2 <- ff[basename(ff) == "Temperature & humidity.csv"]
	r <- read.csv(f1, sep=";", na.strings=c("NA", ""), check.names=FALSE)
	names(r) <- trimws(names(r))

	d <- data.frame(
		trial_id = as.character(r$code_field),
		plot_id = as.character(r$code_LB),
		country = "Martinique",
		location = carobiner::fix_name(r$plantation, "title"),
		crop = "banana",
		farming_ = tolower(trimws(r$agriculture)),
		mesh_ = ifelse(r$mesh_size == "S", "small", ifelse(r$mesh_size == "L", "large", r$mesh_size)),
		glyphosate_ = r$litter_glypho == "G+",
		soil_type = trimws(r$soil),
		litter_mass_loss_ = as.numeric(r$mass_loss),
		decomposition_ = as.numeric(r$decomp),
		faunal_contribution_ = as.numeric(r$faunal_contrib),
		on_farm = TRUE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$treatment <- paste(d$farming_, d$mesh_, ifelse(d$glyphosate_, "glyphosate", "no glyphosate"))

	r1 <- read.csv(f2, sep=";", na.strings=c("NA", ""), check.names=FALSE)
	names(r1) <- trimws(names(r1))
	wth <- data.frame(
		station_id = as.character(r1$code_plot),
		station_name = carobiner::fix_name(r1$plantation, "title"),
		country = "Martinique",
		date = as.character(as.Date(r1$date, "%d/%m/%Y")),
		tmin = as.numeric(r1$Temp_min),
		tmax = as.numeric(r1$Temp_max),
		temp = as.numeric(r1$Temp_mean),
		rhmn = as.numeric(r1$hum_min),
		rhmx = as.numeric(r1$hum_max),
		rhum = as.numeric(r1$hum_mean)
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
