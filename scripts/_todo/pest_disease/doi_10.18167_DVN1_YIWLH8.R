# R script for "carob"
# license: GPL (>=3)

## NOTES
# Macroinvertebrate counts in six banana fields in Martinique, three agroforestry and three conventional. Country is geographicCoverage.
# The file writes Conventionalforestry for the conventional system.
# Date codes (2Q, 2V, 3Q, 3V, 4Q, 4V) are campaign labels. They are not calendar dates, and the description's years 2019-2021 are not assigned to them.
# abundance is the count in the sample, not a per-hectare density.
# Identical count rows are dropped.

## ISSUES
# missing variables: yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variable: trap_method_ (method?)
# unknown variable: taxon_order_ (order?)
# unknown variables: system_, campaign_, pair_, taxon_class_, taxon_family_, taxon_species_, abundance_, diversity_used_

## NOT PROCESSED
# arthropods_imagej.csv and earthworms_imagej.csv. Body-size measurements.
# soil_properties.csv, canopee_opening.csv, composition_litter.csv, and traits_litter.csv.
# arthropods_imagej_labels.xlsx: not read. sheet Feuil1: Date, The year of the sample among the three fields. Observation records are missing from d until this file is standardized.
# canopee_opening_labels.xlsx: not read. sheet Feuil1: Date, The year of the sample among the three fields. Observation records are missing from d until this file is standardized.
# composition_litter_labels.xlsx: not read. sheet Feuil1: Category_TW, The masse of the twig of the litter sample (g). Observation records are missing from d until this file is standardized.
# diversity _label.csv: not read. columns: Date;The year of the sample among the three fields. Observation records are missing from d until this file is standardized.
# earthworms_imagej_labels.xlsx: not read. sheet Feuil1: Date, The year of the sample among the three fields. Observation records are missing from d until this file is standardized.
# soil_properties_labels.xlsx: not read. sheet Feuil1: Plot, The plot of the sample among the six plots. Observation records are missing from d until this file is standardized.
# traits_litter_labels.xlsx: not read. sheet Feuil1: Date, The year of the sample among the three fields. Observation records are missing from d until this file is standardized.


## NEW VARIABLES
# system_: agroforestry or conventional.
# campaign_: sampling code in the Date column.
# pair_: plot pair named in the file.
# trap_method_: litter quadrat or soil quadrat method, as stored.
# taxon_class_: class.
# taxon_order_: order.
# taxon_family_: family.
# taxon_species_: species name, as stored.
# abundance_: individuals in the sample.
# diversity_used_: TRUE when the row was flagged for the diversity analysis.


carob_script <- function(path) {

"
Abundance, Biomass, and Functional Traits of Macroinvertebrates and Environmental Parameters in Banana Agroecosystems: A Dataset Comparing Agroforestry and Conventional Systems

This dataset includes values for abundance, biomass, and functional traits of macroinvertebrates (arthropods and earthworms), as well as data on soil physicochemical properties, canopy cover and litter traits. A total of 144 samples were collected from the soil of 6 banana fields (3 conventional and 3 agroforestry) in Martinique. Three sampling campaigns were conducted over three consecutive years (2019, 2020, and 2021). Eight independent sampling points were defined per plot for each of which two distinct methods were used: (i) the litter quadrat followed by Tullgren extraction of macroarthropods and (ii) the soil quadrat followed by manual sorting of soil to collect earthworms until 30 cm depth. The individuals were identified to the most precise taxonomic level possible. Soil physico-chemical properties and vegetation cover were measured only during the last campaign.
"

	uri <- "doi:10.18167/DVN1/YIWLH8"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "system_",
		response_vars = "abundance_",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "diversity.csv"]
	# Species names are Latin-1. Read that way so later name checks see valid UTF-8.
	r <- read.csv(f, sep=";", na.strings=c("NA", ""), check.names=FALSE, fileEncoding="Latin1")
	names(r) <- trimws(names(r))
	r <- r[, names(r) != ""]
	for (nm in names(r)) if (is.character(r[[nm]])) r[[nm]] <- enc2utf8(r[[nm]])

	sys <- trimws(r$System)
	sys[sys == "Agroforestry"] <- "agroforestry"
	sys[sys == "Conventionalforestry"] <- "conventional"
	used <- tolower(trimws(r$diversity))

	d <- data.frame(
		trial_id = paste(trimws(r$Plot), trimws(r$Date)),
		plot_id = trimws(r$Plot),
		country = "Martinique",
		crop = "banana",
		system_ = sys,
		treatment = sys,
		campaign_ = trimws(r$Date),
		pair_ = trimws(r$Pair),
		trap_method_ = trimws(r$trapping_method),
		taxon_class_ = trimws(r$nom_classe),
		taxon_order_ = trimws(r$nom_ordre),
		taxon_family_ = trimws(r$nom_famille),
		taxon_species_ = trimws(r$nom_espece),
		abundance_ = as.numeric(r$abundance),
		diversity_used_ = ifelse(used == "yes", TRUE, ifelse(used == "no", FALSE, NA)),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)
	d <- d[!duplicated(d), ]

	carobiner::write_files(path, meta, d)
}
