# R script for "carob"
# license: GPL (>=3)

## NOTES
# Sorghum with undersown vetch. Grain yield is already kg/ha. Stover is t/ha and is stored as kg/ha. The dictionary does not say whether stover is fresh or dry.
# Vetch forage is dry-matter yield in t/ha, stored as kg/ha. yield_moisture for vetch is 100 minus the dry-matter percentage. Sorghum grain has no moisture value.
# Treatment 1 is sole sorghum. Treatments 2 to 7 are sorghum with vetch. Row spacing is taken from the treatment text where it is stated.
# The dictionary gives collection periods (March 2014 to February 2015, and March 2015 to January 2016). Those are not planting and harvest dates.
# This dataset does not name the site. The project is in north-western Ethiopia.

## ISSUES
# No coordinates, no site name, and no sorghum grain moisture.
# write_files: , on_farm, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: stover_yield_kg_ha_.

## NEW VARIABLES
# stover_yield_kg_ha_: stover in kg/ha. The dictionary says t/ha and does not say fresh or dry.


## NOT PROCESSED
# Head weight and head length are per-head measurements, not a per-hectare yield.
# DataDictionary_ElementDescription.csv: documentation or a codebook, not an observation table.
# DataDictionary_Introduction.csv: documentation or a codebook, not an observation table.
# DataDictionary_UniqueIdentifier.csv: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Evaluation of undersowing vetch in sorghum for intensifying existing production systems. Data for the ICARDA project on land degradation in the highland dry areas of north-western Ethiopia. Year 1 is March 2014 to February 2015 and year 2 is March 2015 to January 2016.
"

	uri <- "hdl:20.500.11766.1/FK2/SXXKWI"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=0,
		data_organization = "ICARDA",
		publication = NA,
		project = "Reducing land degradation and farmers' vulnerability to climate change in the highland dry areas of north-western Ethiopia",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.45
	)

	f1 <- ff[basename(ff) == "Sorghum_Data.csv"]
	f2 <- ff[basename(ff) == "Vetch_Data.csv"]
	f3 <- ff[basename(ff) == "Treatment.csv"]
	r1 <- read.csv(f1, sep=";", check.names=FALSE)
	r2 <- read.csv(f2, sep=";", check.names=FALSE)
	r3 <- read.csv(f3, sep=";", check.names=FALSE)
	names(r1) <- trimws(names(r1))
	names(r2) <- trimws(names(r2))
	names(r3) <- trimws(names(r3))
	lab <- r3$Treatment_Detail
	names(lab) <- r3$Treatment
	space <- c("2"=75, "3"=75, "4"=75, "5"=150, "6"=150, "7"=150)

	d1 <- data.frame(
		trial_id = paste0("year", r1$Year),
		country = "Ethiopia",
		plot_id = paste(r1$Year, r1$Treatment, r1$Replication, sep="_"),
		rep = r1$Replication,
		crop = "sorghum",
		treatment = unname(lab[as.character(r1$Treatment)]),
		row_spacing = unname(space[as.character(r1$Treatment)]),
		intercropped = r1$Treatment != 1,
		intercrops = ifelse(r1$Treatment == 1, NA_character_, "vetch"),
		yield = as.numeric(r1$Grain_yield),
		yield_moisture = NA_real_,
		yield_part = "grain",
		stover_yield_kg_ha_ = as.numeric(r1$Stover_yield) * 1000,
		plant_height = as.numeric(r1$Plant_height),
		seed_weight = as.numeric(r1[["1000sw"]]),
		geo_from_source = FALSE,
		is_survey = FALSE
	)

	d2 <- data.frame(
		trial_id = paste0("year", r2$Year),
		country = "Ethiopia",
		plot_id = paste(r2$Year, r2$Treatment, r2$Replication, sep="_"),
		rep = r2$Replication,
		crop = "vetch",
		treatment = unname(lab[as.character(r2$Treatment)]),
		row_spacing = unname(space[as.character(r2$Treatment)]),
		intercropped = TRUE,
		intercrops = "sorghum",
		yield = as.numeric(r2$Dry_matter_forage_yield) * 1000,
		yield_moisture = 100 - as.numeric(r2$Dry_matter_percentage),
		yield_part = "aboveground biomass",
		stover_yield_kg_ha_ = NA_real_,
		plant_height = as.numeric(r2$Plant_height),
		seed_weight = NA_real_,
		geo_from_source = FALSE,
		is_survey = FALSE
	)

	d <- rbind(d1, d2)
	carobiner::write_files(path, meta, d)
}
