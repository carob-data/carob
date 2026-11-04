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

	fs <- ff[basename(ff) == "Sorghum_Data.csv"]
	fv <- ff[basename(ff) == "Vetch_Data.csv"]
	ft <- ff[basename(ff) == "Treatment.csv"]
	s <- read.csv(fs, sep=";", check.names=FALSE)
	v <- read.csv(fv, sep=";", check.names=FALSE)
	tr <- read.csv(ft, sep=";", check.names=FALSE)
	names(s) <- trimws(names(s))
	names(v) <- trimws(names(v))
	names(tr) <- trimws(names(tr))
	lab <- tr$Treatment_Detail
	names(lab) <- tr$Treatment
	space <- c("2"=75, "3"=75, "4"=75, "5"=150, "6"=150, "7"=150)

	d1 <- data.frame(
		trial_id = paste0("year", s$Year),
		country = "Ethiopia",
		plot_id = paste(s$Year, s$Treatment, s$Replication, sep="_"),
		rep = s$Replication,
		crop = "sorghum",
		treatment = unname(lab[as.character(s$Treatment)]),
		row_spacing = unname(space[as.character(s$Treatment)]),
		intercropped = s$Treatment != 1,
		intercrops = ifelse(s$Treatment == 1, NA_character_, "vetch"),
		yield = as.numeric(s$Grain_yield),
		yield_moisture = NA_real_,
		yield_part = "grain",
		stover_yield_kg_ha_ = as.numeric(s$Stover_yield) * 1000,
		plant_height = as.numeric(s$Plant_height),
		seed_weight = as.numeric(s[["1000sw"]]),
		geo_from_source = FALSE,
		is_survey = FALSE
	)

	d2 <- data.frame(
		trial_id = paste0("year", v$Year),
		country = "Ethiopia",
		plot_id = paste(v$Year, v$Treatment, v$Replication, sep="_"),
		rep = v$Replication,
		crop = "vetch",
		treatment = unname(lab[as.character(v$Treatment)]),
		row_spacing = unname(space[as.character(v$Treatment)]),
		intercropped = TRUE,
		intercrops = "sorghum",
		yield = as.numeric(v$Dry_matter_forage_yield) * 1000,
		yield_moisture = 100 - as.numeric(v$Dry_matter_percentage),
		yield_part = "aboveground biomass",
		stover_yield_kg_ha_ = NA_real_,
		plant_height = as.numeric(v$Plant_height),
		seed_weight = NA_real_,
		geo_from_source = FALSE,
		is_survey = FALSE
	)

	d <- rbind(d1, d2)
	carobiner::write_files(path, meta, d)
}
