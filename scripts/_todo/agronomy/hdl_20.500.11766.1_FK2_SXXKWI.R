# R script for "carob"
# license: GPL (>=3)

## NOTES
# Sorghum with undersown vetch. Grain yield is already kg/ha. Stover is t/ha and is stored as kg/ha. The dictionary does not say whether stover is fresh or dry.
# Vetch forage is dry-matter yield in t/ha, stored as kg/ha in both yield and dmy_total with yield_moisture = 0 and yield_isfresh = FALSE (previous script incorrectly set yield_moisture = 100 - DM% on a dry-matter yield). Sorghum grain has no moisture value.
# Treatment 1 is sole sorghum (intercrops = "none"). Treatments 2 to 7 are sorghum with vetch. Row spacing is taken from the treatment text where it is stated.
# The dictionary gives collection periods (March 2014 to February 2015, and March 2015 to January 2016). Those are not planting and harvest dates.
# ICARDA-ARARI project on land degradation in north-western Ethiopia (Gumara-Maksegnit watershed, North Gondar, Amhara; approximate watershed coordinates imputed with geo_from_source = FALSE).

## ISSUES
# Coordinates are approximate for the Gumara-Maksegnit watershed (geo_from_source = FALSE); no sorghum grain moisture or planting/harvest dates in the raw files.
# Sorghum stover (t/ha -> kg/ha) is stored in y_residue because fresh vs. dry is not stated.

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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 55,
		carob_effort = 0
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
		adm1 = "Amhara",
		adm2 = "North Gondar",
		location = "Gumara-Maksegnit",
		latitude = 12.392,
		longitude = 37.559,
		plot_id = paste(r1$Year, r1$Treatment, r1$Replication, sep="_"),
		rep = r1$Replication,
		crop = "sorghum",
		treatment = unname(lab[as.character(r1$Treatment)]),
		row_spacing = unname(space[as.character(r1$Treatment)]),
		intercropped = r1$Treatment != 1,
		intercrops = ifelse(r1$Treatment == 1, "none", "vetch"),
		yield = as.numeric(r1$Grain_yield),
		dmy_total = NA_real_,
		yield_moisture = NA_real_,
		yield_isfresh = NA,
		yield_part = "grain",
		y_residue = as.numeric(r1$Stover_yield) * 1000,
		plant_height = as.numeric(r1$Plant_height),
		seed_weight = as.numeric(r1[["1000sw"]]),
		geo_from_source = FALSE,
		is_survey = FALSE
	)

	d2 <- data.frame(
		trial_id = paste0("year", r2$Year),
		country = "Ethiopia",
		adm1 = "Amhara",
		adm2 = "North Gondar",
		location = "Gumara-Maksegnit",
		latitude = 12.392,
		longitude = 37.559,
		plot_id = paste(r2$Year, r2$Treatment, r2$Replication, sep="_"),
		rep = r2$Replication,
		crop = "vetch",
		treatment = unname(lab[as.character(r2$Treatment)]),
		row_spacing = unname(space[as.character(r2$Treatment)]),
		intercropped = TRUE,
		intercrops = "sorghum",
		yield = as.numeric(r2$Dry_matter_forage_yield) * 1000,
		dmy_total = as.numeric(r2$Dry_matter_forage_yield) * 1000,
		yield_moisture = 0,
		yield_isfresh = FALSE,
		yield_part = "aboveground biomass",
		y_residue = NA_real_,
		plant_height = as.numeric(r2$Plant_height),
		seed_weight = NA_real_,
		geo_from_source = FALSE,
		is_survey = FALSE
	)

	d <- rbind(d1, d2)
	carobiner::write_files(path, meta, d)
}
