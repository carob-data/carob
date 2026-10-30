# R script for "carob"
# license: GPL (>=3)

## NOTES
# Farmer ranking of common bean varieties (PVS interviews, 93 farmers in
# Embu and Nakuru, 2019). Combined Dataset.dta labels used for variety
# names. Long table: one row per farmer × ranked variety (variety1-3).
# land and land_beans are already in hectares in the combined file.

## ISSUES
# No plot-level yield; questionnaire ranks only. County coordinates
# estimated (not household GPS). Variety type for many local names is
# uncertain. write_files: missing yield, yield_moisture, yield_part,
# N/P/K_fertilizer, harvest_date.

carob_script <- function(path) {

"
Gender-Responsive Participatory Variety Selection in Kenya: Implications for Common Bean (Phaseolus vulgaris L.) Breeding in Kenya

The data collection aimed to investigate gender-responsive participatory variety selection (PVS) for common bean breeding in Kenya. It was conducted across Embu and Nakuru Counties in 2019 to understand gender differences in varietal and trait preferences among farmers. The study focused on preferences for biofortified released and local bean varieties, covering major topics like yield, maturity, pest resistance, marketability, and cooking time. The Principal Investigators sought to identify gender-specific adoption barriers, potential trade-offs, and socio-economic influences on varietal preferences

 Methodology:The study used a modified gender-responsive PVS tool integrating socio-demographic data with gender gap questions. Data was collected from 93 farmers through systematic random sampling. On-farm trials and demonstrations were conducted to facilitate farmer engagement, and participants ranked their preferred bean varieties. Descriptive statistical analysis was used to highlight gendered differences in preferences, examining factors such as land ownership, age, and education.
"

	uri <- "doi:10.7910/DVN/MO9LPT"
	group <- "varieties_other"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIAT",
		publication = NA,
		project = NA,
		design = "gender-responsive PVS interviews",
		data_type = "survey",
		treatment_vars = "variety",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 60,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Combined Dataset.dta"]
	r <- as.data.frame(haven::as_factor(haven::read_dta(f)))

	# one row per ranked variety
	d <- lapply(1:3, function(i) {
		data.frame(
			hhid = as.character(r$id),
			adm1 = as.character(r$county),
			farmer_gender = tolower(as.character(r$sex)),
			variety = as.character(r[[paste0("variety", i)]]),
			rank_score = i,
			field_size = as.numeric(r$land_beans)
		)
	})
	d <- do.call(rbind, d)
	d <- d[!is.na(d$variety) & nzchar(trimws(d$variety)), ]

	local <- c("Local white", "Local", "Rose coco", "Wairimu", "Katune",
		"Mwitamania", "Yellow bean", "Red beans")
	biof <- c("Angaza", "Nyota")
	d$variety_type <- ifelse(d$variety %in% local, "landrace", "improved")
	d$variety_traits <- ifelse(d$variety %in% biof, "biofortified", NA_character_)
	d$treatment <- d$variety
	d$crop <- "common bean"
	d$country <- "Kenya"
	d$planting_date <- "2019"
	d$trial_id <- d$hhid
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- NA

	geo <- data.frame(
		adm1 = c("Embu", "Nakuru"),
		longitude = c(37.457, 36.080),
		latitude = c(-0.539, -0.303),
		geo_uncertainty = c(25000, 40000),
		geo_source = "estimated from county"
	)
	d <- merge(d, geo, by="adm1", all.x=TRUE)
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
