# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Two of the four trials (greenhouse_2023 and greenhouse_2024) are greenhouse variety-trait evaluations (kept alongside field_2023 and field_2024 and distinguished via environment_ and trial_id; reviewer may confirm whether to retain greenhouse rows).
# Field scores (FHB23, FHB24) are Fusarium head blight index (%) while greenhouse scores (GH_FHB23, GH_FHB24) are FHB severity (%); both are stored in disease_severity with severity_scale noting the distinction.
# No yield, fertilizer, irrigation, experimental design, or exact planting/harvest dates are provided (disease and mycotoxin resistance screening dataset only).

## NEW
# deoxynivalenol_: grain deoxynivalenol (DON) mycotoxin content (ppm).
# environment_: test environment ("field" or "greenhouse").

## MISSING
# yield, yield_moisture, yield_part, harvest_date, irrigated, N_fertilizer, P_fertilizer, K_fertilizer: not reported in this FHB/DON resistance screening dataset.

## NOTES
# Field (2023, 2024) and greenhouse (2023, 2024) Fusarium head blight (FHB) and deoxynivalenol (DON) screening for a panel of 47 African wheat varieties at CIMMYT El Batan, Mexico.
# Coordinates are estimated for the CIMMYT El Batan research station (Texcoco, Mexico).
# Germplasm Cross is mapped to variety, GID to variety_code, and Sel_Hist to variety_pedigree; planting_date is set to trial year ("2023" / "2024").

## NOT PROCESSED
# African_panel.csv: Ent, CID, SID (internal CIMMYT entry, cross, and source identifiers; GID is kept in variety_code).


carob_script <- function(path) {

"
Fusarium head blight data for a panel of African wheat varieties

Field and greenhouse FHB resistance evaluation for a panel of African wheat varieties, with deoxynivalenol (DON) toxin data provided.
"

	uri <- "hdl:11529/10549125"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "disease_severity",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "African_panel.csv"]
	r <- read.csv(f, na.strings = c("NA", ""))

	one <- function(sev, don, trial, yr, env, scale) {
		data.frame(
			country = "Mexico",
			adm1 = "México",
			adm2 = "Texcoco",
			location = "El Batán",
			site = "CIMMYT",
			# CIMMYT El Batan research station, Texcoco
			longitude = -98.848,
			latitude = 19.533,
			geo_from_source = FALSE,
			geo_uncertainty = 1000,
			geo_source = "CIMMYT El Batan research station",
			trial_id = trial,
			environment_ = env,
			planting_date = yr,
			crop = "wheat",
			variety = trimws(r$Cross),
			variety_code = as.character(r$GID),
			variety_pedigree = trimws(r$Sel_Hist),
			treatment = trimws(r$Cross),
			disease = "fusarium scab spike",
			disease_severity = ifelse(is.na(sev), NA_character_, as.character(round(as.numeric(sev), 2))),
			severity_scale = scale,
			deoxynivalenol_ = as.numeric(don),
			stress = "disease",
			on_farm = FALSE,
			is_survey = FALSE
		)
	}

	d <- rbind(
		one(r$FHB23, r$DON23, "field_2023", "2023", "field", "FHB index (0-100%)"),
		one(r$FHB24, NA_real_, "field_2024", "2024", "field", "FHB index (0-100%)"),
		one(r$GH_FHB23, NA_real_, "greenhouse_2023", "2023", "greenhouse", "0-100%"),
		one(r$GH_FHB24, r$GH_DON24, "greenhouse_2024", "2024", "greenhouse", "0-100%")
	)
	d <- d[!is.na(d$disease_severity) | !is.na(d$deoxynivalenol_), ]

	carobiner::write_files(path, meta, d)
}
