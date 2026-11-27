# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Coordinates for ARS Vizianagaram (Gajularega) are not in the file or Dataverse metadata and were georeferenced externally.
# ENTRY_TYPE is "T" on every row, including "Local check", "GPU 28", and "VR 708"; only "Local check" was flagged in variety_type.
# Panicle and finger counts are decimal plot means (likely 5-plant averages), but the sampling unit is not stated in the file.

## NEW
# neck_blast_panicles_: mean panicle count recorded for neck blast (sampling unit not stated)
# neck_blast_infected_panicles_: mean infected panicle count for neck blast (sampling unit not stated)
# finger_blast_fingers_: mean finger count recorded for finger blast (sampling unit not stated)
# finger_blast_infected_fingers_: mean infected finger count for finger blast (sampling unit not stated)
# disease: finger millet leaf blast; finger millet neck blast; finger millet finger blast

## MISSING
# yield, yield_part, yield_moisture, harvest_date, irrigated, N_fertilizer, P_fertilizer, K_fertilizer

## NOTES
# Finger millet blast resistance nursery (FMBN-3) with 18 entries x 3 reps at ARS (ANGRAU), Vizianagaram, Andhra Pradesh during rainy (kharif) 2020.
# Sowing date from sheet header is 2020-07-29; Dataverse timePeriodCovered ends 2020-11-06 (not assumed to be harvest_date).
# Leaf blast severity (0-9 scale at 30 DAS and soft dough stage) and neck/finger blast incidence (%) are stored in the long table without wide duplication.

## NOT PROCESSED
# FMBN-3 Angrau Viziainagram.xlsx, ENTRY_TYPE: constant "T" across all rows (including checks).

carob_script <- function(path) {

"
Replication data for evaluation of blast resistance (FMBN-3) in finger millet conducted at ARS, Vizianagaram during rainy 2020 season

The trial contains information on blast scores (Leaf blast, neck blast and finger blast) carried out on a set of 18 entries at ARS, ANGRAU, Vizianagaram during rainy 2020 season. Blast resistant materials identified from the study could be promoted for release as a variety or as parent in crossing program.
"

	uri <- "doi:10.21421/D2/8PJM79"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
		data_organization = "ICRISAT",
		publication = NA,
		project = "CRP-GLDC",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "disease_severity;disease_incidence",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "FMBN-3 Angrau Viziainagram.xlsx"]
	r <- carobiner::read.excel(f, skip = 6, col_names = FALSE)
	r <- r[!is.na(r[[5]]), ]
	colnames(r) <- c("obs_id", "entry_type", "gid", "designation", "plot_no", "entry_no", "rep_no",
		"lb30", "lb_soft", "neck_panicles", "neck_infected", "neck_pct",
		"finger_n", "finger_infected", "finger_pct")

	d <- data.frame(
		country = "India",
		adm1 = "Andhra Pradesh",
		adm2 = "Vizianagaram",
		location = "ARS Vizianagaram",
		crop = "finger millet",
		variety = trimws(r$designation),
		variety_code = trimws(as.character(r$gid)),
		treatment = trimws(r$designation),
		rep = as.integer(r$rep_no),
		plot_id = as.character(r$plot_no),
		neck_blast_panicles_ = as.numeric(r$neck_panicles),
		neck_blast_infected_panicles_ = as.numeric(r$neck_infected),
		finger_blast_fingers_ = as.numeric(r$finger_n),
		finger_blast_infected_fingers_ = as.numeric(r$finger_infected),
		stress = "disease",
		season = "kharif",
		planting_date = "2020-07-29",
		trial_id = "1",
		record_id = as.integer(seq_len(nrow(r))),
		on_farm = FALSE,
		is_survey = FALSE,
		longitude = 83.3988,
		latitude = 18.1238,
		geo_from_source = FALSE,
		geo_uncertainty = 1500,
		geo_source = "ANGRAU ARS Vizianagaram (Gajularega)"
	)
	d$variety_type <- ifelse(d$variety == "Local check", "local check", NA_character_)

	n <- nrow(d)
	long <- rbind(
		data.frame(
			record_id = d$record_id,
			order = rep(1L, n),
			DAP = rep(30L, n),
			growth_stage = rep(NA_character_, n),
			disease = rep("finger millet leaf blast", n),
			disease_severity = as.character(round(as.numeric(r$lb30), 4)),
			disease_incidence = rep(NA_character_, n),
			severity_scale = rep("0-9", n)
		),
		data.frame(
			record_id = d$record_id,
			order = rep(2L, n),
			DAP = rep(NA_integer_, n),
			growth_stage = rep("soft dough", n),
			disease = rep("finger millet leaf blast", n),
			disease_severity = as.character(round(as.numeric(r$lb_soft), 4)),
			disease_incidence = rep(NA_character_, n),
			severity_scale = rep("0-9", n)
		),
		data.frame(
			record_id = d$record_id,
			order = rep(3L, n),
			DAP = rep(NA_integer_, n),
			growth_stage = rep(NA_character_, n),
			disease = rep("finger millet neck blast", n),
			disease_severity = rep(NA_character_, n),
			disease_incidence = as.character(round(as.numeric(r$neck_pct), 4)),
			severity_scale = rep(NA_character_, n)
		),
		data.frame(
			record_id = d$record_id,
			order = rep(4L, n),
			DAP = rep(NA_integer_, n),
			growth_stage = rep(NA_character_, n),
			disease = rep("finger millet finger blast", n),
			disease_severity = rep(NA_character_, n),
			disease_incidence = as.character(round(as.numeric(r$finger_pct), 4)),
			severity_scale = rep(NA_character_, n)
		)
	)

	carobiner::write_files(path, meta, d, long = long)
}
