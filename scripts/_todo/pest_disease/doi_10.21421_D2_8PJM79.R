# R script for "carob"
# license: GPL (>=3)

## NOTES
# Finger millet blast nursery FMBN-3 at ARS, ANGRAU, Vizianagaram, rainy 2020. Sowing date on the sheet is 29/07/2020.
# The sheet has a title block and two header rows. Data start at row 7. Two blank separator rows were dropped.
# Leaf blast is a 0–9 score at 30 days after sowing and again at soft dough. Neck and finger blast are incidence (percent).
# Panicle, infected-panicle, finger, and infected-finger columns are means (they are not integers). The sheet does not say per plant or per plot; values are about 2–9, so they were left unscaled.
# The long table repeats leaf blast (with growth stage), neck blast incidence, and finger blast incidence.
# State is Andhra Pradesh from the Dataverse record. There is no coordinate in the file or the metadata.

## ISSUES
# No yield, fertilizer, plot size, or coordinates. Vizianagaram still needs a georeference.
# Disease names are not in the disease vocabulary.
# ENTRY_TYPE is "T" on every data row, including Local check, GPU 28, and VR 708, so it was not used as a check flag.
# No publication DOI in the Dataverse record.
# write_files: missing carob_contributor, yield, yield_part, yield_moisture, latitude, longitude, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, harvest_date.
# unknown variables: the leaf, neck, and finger blast scores and counts. The hint leaf_B is leaf boron, not blast.
# invalid terms: disease finger millet leaf blast, finger millet neck blast, finger millet finger blast.

## NOT PROCESSED
# xlsx, OBS_UNIT_ID: plot UUID.
# xlsx, ENTRY_TYPE: "T" on every row.
# xlsx, Entry No: one number per designation.

## NEW VARIABLES
# leaf_blast_30_das_: leaf blast score at 30 days after sowing, 0–9 scale
# leaf_blast_soft_dough_: leaf blast score at soft dough, 0–9 scale
# neck_blast_incidence_: neck blast incidence, percent
# finger_blast_incidence_: finger blast incidence, percent
# neck_blast_panicles_: panicle count recorded with neck blast (basis not stated)
# neck_blast_infected_panicles_: infected panicle count (basis not stated)
# finger_blast_fingers_: finger count recorded with finger blast (basis not stated)
# finger_blast_infected_fingers_: infected finger count (basis not stated)

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
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "leaf_blast_30_das_;leaf_blast_soft_dough_;neck_blast_incidence_;finger_blast_incidence_",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "FMBN-3 Angrau Viziainagram.xlsx"]
	r0 <- carobiner::read.excel(f, skip = 6, col_names = FALSE)
	r0 <- r0[!is.na(r0[[5]]), ]
	colnames(r0) <- c("obs_id", "entry_type", "gid", "designation", "plot_no", "entry_no", "rep_no",
		"lb30", "lb_soft", "neck_panicles", "neck_infected", "neck_pct",
		"finger_n", "finger_infected", "finger_pct")

	d <- data.frame(
		country = "India",
		adm1 = "Andhra Pradesh",
		location = "ARS Vizianagaram",
		crop = "finger millet",
		variety = trimws(r0$designation),
		variety_code = trimws(r0$gid),
		treatment = trimws(r0$designation),
		rep = as.integer(r0$rep_no),
		plot_id = as.character(r0$plot_no),
		leaf_blast_30_das_ = as.numeric(r0$lb30),
		leaf_blast_soft_dough_ = as.numeric(r0$lb_soft),
		neck_blast_panicles_ = as.numeric(r0$neck_panicles),
		neck_blast_infected_panicles_ = as.numeric(r0$neck_infected),
		neck_blast_incidence_ = as.numeric(r0$neck_pct),
		finger_blast_fingers_ = as.numeric(r0$finger_n),
		finger_blast_infected_fingers_ = as.numeric(r0$finger_infected),
		finger_blast_incidence_ = as.numeric(r0$finger_pct),
		stress = "disease",
		season = "kharif",
		planting_date = "2020-07-29",
		trial_id = "1",
		record_id = as.integer(seq_len(nrow(r0))),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	blank <- rep(NA_character_, nrow(d))
	x <- rbind(
		data.frame(
			record_id = d$record_id,
			growth_stage = "30 DAS",
			disease = "finger millet leaf blast",
			disease_severity = as.character(d$leaf_blast_30_das_),
			disease_incidence = blank,
			severity_scale = "0-9"
		),
		data.frame(
			record_id = d$record_id,
			growth_stage = "soft dough",
			disease = "finger millet leaf blast",
			disease_severity = as.character(d$leaf_blast_soft_dough_),
			disease_incidence = blank,
			severity_scale = "0-9"
		),
		data.frame(
			record_id = d$record_id,
			growth_stage = blank,
			disease = "finger millet neck blast",
			disease_severity = blank,
			disease_incidence = as.character(d$neck_blast_incidence_),
			severity_scale = blank
		),
		data.frame(
			record_id = d$record_id,
			growth_stage = blank,
			disease = "finger millet finger blast",
			disease_severity = blank,
			disease_incidence = as.character(d$finger_blast_incidence_),
			severity_scale = blank
		)
	)

	carobiner::write_files(path, meta, d, long = x)
}
