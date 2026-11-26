# R script for "carob"
# license: GPL (>=3)

## NOTES
# Farmer survey of potato seed degeneration in the Andes of Ecuador. median_yield is bags of 45.45 kg harvested per bag of 45.45 kg planted (yield_ratio_), not kg/ha.
# Single-virus DAS-ELISA percentages (PVS, PVX, PVY, PLRV, APLV, APMoV) are stored in the long table under disease and disease_incidence (%). Co-infection combination percentages and clean-seed percentage are kept on the wide table.

## ISSUES
# No coordinates or place names below the country. No kg/ha yield.
# Andean potato latent virus and Andean potato mottle virus are not yet in terminag values_disease; kept in disease per Carob guidelines so maintainers can add them.
# The source has 184 rows, 181 IDs, two fully repeated rows, and one ID that occurs twice with different values. Each row keeps its own record_id and trial_id.

## NOT PROCESSED
# None. Combination columns are kept.

## NEW VARIABLES
# farm_type_: farmer typology code.
# seed_age_: how long the seed lot was kept without renewal.
# yield_ratio_: bags harvested per bag planted (45.45 kg bags).
# clean_seed_ and co-infection columns (pvs_pvx_, ..., pvs_pvx_pvy_plrv_): percent of the seed lot (%) with no virus or specified multi-virus co-infection.


carob_script <- function(path) {

"
Dataset for: Seed degeneration in a Andean highland landscape

Potato seed degeneration threatens productivity of small-scale potato producers in Ecuador. For this reason, two studies were carried out a socio-economical survey and a exploratory survey to identify the presence of viruses in the seed lots of small-scale potato farmers. This relates with internal report to understand potato seed degeneration in the Andes of Ecuador.
"

	uri <- "doi:10.21223/KTWFGH"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "disease_incidence",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "Seed_degeneration_in_a_Andean_highland_landscape.xlsx"]
	r <- carobiner::read.excel(f, sheet="Data")
	d <- data.frame(
		record_id = seq_len(nrow(r)),
		hhid = as.character(r$ID),
		farm_type_ = as.character(r$farmtype),
		seed_age_ = trimws(as.character(r$timeclass)),
		yield_ratio_ = as.numeric(r$median_yield),
		pvs_pvx_pvy_plrv_ = as.numeric(r$perfour),
		pvs_pvx_pvy_ = as.numeric(r$persxy),
		pvs_pvx_plrv_ = as.numeric(r$persxl),
		pvx_pvy_plrv_ = as.numeric(r$perxyl),
		pvs_pvy_plrv_ = as.numeric(r$persyl),
		pvs_pvx_ = as.numeric(r$persx),
		pvs_pvy_ = as.numeric(r$persy),
		pvx_pvy_ = as.numeric(r$perxy),
		pvx_plrv_ = as.numeric(r$perxl),
		pvs_plrv_ = as.numeric(r$persl),
		pvy_plrv_ = as.numeric(r$peryl),
		clean_seed_ = as.numeric(r$perno_virus)
	)
	d$crop <- "potato"
	d$country <- "Ecuador"
	d$trial_id <- paste0("Ecuador_seed_", d$hhid, "_", seq_len(nrow(d)))
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE
	d$stress <- "disease"

	vspecs <- list(
		list("pers", "potato virus S"),
		list("perx", "potato virus X"),
		list("pery", "potato virus Y"),
		list("perl", "potato leafroll virus"),
		list("peraplv", "Andean potato latent virus"),
		list("perapmov", "Andean potato mottle virus")
	)
	dlong <- do.call(rbind, lapply(seq_along(vspecs), function(i) {
		val <- as.numeric(r[[vspecs[[i]][[1]]]])
		keep <- !is.na(val)
		data.frame(
			record_id = d$record_id[keep],
			order = i,
			disease = vspecs[[i]][[2]],
			disease_incidence = as.character(val[keep])
		)
	}))

	carobiner::write_files(path, meta, d, long=dlong)
}
