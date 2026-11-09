# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Each clone has one plot (the check CIP392797.22 has two), and the "Installation" sheet gives 1
#    replication. The fieldbook column "REP" (values 1-3) is therefore not a replication; its meaning
#    is not described. It is stored as block_id.
# 2. All clones were screened after inoculation with PVY (description); which plants were infected
#    is not recorded in the fieldbook.
# 3. Six plots have no yield (TTWP missing); they are kept (vigor and flowering not processed, but
#    other information such as number of plants harvested may be present).
# 4. The "Vine cutting / killing" date (2006-12-01) is only 38 days after planting; harvest on
#    2007-03-24. Not used.

## NOTES
# 1. CIP experiment station La Victoria, Huancayo (coordinates and elevation from the "Minimal" sheet).
#    Plots of 5 plants in one row, 0.3 m between plants and 0.9 m between rows (1.35 m2).
#    Irrigated (confirmed by the contributor; not stated in the data).
# 2. Yield is the non-adjusted total tuber yield (TTWP / plot size); marketable yield is not reported.
# 3. plant_density from the number of plants harvested (NPH), seed_density from tubers planted (NTP),
#    tuber_density from the total number of tubers (TNTP).
# 4. yield_moisture is 100 - average tuber dry matter (AVDM, %), measured per plot.
# 5. Not processed: plant vigor, flowering, stolons, tuber appearance, uniformity and size (1-9 scales),
#    chipping color, and the other sheets with inoculation and greenhouse lists.

carob_script <- function(path) {

"
Dataset for: Evaluation of Potato clones in Huancayo, Junin,  2006

490 clones were screened after PVY inoculation in the La Victoria area during 2006, Junin.
"

	uri <- "doi:10.21223/MVJD3J"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "unreplicated",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-06",
		carob_completion = 80,
		carob_effort = 1
	)

	## the fieldbook has the standard CIP sheets ("Minimal", "Installation", "Fieldbook")
	process <- carobiner::get_function("process_cip_lbvars", path, group)
	f <- ff[basename(ff) == "PTYL200610_LAVICT.xls"]
	# this fieldbook has no MTWP (marketable tuber weight)
	d <- process(f, addvars="mtwp")
#	d$yield_marketable <- NULL
#	d$yield_adjusted <- NULL

	# was up to 100 times higher than seed_density
    d$plant_density <- d$plant_density / 100
	
	r <- carobiner::read.excel(f, sheet="Fieldbook")
	d$block_id <- as.character(d$rep)  # ISSUES 1
	d$rep <- NULL
#	plot_size <- 1.35  # m2, "Installation" sheet
#	d$seed_density <- as.numeric(r$NTP) * 10000 / plot_size
#	d$plant_density <- as.numeric(r$NPH) * 10000 / plot_size
#	d$tuber_density <- as.numeric(r$TNTP) * 10000 / plot_size
#	d$yield_moisture <- 100 - as.numeric(r$AVDM)
	d$on_farm <- FALSE
	# trials at the CIP station La Victoria are irrigated (confirmed by the contributor)
	d$irrigated <- TRUE

	carobiner::write_files(path, meta, d)
}
