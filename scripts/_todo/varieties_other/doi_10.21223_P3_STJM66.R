# R script for "carob"
# license: GPL (>=3)

## NOTES
# Quinoa germplasm trial at the National University of San Antonio Abad of Cusco. Country Peru is the Dataverse geographic coverage. The description states a randomized complete block design with four blocks and 25 accessions. Ten plants were measured in each plot, so the table keeps the plant rows (1000).
# SY is t/ha (dictionary). yield is SY * 1000. TSW is g per 1000 seeds (seed_weight). Plant height, panicle diameter, stem diameter, leaf length, leaf width, petiole length, and panicle length are cm. Panicle density is panicles/m2.
# SPAD at 98 and 123 days after planting is in the long table. There is no calendar planting date and no coordinate.

## ISSUES
# The Cusco trial site needs georeferencing.
# TSW is stored in the recorded numbers. The dictionary says g/1000 seeds. Most values are far above a quinoa thousand-seed weight in grams and were not rescaled.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: plant_sample_, panicle_diameter_, primary_branches_, stem_diameter_, leaf_length_, leaf_width_, petiole_length_, panicle_length_, panicle_density_. out of bounds: seed_weight (3.876, 6035).

## NEW VARIABLES
# plant_sample_: plant measured within the plot (REP).
# panicle_diameter_, stem_diameter_, leaf_length_, leaf_width_, petiole_length_, panicle_length_: cm.
# primary_branches_: number of primary branches.
# panicle_density_: panicles per m2.

## NOT PROCESSED
# Dictionary reference rows.


carob_script <- function(path) {

"
Dataset for: Agro-morphological Evaluation of Quinoa Germplasm from the Cusco Quinoa Collection

Twenty-five quinoa accessions belonging to the Cusco Quinoa Collection (QC), native to the inter-Andean valleys, obtained from the Germplasm Bank of the Andean Crops Research Center (CICA), Faculty of Agricultural Sciences, National University of San Antonio Abad of Cusco (FCA-UNSAAC), were used to conduct a field trial under a randomized complete block design with four replications, resulting in a total of 100 experimental units evenly distributed across four blocks. For each experimental unit, 10 plants were randomly selected from the central row, excluding those located at the borders and row ends. The selected plants were properly labeled at the middle third of the stem, and several phenotypic (agro-morphological) traits, including panicle diameter, panicle density, plant height, number of primary branches, among others, were evaluated according to the descriptors proposed by Bioversity International et al. (2013), Descriptors for Quinoa and Wild Relatives.
"

	uri <- "doi:10.21223/P3/STJM66"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;plant_height",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 80,
		carob_effort = 0.35
	)

	f <- ff[grepl("^01_Data", basename(ff))]
	r <- carobiner::read.excel(f)
	num <- function(x) as.numeric(as.character(x))

	variety <- trimws(as.character(r$Accession))
	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = "Cusco_quinoa",
		plot_id = paste(variety, r$BLOQ, sep="_"),
		rep = as.integer(sub("^B", "", as.character(r$BLOQ))),
		plant_sample_ = as.integer(sub("^R", "", as.character(r$REP))),
		crop = "quinoa",
		variety = variety,
		treatment = variety,
		country = "Peru",
		location = "Cusco",
		yield = num(r$SY) * 1000,
		yield_part = "seed",
		seed_weight = num(r$TSW),
		plant_height = num(r$PLH),
		panicle_diameter_ = num(r$PAD),
		primary_branches_ = num(r$NPB),
		stem_diameter_ = num(r$STD),
		leaf_length_ = num(r$LEL),
		leaf_width_ = num(r$LEW),
		petiole_length_ = num(r$PEL),
		panicle_length_ = num(r$PAL),
		panicle_density_ = num(r$PDE),
		geo_from_source = FALSE,
		on_farm = NA,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)

	spad1 <- data.frame(record_id=d$record_id, DAP=98L, SPAD=num(r[["SPAD 98DAP"]]), stringsAsFactors=FALSE)
	spad2 <- data.frame(record_id=d$record_id, DAP=123L, SPAD=num(r[["SPAD 123DAP"]]), stringsAsFactors=FALSE)
	dlong <- rbind(spad1, spad2)
	dlong <- dlong[!is.na(dlong$SPAD), ]

	carobiner::write_files(path, meta, d, long=dlong)
}
