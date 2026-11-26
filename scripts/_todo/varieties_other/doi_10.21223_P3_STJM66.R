# R script for "carob"
# license: GPL (>=3)

## NOTES
# Quinoa germplasm trial at the National University of San Antonio Abad of Cusco. Country Peru is the Dataverse geographic coverage. The description states a randomized complete block design with four blocks and 25 accessions. Ten plants were measured in each plot, so the table keeps the plant rows (1000).
# SY is t/ha (dictionary). yield is SY * 1000. TSW is g per 1000 seeds (seed_weight). Plant height, panicle diameter, stem diameter, leaf length, leaf width, petiole length, and panicle length are cm. Panicle density is panicles/m2.
# SPAD at 98 and 123 days after planting is in the long table. There is no calendar planting date and no coordinate.

## ISSUES
# Coordinates are approximate for UNSAAC, Cusco (geo_from_source = FALSE).
# TSW has 998 values between 2123 and 6035 and 2 values at 3.876 and 4.069 (mg/1000 seeds or decimal-separator shift); values > 100 are divided by 1000 to yield g/1000 seeds (2.12-6.04 g).

## NEW VARIABLES
# plant_sample_: plant measured within the plot (REP).
# panicle_diameter_, leaf_length_, leaf_width_, petiole_length_: cm.
# primary_branches_: number of primary branches.
# panicle_density_: panicles per m2.

## NOT PROCESSED
# Dictionary reference rows.
# 02_Data_dictionary_Agro-morphological.xlsx: documentation or a codebook, not an observation table.


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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "01_Data_Agro-morphological.xlsx"]
	r <- carobiner::read.excel(f)
	num <- function(x) as.numeric(as.character(x))

	variety <- trimws(as.character(r$Accession))
	tsw <- num(r$TSW)
	# 998 of 1000 values are 2123-6035 (mg per 1000 seeds / decimal-separator shift) vs 3.876, 4.069 g
	tsw <- ifelse(!is.na(tsw) & tsw > 100, tsw / 1000, tsw)
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
		adm1 = "Cusco",
		location = "Cusco",
		latitude = -13.522,
		longitude = -71.959,
		yield = num(r$SY) * 1000,
		yield_part = "seed",
		seed_weight = tsw,
		plant_height = num(r$PLH),
		panicle_diameter_ = num(r$PAD),
		primary_branches_ = num(r$NPB),
		stem_diameter = num(r$STD),
		leaf_length_ = num(r$LEL),
		leaf_width_ = num(r$LEW),
		petiole_length_ = num(r$PEL),
		panicle_length = num(r$PAL),
		panicle_density_ = num(r$PDE),
		geo_from_source = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	spad1 <- data.frame(record_id=d$record_id, DAP=98L, SPAD=num(r[["SPAD 98DAP"]]))
	spad2 <- data.frame(record_id=d$record_id, DAP=123L, SPAD=num(r[["SPAD 123DAP"]]))
	dlong <- rbind(spad1, spad2)
	dlong <- dlong[!is.na(dlong$SPAD), ]

	carobiner::write_files(path, meta, d, long=dlong)
}
