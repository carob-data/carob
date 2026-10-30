# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are Tabla 12 (laboratory minerals and quality). Header row includes INSTN, BREEDERCODE, Mean_FEDW, and Mean_ZNDW.
# Those means are stored as recorded. The header does not print mg/kg, so they are not renamed to a standard iron term.
# Country Peru is in the dataset description. The tables cover several departments and are not one coordinate.
# Publication doi:10.4160/9789290605935 is the report DOI in the metadata.
# The abstract states fertilizer rates. It does not say whether the figures are elemental or oxide, so N, P, and K are not filled.

## ISSUES
# No yield in kg/ha in this laboratory table.
# Sites need georeferencing.
# write_files: missing variables: carob_contributor; yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variables: fe_dw_mean_, zn_dw_mean_.

## NOT PROCESSED
# Soil, date, vote, organoleptic, and correlation workbooks other than Tabla 12.
# Standard deviations and sample sizes next to the mineral means.

## NEW VARIABLES
# fe_dw_mean_: Mean_FEDW as recorded
# zn_dw_mean_: Mean_ZNDW as recorded

carob_script <- function(path) {

"
Dataset for: Potential new varieties of biofortified potato with high iron and zinc content, to contribute to food and nutritional security in high Andean family farming.

In total 30 tetraploid BIOT clones and 4 commercial varieties were evaluated in multi-location trials in nine field experiments, in five departments of Peru using participatory varietal selection. Trials were planted according to local practices using 40 plants per plot with a randomized complete block design with 3 replications at each location. The plot size was 9.72m2, with 4 rows of 2.7m long and 0.9m between rows. Nitrogen, Phosphorus, Potassium (NPK) (180-180-160) and organic amendment ('guano de corral') was applied before planting. Soil samples of 1 kg each were collected before planting and sent to the agricultural chemistry laboratory Valle Grande – Cañete for soil, salinity and micronutrient content analysis. Participatory varietal selection (PVS) evaluations were conducted following the protocol 'Participatory Varietal Selection of Potato Using the Mother & Baby Trial Design: A Gender-responsive Trainer's Guide'. The data was collected using Highly Interactive Data Analysis Platform, i.e. HIDAP, software. Evaluations at the flowering, harvest and post-harvest (storage) phase were performed. Farmers identified and ranked characteristics of potato that they considered important, and afterwards they selected the genotype they preferred. Each farmer's varietal selection and the criteria for selection were recorded.
"

	uri <- "doi:10.21223/N11QJS"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = "doi:10.4160/9789290605935",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "fe_dw_mean_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.4
	)

	f <- ff[grepl("^Tabla 12", basename(ff))]
	x <- as.data.frame(carobiner::read.excel(f, sheet="Tabla 12_data", col_names=FALSE), stringsAsFactors=FALSE)
	hdr <- trimws(as.character(unlist(x[1, ])))
	y <- x[-1, ]
	names(y) <- make.names(hdr, unique=TRUE)
	num <- function(v) {
		z <- trimws(as.character(v))
		z[z %in% c("", "NA")] <- NA_character_
		as.numeric(z)
	}
	variety <- trimws(as.character(y$BREEDERCODE))
	blank <- is.na(variety) | !nzchar(variety)
	variety[blank] <- trimws(as.character(y$INSTN))[blank]
	d <- data.frame(
		crop = "potato",
		variety_code = trimws(as.character(y$INSTN)),
		variety = variety,
		fe_dw_mean_ = num(y$Mean_FEDW),
		zn_dw_mean_ = num(y$Mean_ZNDW),
		country = "Peru",
		trial_id = "PVS_sierra",
		on_farm = NA,
		is_survey = FALSE,
		irrigated = NA,
		stringsAsFactors = FALSE
	)
	d$treatment <- d$variety
	d <- d[!is.na(d$variety) & nzchar(d$variety), ]
	d$plot_id <- as.character(seq_len(nrow(d)))
	carobiner::write_files(path, meta, d)
}
