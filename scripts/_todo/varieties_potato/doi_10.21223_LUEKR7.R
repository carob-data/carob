# R script for "carob"
# license: GPL (>=3)

## NOTES
# Morphological scores for 91 wild potato accessions, recorded in a greenhouse. The abstract names eight Solanum species and 39 descriptors.
# Descriptor columns D1 to D39 follow the dictionary order. The stored names are the dictionary variable names. Scores are the codes in the fieldbook.
# Average tuber weight is the dictionary item AvTbWg_Cp_g, in grams. It is not a yield in kg/ha.
# The dataset geographic coverage is one point at Quito. That point is the place published for the record. It is not a coordinate for each collection site.

## ISSUES
# Collection sites of the accessions are not in the fieldbook.
# No fertilizer, irrigation, planting date, harvest date, or plot yield.
# A few descriptor abbreviations in the dictionary are blank. Those columns use the variable name.
# Some rows have no accession id, so the treatment variable variety is missing there.
# leaf_S, flesh_color, and weed_species were suggested for a few names. Those terms are not the descriptor that was scored, so the dictionary names were kept.
# write_files: missing variables: carob_contributor, yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. metadata: missing values in treatment variable variety. unknown variables: the morphological descriptor columns, species_, and tuber_weight_g_.

## NOT PROCESSED
# 02 and 03 material lists: the same accessions summarized.
# Dictionary category labels. The fieldbook codes are kept.
# Days and size descriptors are included under their names. They are not converted to dates.


carob_script <- function(path) {

"
Dataset for: Morphological  and ecogeographical characterization of potato wild relatives collected in Ecuador

A total of 91 accessions representing eight wild Solanum species (S. acaule, S. albicans, S. albornozii, S. andreanum, S. chilliasense, S.chomatophilum, S. colombianum and S. minutifoliolum) collected and preserved at the National Germplasm Bank, INIAP in Ecuador were comprehensive characterized involved the utilization of 39 morphological descriptors under controlled greenhouse conditions. The data collection spanned various developmental stages of the plants.
"

	uri <- "doi:10.21223/LUEKR7"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "plant_growth_habit_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.4
	)

	slug <- function(x) {
		x <- tolower(trimws(as.character(x)))
		x <- gsub("[^a-z0-9]+", "_", x)
		x <- gsub("^_|_$", "", x)
		paste0(x, "_")
	}

	fd <- ff[grepl("^04_Data_dictionary", basename(ff))]
	fb <- ff[grepl("^01_Fieldbook", basename(ff))]
	dic <- carobiner::read.excel(fd)
	r <- carobiner::read.excel(fb, na=c("", "NA"))
	names(r) <- trimws(names(r))
	vname <- trimws(as.character(dic[["Variable name"]]))
	vname[39] <- "flower_size"
	# Average tuber weight is in grams in the abbreviation AvTbWg_Cp_g
	vname[vname == "Average of tuber weight"] <- "tuber_weight_g"
	cols <- slug(vname)
	variety <- trimws(as.character(r$id))

	d <- data.frame(
		trial_id = "INIAP_wild_potato",
		country = "Ecuador",
		location = "Quito",
		# dataset geographic coverage: one west longitude and one south latitude
		latitude = -0.36897,
		longitude = -78.55568,
		geo_from_source = TRUE,
		crop = "potato",
		variety = variety,
		variety_code = variety,
		treatment = variety,
		record_id = seq_len(nrow(r)),
		species_ = trimws(as.character(r$specie)),
		on_farm = FALSE,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)
	for (i in seq_along(cols)) {
		d[[cols[i]]] <- trimws(as.character(r[[paste0("D", i)]]))
	}
	wt <- d$tuber_weight_g_
	d$tuber_weight_g_ <- as.numeric(ifelse(grepl("^[0-9.]+$", wt), wt, NA))

	carobiner::write_files(path, meta, d)
}
