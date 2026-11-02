# R script for "carob"
# license: GPL (>=3)

## NOTES
# Sensory scores of boiled potato at Huancavelica, Peru, in 2015. The abstract says the scale runs from 0 to 10.
# Each row is one panelist score for one attribute. Sample codes are joined to the genotype file for the variety name.
# post_harvest is fresh or after_storage.
# Replication is coded rep-1 and rep-2. A dash is missing.

## ISSUES
# No coordinates. Huancavelica needs georeferencing.
# No yield, fertilizer, irrigation, or planting and harvest dates.
# Dry matter in the genotype file is two replicate columns and is not joined, because it is not one value per sensory row.
# write_files: , yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variable: storage_ (dmy_storage?). unknown variables: sample_code_, panelist_, session_, attribute_, sensory_score_.

## NOT PROCESSED
# Dataset Sensory Analysis - genotypes.xlsx: dry matter replicates, species, flesh color, and processing notes. The genotype name is used.
# panelist, session, and N are kept only as the observation is one score. N is not a count that was defined in a dictionary in this folder.


carob_script <- function(path) {

"
Dataset for: Sensory analysis of boiled potato samples from wild species, landraces and modern varieties.

15 native potato cultivars,  15 improved native potato hybrids, 29 advanced tetraploid breeding lines or cultivars and 4 wild Solanum accessions were planted in the field in 2015, in Huancavelica, Peru. The harvest was divided for immediate analysis (fresh) and analysis after a four month storage period (stored). The potatoes were boiled and peeled before serving. A trained panel of 15 people (6 women, 9 men) evaluated the sensory attributes sourness, bitterness, sweetness, potato flavour (tastiness), savouriness and texture (mealiness) on an unstructured interval scale, ranging from 0 till 10.
"

	uri <- "doi:10.21223/6OVZJC"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;storage_",
		response_vars = "sensory_score_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.35
	)

	fd <- ff[grepl("data.xlsx", basename(ff))]
	fg <- ff[grepl("genotypes.xlsx", basename(ff))]
	r <- carobiner::read.excel(fd, na=c("", "NA"))
	g <- carobiner::read.excel(fg, na=c("", "NA"))
	names(r) <- trimws(names(r))
	names(g) <- trimws(names(g))
	sample <- trimws(as.character(r$sample))
	code <- trimws(as.character(g[["Sample code"]]))
	gen <- trimws(as.character(g$Genotype))
	variety <- gen[match(sample, code)]
	variety[is.na(variety)] <- sample[is.na(variety)]
	storage <- trimws(as.character(r$post_harvest))
	rp <- sub("^rep-", "", trimws(as.character(r$rep)))
	rp[!grepl("^[0-9]+$", rp)] <- NA_character_

	d <- data.frame(
		trial_id = "Huancavelica_sensory_2015",
		country = "Peru",
		adm1 = "Huancavelica",
		location = "Huancavelica",
		crop = "potato",
		variety = variety,
		variety_type = trimws(as.character(r$potato_type)),
		storage_ = storage,
		treatment = paste(variety, storage),
		rep = as.integer(rp),
		sample_code_ = sample,
		panelist_ = trimws(as.character(r$panelist)),
		session_ = trimws(as.character(r$session)),
		attribute_ = trimws(as.character(r$attribute)),
		sensory_score_ = as.numeric(trimws(as.character(r$data))),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
