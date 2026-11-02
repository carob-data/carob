# R script for "carob"
# license: GPL (>=3)

## NOTES
# Bean experimental lines. The title names the crop. The file is a line and pedigree list, not the trial results named in the description.
# linea is the line name. cruza is the cross code, shared by sister lines. The long pedigree column includes the cross and the selection history, and that is variety_pedigree.
# comentario is stored as recorded: linea mesos or linea andinos. Año Línea is 2019 on every row. That is the line year in this file, not a release year.
# The file has no country, location, yield, or grain iron and zinc columns. The description says those were evaluated.

## ISSUES
# write_files: missing variables: yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. NA detected: country, latitude, longitude.

## NOT PROCESSED
# Pedigrí. The short cross formula. It is the start of the long pedigree column.
# Año Línea. 2019 on every row.


carob_script <- function(path) {

"
Replication Data for: Bean experimental lines selected for tolerance to Drought, high temperatures, low P in the soil, high aluminum in the soil and high content of Fe / Zn in grain.

Bean experimental lines selected for tolerance to Drought, high temperatures, low P in the soil, high aluminum in the soil and high content of Fe / Zn in grain. These lines will be sent to collaborators in Central America and Africa to be evaluated under their local conditions. Yield in Kg / Ha and Fe / Zn content in mg / Kg were evaluated.
"

	uri <- "doi:10.7910/DVN/CDBHQW"
	group <- "genotypes"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		treatment_vars = "none",
		response_vars = "none",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "Code Lines_2019..xlsx"]
	r <- carobiner::read.excel(f)

	squash <- function(x) {
		x <- trimws(as.character(x))
		x <- gsub("[[:space:]]+", " ", x)
		x[!nzchar(x)] <- NA_character_
		x
	}

	d <- data.frame(
		country = NA_character_,
		latitude = NA_real_,
		longitude = NA_real_,
		geo_from_source = FALSE,
		trial_id = "CIAT_bean_lines_2019",
		variety = squash(r$linea),
		variety_code = squash(r$cruza),
		variety_pedigree = squash(r[["Pedigrí + genealogia - opcion1"]]),
		variety_traits = squash(r$comentario),
		crop = "common bean",
		on_farm = NA,
		is_survey = FALSE,
		irrigated = NA
	)
	d <- d[!is.na(d$variety), ]
	d$plot_id <- as.character(seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
