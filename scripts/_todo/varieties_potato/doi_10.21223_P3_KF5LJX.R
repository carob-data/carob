# R script for "carob"
# license: GPL (>=3)

## NOTES
# Greenhouse late-blight assays of potato landraces. The title says the landraces are from Peru. The sheets do not name the greenhouse or give coordinates.
# Severity at 3, 5, and 7 days after inoculation is the percent foliage affected. Those three readings are the long table.
# rAUDPC on the sheet matches the relative area under that curve on a 0 to 1 scale. One row, CIP702568 rep 1, is stored as 5 although the severities 0, 50, and 100 imply 0.5.
# Variety is the material-list name when the accession number matches, otherwise the label on the screening sheet. Tomasa, Yungay, and Chucmarina are labels, not CIP numbers. The abstract names Chucmarina and Tomasa Condemayta as the controls.

## ISSUES
# No coordinates, fertilizer, irrigation, yield, or planting date.
# The Dataverse place is Huancayo, Junin. That place is not in the screening sheet.
# Dataverse lists Perez et al. 2014, Plant Disease 98:1530-1533. publication is left empty.
# write_files: missing yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: taxon_, dai_, late_blight_pct_. out of bounds: rAUDPC (0, 5). The long table has no date or depth column.

## NOT PROCESSED
# Data_dictionary.xls. The severity and rAUDPC definitions used above.
# Material-list parents, cycle, and the empty control flag. The screening label is the variety.
# Family and genus. Every row is Solanaceae, Solanum.
# The screening sheet has no AUDPC column. rAUDPC is the value that was stored.


## NEW VARIABLES
# taxon_: taxon name on the screening sheet.
# dai_: days after inoculation for one late-blight reading.
# late_blight_pct_: percent of foliage affected at that reading.


carob_script <- function(path) {

"
Dataset for: Replication data for: Wide phenotypic diversity for resistance to Phytophthora infestans found in potato landraces from Peru

The wild and cultivated species of potato have been utilized in potato breeding to good effect but only a very small sample of the available biodiversity has been exploited. In total, 468 accessions of wild and cultivated species of potato were assessed for resistance to the oomycete pathogen Phytophthora infestans using greenhouse assays. Wide phenotypic variation for resistance was found within a species (i.e., among accessions) but not among species which, on average, were similar. Nineteen accessions had resistance levels better than or similar to the variety Chucmarina, which is routinely used by the International Potato Center as a resistant control. Surprisingly, a number of accessions were significantly more susceptible than the susceptible control, Tomasa Condemayta. Frequency histograms of species indicated continuous variation for resistance with little evidence for functional resistance genes.
"

	uri <- "doi:10.21223/P3/KF5LJX"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = "Global Late Blight",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "rAUDPC;late_blight_pct_",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "Data_screening_for_resistance.xls"]
	r <- carobiner::read.excel(f, na=c("", ".", "NA"))
	f1 <- ff[basename(ff) == "Material_List.xls"]
	r1 <- carobiner::read.excel(f1, na=c("", "NA"))
	clean <- function(x) {
		x <- gsub("\u00a0", " ", as.character(x), fixed=TRUE)
		x <- trimws(x)
		x[!nzchar(x)] <- NA_character_
		x
	}
	key <- clean(r1$Accession_Number)
	nm <- clean(r1$Accession_Name)
	acc <- clean(r$Accession)
	i <- match(acc, key)
	variety <- nm[i]
	variety[is.na(variety)] <- acc[is.na(variety)]

	d <- data.frame(
		trial_id = paste0("GH", r$Experiment),
		crop = "potato",
		variety = variety,
		variety_code = acc,
		treatment = variety,
		taxon_ = clean(r[["Taxon full name"]]),
		rep = as.integer(r$Rep),
		plot_id = paste(r$Experiment, acc, r$Rep, sep="-"),
		rAUDPC = as.numeric(r$rAUDPC),
		country = "Peru",
		stress = "disease",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$record_id <- seq_len(nrow(d))

	scores <- c("3 DAI"=3L, "5 DAI"=5L, "7 DAI"=7L)
	long <- lapply(names(scores), function(nm) {
		data.frame(
			record_id = d$record_id,
			dai_ = scores[[nm]],
			late_blight_pct_ = as.numeric(r[[nm]])
		)
	})
	long <- do.call(rbind, long)
	rownames(d) <- NULL
	rownames(long) <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
