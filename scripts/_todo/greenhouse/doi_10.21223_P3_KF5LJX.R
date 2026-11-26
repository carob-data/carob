# R script for "carob"
# license: GPL (>=3)

## NOTES
# Greenhouse late-blight assays of potato landraces at CIP Huancayo, Junin, Peru (-12.0264, -75.2449, geo_from_source = FALSE from Dataverse geographic coverage).
# Severity at 3, 5, and 7 days after inoculation (% foliage affected) is stored in the long table under disease = "potato late blight", disease_severity, severity_scale = "0-100", with order = 1..3 and days_after_inoculation_.
# rAUDPC on the sheet is on a 0 to 1 scale. One row (CIP702568 rep 1) is recorded as 5 although severities 0, 50, 100 imply 0.5; left as recorded.
# Variety is the material-list name when the accession number matches, otherwise the screening-sheet label. Taxon full name is stored in variety_type.

## ISSUES
# Greenhouse screen: no fertilizer, irrigation, yield, or planting date.
# One rAUDPC value is 5 (out of 0-1 bounds); kept as recorded.

## NEW VARIABLES
# days_after_inoculation_: days after inoculation (3, 5, or 7).


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
		response_vars = "rAUDPC;disease_severity",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 65,
		carob_effort = 0
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
		variety_type = clean(r[["Taxon full name"]]),
		treatment = variety,
		rep = as.integer(r$Rep),
		plot_id = paste(r$Experiment, acc, r$Rep, sep="-"),
		rAUDPC = as.numeric(r$rAUDPC),
		pathogen = "Phytophthora infestans",
		country = "Peru",
		adm1 = "Junín",
		adm2 = "Huancayo",
		location = "Huancayo",
		latitude = -12.0264,
		longitude = -75.2449,
		stress = "disease",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$record_id <- seq_len(nrow(d))

	scores <- c("3 DAI"=3L, "5 DAI"=5L, "7 DAI"=7L)
	long <- lapply(seq_along(scores), function(i) {
		nm <- names(scores)[i]
		sev <- as.numeric(r[[nm]])
		keep <- !is.na(sev)
		data.frame(
			record_id = d$record_id[keep],
			order = i,
			DAI = as.integer(scores[[nm]]),
			disease = "potato late blight",
			disease_severity = as.character(sev[keep]),
			severity_scale = "0-100"
		)
	})
	long <- do.call(rbind, long)
	rownames(d) <- NULL
	rownames(long) <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
