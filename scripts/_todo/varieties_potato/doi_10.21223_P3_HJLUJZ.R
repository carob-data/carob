# R script for "carob"
# license: GPL (>=3)

## NOTES
# One plot table of late blight scores. lb1 to lb5 are the percentage of foliage affected. Those scores are in the long table, in column order.
# The dictionary defines audpc from those percentages. Vocabulary AUDPC is the sum of the infected fraction, so the stored value is the sheet value divided by 100.
# "na" and "-" are the dictionary's missing and empty markers.
# Rows whose genotype is "empty" are blank cells in the field plan. They have no plot number and no scores.
# The description places the trial at Oxapampa, Pasco, Peru, at 1810 m. That place is not in the sheets or the title, so country and coordinates are not filled.
# The description says the first season was an RCBD adjusted to a row-column design (8 plants per plot) and the second was a row-column design (5 plants per plot). The sheet has no season column.

## ISSUES
# No coordinates, fertilizer rate, irrigation statement, yield, or planting date.
# Oxapampa and Pasco are only in the description. They still need to be checked against GADM and written in by the reviewer if that text is accepted.
# The abstract says 492 accessions. After dropping empty field-plan cells the plot sheet has 482 genotype codes.
# The same genotype is planted in more than one plot of a replicate. Those plots are kept.
# The score sequence has no date, so the long table uses order rather than a day count.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, country, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: germplasm_group_, entry_role_, species_, late_blight_pct_. The species_ hint weed_species does not apply.

## NOT PROCESSED
# 02_Material_list_CIP_genebank.xlsx. Species, group, and type are already on the plot sheet.
# The dictionary workbook, other than the missing-value markers and the AUDPC definition.


## NEW VARIABLES
# late_blight_pct_: percent of foliage affected by late blight at one score in the trial sequence.
# germplasm_group_: material group on the sheet. Native and native are stored as native.
# entry_role_: test or check.
# species_: taxonomic group printed on the plot sheet (for example phureja, andigena, tuberosum).


carob_script <- function(path) {

"
Replication Data for: Late blight severity in potato accessions from the CIP genebank core collection

The assessment of late blight resistance in 492 potato genebank accessions of the CIP core collection under field conditions with high endemic disease pressure located in Oxapampa (1,810 m a.s.l. Pasco-Perú) were conducted. Randomized complete block design (RBDC) with three replications of 8 hill plots which were adjusted to a row-column design in the first season and second season using row-column design with three replications of 5 hill plots were established. After 40 days after planting (DAP), the percentage of leaf area affected by late blight infection was recorded by plot throughout the season to subsequently compute the area under the disease progress curve (AUDPC).
"

	uri <- "doi:10.21223/P3/HJLUJZ"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "AUDPC",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "01_data_CIP_genebank.xlsx"]
	r <- carobiner::read.excel(f, sheet="raw_trialB", na=c("na", "-", "NA", ""))
	genotype <- trimws(as.character(r$genotype))
	ok <- !is.na(genotype) & nzchar(genotype) & genotype != "empty"
	r <- r[ok, ]
	genotype <- genotype[ok]
	grp <- trimws(as.character(r$group))
	grp[tolower(grp) == "native"] <- "native"
	grp[!nzchar(grp)] <- NA_character_
	role <- trimws(as.character(r$type))
	role[!nzchar(role)] <- NA_character_
	species <- trimws(as.character(r$specie))
	species[!nzchar(species)] <- NA_character_

	d <- data.frame(
		trial_id = "genebank_late_blight",
		crop = "potato",
		variety = genotype,
		treatment = genotype,
		germplasm_group_ = grp,
		entry_role_ = role,
		species_ = species,
		rep = as.integer(r$rep),
		plot_id = as.character(r$plot),
		AUDPC = as.numeric(r$audpc) / 100,
		stress = "disease",
		disease = "potato late blight",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	rownames(d) <- NULL
	d$record_id <- seq_len(nrow(d))

	lbs <- c("lb1", "lb2", "lb3", "lb4", "lb5")
	long <- lapply(seq_along(lbs), function(i) {
		data.frame(
			record_id = d$record_id,
			order = as.integer(i),
			late_blight_pct_ = as.numeric(r[[lbs[i]]])
		)
	})
	long <- do.call(rbind, long)
	rownames(long) <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
