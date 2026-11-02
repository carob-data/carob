# R script for "carob"
# license: GPL (>=3)

## NOTES
# Earthworm counts from a long-term rice-wheat trial. The workbook is the paper's figure file. The first block is population density per m2, already converted from 0.5 m2, with three size classes and a total, at 0-15 cm and 15-30 cm.
# Rows kept are those whose size-class counts are whole numbers and whose replicate is 1, 2, or 3. Square-root transformed columns are not used.
# geographicCoverage is empty and the title does not name a country, so country is not set. Author affiliations are CSSRI, Karnal.

## ISSUES
# No coordinates and no country column.
# write_files: missing variables: on_farm, country, latitude, longitude. unknown variables: earthworm_small_m2_, earthworm_medium_m2_, earthworm_large_m2_, earthworm_m2_.

## NEW VARIABLES
# earthworm_m2_: total earthworms per m2 at that depth. The sheet says the 0.5 m2 count was already doubled.
# earthworm_small_m2_: earthworms shorter than 2 cm, per m2.
# earthworm_medium_m2_: earthworms 2 to 4 cm, per m2.
# earthworm_large_m2_: earthworms longer than 4 cm, per m2.

## NOT PROCESSED
# Bulk density, infiltration, aggregates, pH, EC, N, P, K, micronutrients, carbon, enzymes, and penetration resistance. Those sheets mix original values, means, standard errors, and transformed values side by side.
# The PDF of the paper.


carob_script <- function(path) {

"
A Decade of Climate-Smart Agriculture in Major Agri-Food Systems: Earthworm Abundance and Soil Physico-Biochemical Properties

Earthworm abundance and soil properties after 10 years of climate-smart agriculture practices in a rice-wheat system.
"

	uri <- "hdl:11529/10548706"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "earthworm_m2_",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.5
	)

	f <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)][1]
	r <- carobiner::read.excel(f, sheet=1, col_names=FALSE)
	num <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		x[grepl("[A-Za-z]", x)] <- NA_character_
		as.numeric(x)
	}
	whole <- function(x) !is.na(x) & abs(x - round(x)) < 1e-6
	treat <- trimws(as.character(r[[1]]))
	repn <- num(r[[3]])
	small <- num(r[[4]])
	med <- num(r[[5]])
	large <- num(r[[6]])
	total <- num(r[[7]])
	small2 <- num(r[[8]])
	med2 <- num(r[[9]])
	large2 <- num(r[[10]])
	total2 <- num(r[[11]])
	ok <- !is.na(treat) & nzchar(treat) & !is.na(repn) & repn %in% 1:3
	ok <- ok & whole(small) & whole(med) & whole(large) & whole(total)
	treat <- treat[ok]
	repn <- repn[ok]
	a <- data.frame(
		treatment = treat,
		rep = as.integer(repn),
		depth_top = 0,
		depth_bottom = 15,
		earthworm_small_m2_ = small[ok],
		earthworm_medium_m2_ = med[ok],
		earthworm_large_m2_ = large[ok],
		earthworm_m2_ = total[ok]
	)
	b <- data.frame(
		treatment = treat,
		rep = as.integer(repn),
		depth_top = 15,
		depth_bottom = 30,
		earthworm_small_m2_ = small2[ok],
		earthworm_medium_m2_ = med2[ok],
		earthworm_large_m2_ = large2[ok],
		earthworm_m2_ = total2[ok]
	)
	d <- rbind(a, b)
	d <- d[!is.na(d$earthworm_m2_), ]
	d <- d[!duplicated(paste(d$treatment, d$rep, d$depth_top)), ]
	d$trial_id <- "1"
	d$sample_id <- paste(d$treatment, d$rep, d$depth_top, d$depth_bottom)
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
