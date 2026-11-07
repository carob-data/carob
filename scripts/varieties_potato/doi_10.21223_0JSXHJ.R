# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The "Vine cutting / killing" date (2005-09-01) is before planting (2005-09-22); not used.
# 2. Plot 92 (CIP301044.37, rep 3) has no dry matter value for sample 1, so AVDM is missing.
# 3. PPE ("percentage plants emerged") has the same values as NPE (number of plants emerged), so it is
#    not a percentage. The number of plants harvested is not reported; plant_density is computed
#    from NPE (plants emerged).
# 4. In the "Fieldbook" sheet, three cells with the clone name are highlighted (CIP301029.18 reps 1-2,
#    CIP301037.67 rep 3). The data are the same as in the "original" sheet; the meaning of the colour
#    is not explained.

## NOTES
# 1. Trial at the CIP experiment station in La Molina, Lima (coordinates and elevation from the
#    "Minimal" sheet); on_farm = FALSE. RCBD with 3 replications; plots of 10 plants in one row,
#    0.3 m between plants and 0.9 m between rows (2.7 m2). Irrigated (confirmed by the contributor;
#    not stated in the data).
# 2. Early yield and heat tolerance of 32 clones (Ranchitos population and checks): harvested at 91 days
#    after planting, in spring (September to December 2005).
# 3. Yield is the non-adjusted total tuber yield (TTWP / plot size). Marketable yield from MTWP.
# 4. yield_moisture is 100 - average tuber dry matter (AVDM, %), measured per plot.
# 5. Variety names from the material list are used where available; the CIP number is in variety_code.
#    Pedigree (female x male) from the material list (code if available, otherwise CIP number).
# 6. Not processed: plant vigor, flowering, senescence, stolon number and length, tuber appearance and
#    uniformity (1-9 scales), chipping color. The "Soil_analysis", "Weather_data" and "Hobo_data"
#    sheets are empty.

carob_script <- function(path) {

"
Dataset for: Evaluation of Clones Potatoes of Ranchitos population, Lima, 2005

The objective of this study was to determine early yield, and heat tolerance for 32 potato clones during the spring of 2005 in La Molina, Lima.
"

	uri <- "doi:10.21223/0JSXHJ"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-06",
		carob_completion = 80,
		carob_effort = 1
	)

	## the fieldbook has the standard CIP sheets ("Minimal", "Installation", "Fieldbook")
	process <- carobiner::get_function("process_cip_lbvars", path, group)
	f <- ff[basename(ff) == "PTYL200509_CIPHQ_Ranchitos-LMspr.xls"]
	# this fieldbook has no TTYA (adjusted yield) and no PPH (% plants harvested)
	d <- process(f, addvars=c("ttya"))
	d$yield_adjusted <- NULL

## This is done, or should be done by the "process" function
	##r <- carobiner::read.excel(f, sheet="Fieldbook")
	##plot_size <- 2.7  # m2, "Installation" sheet
	##d$seed_density <- as.numeric(r$NTP) * 10000 / plot_size
	##d$plant_density <- as.numeric(r$NPE) * 10000 / plot_size
	##d$tuber_density <- as.numeric(r$TNTP) * 10000 / plot_size
	##d$yield_moisture <- 100 - as.numeric(r$AVDM)

	d$on_farm <- FALSE
	# trials at the CIP station in La Molina (arid coast) are irrigated (confirmed by the contributor)
	d$irrigated <- TRUE

## Excellent, also moved to the function
	## material list: names and pedigree (NOTES 5)
#	f2 <- ff[basename(ff) == "02_Material_list.txt"]
#	m <- utils::read.delim(f2, colClasses="character", na.strings="", strip.white=TRUE)
#	fem <- ifelse(is.na(m$Female.code), m$Female.Institutional.number, m$Female.code)
#	mal <- ifelse(is.na(m$Male.code), m$Male.Institutional.number, m$Male.code)
#	i <- match(d$variety, m$Institutional.number)
#	d$variety_code <- d$variety
#    d$female_parent <- fem
#    d$male_parent <- mal
#    d$variety_pedigree <- paste(fem, mal, sep=" x ")[i]
#	nm <- m$Clone.or.variety.name[i]
#	d$variety[!is.na(nm)] <- nm[!is.na(nm)]

	carobiner::write_files(path, meta, d)
}
