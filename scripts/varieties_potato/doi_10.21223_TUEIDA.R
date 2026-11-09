# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Location: the "Minimal" sheet gives Poroma, Nazca (Ica), but the "Resumen Selectos" and "CROQUIS"
#    sheets say that these clones, selected in Nazca (first clonal generation), were planted in
#    La Molina in spring 2005 (12-10-2005), in the field of E. Mihovilovich. La Molina is used (confirmed
#    by the contributor), with the coordinates and elevation of the CIP station from the "Minimal" sheet
#    of doi:10.21223/0JSXHJ (La Molina, 2005).
# 2. The description says 183 clones in an RCBD with 3 replications, but the data have 71 clones in
#    131 plots: 30 clones with 1 plot, 23 with 2, 17 with 3 and 1 with 4. "REP" only numbers the plots
#    of each clone, so it is not used as rep.
# 3. The "Vine cutting / killing" date (2005-10-01) is before planting (2005-10-12); not used.

## NOTES
# 1. Plots of 5 plants in one row, 0.3 m between plants and 0.9 m between rows (1.35 m2).
#    The CIP station in La Molina is irrigated (confirmed by the contributor).
# 2. Yield is the non-adjusted total tuber yield (TTWP / plot size). Marketable yield is not reported.
#    The adjusted yield (TTYA, corrected for missing plants) is not used.
# 3. plant_density is from the number of plants harvested (NPH); seed_density from the number of tubers
#    planted (NTP).
# 4. Pedigree (female x male) from the material list, where available. Variety names from the
#    material list (the check CIP388611.22 = REICHE); the CIP number is in variety_code.
# 5. Not processed: the "Obs" column in "Resumen Selectos" ("Eliminado" for some clones).

carob_script <- function(path) {

"
Dataset for: Assessment of Potato Clones during Spring in Nazca, 2005

An evaluation of 183 clones was conducted in the Poroma area of Nazca in 2005, employing a randomized complete block design (RCBD) with 3 replications.
"

	uri <- "doi:10.21223/TUEIDA"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		# first clonal generation, not replicated (ISSUES 2)
		design = "unreplicated",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-06",
		carob_completion = 90,
		carob_effort = 1
	)

	## the fieldbook has the standard CIP sheets ("Minimal", "Installation", "Fieldbook")
	process <- carobiner::get_function("process_cip_lbvars", path, group)
	f <- ff[basename(ff) == "PTYL200510_NAZCA.xls"]
	# this fieldbook has no MTWP (marketable tuber weight)
	d <- process(f, addvars="mtwp")
	d$yield_marketable <- NULL
	d$yield_adjusted <- NULL
	d$rep <- NULL  # ISSUES 2

	r <- carobiner::read.excel(f, sheet="Fieldbook")
	plot_size <- 1.35  # m2, "Installation" sheet
	d$seed_density <- as.numeric(r$NTP) * 10000 / plot_size
	d$plant_density <- as.numeric(r$NPH) * 10000 / plot_size

	## La Molina (ISSUES 1)
	d$adm1 <- "Lima"
	d$adm2 <- "Lima"
	d$adm3 <- "La Molina"
	d$location <- "La Molina"
	d$longitude <- -76.948417
	d$latitude <- -12.076289
	d$elevation <- 244
	d$geo_from_source <- FALSE
	d$on_farm <- FALSE
	d$irrigated <- TRUE

	## material list: pedigree (NOTES 4)
	f2 <- ff[basename(ff) == "02_Material_list.txt"]
	m <- utils::read.delim(f2, colClasses="character", na.strings="", strip.white=TRUE, fileEncoding="UTF-8-BOM")
	fem <- ifelse(is.na(m$Female.code), m$Female.Institutional.number, m$Female.code)
	mal <- ifelse(is.na(m$Male.code), m$Male.Institutional.number, m$Male.code)
	ped <- ifelse(is.na(fem) & is.na(mal), NA, paste(fem, mal, sep=" x "))
	i <- match(d$variety, m$Institutional.number)
	d$variety_pedigree <- ped[i]
	d$variety_code <- d$variety
	nm <- m$Clone.or.variety.name[i]
	d$variety[!is.na(nm)] <- nm[!is.na(nm)]

	carobiner::write_files(path, meta, d)
}
