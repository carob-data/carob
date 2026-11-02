# R script for "carob"
# license: GPL (>=3)

## NOTES
# CIAT Soil Biology (PE-2) long-term field experiments in Colombia (1990-2000). The to-do list had this as a Kenya GHG survey; that label is wrong.
# Only 17.MBIO_PRODUCCI.csv has grain yield: maize N-source trial (TSBF) at CIAT Santander de Quilichao.
# RENDIMIENTO is kg/ha at 12% moisture: GRAN_12 / AREA_MUE(m2) * 10000.
# N_fertilizer parsed from TRAT strings (e.g. "90N Urea", "0N", "19N Urea + 56N T.diversifolia"); 0N recorded as 0.
# Greenhouse inoculation tables (11, 14) and CIAT-Palmira invernaderos site not processed.

## ISSUES
# Site table has place names only (Carimagua, S. Quilichao, Matazul, Palmira, Caldono). Human must georeference; do not guess.
# Soil (07.MBIO_ANAL_SUE N_TOT, MO by depth), plant height (18), litter, 15N, N-fixation, microbial biomass, and treatment catalog (pastures B.dictyoneura / S.capitata) not processed.
# P and K rates not in the production table.
# N_TOT in 07 is soil total N (mg/kg range ~700-2200), not fertilizer — draft() mapping would be wrong.
# write_files: missing latitude/longitude/geo_from_source; NA yield on 66 of 132 production rows (kept).

## NEW VARIABLES
# none


carob_script <- function(path) {

"Long-term field experiments carried out in savannas and hillsides agroecosystems by Soil Biology (PE-2: Soils project of CIAT)

The soil biology (PE-2: Soils project of CIAT) had several field experiments in both tropical savannas and hillsides agroecosystems. The data collected from 1990 to 2000 were organized systematically."

	uri <- "doi:10.7910/DVN/NQ5GDR"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "CIAT",
		publication = NA,
		project = "CIAT PE-2 Soils",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;treatment",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 20,
		carob_effort = 0.4
	)

	fp <- ff[basename(ff) == "17.MBIO_PRODUCCI.csv"]
	fs <- ff[basename(ff) == "02.MBIO_SITIOS.csv"]
	r <- read.csv(fp)
	sites <- read.csv(fs)

	# skipped: 01 dictionary; 03-06 treatments/litter; 07 soil; 08-16 N-15/inoculation/microbial; 18 plants; 19 determinations

	parse_N <- function(x) {
		m <- regmatches(x, gregexpr("[0-9.]+(?=N)", x, perl=TRUE))
		sapply(m, function(z) {
			if (length(z) == 0) NA_real_ else sum(as.numeric(z))
		})
	}

	i <- match(r$SITIO, sites$SITIO)
	d <- data.frame(
		country = sites$PAIS[i],
		adm1 = carobiner::fix_name(sites$DEPARTAMENTO[i], "title"),
		adm2 = carobiner::fix_name(sites$CIUDAD[i], "title"),
		location = sites$UBICACION[i],
		site = r$SITIO,
		trial_id = paste(r$SITIO, r$ENSAYO),
		treatment = r$TRAT,
		crop = "maize",
		rep = as.integer(r$REPI),
		yield = as.numeric(r$RENDIMIENTO),
		yield_part = "grain",
		yield_moisture = 12,
		N_fertilizer = parse_N(r$TRAT),
		harvest_date = as.character(as.Date(r$FECH_EVA, format="%d-%b-%y"))
	)

	ft <- rep(NA_character_, nrow(d))
	ft[grepl("Urea", r$TRAT, ignore.case=TRUE)] <- "urea"
	d$fertilizer_type <- ft

	d$on_farm <- FALSE
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
