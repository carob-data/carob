# R script for "carob"
# license: GPL (>=3)

## NOTES
# CIAT Soil Physics (PE-2) long-term field experiments in Colombia (and one Nicaragua site in the site list). The to-do list had this as a Kenya household survey; that label is wrong.
# Yield file 36.FIS_RENDIMIE.csv has 60 rows: rice and Pasto llanero at Finca Matazul, compaction and harrow-intensity trials.
# REND_TOT treated as kg/ha (values ~1700-3300 for rice). HUME is % grain moisture when present.
# 07.FIS_FECHAS planting/harvest dates are for Ensayo Culticore, not these Matazul yield rows.

## ISSUES
# Only Matazul yield rows processed. Treatment catalog, Culticore maize/rice dates, and Pronata farm soil chemistry not merged into yield.
# Soil physics tables (texture, penetration, infiltration, Ksat, bulk density, soil moisture, 29.FIS_ANAL_SUE) not processed.
# Site list includes Caldono farms, Carimagua, CIAT S. Quilichao, Pronata fincas, and San Dionisio (Matagalpa, Nicaragua) without coordinates. Human must georeference.
# Pasto llanero mapped to pasture; species is likely Urochloa humidicola / Brachiaria dictyoneura.
# P/K and lime from Pronata "Cal NNNN kgr/ha" treatments not applied (those treatments are not in the yield table).
# write_files: missing latitude/longitude/geo_from_source; NA yield for Pasto llanero rows (kept).

## NEW VARIABLES
# none


carob_script <- function(path) {

"Long-term field experiments carried out in savannas and hillsides agroecosystems by Soil Physics (PE-2: Soils project of CIAT)

The soil physics (PE-2: Soils project of CIAT) had several field experiments in both tropical savannas and hillsides agroecosystems.
To organize the data, three big blocks were established: one block for hillsides agroecosystem and two blocks (Culticore and Plains (Altillanura)) for savannas agroecosystem. For hillsides agroecosystem, data from 1995 to 2000 were organized systematically. For savannas, data from 1993 to 1999 were completed."

	uri <- "doi:10.7910/DVN/WR18SC"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIAT",
		publication = NA,
		project = "CIAT PE-2 Soils",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 15,
		carob_effort = 0.4
	)

	fy <- ff[basename(ff) == "36.FIS_RENDIMIE.csv"]
	fs <- ff[basename(ff) == "04.FIS_SITIOS.csv"]
	r <- read.csv(fy)
	sites <- read.csv(fs)

	# skipped: 01-03/05-35 soil physics, weather, texture, Ksat, bulk density, soil lab, Culticore dates

	i <- match(r$SITIO, sites$SITIO)
	crop <- ifelse(r$CULT == "Arroz", "rice",
		ifelse(r$CULT == "Pasto llanero", "pasture", tolower(r$CULT)))

	d <- data.frame(
		country = sites$PAIS[i],
		adm1 = carobiner::fix_name(sites$DEPARTAMENTO[i], "title"),
		adm2 = carobiner::fix_name(sites$CIUDAD[i], "title"),
		location = sites$UBICACION[i],
		site = r$SITIO,
		trial_id = paste(r$SITIO, r$ENSAYO),
		treatment = r$TRAT,
		crop = crop,
		rep = as.integer(r$REPI),
		yield = as.numeric(r$REND_TOT),
		yield_part = ifelse(crop == "rice", "grain", "aboveground biomass"),
		yield_moisture = as.numeric(r$HUME),
		harvest_date = as.character(as.Date(r$FECH_EVA, format="%d-%b-%y"))
	)

	d$on_farm <- TRUE
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
