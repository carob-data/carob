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



## NOT PROCESSED
# 01.DICTONARY.xls: not read. sheet Dictonary: TABLA/TABLE, COLUMNA/COLUMN, DESCRIPCION, DESCRIPTION, UNIDAD DE MEDIDA/UNITS OF MEASUREMENT. Observation records are missing from d until this file is standardized.
# 02.FIS_DETERMIN.csv: not read. columns: "GRUPO", "DETER", "FECH_MUE", "SITIO", "ENSAYO", "TRAT", "CULT". Observation records are missing from d until this file is standardized.
# 03.FIS_ENSAYOS.csv: not read. columns: "ENSAYO", "DESC_ENSA". Observation records are missing from d until this file is standardized.
# 05.FIS_CLIMA.csv: not read. columns: "SITIO", "FECH_MUE", "RADI_SOL", "TEMP_MAX", "TEMP_MIN", "PRECIPITACION". Observation records are missing from d until this file is standardized.
# 06.FIS_TRATAMIENTOS.csv: not read. columns: "SITIO", "ENSAYO", "TRAT", "CULT", "DESC_TRAT", "DESC_CULT". Observation records are missing from d until this file is standardized.
# 08.FIS_TEXTURA.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "PESO_SH_CAP", "PESO_SS_CAP", "PESO_SS", .... Observation records are missing from d until this file is standardized.
# 09.FIS_FRAC_ARE.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "PESO_INI", "APER_TAM", "PESO_ARE", .... Observation records are missing from d until this file is standardized.
# 10.FIS_LECT_PEN.csv: not read. columns: "HERRAMIENTA", "TIPO", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "NUME_LEC", "UBIC_LEC", "VALO_LEC". Observation records are missing from d until this file is standardized.
# 11.FIS_RESI_TAN.csv: not read. columns: "HERRAMIENTA", "TIPO", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "NUME_LEC", "UBIC_LEC", "VALO_LEC". Observation records are missing from d until this file is standardized.
# 12.FIS_DIST_AGR.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "PESO_INI", "DIAM_PON", "APER_TAM", .... Observation records are missing from d until this file is standardized.
# 13.FIS_ESTA_AGR.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "AGRE_TAM", "PESO_INI", "APER_TAM", .... Observation records are missing from d until this file is standardized.
# 14.FIS_PLASTICI.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "PESO_REC_SUP", "PESO_SH_SUP", "PESO_105_SUP", .... Observation records are missing from d until this file is standardized.
# 15.FIS_INFI.csv: not read. columns: "SITIO", "ENSAYO", "FECH_EVA", "TRAT", "CULT", "REPI", "PROF", "NUME_LEC", "NUME_MIN", "DIFE_MIN", "ALTU_COL", "CAMBIO", .... Observation records are missing from d until this file is standardized.
# 16.FIS_SORTIVIL.csv: not read. columns: "SITIO", "ENSAYO", "FECH_EVA", "TRAT", "CULT", "REPI", "PROF", "NUME_SEG", "VALO_LEC". Observation records are missing from d until this file is standardized.
# 17.FIS_CHS_CALC.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "Q", "A", "T", .... Observation records are missing from d until this file is standardized.
# 18.FIS_CHS_TIEM.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "NUME_MIN", "ALTU_COL". Observation records are missing from d until this file is standardized.
# 19.FIS_PERMEABI.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "DIAM_CIL", "ALTU_CIL", "AREA_CIL", .... Observation records are missing from d until this file is standardized.
# 20.FIS_PROC_SEC.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "DIA", "PESO", "DIFE_PES", .... Observation records are missing from d until this file is standardized.
# 21.FIS_HUMEDAD.csv: not read. columns: "FECH_ENT", "FECH_SAL", "TIPO_HUM", "TIPO", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "AGUA", .... Observation records are missing from d until this file is standardized.
# 22.FIS_DENS_APA.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "PESO_SH_ACC", "PESO_ACC", "PESO_SH_TOT", .... Observation records are missing from d until this file is standardized.
# 23.FIS_DENS_REA.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "PESO_BAL", "PESO_BAL_AGU", "PESO_BAL_SUE", .... Observation records are missing from d until this file is standardized.
# 24.FIS_HUME_GV.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "DENS_APA", "COND_HS", "SUCC", .... Observation records are missing from d until this file is standardized.
# 25.FIS_CURV_RET.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "PESO_SH_ACCE_CAM", "PESO_SH_ACCE_LAB", "PESO_ACCE_CAM", .... Observation records are missing from d until this file is standardized.
# 26.FIS_AQUATER.csv: not read. columns: "SITIO", "ENSAYO", "FECH_EVA", "TRAT", "CULT", "REPI", "PROF", "NUME_LEC", "VALO_LEC". Observation records are missing from d until this file is standardized.
# 27.FIS_COMPACTA.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "PESO_SH_ACC_CAM", "PESO_SH_ACC_LAB", "PESO_ACC_CAM", .... Observation records are missing from d until this file is standardized.
# 28.FIS_ANAL_FER.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SERIE", "CONS_LAB", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "DESC_MUE", .... Observation records are missing from d until this file is standardized.
# 30.FIS_MATE_ORG.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "APER_TAM", "PESO", "C", .... Observation records are missing from d until this file is standardized.
# 31.FIS_SOLU_NIT.csv: not read. columns: "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "HUME", "NNH4_SS", "NNO3_SS", "NNH4", "NNO3", .... Observation records are missing from d until this file is standardized.
# 32.FIS_ANAL_TEJ.csv: not read. columns: "FECH_ENT", "FECH_SAL", "SERIE", "CONS_LAB", "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PARTE", "C", .... Observation records are missing from d until this file is standardized.
# 33.FIS_EVAL_RAI.csv: not read. columns: "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "VOLU_SUE", "PESO_S_RAI", "VOLU_RAI", "DENS_RAI", "DENS_RAI_GR_M3", .... Observation records are missing from d until this file is standardized.
# 34.FIS_PROD_MS.csv: not read. columns: "SITIO", "ENSAYO", "FECH_EVA", "TRAT", "CULT", "REPI", "PARTE", "PESO_V", "PESO_VS", "PESO_SS", "PARTE_GR_M2". Observation records are missing from d until this file is standardized.
# 35.FIS_NUME_PLA.csv: not read. columns: "SITIO", "ENSAYO", "FECH_EVA", "TRAT", "CULT", "REPI", "NUME_LEC", "DOSIS", "NUME_PLA", "ALTU_PLA", "NUME_HOJ". Observation records are missing from d until this file is standardized.
# 37.FIS_EVAL_PAS.csv: not read. columns: "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "B_HUMI", "P_MAXI", "A_PINT", "C_ACUT", "P_PHAS", "S_CAPI", .... Observation records are missing from d until this file is standardized.
# 38.FIS_EVAL_FEN.csv: not read. columns: "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "ETAPA", "HOJA", "GRAN_HUM", "SOCA", "IAF", "VAIN", .... Observation records are missing from d until this file is standardized.
# 39.FIS_PBIO_ROT.csv: not read. columns: "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "ETAPA", "BIOM", "GRAN_15H". Observation records are missing from d until this file is standardized.
# 40.FIS_PBIO_5X5.csv: not read. columns: "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "MARCO", "GRAN_HUM", "SOCA", "MALE". Observation records are missing from d until this file is standardized.
# 41.FIS_PBIO_1X1.csv: not read. columns: "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "MARCO", "EVAL", "ETAPA", "HOJA", "GRAN_HUM", "SOCA", .... Observation records are missing from d until this file is standardized.
# 42.FIS_PBIO_OTROS.csv: not read. columns: "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "GRAN_HUM", "SOCA", "MALE", "CAUP_GRA", "CAUP_FOR", "SOYA", .... Observation records are missing from d until this file is standardized.
# 43.FIS_EFEC_MUL.csv: not read. columns: "SITIO", "ENSAYO", "FECH_MUE", "TRAT", "CULT", "REPI", "PROF", "SUPE", "INFE". Observation records are missing from d until this file is standardized.
# 44.FIS_LECT_REL.csv: not read. columns: "HERRAMIENTA", "TIPO", "SITIO", "ENSAYO", "FECH_EVA", "TRAT", "CULT", "REPI", "NUME_VAR", "VALO_LEC". Observation records are missing from d until this file is standardized.

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

	f1 <- ff[basename(ff) == "36.FIS_RENDIMIE.csv"]
	f2 <- ff[basename(ff) == "04.FIS_SITIOS.csv"]
	r <- read.csv(f1)
	r1 <- read.csv(f2)

	# skipped: 01-03/05-35 soil physics, weather, texture, Ksat, bulk density, soil lab, Culticore dates

	i <- match(r$SITIO, r1$SITIO)
	crop <- ifelse(r$CULT == "Arroz", "rice",
		ifelse(r$CULT == "Pasto llanero", "pasture", tolower(r$CULT)))

	d <- data.frame(
		country = r1$PAIS[i],
		adm1 = carobiner::fix_name(r1$DEPARTAMENTO[i], "title"),
		adm2 = carobiner::fix_name(r1$CIUDAD[i], "title"),
		location = r1$UBICACION[i],
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
