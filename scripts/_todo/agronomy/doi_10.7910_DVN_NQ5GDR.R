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



## NOT PROCESSED
# 01.DICTONARY.xlsx: not read. sheet Dictonary: TABLA/TABLE, COLUMNA/COLUMN, DESCRIPCION, DESCRIPTION, UNIDAD DE MEDIDA/UNITS OF MEASUREMENT. Observation records are missing from d until this file is standardized.
# 03.MBIO_ENSAYOS.csv: not read. columns: ENSAYO, DESC_ENSA. Observation records are missing from d until this file is standardized.
# 04.MBIO_TRATAMIENTOS.csv: not read. columns: SITIO, ENSAYO, TRAT, CULT, DESC_TRAT, DESC_CULT. Observation records are missing from d until this file is standardized.
# 05.MBIO_DESC_HOJ_RAI.csv: not read. columns: CODIGO, SITIO, ENSAYO, FECH_EVA, TRAT, CULT, REPI, SUBTRAT, PARTE, EVAL, DISTRIBUCION, DIA_DES_DIS, .... Observation records are missing from d until this file is standardized.
# 06.MBIO_PROD_HOJ_RAI.csv: not read. columns: CODIGO, SITIO, ENSAYO, FECH_EVA, TRAT, CULT, REPI, MARCO, TAM_MARCO, DIA_DES_DIS, MATE_SEC_MAR, MATE_SEC_M2, .... Observation records are missing from d until this file is standardized.
# 08.MBIO_FIJA_N.csv: not read. columns: CODIGO, SITIO, ENSAYO, FECH_EVA, TRAT, CULT, REPI, MATE_SEC_LEG, MATE_SEC_GRAM, N_LEG, N_GRAM, N_TOT_LEG, .... Observation records are missing from d until this file is standardized.
# 09.MBIO_15N.csv: not read. columns: CODIGO, SITIO, ENSAYO, FECH_EVA, TRAT, CULT, REPI, DIA_DES_15N, CANT_15N, MATE_SEC, N, N_TOT, .... Observation records are missing from d until this file is standardized.
# 10.MBIO_BALANCE.csv: not read. columns: CODIGO, SITIO, ENSAYO, FECH_EVA, TRAT, CULT, REPI, AREA_MUE, TALLO, HOJA, CAPACHO, TUSA, .... Observation records are missing from d until this file is standardized.
# 11.MBIO_INOC_INV.csv: not read. columns: CODIGO, SITIO, ENSAYO, FECH_EVA, TRAT, CULT, REPI, TIPO, NUME_HOJ, AREA_FOL, TALLO, RAIZ, .... Observation records are missing from d until this file is standardized.
# 12.MBIO_15N_SUE.csv: not read. columns: CODIGO, SITIO, ENSAYO, FECH_EVA, TRAT, CULT, REPI, PROF, CANT_15N, CANT_AT15_EXC, AREA_MUE, AT15_FER, .... Observation records are missing from d until this file is standardized.
# 13.MBIO_POTE_MIN.csv: not read. columns: CODIGO, SITIO, ENSAYO, FECH_EVA, TRAT, CULT, REPI, EVAL, PROF, NNH4_T0, NNO3_T0, NNH4_T7, .... Observation records are missing from d until this file is standardized.
# 14.MBIO_FIJA_N_INV.csv: not read. columns: CODIGO, SITIO, ENSAYO, FECH_EVA, TRAT, CULT, REPI, AREA_FOL, MATE_SEC_ARE, RAIZ, MATE_SEC_TOT, N, .... Observation records are missing from d until this file is standardized.
# 15.MBIO_NIVE_N.csv: not read. columns: CODIGO, SITIO, ENSAYO, FECH_EVA, TRAT, CULT, REPI, PROF, PESO_SH, PESO_SS, NNH4, NNO3, .... Observation records are missing from d until this file is standardized.
# 16.MBIO_BIOM_MIC.csv: not read. columns: CODIGO, SITIO, ENSAYO, FECH_EVA, TRAT, CULT, REPI, PROF, PESO_SH, PESO_SS, C_FUM, C_NFUM, .... Observation records are missing from d until this file is standardized.
# 18.MBIO_PLANTAS.csv: not read. columns: CODIGO, SITIO, ENSAYO, FECH_EVA, TRAT, CULT, NUME_PLA, DIA_DES_TRA, DIAM_PLA, ALTU_PLA. Observation records are missing from d until this file is standardized.
# 19.MBIO_DETERMIN.csv: not read. columns: DETER, CODIGO, FECH_EVA, SITIO, ENSAYO, TRAT, CULT. Observation records are missing from d until this file is standardized.

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

	f1 <- ff[basename(ff) == "17.MBIO_PRODUCCI.csv"]
	f2 <- ff[basename(ff) == "02.MBIO_SITIOS.csv"]
	r <- read.csv(f1)
	r1 <- read.csv(f2)

	# skipped: 01 dictionary; 03-06 treatments/litter; 07 soil; 08-16 N-15/inoculation/microbial; 18 plants; 19 determinations

	parse_N <- function(x) {
		m <- regmatches(x, gregexpr("[0-9.]+(?=N)", x, perl=TRUE))
		sapply(m, function(z) {
			if (length(z) == 0) NA_real_ else sum(as.numeric(z))
		})
	}

	i <- match(r$SITIO, r1$SITIO)
	d <- data.frame(
		country = r1$PAIS[i],
		adm1 = carobiner::fix_name(r1$DEPARTAMENTO[i], "title"),
		adm2 = carobiner::fix_name(r1$CIUDAD[i], "title"),
		location = r1$UBICACION[i],
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
