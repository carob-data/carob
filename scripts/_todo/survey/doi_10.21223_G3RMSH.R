# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline CAP survey of  potato-producing households in Curgos and Julcan, Sanchez Carrion, La Libertad, Peru, 10 October to 21 December 2020. Country Peru is the geographic coverage. Department, province, district, and community are columns.
# Up to three potato varieties are recorded per household. P61.Has, P67.Has, and P73a Has are hectares. P62a, P68a, and P74a are tonnes harvested. Yield (kg/ha) is tonnes / hectares * 1000 when both numbers are present. Each variety is its own row.
# Grupo is the survey arm (intervention or control). It is kept as treatment.

## ISSUES
# No GPS coordinates. Communities need georeferencing.
# Seed quantity has no unit in the column name, so it is not a seed rate.
# A repeated header cell (the column name itself) in the hectare and tonne columns is treated as missing.
# write_files: missing carob_contributor, latitude, longitude. NA detected: yield, yield_part.

## NOT PROCESSED
# Consent, parent and child descriptors, and the anemia knowledge and attitude questions. Those are not crop measurements. Child birth dates and caregiver descriptors are not copied.
# Seed amount columns and the tonne columns for sale, consumption, and seed of each variety. Harvest tonnes are the yield.
# The total cropped-area question, because the column mixes a quantity and a unit.


carob_script <- function(path) {

"
Dataset for: Linea de base de Conocimientos, Actitudes y Prácticas (CAP) sobre Deficiencia de Hierro (Anemia) en familias productoras de papa en la Sierra de La Libertad

Through a gender-responsive pilot intervention, the project Anemia Zero is contributing to improve food security, nutrition, gender equality, and empowerment in potato-based production systems in high Andean regions of Peru (Curgos and Julcan Districts, Province Sanchez Carrion, Department of La Libertad) and to scale agricultural and nutrition innovations in coordination with the social protection programs (SPP) Juntos and Cuna Mas (both overseen by the Ministry of Development and Social Inclusion (MIDIS), as well as with other stakeholders. This survey was conducted between October 10th and December 21st, 2020 to measure current knowledge, attitude and practices corresponding to Anemia among 242 potato producing households (142 households from the intervention group and 100 households from the control group. The survey presented 75 questions divided in the following sections: (1) Informed consent (2 questions), (A) Socioeconomic information from both parents and children (21 questions), (B) knowledge, attitude and practices corresponding to Anemia (31 questions), (C) crop production aspects (20 questions).
"

	uri <- "doi:10.21223/G3RMSH"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "treatment",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.4
	)

	f <- ff[grepl("^01_DataSet_CAP", basename(ff))]
	r <- carobiner::read.excel(f)
	num <- function(x) {
		z <- trimws(as.character(x))
		z[grepl("^P[0-9]", z)] <- NA_character_
		z[!nzchar(z)] <- NA_character_
		as.numeric(z)
	}
	txt <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z)] <- NA_character_
		z
	}

	blocks <- list(
		c("¿Qué variedades de papa sembró, en la última campaña de siembra? liste el nombre de las Tres principales, en orden de importancia: Sembró:  VARIEDAD 1",
			"P61.Has", "P62a Toneladas"),
		c("Sembró: VARIEDAD 2", "P67.Has", "P68a Toneladas"),
		c("Sembró: VARIEDAD 3", "P73a Has", "P74a Toneladas")
	)

	base <- data.frame(
		hhid = txt(r$ID),
		treatment = txt(r$Grupo),
		country = "Peru",
		adm1 = txt(r$Departamento),
		adm2 = txt(r$Provincia),
		adm3 = txt(r$Distrito),
		location = txt(r$Comunidad)
	)

	parts <- lapply(blocks, function(cols) {
		data.frame(
			base,
			variety = txt(r[[cols[1]]]),
			area_ha = num(r[[cols[2]]]),
			tonnes = num(r[[cols[3]]])
		)
	})
	d <- do.call(rbind, parts)
	keep <- !is.na(d$variety) | !is.na(d$area_ha) | !is.na(d$tonnes)
	# Households with no variety row still appear once.
	missing_hh <- !base$hhid %in% d$hhid[keep]
	if (any(missing_hh)) {
		extra <- base[missing_hh, ]
		extra$variety <- NA_character_
		extra$area_ha <- NA_real_
		extra$tonnes <- NA_real_
		d <- rbind(d[keep, ], extra)
	} else {
		d <- d[keep, ]
	}
	d$crop <- ifelse(is.na(d$variety) & is.na(d$tonnes), NA_character_, "potato")
	d$yield <- ifelse(!is.na(d$tonnes) & !is.na(d$area_ha) & d$area_ha > 0, d$tonnes / d$area_ha * 1000, NA_real_)
	d$yield_part <- ifelse(is.na(d$yield), NA_character_, "tubers")
	d$area_ha <- NULL
	d$tonnes <- NULL
	d$geo_from_source <- FALSE
	d$is_survey <- TRUE
	d$on_farm <- TRUE
	d$trial_id <- "LaLibertad_CAP_2020"
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
