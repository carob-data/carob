# R script for "carob"
# license: GPL (>=3)

## NOTES
# Mid-term COVID-19 survey of potato producers in the northern highlands of Peru, 2021. Country Peru and region La Libertad are in the file (P06a is La Libertad for every row). Province, district, and populated center are columns.
# P09 is the most important crop. Spanish names that match the crop list are translated (papa, trigo, maiz, arveja, quinua, tomate, cebada, habas, lenteja, avena, chocho, fresa). Oca, olluco, mashua, verdura, and hortalizas stay in local_crop_.
# P31 and P32 area abbreviations end in m2. The dictionary unit cell says Number. The number is stored as field_area_m2_ and is not a plot area.

## ISSUES
# No coordinates.
# P18 is the harvest amount and P22 says to convert arroba and sack to kg, but the unit cell does not state the unit of the stored number, so there is no yield.
# write_files: missing carob_contributor, latitude, longitude. unknown variable: local_crop_ (crop?). unknown variable: crop_record_ (crop?). unknown variables: field_area_m2_.

## NEW VARIABLES
# field_area_m2_: cropped area from the P31 and P32 columns whose abbreviation ends in m2.
# local_crop_: crop name as written when it is not in the crop list.
# crop_record_: main (P09), new_2021 (P31), or current_2021 (P32).

## NOT PROCESSED
# Consent, gender, prices in soles, hired labor, harvest amount (P18), sales, COVID information, assistance, food, and migration questions. Prices are not per kg in the unit cell.


carob_script <- function(path) {

"
Dataset for: Impacto Productivo y Social del Covid-19 en Productores de papa de la Sierra Norte del Perú en 2021

During 2020, the challenges created by the COVID-19 health crisis were critical and household livelihood were affected and created challenges for everybody. In order to understand the extent of the crisis, challenges and measures taken to overcome the crisis, we conducted a mid-term survey to understand how COVID-19 has affected rural families. The survey corresponds to the following sections: (1) General information, (2) Informed consent, (3) Crop production, (4) COVID-19 knowledge, (5) mitigation measures, (6) migration and health services, (7) General comments, (8) Farewell.
"

	uri <- "doi:10.21223/PREYUF"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.4
	)

	f <- ff[grepl("^01_DataSet_COVID", basename(ff))]
	r <- carobiner::read.excel(f)
	txt <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z) | z == "NA"] <- NA_character_
		z
	}
	num <- function(x) {
		z <- txt(x)
		z[grepl("^P[0-9]", z)] <- NA_character_
		as.numeric(z)
	}
	norm <- function(x) {
		x <- tolower(txt(x))
		chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00f1", "aeioun", x)
	}
	crop_of <- function(x) {
		key <- norm(x)
		c(papa="potato", trigo="wheat", maiz="maize", arveja="pea", quinua="quinoa",
			tomate="tomato", cebada="barley", habas="faba bean", haba="faba bean",
			lenteja="lentil", avena="oats", chocho="tarwi", fresa="strawberry")[key]
	}
	when <- function(x) {
		z <- txt(x)
		ok <- grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$", z)
		out <- rep(NA_character_, length(z))
		if (any(ok)) {
			p <- strsplit(z[ok], "-", fixed=TRUE)
			out[ok] <- vapply(p, function(v) sprintf("%s-%02d-%02d", v[3], as.integer(v[2]), as.integer(v[1])), "")
		}
		out
	}

	base <- data.frame(
		hhid = txt(r$ID),
		date = when(r$P01),
		country = "Peru",
		adm1 = txt(r$P06a),
		adm2 = txt(r$P06b),
		adm3 = txt(r$P06c),
		location = txt(r$P06d)
	)
	main <- base
	main$local_crop_ <- txt(r$P09)
	main$crop <- as.character(crop_of(r$P09))
	main$field_area_m2_ <- NA_real_
	main$crop_record_ <- ifelse(is.na(main$local_crop_), NA_character_, "main")

	blocks <- list(
		c("P31Acul", "P31Aarem2", "new_2021"),
		c("P31Bcul", "P31Barem2", "new_2021"),
		c("P31Ccul", "P31Carem2", "new_2021"),
		c("P32Acul", "P32Aarem2", "current_2021"),
		c("P32Bcul", "P32Barem2", "current_2021"),
		c("P32Ccul", "P32Carem2", "current_2021")
	)
	parts <- lapply(blocks, function(cols) {
		data.frame(base, local_crop_ = txt(r[[cols[1]]]), crop = as.character(crop_of(r[[cols[1]]])),
			field_area_m2_ = num(r[[cols[2]]]), crop_record_ = cols[3])
	})
	extra <- do.call(rbind, parts)
	extra <- extra[!is.na(extra$local_crop_) | !is.na(extra$field_area_m2_), ]
	d <- rbind(main, extra)
	d <- d[!is.na(d$crop) | !is.na(d$local_crop_) | !is.na(d$field_area_m2_), ]
	d$crop_record_[is.na(d$local_crop_) & is.na(d$crop)] <- NA_character_
	d$geo_from_source <- FALSE
	d$is_survey <- TRUE
	d$on_farm <- TRUE
	d$trial_id <- "LaLibertad_COVID_2021"
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
