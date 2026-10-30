# R script for "carob"
# license: GPL (>=3)

## NOTES
# Phureja collection evaluated at Comas, Concepcion, Junin, Peru, in 2006. Country Peru is the geographic coverage and the mineral sheet (written there as Perú). Department, province, district, and locality are columns.
# Minimal dates are planting 2006-05-09 and harvest 2006-09-21. The installation sheet also says harvest 2006-09-21, but its planting cell is 2005-04-18, which is not used.
# Installation: RCBD, 3 blocks, 8 plants per plot, plot 2.16 m2, 0.3 m between plants and 0.9 m between rows (30 cm and 90 cm), density 37037.037 plants/ha.
# Mineral columns state the unit: mg/kg dry weight or mg/100 g fresh weight. They are tuber composition, not fertilizer. Dry matter is percent. The bounding box publishes only the south and west corner for this one site.

## ISSUES
# No yield in the mineral sheet.
# Vitamin C, phenolics, and morphology workbooks are separate evaluations and are not joined.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: tuber_dry_matter_ and the tuber mineral columns (mg/kg dry weight and mg/100 g fresh weight).

## NEW VARIABLES
# tuber_dry_matter_: percentage dry matter.
# tuber_<element>_mg_kg_dw_ and tuber_<element>_mg_100g_fw_: tuber minerals in the units printed in the header.

## NOT PROCESSED
# TVIT, TPHEN, and TMORP workbooks.
# Morphology scores in the fieldbook. Those codes are not mapped to a scale here.


carob_script <- function(path) {

"
Dataset for: Field Evaluation and Nutritional Analysis of Phureja Collection Native Potatoes in Comas - Junin, 2006

An evaluation was carried out on 192 varieties from the Phureja potato collection. The assessment took place in the Comas locality in Junin during the 2006 growing season. Additionally, nutritional analyses were conducted for micronutrients, vitamin C, carotenoids, polyphenols, and a morphological characterization.
"

	uri <- "doi:10.21223/JMRQMO"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.5
	)

	f <- ff[grepl("^TMIN", basename(ff))]
	raw <- as.data.frame(carobiner::read.excel(f, sheet="Minerals", col_names=FALSE), stringsAsFactors=FALSE)
	hdr <- trimws(as.character(unlist(raw[6, ])))
	hdr[!nzchar(hdr) | is.na(hdr)] <- paste0("blank_", seq_along(hdr))[!nzchar(hdr) | is.na(hdr)]
	body <- raw[7:nrow(raw), ]
	names(body) <- make.unique(hdr)
	txt <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z) | z == "NA"] <- NA_character_
		z
	}
	num <- function(x) {
		z <- txt(x)
		z[grepl("^<", z)] <- NA_character_
		ok <- is.na(z) | grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
		z[!ok] <- NA_character_
		as.numeric(z)
	}
	col <- function(pattern) {
		nm <- names(body)[grepl(pattern, names(body))][1]
		if (is.na(nm)) return(rep(NA_real_, nrow(body)))
		num(body[[nm]])
	}
	variety <- txt(body[["Accession name"]])
	cip <- txt(body[["Identification"]])
	variety[is.na(variety)] <- cip[is.na(variety)]
	country <- txt(body[["Country"]])
	country[country %in% c("Perú", "Peru")] <- "Peru"
	d <- data.frame(
		variety = variety,
		treatment = variety,
		rep = as.integer(num(body[["Block"]])),
		country = country,
		adm1 = txt(body[["Departament"]]),
		adm2 = txt(body[["Province"]]),
		adm3 = txt(body[["District"]]),
		location = txt(body[["Locality"]]),
		latitude = -11.7178,
		longitude = -75.0814,
		geo_from_source = TRUE,
		geo_uncertainty = 2000,
		geo_source = "CIP Dataverse bounding box (south and west only)",
		planting_date = "2006-05-09",
		harvest_date = "2006-09-21",
		crop = "potato",
		plot_area = 2.16,
		plant_spacing = 30,
		row_spacing = 90,
		plant_density = 37037.037,
		tuber_dry_matter_ = col("^Percentage Dry matter"),
		trial_id = "Comas_phureja_2006",
		on_farm = FALSE,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)
	elems <- c("Fe", "Mn", "B", "Cu", "Zn", "Ca", "Mg", "K", "P", "S", "Na", "Al", "Ti", "Cr", "Se", "Pb", "Cd", "Mo", "Co", "Ni")
	for (el in elems) {
		dw <- col(paste0("^", el, " +\\(mg/kg"))
		fw <- col(paste0("^", el, " +\\(mg/100g"))
		d[[paste0("tuber_", tolower(el), "_mg_kg_dw_")]] <- dw
		if (!all(is.na(fw))) d[[paste0("tuber_", tolower(el), "_mg_100g_fw_")]] <- fw
	}
	d <- d[!is.na(d$variety), ]
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
