# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are 01_Fieldbook_TMIN200312_MUQUI_Varieties.txt (tab-separated, Latin-1).
# Dry matter is labeled Percentage Dry matter(%). Iron and zinc DW columns are mg/kg. FW columns are mg/100g.
# Values that start with "<" are detection limits and are stored as missing.
# Coordinates are the bounding box: southLatitude -11.8336, westLongitude -75.4343. Place names are in the file (Muqui, Jauja, Junin, Peru).

## ISSUES
# No tuber yield in kg/ha, no fertilizer, and no planting or harvest date. Year is 2004.
# The file spells the country Perú. It is stored as Peru.
# write_files: missing variables: carob_contributor; yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variables: dm_pct_, tuber_Fe_dw_, tuber_Fe_fw_, tuber_Zn_dw_, tuber_Zn_fw_.

## NOT PROCESSED
# TMIN200312_MUQUI_Varieties.xls and TVIT200312_MUQUI_Varieties.xls: workbook summaries. The tab file has one row per replicate.
# Mn, B, Cu, Al, Se, Pb, Cd, Mo, Co, and Ni columns. The mapped minerals are dry matter, iron, and zinc.

## NEW VARIABLES
# dm_pct_: dry matter, percent, from the column header
# tuber_Fe_dw_: iron, mg/kg DW
# tuber_Fe_fw_: iron, mg/100g FW
# tuber_Zn_dw_: zinc, mg/kg DW
# tuber_Zn_fw_: zinc, mg/100g FW

carob_script <- function(path) {

"
Data set for: Analysis of Minerals and Vitamin C in Varieties of potato in Muqui, Junin

Mineral and vitamin C assessments were conducted on five potato varieties: Canchan-INIA, Amarilis-INIA, REICHE, UNICA, Desiree, and Atlantic, all planted in Muqui, Jauja - Junin. The analysis methodology involved the use of chemical spectrophotometry. The samples were carried out on raw samples with peel, based on a dry weight basis with three replications following a Completely Randomized Design (CRD) during the year 2004.
"

	uri <- "doi:10.21223/W9MMSN"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "dm_pct_;tuber_Fe_dw_;tuber_Zn_dw_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "01_Fieldbook_TMIN200312_MUQUI_Varieties.txt"]
	r <- utils::read.delim(f, fileEncoding="latin1", check.names=FALSE, stringsAsFactors=FALSE)
	names(r) <- trimws(names(r))
	pick <- function(pattern) {
		i <- grep(pattern, names(r))[1]
		if (is.na(i)) stop("column not found: ", pattern)
		r[[i]]
	}
	num <- function(x) {
		y <- trimws(as.character(x))
		y[grepl("^<", y) | y %in% c("", "NA")] <- NA_character_
		as.numeric(y)
	}

	d <- data.frame(
		crop = "potato",
		variety_code = trimws(as.character(pick("^INSTN$"))),
		variety = trimws(as.character(pick("^Accession name$"))),
		rep = as.integer(pick("^REP$")),
		dm_pct_ = num(pick("Dry matter")),
		tuber_Fe_dw_ = num(pick("^Fe \\(mg/kg,DW\\)$")),
		tuber_Fe_fw_ = num(pick("^Fe .*mg/100g,FW")),
		tuber_Zn_dw_ = num(pick("^Zn \\(mg/kg,DW\\)$")),
		tuber_Zn_fw_ = num(pick("^Zn .*mg/100g,FW")),
		country = trimws(as.character(pick("^Country$"))),
		adm1 = trimws(as.character(pick("^Departament$"))),
		adm2 = trimws(as.character(pick("^Province$"))),
		adm3 = trimws(as.character(pick("^District$"))),
		location = trimws(as.character(pick("^Locality$"))),
		latitude = -11.8336,
		longitude = -75.4343,
		geo_from_source = TRUE,
		geo_source = "Dataverse bounding box",
		trial_id = "Muqui_2004",
		on_farm = NA,
		is_survey = FALSE,
		irrigated = NA,
		stringsAsFactors = FALSE
	)
	d$treatment <- d$variety
	d$country[d$country %in% c("Perú", "Peru ")] <- "Peru"
	d$plot_id <- paste(d$variety_code, d$rep, sep="_")

	carobiner::write_files(path, meta, d)
}
