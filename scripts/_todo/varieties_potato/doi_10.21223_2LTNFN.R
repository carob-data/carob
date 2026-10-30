# R script for "carob"
# license: GPL (>=3)

## NOTES
# Minerals and vitamin C of B1C5 clones at Muqui, Jauja, Junin. The measurement files state the units in the column names.
# Mineral concentrations are tuber contents, including K and P. They are not fertilizer rates.
# Installation on both fieldbooks: completely randomized design, planted 2003-12-04, harvested 2004-05-04.
# Coordinates and elevation 3366 m are on the Identification sheet.
# A token such as "< 6" is below the reported detection limit. Those cells are missing, not a measured number.

## ISSUES
# Plot size and spacing are blank in the Installation sheet.
# Crop management has no product and no rate.
# No tuber yield.
# Dry matter in the mineral file and the vitamin C file are not the same number for the same clone and replicate. Both are kept.
# write_files: missing variables: carob_contributor, yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: the tuber mineral, dry-matter, and ascorbic-acid columns named with a trailing underscore.

## NOT PROCESSED
# 04_Datadictionary.txt, Lists, Notes, Summary by clone, ICPAES Results: definitions, summaries, or a laboratory layout that is not one row per plot.
# The Minerals and VitaminC sheets in the xls fieldbooks repeat the tidy text files in a layout that is not one row per plot.
# Breeder code, laboratory code, and numeration.


carob_script <- function(path) {

"
Dataset for: Analysis of Minerals and Vitamin C in Varieties of the B1C5 Population in Muqui, Junin

Laboratory assays for minerals and vitamin C were conducted on raw samples with peel in dry weight basis for the B1C5 population group, planted in the Muqui locality in Jauja, Junin. The analysis methodology employed chemical spectrophotometry. Thirteen clones from the B1C5 population were analyzed with three replications following a Completely Randomized Design (CRD) during the year 2004.
"

	uri <- "doi:10.21223/2LTNFN"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "Completely Randomized Design (CRD)",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "dry_matter_minerals_percent_;tuber_Fe_mg_kg_dw_;tuber_Zn_mg_kg_dw_;ascorbic_acid_mg_100g_fw_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.5
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x %in% c("NA", "-")] <- NA_character_
		x[!is.na(x) & !grepl("^-?[0-9]+(\\.[0-9]+)?$", x)] <- NA_character_
		as.numeric(x)
	}

	fm <- ff[basename(ff) == "01_Fieldbook_TMIN200312_MUQUI_B1C5_RanchaII.txt"]
	fv <- ff[basename(ff) == "02_Fieldbook_TVIT200312_MUQUI_B1C5_RanchaII.txt"]
	fl <- ff[basename(ff) == "03_Material_list.txt"]
	m <- read.delim(fm, check.names=FALSE, fileEncoding="latin1", quote="\"", stringsAsFactors=FALSE)
	v <- read.delim(fv, check.names=FALSE, fileEncoding="latin1", quote="\"", stringsAsFactors=FALSE)
	ml <- read.delim(fl, check.names=FALSE, fileEncoding="latin1", quote="\"", stringsAsFactors=FALSE)
	names(m) <- trimws(names(m))
	names(v) <- trimws(names(v))
	names(ml) <- trimws(names(ml))

	instn <- trimws(as.character(m$INSTN))
	key <- gsub(" ", "", trimws(as.character(ml[["CIP number"]])))
	acc <- trimws(as.character(ml[["Accession name"]]))
	acc[!nzchar(acc) | acc == "-"] <- NA_character_
	nm <- acc[match(gsub(" ", "", instn), key)]
	variety <- ifelse(is.na(nm), instn, nm)

	vk <- paste(trimws(as.character(v$INSTN)), as.integer(v$REP))
	mk <- paste(instn, as.integer(m$REP))
	vi <- match(mk, vk)

	# Column order is the published fieldbook. Names are trimmed above; positions are used because the header text is long.
	mc <- names(m)
	d <- data.frame(
		trial_id = "Muqui_2003",
		crop = "potato",
		variety = variety,
		variety_code = instn,
		treatment = variety,
		rep = as.integer(m$REP),
		country = "Peru",
		adm1 = "Junin",
		adm2 = "Jauja",
		adm3 = "Muqui",
		location = "Muqui",
		elevation = 3366,
		# Identification sheet of TMIN200312_MUQUI_B1C5_RanchaII.xls
		latitude = -11.8336,
		longitude = -75.4343,
		geo_from_source = TRUE,
		planting_date = "2003-12-04",
		harvest_date = "2004-05-04",
		dry_matter_minerals_percent_ = num(m[[mc[13]]]),
		tuber_Fe_mg_kg_dw_ = num(m[[mc[14]]]),
		tuber_Fe_mg_100g_fw_ = num(m[[mc[15]]]),
		tuber_Mn_mg_kg_dw_ = num(m[[mc[16]]]),
		tuber_Mn_mg_100g_fw_ = num(m[[mc[17]]]),
		tuber_B_mg_kg_dw_ = num(m[[mc[18]]]),
		tuber_B_mg_100g_fw_ = num(m[[mc[19]]]),
		tuber_Cu_mg_kg_dw_ = num(m[[mc[20]]]),
		tuber_Cu_mg_100g_fw_ = num(m[[mc[21]]]),
		tuber_Zn_mg_kg_dw_ = num(m[[mc[22]]]),
		tuber_Zn_mg_100g_fw_ = num(m[[mc[23]]]),
		tuber_Ca_mg_kg_dw_ = num(m[[mc[24]]]),
		tuber_Ca_mg_100g_fw_ = num(m[[mc[25]]]),
		tuber_Mg_mg_kg_dw_ = num(m[[mc[26]]]),
		tuber_Mg_mg_100g_fw_ = num(m[[mc[27]]]),
		tuber_K_mg_kg_dw_ = num(m[[mc[28]]]),
		tuber_K_mg_100g_fw_ = num(m[[mc[29]]]),
		tuber_P_mg_kg_dw_ = num(m[[mc[30]]]),
		tuber_P_mg_100g_fw_ = num(m[[mc[31]]]),
		tuber_S_mg_kg_dw_ = num(m[[mc[32]]]),
		tuber_S_mg_100g_fw_ = num(m[[mc[33]]]),
		tuber_Na_mg_kg_dw_ = num(m[[mc[34]]]),
		tuber_Na_mg_100g_fw_ = num(m[[mc[35]]]),
		tuber_Al_mg_kg_dw_ = num(m[[mc[36]]]),
		tuber_Cd_mg_kg_dw_ = num(m[[mc[37]]]),
		tuber_Mo_mg_kg_dw_ = num(m[[mc[38]]]),
		tuber_Co_mg_kg_dw_ = num(m[[mc[39]]]),
		tuber_Ni_mg_kg_dw_ = num(m[[mc[40]]]),
		ascorbic_acid_mg_100g_dw_ = num(v[[names(v)[13]]][vi]),
		ascorbic_acid_mg_100g_fw_ = num(v[[names(v)[14]]][vi]),
		dry_matter_vitamin_c_percent_ = num(v[[names(v)[15]]][vi]),
		on_farm = FALSE,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
