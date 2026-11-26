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
# Dry matter in the mineral file and the vitamin C file differ slightly for the same clone/rep; mineral-file dry matter is mapped to yield_moisture (100 - DM) and vitamin-C-file dry matter is kept in dm_vitamin_c_.

## NEW VARIABLES
# tuber_<element>_fw_: tuber mineral concentration (mg/100g fresh weight) for Fe, Mn, B, Cu, Zn, Ca, Mg, K, P, S, Na. (Dry-weight minerals are stored in standard tuber_<element> columns in mg/g DW; fresh-weight ascorbic acid in vitamin_C in mg/100g FW.)
# tuber_ascorbate_dw_: tuber ascorbic acid concentration (mg/100g dry weight).
# dm_vitamin_c_: dry matter percentage from the vitamin C file (%).

## NOT PROCESSED
# 04_Datadictionary.txt, Lists, Notes, Summary by clone, ICPAES Results: definitions, summaries, or a laboratory layout that is not one row per plot.
# The Minerals and VitaminC sheets in the xls fieldbooks repeat the tidy text files in a layout that is not one row per plot.
# Breeder code, laboratory code, and numeration.
# TVIT200312_MUQUI_B1C5_RanchaII.xls: not read. sheet Minimal (of 10): Factor, Value. Observation records are missing from d until this file is standardized.


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
		response_vars = "yield_moisture;tuber_Fe;tuber_Zn;vitamin_C",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x %in% c("NA", "-")] <- NA_character_
		x[!is.na(x) & !grepl("^-?[0-9]+(\\.[0-9]+)?$", x)] <- NA_character_
		as.numeric(x)
	}

	f1 <- ff[basename(ff) == "01_Fieldbook_TMIN200312_MUQUI_B1C5_RanchaII.txt"]
	f2 <- ff[basename(ff) == "02_Fieldbook_TVIT200312_MUQUI_B1C5_RanchaII.txt"]
	f3 <- ff[basename(ff) == "03_Material_list.txt"]
	r1 <- read.delim(f1, check.names=FALSE, fileEncoding="latin1", quote="\"")
	r2 <- read.delim(f2, check.names=FALSE, fileEncoding="latin1", quote="\"")
	r3 <- read.delim(f3, check.names=FALSE, fileEncoding="latin1", quote="\"")
	names(r1) <- trimws(names(r1))
	names(r2) <- trimws(names(r2))
	names(r3) <- trimws(names(r3))

	instn <- trimws(as.character(r1$INSTN))
	key <- gsub(" ", "", trimws(as.character(r3[["CIP number"]])))
	acc <- trimws(as.character(r3[["Accession name"]]))
	acc[!nzchar(acc) | acc == "-"] <- NA_character_
	nm <- acc[match(gsub(" ", "", instn), key)]
	variety <- ifelse(is.na(nm), instn, nm)

	vk <- paste(trimws(as.character(r2$INSTN)), as.integer(r2$REP))
	mk <- paste(instn, as.integer(r1$REP))
	vi <- match(mk, vk)

	# Column order is the published fieldbook. Names are trimmed above; positions are used because the header text is long.
	mc <- names(r1)
	d <- data.frame(
		trial_id = "Muqui_2003",
		crop = "potato",
		variety = variety,
		variety_code = instn,
		treatment = variety,
		rep = as.integer(r1$REP),
		country = "Peru",
		adm1 = "Junín",
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
		yield_moisture = 100 - num(r1[[mc[13]]]),
		# Dry-weight minerals converted from mg/kg to mg/g (terminag standard)
		tuber_Fe = num(r1[[mc[14]]]) / 1000,
		tuber_Fe_fw_ = num(r1[[mc[15]]]),
		tuber_Mn = num(r1[[mc[16]]]) / 1000,
		tuber_Mn_fw_ = num(r1[[mc[17]]]),
		tuber_B = num(r1[[mc[18]]]) / 1000,
		tuber_B_fw_ = num(r1[[mc[19]]]),
		tuber_Cu = num(r1[[mc[20]]]) / 1000,
		tuber_Cu_fw_ = num(r1[[mc[21]]]),
		tuber_Zn = num(r1[[mc[22]]]) / 1000,
		tuber_Zn_fw_ = num(r1[[mc[23]]]),
		tuber_Ca = num(r1[[mc[24]]]) / 1000,
		tuber_Ca_fw_ = num(r1[[mc[25]]]),
		tuber_Mg = num(r1[[mc[26]]]) / 1000,
		tuber_Mg_fw_ = num(r1[[mc[27]]]),
		tuber_K = num(r1[[mc[28]]]) / 1000,
		tuber_K_fw_ = num(r1[[mc[29]]]),
		tuber_P = num(r1[[mc[30]]]) / 1000,
		tuber_P_fw_ = num(r1[[mc[31]]]),
		tuber_S = num(r1[[mc[32]]]) / 1000,
		tuber_S_fw_ = num(r1[[mc[33]]]),
		tuber_Na = num(r1[[mc[34]]]) / 1000,
		tuber_Na_fw_ = num(r1[[mc[35]]]),
		tuber_Al = num(r1[[mc[36]]]) / 1000,
		tuber_Cd = num(r1[[mc[37]]]) / 1000,
		tuber_Mo = num(r1[[mc[38]]]) / 1000,
		tuber_Co = num(r1[[mc[39]]]) / 1000,
		tuber_Ni = num(r1[[mc[40]]]) / 1000,
		tuber_ascorbate_dw_ = num(r2[[names(r2)[13]]][vi]),
		vitamin_C = num(r2[[names(r2)[14]]][vi]),
		dm_vitamin_c_ = num(r2[[names(r2)[15]]][vi]),
		on_farm = FALSE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
