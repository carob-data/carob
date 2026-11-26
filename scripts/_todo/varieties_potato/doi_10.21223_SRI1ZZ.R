# R script for "carob"
# license: GPL (>=3)

## NOTES
# Diploid potato clones at Holetta, Ethiopia during the 2015 irrigation season. Country, admin units, locality, elevation, and coordinates are on the Minimal sheet of the yield workbook.
# TTWP and MTWP are total and marketable tuber weight per plot (kg/plot for 20 planted tubers, ~0.3-0.5 kg/plant in TTWPL). No plot area is recorded in the file, so they are stored in ttwp_ and mtwp_ rather than yield (kg/ha). TTYA is empty.
# FeDW, ZnDw, and DM are accession-level mineral values joined by accession number. DM (%) is mapped to yield_moisture = 100 - DM.
# End_date in Minimal is 15/07/2015 (stored as harvest_date = "2015-07-15").

## ISSUES
# No plot area is reported in Minimal, so yield in kg/ha cannot be computed from kg/plot (TTWP/MTWP).
# Begin_date in Minimal is Excel serial 42250 (2015-09-03), which is after End_date (2015-07-15); omitted as planting_date.
# No fertilizer rates reported.

## NOT PROCESSED
# NTP, NPH, category weights, insect counts, and the material list. The element-description dictionary is documentation.
# 4029_DataDictionary_ElementDescription.xlsx: documentation or a codebook, not an observation table.
# 4029_DataDictionary_UniqueIdentifier.xlsx: documentation or a codebook, not an observation table.

## NEW VARIABLES
# ttwp_: total tuber weight per plot (kg/plot, 20 tubers planted)
# mtwp_: marketable tuber weight per plot (kg/plot)
# tuber_Fe_dw_: tuber iron concentration on dry-weight basis (mg/kg DW, unit not printed on sheet)
# tuber_Zn_dw_: tuber zinc concentration on dry-weight basis (mg/kg DW, unit not printed on sheet)


carob_script <- function(path) {

"
Dataset for: 100 promising diploid clones from biofortification cycle III selected and assessed in Ethiopia

Summary of selected genotypes from cycle III bio-fortified genotypes at Holetta/Ethiopia  during 2015 irrigation season. Based on Fe-Zn content 40 clones were selected, of which 5 were also comparatively high yielding
"

	uri <- "doi:10.21223/SRI1ZZ"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "ttwp_;mtwp_;yield_moisture;tuber_Fe_dw_;tuber_Zn_dw_",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 65,
		carob_effort = 0.45
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9.]+$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	chr <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}
	f1 <- ff[basename(ff) == "4029_PTYield2015_Hole_Harvesting_parameters.xlsx"]
	f2 <- ff[basename(ff) == "4029_2015_Minerals.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="Yield")
	r2 <- carobiner::read.excel(f2, sheet="Minerals_2015")
	minr <- data.frame(
		variety = chr(r2$ACCENUMB),
		tuber_Fe_dw_ = num(r2$FeDW),
		tuber_Zn_dw_ = num(r2$ZnDw),
		yield_moisture = 100 - num(r2$DM)
	)
	minr <- minr[!is.na(minr$variety) & !duplicated(minr$variety), ]
	d <- data.frame(
		variety = chr(r1$ACCENUMB),
		treatment = chr(r1$ACCENUMB),
		block_id = chr(r1$BLOCK),
		plot_id = as.character(seq_len(nrow(r1))),
		ttwp_ = num(r1$TTWP),
		mtwp_ = num(r1$MTWP)
	)
	d <- merge(d, minr, by="variety", all.x=TRUE)
	d$crop <- "potato"
	d$yield_part <- "tubers"
	d$irrigated <- TRUE
	d$harvest_date <- "2015-07-15"
	d$country <- "Ethiopia"
	d$adm1 <- "Oromiya"
	d$adm2 <- "Finifine zuria"
	d$adm3 <- "Wolmera"
	d$location <- "Holetta"
	d$elevation <- 2400
	d$latitude <- 8.88333
	d$longitude <- 38.38333
	d$geo_from_source <- TRUE
	d$trial_id <- "Holetta_2015"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d <- d[!is.na(d$variety), ]

	carobiner::write_files(path, meta, d)
}
