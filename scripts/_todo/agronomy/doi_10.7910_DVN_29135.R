# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI Genebank accessions screened for yield under drought and well-watered conditions, 2006-2009 dry seasons.
# Data are entry MEANS (not plot data) from the five sheets of "GBank_Data.xlsx":
#   06DS_Lc: 2006DS lowland control, non-replicated; 06DS_U: 2006DS upland ("URV2a&b combined");
#   07_DSU: 2007DS upland, control and stress blocks; 08DS: trt "c" and "r"; 09DS: control and stress blocks.
# Units from sheet "Parameter_definition": GY14 = grain yield at 14% MC (kg/ha); TDMY = total above-ground
# dry matter (kg/ha); HI = %; Ht = plant height at maturity (cm); DTF = days to flowering.
# Associated paper (found by search, same title and authors): Torres et al. 2013, Field Crops Res. 147:12-22.

## ISSUES
# The paper is paywalled and was not read: site (IRRI, Los Banos assumed from authors), upland/lowland
# environment of 08DS and 09DS, the meaning of 08DS trt "r" (assumed drought stress) and the stress type of
# 06DS_U (control or stress?) need to be checked in the Methods. Planting dates, fertilizer and design also.
# No coordinates in the data; georeference IRRI experiment station, Los Banos, Laguna.
# Standard errors per entry (SE columns) are not mapped. 09DS yield frequency table not mapped (summary).
# 06DS_U has negative TDMY and HI values for 4 entries (e.g. R 900), and many 0 yields (entries that did not
#   flower/mature by 150 DAS per the sheet note); left as recorded.
# "Unnamed" accession names kept. No IRGC accession numbers given, only entry numbers (kept as variety_code).
# 06DS_Lc entry 134 has no name (variety NA); 06DS_U treatment unknown (stress NA).
# write_files remaining: missing longitude/latitude/geo_from_source, harvest date, N/P/K fertilizer; NA yield;
#   out of bounds dmy_total and harvest_index (negative values); missing values in treatment vars variety, stress.


carob_script <- function(path) {

"
Screening of rice Genebank germplasm for yield and selection of new drought tolerance donors.

N/A
"

	uri <- "doi:10.7910/DVN/29135"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = "doi:10.1016/j.fcr.2013.03.016",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;stress",
		response_vars = "yield;dmy_total;harvest_index;flowering_days;plant_height",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 60,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "GBank_Data.xlsx"]

	get_block <- function(sheet, skip, cols, trial, trt) {
		r <- carobiner::read.excel(f, sheet=sheet, col_names=FALSE, na=c("", "no data"))
		r <- r[-(1:skip), ]
		r <- r[!is.na(r[[cols["entry"]]]), ]
		v <- function(x) if (is.na(cols[x])) NA else as.numeric(r[[cols[x]]])
		data.frame(
			trial_id = trial,
			treatment = if (is.na(cols["trt"])) trt else r[[cols["trt"]]],
			variety_code = as.character(r[[cols["entry"]]]),
			variety = trimws(r[[cols["name"]]]),
			flowering_days = v("DTF"),
			plant_height = v("Ht"),
			yield = v("GY"),
			dmy_total = v("TDMY"),
			harvest_index = v("HI")
		)
	}

	d1 <- get_block("06DS_Lc", 2, c(entry=1, name=2, DTF=3, Ht=4, GY=5, TDMY=6, HI=7, trt=NA), "2006DS_lowland", "control")
	d2 <- get_block("06DS_U", 7, c(entry=1, name=2, DTF=4, Ht=5, GY=6, TDMY=7, HI=8, trt=NA), "2006DS_upland", NA)
	d3 <- get_block("07_DSU", 3, c(entry=1, name=2, trt=3, DTF=4, Ht=NA, GY=5, TDMY=6, HI=7), "2007DS_upland")
	d4 <- get_block("07_DSU", 3, c(entry=14, name=15, trt=16, DTF=17, Ht=NA, GY=18, TDMY=19, HI=20), "2007DS_upland")
	d5 <- get_block("08DS", 2, c(trt=1, entry=2, name=3, DTF=4, Ht=5, GY=6, TDMY=7, HI=8), "2008DS")
	d6 <- get_block("09DS", 3, c(entry=1, name=2, Ht=3, DTF=4, TDMY=5, GY=6, HI=NA, trt=NA), "2009DS", "control")
	d7 <- get_block("09DS", 3, c(entry=1, name=2, Ht=13, DTF=14, TDMY=15, GY=16, HI=NA, trt=NA), "2009DS", "stress")
	d <- rbind(d1, d2, d3, d4, d5, d6, d7)

	# 08DS uses "c" and "r"; "r" assumed to be (reproductive-stage) drought stress
	d$treatment[d$treatment == "c"] <- "control"
	d$treatment[d$treatment == "r"] <- "stress"
	d$stress <- ifelse(d$treatment == "stress", "drought", ifelse(d$treatment == "control", "none", NA))
	d$trial_id <- paste0(d$trial_id, "_", d$treatment)

	d$season <- "dry"
	d$planting_date <- substr(d$trial_id, 1, 4)
	d$flooded <- ifelse(grepl("upland", d$trial_id), FALSE, ifelse(grepl("lowland", d$trial_id), TRUE, NA))
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA

	d$country <- "Philippines"
	d$adm1 <- "Laguna"
	d$location <- "IRRI, Los Banos"

	carobiner::write_files(path, meta, d)
}
