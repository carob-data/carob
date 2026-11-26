# R script for "carob"
# license: GPL (>=3)

## NOTES
# Clone-level virus reactions (PVX, PVY, PLRV, PVS in long table), late-blight rAUDPC in Yunnan, China (2015, 2016), and marketable tuber weight at 80, 100 and 120 DAP. Not plot replicates.
# Virus resistance and early bulking trials are assigned to CIP-Lima, Peru (-12.078, -76.946, geo_from_source = FALSE); Yunnan late blight trials are assigned to Yunnan, China (25.04, 102.71, geo_from_source = FALSE).
# Marketable tuber weight (mtwp_) has no unit stated.

## ISSUES
# No calendar planting/harvest dates, yield in kg/ha, or fertilizer rates.
# rAUDPC includes -0.003 (slightly below 0); left as recorded.
# Virus percentage infection in raw data is stored as proportion (0-1); multiplied by 100 for disease_incidence (%).

## NOT PROCESSED
# Virus absorbance and infected-plant counts: the resistance class and the infection percentage are kept.
# SLOPE and the early-bulking label: derived from the three tuber weights.
# Material list pedigree.
# 13394_DataDictionary_ElementDescription.xlsx: documentation or a codebook, not an observation table.
# 13394_DataDictionary_UniqueIdentifier.xlsx: documentation or a codebook, not an observation table.
# 13394_Material_List.xlsx: not read. sheet Material_List: Numeration, Accession_Number, Accession_Name, Accession_Code, Scale_audpc, Family_AcceNumb, Female_AcceNumb, Male_AcceNumb, Population, Cycle, Is_control, Seed_source, ....

## NEW VARIABLES
# resistance_class_: virus resistance reaction class as recorded in the virus resistance sheet.
# mtwp_: marketable tuber weight at 80, 100, and 120 DAP (unit not stated).


carob_script <- function(path) {

"
Dataverse for: Evaluation of CIP advanced clones for late blight and virus resistance and early bulking

This dataset contains the evaluation of the International Potato Center (CIP) advanced clones for late blight and virus resistance and early bulking
"

	uri <- "doi:10.21223/DSRBEV"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "rAUDPC;disease_incidence;mtwp_",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 65,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "13394_Data.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="virus resistance")
	r2 <- carobiner::read.excel(f, sheet="Late blight resistance")
	r3 <- carobiner::read.excel(f, sheet="MTW at 80-100-120 d")

	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9.]+([eE][+-]?[0-9]+)?$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	d1 <- data.frame(
		record_id = as.integer(seq_len(nrow(r1))),
		variety = trimws(as.character(r1$ACCENUMB)),
		treatment = trimws(as.character(r1$ACCENUMB)),
		country = "Peru",
		adm1 = "Lima",
		location = "La Molina",
		latitude = -12.078,
		longitude = -76.946,
		trial_id = "virus"
	)
	vspecs <- list(
		list("potato virus X", "PVX", "Percentage of PVX infection"),
		list("potato virus Y", "PVY", "Percentage of PVY infection"),
		list("potato leafroll virus", "PLRV", "Percentage of PLRV infection"),
		list("potato virus S", "PVS", "Percentage of PVS infection")
	)
	vlong <- do.call(rbind, lapply(seq_along(vspecs), function(i) {
		sp <- vspecs[[i]]
		cls <- trimws(as.character(r1[[sp[[2]]]]))
		cls[!nzchar(cls) | cls == "NA"] <- NA_character_
		inf <- num(r1[[sp[[3]]]])
		# Source values are proportions (0-1); convert to percentage
		if (any(!is.na(inf)) && max(inf, na.rm=TRUE) <= 1) inf <- inf * 100
		keep <- !is.na(cls) | !is.na(inf)
		if (!any(keep)) return(NULL)
		data.frame(
			record_id = d1$record_id[keep],
			order = i,
			disease = sp[[1]],
			disease_incidence = ifelse(is.na(inf[keep]), NA_character_, as.character(round(inf[keep], 2))),
			resistance_class_ = cls[keep]
		)
	}))

	d2 <- data.frame(
		record_id = as.integer(seq_len(nrow(r2)) + nrow(r1)),
		variety = trimws(as.character(r2$ACCENUMB)),
		treatment = trimws(as.character(r2$ACCENUMB)),
		rAUDPC = num(r2$rAUDPC_Yunnan2015),
		country = "China",
		adm1 = "Yunnan",
		location = "Yunnan",
		latitude = 25.04,
		longitude = 102.71,
		planting_date = "2015",
		trial_id = "late_blight_2015"
	)
	d2b <- d2
	d2b$record_id <- as.integer(seq_len(nrow(r2)) + nrow(r1) + nrow(r2))
	d2b$rAUDPC <- num(r2$rAUDPC_Yunnan2016)
	d2b$planting_date <- "2016"
	d2b$trial_id <- "late_blight_2016"

	id <- as.integer(seq_len(nrow(r3)) + nrow(r1) + 2L * nrow(r2))
	d3 <- data.frame(
		record_id = id,
		variety = trimws(as.character(r3$ACCENUMB)),
		treatment = trimws(as.character(r3$ACCENUMB)),
		country = "Peru",
		adm1 = "Lima",
		location = "La Molina",
		latitude = -12.078,
		longitude = -76.946,
		trial_id = "early_bulking"
	)
	wts <- c("MTWP 80 days after planting", "MTWP 100 days after planting", "MTWP 120 days after planting")
	days <- c(80L, 100L, 120L)
	pieces <- lapply(seq_along(days), function(i) {
		data.frame(
			record_id = id,
			DAP = days[i],
			mtwp_ = num(r3[[wts[i]]])
		)
	})
	mlong <- do.call(rbind, pieces)
	mlong <- mlong[!is.na(mlong$mtwp_), ]
	dlong <- carobiner::bindr(vlong, mlong)

	d <- carobiner::bindr(d1, d2, d2b, d3)
	d$crop <- "potato"
	d$geo_from_source <- FALSE
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$stress <- "disease"
	d$pathogen <- NA_character_
	d$pathogen[d$trial_id %in% c("late_blight_2015", "late_blight_2016")] <- "Phytophthora infestans"

	carobiner::write_files(path, meta, d, long=dlong)
}
