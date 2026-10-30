# R script for "carob"
# license: GPL (>=3)

## NOTES
# Potato pest and disease records from Peru. The dictionary describes departments, provinces, and districts as the administrative divisions of Peru, and the title states the crop was grown in Peru.
# The screening sheet records foliage severity on 14 dates. Dates are Excel serials and days after planting are printed on the same columns. Planting date is the evaluation date minus that day count; every column gives the same planting date. AUDPC and rAUDPC are the sheet values, not recomputed. The severity scale is not stated, so the readings stay as late_blight_severity_.
# The incidence sheet is one row per sampled plant. Place codes do not match the place names on the geographic sheet, so those coordinates are not copied onto the plants. SFLate Blight is visual foliage severity with no scale in the dictionary (values exceed 100). IFLate Blight is 0 or 1.

## ISSUES
# Geographic sheet places are Quinua, Quichas, Candelaria, and Gochachupan, with degrees-minutes-seconds and no hemisphere letter. Incidence place codes are CC, KL1, KL2, KPP, KT, QA, QC, QE, and QJ. There is no crosswalk, so coordinates are omitted.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variables: place_code_, foliage_late_blight_severity_, late_blight_affected_, tuber_number_, late_blight_severity_. out of bounds: AUDPC (206.5, 4732).

## NEW VARIABLES
# late_blight_severity_: screening-sheet severity at one evaluation. Scale is not stated.
# place_code_: incidence locality code.
# foliage_late_blight_severity_: SFLate Blight as recorded.
# late_blight_affected_: TRUE when IFLate Blight is 1 and FALSE when it is 0.
# tuber_number_: tuber number harvested. This is a count, not a yield.

## NOT PROCESSED
# Early blight, Andean potato weevil, flea beetle, leaf beetle, Rhizoctonia, virus columns, and tuber-rot proportions. The dictionary says visual estimation and does not give a unit.
# Fields_Geographic_locations coordinates, because they cannot be matched to the incidence codes.
# Total plot area in square metres on that sheet, for the same reason.


carob_script <- function(path) {

"
Replication data for: Pests and diseases affecting potato landraces and bred varieties grown in Peru under indigenous farming system

The major pests and diseases were identified and quantified on thirteen potato landraces and three bred varieties cultivated in Peru. Late blight (Phytophthora infestans) was the primary biotic constraint affecting plants before flowering with an average severity of 24%. No other pathogens caused severe foliar disease, but black scurf (Rhizoctonia solani) was relatively common on tubers of some genotypes with incidence ranging from 4.30 to 33.33%. The viruses most generally considered important in potato seed degeneration, PVY and PLRV, were extremely rare, with 1.11 and 0.12 % incidence, respectively. Other viruses considered mild, such as PVX and PVS, were more common, with incidence of 28.23 and 22.29 %, respectively. Potato flea beetle (Epitrix spp.), potato leaf beetle (Diabrotica spp.) and Andean potato weevil (Premnotrypes spp.) were common, with incidence of 28.14, 18.75 and 13.61%, respectively. Potato landraces known as Ishkupuru, Lengua de vaca, Chaulina, Chaulina Tajacaja and Negro cayash were identified as potentially resistant to P. infestans.
"

	uri <- "doi:10.21223/P3/B6ACYT"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		treatment_vars = "variety",
		response_vars = "AUDPC;rAUDPC",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.5
	)

	f <- ff[grepl("incidence.xls$", basename(ff))]
	sh <- readxl::excel_sheets(f)
	raw <- as.data.frame(carobiner::read.excel(f, sheet=sh[3], col_names=FALSE), stringsAsFactors=FALSE)
	txt <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z) | z == "NA"] <- NA_character_
		z
	}
	num <- function(x) {
		z <- txt(x)
		ok <- is.na(z) | grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
		z[!ok] <- NA_character_
		as.numeric(z)
	}

	lab <- txt(unlist(raw[2, ]))
	ev <- which(grepl("^[0-9]+ Ev$", lab))
	dap <- as.integer(num(unlist(raw[4, ev])))
	when <- as.Date(num(unlist(raw[3, ev])), origin="1899-12-30")
	plant <- format(when - dap, "%Y-%m-%d")
	planting <- if (length(unique(plant)) == 1) plant[1] else NA_character_
	audpc_col <- which(lab == "AUDPC")[1]
	raudpc_col <- which(lab == "rAUDPC")[1]

	body <- raw[6:nrow(raw), ]
	variety <- txt(body[[2]])
	keep <- !is.na(variety)
	body <- body[keep, ]
	variety <- variety[keep]
	d1 <- data.frame(
		variety = variety,
		rep = as.integer(num(body[[1]])),
		treatment = variety,
		trial_id = "late_blight_screening",
		planting_date = planting,
		AUDPC = num(body[[audpc_col]]),
		rAUDPC = num(body[[raudpc_col]]),
		country = "Peru",
		crop = "potato",
		pathogen = "Phytophthora infestans",
		geo_from_source = FALSE,
		on_farm = NA,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)
	d1$record_id <- seq_len(nrow(d1))
	longs <- lapply(seq_along(ev), function(i) {
		j <- ev[i]
		data.frame(
			record_id = d1$record_id,
			date = format(when[i], "%Y-%m-%d"),
			DAP = dap[i],
			late_blight_severity_ = num(body[[j]]),
			disease = "potato late blight",
			stringsAsFactors = FALSE
		)
	})
	dlong <- do.call(rbind, longs)
	dlong <- dlong[!is.na(dlong$late_blight_severity_), ]
	dlong$record_id <- as.integer(dlong$record_id)
	dlong$DAP <- as.integer(dlong$DAP)

	inc <- as.data.frame(carobiner::read.excel(f, sheet=sh[2]), stringsAsFactors=FALSE)
	affected <- num(inc[["IFLate Blight"]])
	d2 <- data.frame(
		variety = txt(inc$Variety),
		rep = NA_integer_,
		treatment = txt(inc$Variety),
		trial_id = "field_incidence",
		planting_date = NA_character_,
		AUDPC = NA_real_,
		rAUDPC = NA_real_,
		country = "Peru",
		crop = "potato",
		pathogen = "Phytophthora infestans",
		geo_from_source = FALSE,
		on_farm = TRUE,
		is_survey = FALSE,
		place_code_ = txt(inc[["Place code"]]),
		foliage_late_blight_severity_ = num(inc[["SFLate Blight"]]),
		late_blight_affected_ = ifelse(affected == 1, TRUE, ifelse(affected == 0, FALSE, NA)),
		tuber_number_ = num(inc[["Tuber number harvested"]]),
		stringsAsFactors = FALSE
	)
	d2 <- d2[!is.na(d2$variety), ]
	d1$place_code_ <- NA_character_
	d1$foliage_late_blight_severity_ <- NA_real_
	d1$late_blight_affected_ <- NA
	d1$tuber_number_ <- NA_real_
	d2$record_id <- seq_len(nrow(d2)) + nrow(d1)
	d <- rbind(d1, d2)
	rownames(d) <- NULL
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
