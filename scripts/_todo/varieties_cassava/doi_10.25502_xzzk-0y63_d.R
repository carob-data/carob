# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm cassava clone evaluation at Babessi, Northwest Cameroon. One row per plant per sampling date (9 dates, 1 Sep 2010 through 20 Jul 2011). There is no yield.
# Dates are month/day/year: 10/19/2010 cannot be day-first.
# Coordinates are degrees and decimal minutes (05°58.831, 010°32.542) converted to decimal degrees. Elevation 1175 m is from the file.
# Tot_Nym equals NN_WF + YN_WF + BN_WF and does not include Adult_WF, so the nymph total follows the column names. The dictionary text for Adult_WF ("normal nymphs") and NN_WF ("adult whitefly") is the opposite and was not followed.
# Disease scores are in the long table. CMD severity reaches 10 (a 1-5 scale is the usual IITA score; values were left as recorded).

## ISSUES
# No root or shoot yield in this file.
# Field Size is 2500 on every row and Age is 3-14. The dictionary does not give units (Age may be months after planting, because it changes across the sampling dates). Neither was mapped.
# CMD_Inf codes are h, w, c and 1, with no legend (kept as cmd_infection_type_).
# Sooty_Mould is described as presence/absence but values reach 450. WF_damage is mostly missing (scores 1-2).
# GADM spelling of Northwest / Ngoketunjia was not checked (Nord-Ouest, Ngo-Ketunjia).
# N/P/K, irrigation, planting date and harvest date are not in the file.
# write_files: missing yield (and likely N/P/K, irrigated, planting_date, harvest_date).

## NOT PROCESSED
# Size: "size of the field", unit not stated (constant 2500).
# Age: "age of the plants", unit not stated.
# Tot_Nym: sum of NN_WF, YN_WF and BN_WF.
# SN: row number.
# metadata.csv: column dictionary.

## NEW VARIABLES
# adult_wf_, nn_wf_, yn_wf_, bn_wf_: whitefly counts per plant. See NOTES on the dictionary swap. nn/yn/bn are the three columns that add up to Tot_Nym.
# parasitism_percent_: Perc_PAR, percent parasitism.
# spiralling_whitefly_: SWF, 0/1 presence.
# sooty_mould_: Sooty_Mould as recorded (not only 0/1).
# cgm_count_: cassava green mite count.
# typhlodromalus_aripo_: T_aripo count (0 or 1).
# wf_damage_score_: whitefly damage score.
# cmd_infection_type_: CMD_Inf code.
# plant_number_: plant within replicate.


carob_script <- function(path) {

"
Cassava clonal evaluation in Cameroon, 2011

RTB is a partnership collaboration of five research centers, led by the International Potato Center, with decades of experience in these crops on different continents, including four CGIAR research centers (Bioversity International, the International Center for Tropical Agriculture, the International institute of Tropical Agriculture and the International Potato Center) and the French Agricultural Research Centre for International Development (CIRAD). By working with more than 360 other partners, these five centers mobilize complementary expertise and resources; avoid duplication of efforts; and create synergies to increase the benefits of their research for smallholder farmers, consumers and other stakeholders.
"

	uri <- "doi:10.25502/xzzk-0y63/d"
	group <- "varieties_cassava"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "RTB",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "disease_severity",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.8,
		notes = NA
	)

	f <- ff[basename(ff) == "data-on-babessi-clones-aug11_apollin-fotso.csv"]
	txt <- readLines(f, encoding="latin1", warn=FALSE)
	r <- read.csv(text=txt, stringsAsFactors=FALSE, na.strings=c("", "NA"))

	decmin <- function(x) {
		x <- trimws(x)
		deg <- as.numeric(sub("[^0-9].*$", "", x))
		minutes <- as.numeric(sub("^[0-9]+[^0-9]+", "", x))
		deg + minutes / 60
	}

	d <- data.frame(
		country = "Cameroon",
		adm1 = "Northwest",
		adm2 = "Ngoketunjia",
		location = "Babessi",
		trial_id = "Babessi_2010",
		latitude = decmin(r$Latitude),
		longitude = decmin(r$Longitude),
		elevation = r$Elevation,
		geo_from_source = TRUE,
		date = as.character(as.Date(r$Date, "%m/%d/%Y")),
		variety = r$Variety,
		treatment = r$Variety,
		rep = as.integer(r$Repetition),
		plant_number_ = as.integer(r$Plant),
		plot_id = paste(r$Variety, r$Repetition, r$Plant, sep="_"),
		adult_wf_ = r$Adult_WF,
		nn_wf_ = r$NN_WF,
		yn_wf_ = r$YN_WF,
		bn_wf_ = r$BN_WF,
		parasitism_percent_ = r$Perc_PAR,
		spiralling_whitefly_ = r$SWF,
		sooty_mould_ = r$Sooty_Mould,
		cgm_count_ = r$CGM,
		typhlodromalus_aripo_ = r$T_aripo,
		wf_damage_score_ = r$WF_damage,
		cmd_infection_type_ = r$CMD_Inf,
		crop = "cassava",
		yield_part = "roots",
		on_farm = TRUE,
		is_survey = FALSE,
		record_id = seq_len(nrow(r))
	)

	sev <- data.frame(
		record_id = d$record_id,
		cmd = as.character(r$CMD_Sev),
		cad = as.character(r$CAD_Sev),
		cbb = as.character(r$CBB_Sev)
	)
	long <- reshape(sev, varying=c("cmd", "cad", "cbb"), v.names="disease_severity",
		timevar="disease", times=c("cassava mosaic", "cassava anthracnose", "cassava bacterial blight"),
		direction="long")
	long$id <- NULL
	long <- long[!is.na(long$disease_severity), ]
	long$date <- d$date[match(long$record_id, d$record_id)]

	carobiner::write_files(path, meta, d, long=long)
}
