# R script for "carob"
# license: GPL (>=3)

## NOTES
# The file is named Baseline_Endline Survey. The survey column is labeled Baseline = 0 and Endline = 1, so both rounds are kept. Country is Tanzania from geographicCoverage and from the abstract.
# Region, district, ward, and village are the name columns a01a, a02a, a03a, and a04a. Household id is the village number plus the household number. The same household can appear in both rounds, so trial_id adds the round.
# The interview date is a16. One row has day, month, and year components 0, 0, and 2808, and a16 for that row is 2017-08-28.
# A district name is stored with a Windows-1252 apostrophe. It is kept as Waging'ombe.

## ISSUES
# a12 says GPS was obtained for 892 households, but a13a, a13b, a14a, and a14b are empty, and elevation a15 is empty.
# Waging'ombe is the spelling in the file. The district is often spelled Wanging'ombe.
# write_files: missing variables: latitude, longitude. unknown variables: survey_round_.

## NEW VARIABLES
# survey_round_: baseline or endline, from the survey value labels.

## NOT PROCESSED
# The other columns in Baseline_Endline Survey.dta (about 3,000), including nutrition, crops, assets, and knowledge. Quantities are not mapped here.
# Endline Survey Data Dictionary.txt.


carob_script <- function(path) {

"
Dataset for: Viable Sweetpotato Technologies in Africa (VISTA) Endline Survey

Orange-fleshed sweetpotato (OFSP) as a staple food providing necessary body energy in the human diet besides significant amounts of Vitamin A addressing both Vitamin A deficiency (VAD) and body energy needs unlike other sweetpotato types. The Viable Sweetpotato Technologies in Africa (VISTA) – Tanzania project was designed to expand the production and utilization of nutritious OFSP into seven districts in Mbeya, Iringa and Morogoro regions which were part of USAID’s Feed the Future (FTF) zones of influence (ZOI). An End line survey was conducted in August to September 2017 with the major objective of measuring the changes achieved as compared to the baseline survey conducted in October to November 2015 before the project intervention was initiated among the beneficiaries. Key areas of measurement included: knowledge on sweetpotato and OFSP production and farming practices among households; knowledge on Vitamin A and Vitamin A rich foods including OFSP; contribution of OFSP to improved Vitamin A intake among households; and consumption of OFSP and other Vitamin A rich foods by caregivers and children aged 6-59 months old.
"

	uri <- "doi:10.21223/P3/S3HUN0"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 15,
		carob_effort = 0.4
	)

	plain <- function(x) {
		y <- as.character(x)
		y <- vapply(y, function(s) {
			if (is.na(s)) return(NA_character_)
			rb <- charToRaw(s)
			if (any(rb == as.raw(0x92))) {
				rb[rb == as.raw(0x92)] <- charToRaw("'")
				s <- rawToChar(rb)
			}
			if (is.na(iconv(s, "UTF-8", "UTF-8"))) {
				s2 <- iconv(s, from="CP1252", to="UTF-8")
				if (!is.na(s2)) s <- s2
			}
			s
		}, character(1), USE.NAMES=FALSE)
		y <- trimws(y)
		y[!nzchar(y)] <- NA_character_
		y
	}

	f <- ff[basename(ff) == "Baseline_Endline Survey.dta"]
	r <- haven::read_dta(f)
	wave <- as.integer(r$survey)
	# Value labels: Baseline = 0, Endline = 1.
	round <- ifelse(wave == 0, "baseline", ifelse(wave == 1, "endline", NA_character_))
	hid <- paste(trimws(as.character(r$a04)), trimws(as.character(r$a05)), sep="_")

	d <- data.frame(
		hhid = hid,
		survey_round_ = round,
		country = "Tanzania",
		adm1 = carobiner::fix_name(plain(r$a01a), "title"),
		adm2 = carobiner::fix_name(plain(r$a02a), "title"),
		adm3 = carobiner::fix_name(plain(r$a03a), "title"),
		location = carobiner::fix_name(plain(r$a04a), "title"),
		date = as.character(as.Date(r$a16)),
		trial_id = paste(hid, round, sep="_"),
		geo_from_source = FALSE,
		on_farm = TRUE,
		is_survey = TRUE
	)

	carobiner::write_files(path, meta, d)
}
