# R script for "carob"
# license: GPL (>=3)

## NOTES
# End-line survey of 89 Marando Bora decentralised sweetpotato vine multipliers in the Lake Zone, Tanzania, interviewed in February-March 2013.
# Respondent code is unique. Region, district, and village are names. Sex is the respondent (MAN/WOMAN in the file; the dictionary codes 0 = woman and 1 = man).
# Latitude, longitude, and elevation are the missing codes 99, 999.999, and 9999, so they are not used. Irrigation is YES/NO.

## ISSUES
# No usable coordinates.
# missing variables: latitude, longitude

## NOT PROCESSED
# Ward, partner, group officers, vine beds and sales by variety, inspection, and the other technology questions in MB_DVM_dataset.xlsx.
# MB_DVM_questionnaire.xlsx and MB_DVM_data_dictionary.docx.


carob_script <- function(path) {

"
Dataset for: Replication Data for: Marando Bora Decentralised Vine Multiplier Study (Tanzania)

The implementation of the large- scale planting material dissemination component, Marando Bora (better vines) aimed to develop and test cost effective sweetpotato variety distribution models at scale. The survey of decentralised vine multipliers (DVMs) was conducted as part of the Marando Bora project end line survey. All 89 DVMs were interviewed by one data collector, February-March 2013.
"

	uri <- "doi:10.21223/P3/BVBCD6"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 20,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "MB_DVM_dataset.xlsx"]
	r <- carobiner::read.excel(f)
	sex <- trimws(as.character(r$RES_SEX))
	sex <- c("MAN" = "male", "WOMAN" = "female")[sex]
	irr <- trimws(as.character(r$IRRIGATION))
	irr[is.na(irr)] <- ""
	irr[irr == "NA"] <- ""
	irr[!nzchar(irr)] <- NA_character_
	irr <- c("YES" = TRUE, "NO" = FALSE)[irr]
	when <- as.Date(paste(r$YEAR_INT, r$MONTH_INT, r$DAY_INT, sep = "-"))

	d <- data.frame(
		trial_id = "Marando_Bora_DVM_2013",
		hhid = trimws(as.character(r$RES_CD)),
		country = "Tanzania",
		adm1 = carobiner::fix_name(as.character(r$REG_NAME), "title"),
		adm2 = carobiner::fix_name(as.character(r$DIST_NAME), "title"),
		location = carobiner::fix_name(as.character(r$VILL_NAME), "title"),
		crop = "sweetpotato",
		sex = unname(sex),
		irrigated = unname(irr),
		date = as.character(when),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)
	geo <- data.frame(
		adm2 = c("Biharamulo", "Bukombe", "Bunda", "Chato", "Geita", "Ilemela",
		         "Magu", "Misungwi", "Muleba", "Musoma", "Nyamagana", "Sengerema", "Ukerewe"),
		latitude = c(-2.6319, -3.4833, -2.0500, -2.6378, -2.8725, -2.4500,
		             -2.5896, -2.8500, -1.8400, -1.5000, -2.5164, -2.6419, -2.0333),
		longitude = c(31.3089, 31.8333, 33.8667, 31.7669, 32.2295, 32.9167,
		              33.4381, 33.0833, 31.6544, 33.8000, 32.9175, 32.6225, 33.0167)
	)
	m <- match(d$adm2, geo$adm2)
	d$latitude <- geo$latitude[m]
	d$longitude <- geo$longitude[m]

	carobiner::write_files(path, meta, d)
}
