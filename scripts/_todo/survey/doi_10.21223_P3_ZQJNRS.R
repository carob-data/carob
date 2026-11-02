# R script for "carob"
# license: GPL (>=3)

## NOTES
# Marando Bora endline, January-March 2013, plus three interviews dated 2012. Country is Tanzania from geographicCoverage and from the abstract (Lake Zone, Tanzania).
# Region, district, ward, and village are the value labels. There is no latitude or longitude. The elevation column runs from 81 to 1589 and is stored as meters. One record is 81.
# Household size is the number of resident people.

## ISSUES
# No coordinates.
# write_files: missing variables: latitude, longitude.

## NOT PROCESSED
# Project exposure, vine dissemination, consent, and interview clock times on the cover file.
# Sweetpotato production, other crops, sales, labour, groups, credit, food security, shocks, assets, housing, nutrition, and demography files. Several of those are split into part 1 and part 2.
# haconver.dta, sp_conv.dta, the questionnaire workbook, and the dictionary.


carob_script <- function(path) {

"
Replication Data for: Marando Bora Project Endline Survey

The Marando Bora (MB) project was implemented by the International Potato Center (CIP) in collaboration with 12 other partners in the Lake Zone region, Tanzania from October 2009 to June 2012.
The Marando Bora endline survey was conducted from January to March 2013. The objective was to understand dietary diversity of households and children between six(6) to 24 months of age; production and marketing of sweetpotatoes and other crops; vines diffusion and access to quality planting materials; farmers' knowledge about sweetpotato production including diseases, vines conservation, and sweetpotato storage methods; labour-use; membership to farmer associations, access to credit, and access to information; households' food security status, knowledge about vitamin A; nutritional knowledge and dietary habits; frequency of consumption of vitamin A foods; farmers' attitudes and perceptions about sweetpotatoes in general and OFSP in particular; sweetpotato varietal preferences; household's exposure to shocks; and household's asset endowment.
"

	uri <- "doi:10.21223/P3/ZQJNRS"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 20,
		carob_effort = 0.35
	)

	from_labels <- function(x) {
		v <- attr(x, "labels")
		y <- trimws(as.character(x))
		if (is.null(v)) return(y)
		out <- names(v)[match(y, as.character(unname(v)))]
		out
	}
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

	f <- ff[basename(ff) == "mbend_pg1_cover_page.dta"]
	r <- haven::read_dta(f)
	hid <- trimws(as.character(r$hhid))

	d <- data.frame(
		hhid = hid,
		country = "Tanzania",
		adm1 = carobiner::fix_name(plain(from_labels(r$Region)), "title"),
		adm2 = carobiner::fix_name(plain(from_labels(r$District)), "title"),
		adm3 = carobiner::fix_name(plain(from_labels(r$Ward)), "title"),
		location = carobiner::fix_name(plain(from_labels(r$Village)), "title"),
		elevation = as.numeric(r$elevation),
		hh_size = as.numeric(r$HHmembers),
		date = sprintf("%04d-%02d-%02d", as.integer(r$A18C), as.integer(r$A18B), as.integer(r$A18A)),
		trial_id = hid,
		geo_from_source = FALSE,
		on_farm = TRUE,
		is_survey = TRUE
	)

	carobiner::write_files(path, meta, d)
}
