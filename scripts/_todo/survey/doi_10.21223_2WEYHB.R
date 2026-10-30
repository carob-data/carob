# R script for "carob"
# license: GPL (>=3)

## NOTES
# Trader survey in northern and eastern Uganda. Biofort_crop is OFSP, stored as sweetpotato.
# Sex codes in the dictionary: F female, M male.
# loan_access is "None" or a number. None is stored as 0. The currency is not stated.
# qty_bags_sold_2020 is a bag count for 2020, not a yield in kg/ha.

## ISSUES
# No coordinates. Markets need georeferencing.
# Bag weight is not stated, so bags are not converted to kilograms. "1.5bags" is 1.5 and "1/2 bag" is 0.5. One answer, "less than one bag", has no number and is missing.
# write_files: missing variables: carob_contributor, latitude, longitude. unknown variables: bags_sold_, loan_amount_.
# Loan amounts have no currency.
# Northern Region and Eastern Region are the survey regions in the file, not necessarily GADM names.
# write_files messages follow the test.

## NEW VARIABLES
# bags_sold_: bags of sweetpotato the trader reported selling in 2020. Bag weight is not stated.
# loan_amount_: value from loan_access. "None" is 0. Currency is not stated.

## NOT PROCESSED
# Buz_type, affected_COVID, How affected_COVID, Other_comments: trader business notes, not a field measurement.
# month_data collection on the eastern file: interview month, not a planting date.
# 003 dictionary: definitions already used.


carob_script <- function(path) {

"
Dataset for: Sweetpotato value chain actors in Mbale and Gulu markets, Uganda

112 traders in different markets, in Mbale and Gulu of Norther and Tororo, Bugiri, Busia in the Eastern regions of Uganda, were surveyed about the value chain  Orange-fleshed Sweetpotato (OFSP)
"

	uri <- "doi:10.21223/2WEYHB"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.35
	)

	one <- function(f, zone) {
		r <- carobiner::read.excel(f, na="NA")
		sex <- trimws(as.character(r$Sex_trader))
		sex[sex == "F"] <- "female"
		sex[sex == "M"] <- "male"
		loan <- trimws(as.character(r$loan_access))
		loan[loan == "None"] <- "0"
		bad <- !is.na(loan) & !grepl("^[0-9]+(\\.[0-9]+)?$", loan)
		loan[bad] <- NA_character_
		bags <- trimws(as.character(r$qty_bags_sold_2020))
		bags <- trimws(sub("bags$", "", bags, ignore.case=TRUE))
		bags[bags == "1/2 bag"] <- "0.5"
		bags[bags == "less than one bag"] <- NA_character_
		data.frame(
			hhid = paste(zone, trimws(as.character(r$CODE_trader)), sep="_"),
			country = trimws(as.character(r$Country)),
			adm1 = carobiner::fix_name(r$Region, "title"),
			adm2 = carobiner::fix_name(r$district, "title"),
			location = trimws(as.character(r$Name_market)),
			crop = "sweetpotato",
			age = as.numeric(r$Age_trader),
			sex = sex,
			bags_sold_ = as.numeric(bags),
			loan_amount_ = as.numeric(loan),
			stringsAsFactors = FALSE
		)
	}

	f1 <- ff[basename(ff) == "001_Data_entry_of_OFSP_traders_UG_N.xlsx"]
	f2 <- ff[basename(ff) == "002_Data_entry_of_OFSP_traders_UG_S.xlsx"]
	d <- rbind(one(f1, "N"), one(f2, "E"))
	d$country[d$country %in% c("Uganda", "UG", "uganda")] <- "Uganda"
	d$is_survey <- TRUE
	d$on_farm <- FALSE
	d$geo_from_source <- FALSE
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
