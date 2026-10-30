# R script for "carob"
# license: GPL (>=3)

## NOTES
# Seller rows from sheets "2014 All Data" and "2013 All Data".
# Sale_Date is an Excel serial (origin 1899-12-30). 41845 is 2014-07-25, inside the April-August 2014 season named in the abstract.
# Bundles are vine bundles, not a yield in kg/ha.
# Crop is sweetpotato. Country is Uganda, from the title (Gulu region).

## ISSUES
# No coordinates.
# No fertilizer and no tuber yield.
# Some seller ages are blank. Some transactions are identical on the columns kept here.
# write_files: missing variables: carob_contributor; geo_from_source, latitude, longitude.
# write_files: unknown variables: bundles_sold_, bundle_price_.
# write_files: duplicates: duplicate records detected.

## NOT PROCESSED
# Customer village, buyer type, selling point, and distance travelled. The row is the seller's transaction.
# Number of small bundles, kept out because the unit relative to BundlesSold is not defined.

## NEW VARIABLES
# bundles_sold_: vine bundles sold
# bundle_price_: selling price per big bundle, currency not named in the sheet

carob_script <- function(path) {

"
Dataset for: Sweet potato vine transactions in Northern Uganda collected in 2013 and 2014

The goal of this study was to assess sweet potato seed (vine) transactions and associated actors in the Gulu region of northern Uganda. Sellers and multipliers were surveyed for all vine sales that occurred during the growing season (April-August) in 2013 and 2014. Transactions include seller location, seller type, buyer location, buyer type, point of sale, variety, volume, and cost. This work will help to better model and understand key actors in this system, with methods developed being transferable to other seed systems.
"

	uri <- "doi:10.21223/P3/6OEHTR"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=6,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "bundles_sold_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "Transaction Data_2013_2014.xlsx"]
	one <- function(sheet) {
		r <- carobiner::read.excel(f, sheet=sheet)
		serial <- as.numeric(r$Sale_Date)
		age <- trimws(as.character(r$Seller_Age))
		age[!nzchar(age)] <- NA_character_
		data.frame(
			hhid = trimws(as.character(r$Seller_ID)),
			age = as.numeric(age),
			location = trimws(as.character(r$Seller_Village)),
			adm3 = trimws(as.character(r$Seller_Parish)),
			adm2 = trimws(as.character(r$Seller_SubCounty)),
			date = as.character(as.Date(serial, origin="1899-12-30")),
			variety = trimws(as.character(r$Variety)),
			bundles_sold_ = as.numeric(r$BundlesSold),
			bundle_price_ = as.numeric(r[["Selling Price /Bundle(big Size)"]]),
			stringsAsFactors = FALSE
		)
	}
	d <- carobiner::bindr(one("2013 All Data"), one("2014 All Data"))
	d$country <- "Uganda"
	d$adm1 <- "Northern"
	d$crop <- "sweetpotato"
	d$trial_id <- substr(d$date, 1, 4)
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- NA

	carobiner::write_files(path, meta, d)
}
