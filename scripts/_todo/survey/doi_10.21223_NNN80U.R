# R script for "carob"
# license: GPL (>=3)

## NOTES
# Shop survey of fertilizers on sale in potato districts of Rwanda. Rows are products in a shop, not field plots.
# Prices are Rwandan francs per kg as recorded. Composition is the product label, not a rate applied to a crop.
# District and sector come from the availability sheet, joined on Agrodealer_index.

## ISSUES
# No coordinates. Sites need georeferencing. The geographic coverage names Northern and Western Province together, so adm1 is not set per shop.
# write_files: missing carob_contributor, latitude, longitude. unknown variables: fertilizer_type_, product_name_, composition_, manufacturer_, origin_country_, package_quantity_, package_unit_, price_per_kg_, subsidized_price_per_kg_.

## NOT PROCESSED
# 02_Assessment_availability_fertilizers.xlsx columns "Availability of fertilizer1" to "Availability of fertilizer24": the sheet does not name which product each flag is.
# 04_Assessment_fertilizers_ODK survey tool.xlsx: the form, not the responses.

## NEW VARIABLES
# fertilizer_type_, product_name_, composition_, manufacturer_, origin_country_, package_quantity_, package_unit_, price_per_kg_, subsidized_price_per_kg_: product label and shop price. hhid is the agrodealer index.


carob_script <- function(path) {

"
Dataset for: Assessment of fertilizer availability in potato producing areas in Rwanda

An assessment of fertilizer availability in potato producing areas in Rwanda was conducted. The five districts with the highest area under potato cultivation in Rwanda were selected: Nyabihu, Rubavu, Musanze, Burera and Gicumbi, representing > 70% of the annual potato production in Rwanda. Twelve sectors were randomly selected across these districts. Per sector, three agrodealer shops (or less if there are less than 3) were visited to collect data on available fertilizers. Data were collected by an enumerator and entered in an ODK form.
"

	uri <- "doi:10.21223/NNN80U"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.3
	)

	fd <- ff[basename(ff) == "01_Assessment_fertilizers_details.xlsx"]
	fa <- ff[basename(ff) == "02_Assessment_availability_fertilizers.xlsx"]
	p <- carobiner::read.excel(fd, sheet="Fertilizer details")
	a <- carobiner::read.excel(fa, sheet="Availability of fertilizers")
	loc <- data.frame(
		Agrodealer_index = a$Agrodealer_index,
		adm2 = trimws(as.character(a$District)),
		location = trimws(as.character(a$Sector)),
		date = as.character(as.Date(a$Date))
	)
	loc <- loc[!duplicated(loc$Agrodealer_index), ]

	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9.]+$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	d <- data.frame(
		shop_id_ = as.character(p$Agrodealer_index),
		fertilizer_type_ = trimws(as.character(p$Fertilizer)),
		product_name_ = trimws(as.character(p$Commercial_name)),
		composition_ = trimws(as.character(p$Composition)),
		manufacturer_ = trimws(as.character(p$Manufacturer)),
		origin_country_ = trimws(as.character(p$Country_of_origin)),
		package_quantity_ = num(p$Quantity_per_package),
		package_unit_ = trimws(as.character(p$Quantity_unit)),
		price_per_kg_ = num(p$Original_price_per_kg),
		subsidized_price_per_kg_ = num(p$Subsidized_price_per_kg)
	)
	d <- merge(d, loc, by.x="shop_id_", by.y="Agrodealer_index", all.x=TRUE)
	d$hhid <- d$shop_id_
	d$shop_id_ <- NULL
	d$crop <- "potato"
	d$country <- "Rwanda"
	d$trial_id <- paste0("Rwanda_fertilizer_", d$hhid, "_", seq_len(nrow(d)))
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
