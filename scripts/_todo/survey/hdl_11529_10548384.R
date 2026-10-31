# R script for "carob"
# license: GPL (>=3)

## NOTES
# SRFSI household plot records for Bangladesh, Bihar, West Bengal, and Nepal. Country is not a column. Bangladesh and Nepal are the workbook names. Bihar and West Bengal are the workbook names, and the country is set to India.
# Bihar, West Bengal, and Nepal yield columns that say per hectare or kg per ha are stored as kg/ha. Where a per-hectare column and a total-kg column both exist, the per-hectare column is used.
# Bangladesh plot size is in decimals. Crop areas on that plot sheet do not repeat the unit. They are treated as decimals (1 decimal = 1/100 acre = 0.004046863 ha) and yield (kg) is divided by that area.
# Monsoon rice is season kharif, wheat is rabi, and winter paddy is boro.

## ISSUES
# Bangladesh crop-area unit is inferred from the plot-size header, not from the crop-area header.
# Pulses are not identified to species. The vocabulary term pulse is used.
# No coordinates.
# Summer paddy on one Bangladesh plot is 75308 kg/ha because the harvested kilograms sit on a very small area. The median monsoon-rice yield is about 4200 kg/ha, so the decimal conversion is kept and the high value is kept.
# read.excel warns on three cells in the Bangladesh workbook: '1-1.5', '(Plot 2)', and '(Plot 4)'. Those cells are not used as numbers.
# write_files: missing variables: latitude, longitude. high crop yield: rice: 75308.184329913. duplicates: duplicate records detected.

## NEW VARIABLES
# None.

## NOT PROCESSED
# Household roster, migration, land tenure, pump hours, and vegetables, jute, spices, and fallow area.
# Coded variety columns that store 1, 2, or 3.
# Irrigation source flags.


carob_script <- function(path) {

"
10.1-Socioeconomic survey data-SRFSI Project-ACIAR-CIMMYT

Under SRFSI project objective 1, various socio-economic surveys to understand farmers’ socio-cultural settings, gender roles, issues and constraints, constraints to surface and groundwater availability, market structure and dynamics, constraints to machinery availability, existence and role of local and regional government and non-government organization partners in technology generation and dissemination, etc. were conducted using focus group discussions (FGDs); literature review, and formal and informal interviews and questionnaire.
"

	uri <- "hdl:11529/10548384"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "SRFSI",
		design = NA,
		data_type = "survey",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.8
	)

	num <- function(x) {
		if (is.null(x)) return(NA_real_)
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA")] <- NA_character_
		x[grepl("[A-Za-z]", x)] <- NA_character_
		x[grepl("[0-9][ ]*-[ ]*[0-9]", x)] <- NA_character_
		as.numeric(x)
	}
	part_of <- function(crop) {
		part <- rep("seed", length(crop))
		part[crop %in% c("rice", "wheat", "maize")] <- "grain"
		part[crop == "tobacco"] <- "leaves"
		part
	}
	blank <- function(x) {
		x <- trimws(as.character(x))
		x[is.na(x) | !nzchar(x) | x %in% c("NA", "0")] <- NA_character_
		x
	}

	# Bangladesh: yield is kg and area is treated as decimals.
	f <- ff[grepl("Bangladesh", basename(ff))]
	b <- carobiner::read.excel(f, sheet="Plots")
	dec_ha <- 0.4046863 / 100
	bang <- function(area, yld, crop, season) {
		a <- num(b[[area]])
		y <- num(b[[yld]])
		ha <- a * dec_ha
		out <- ifelse(!is.na(y) & !is.na(ha) & ha > 0, y / ha, NA_real_)
		data.frame(
			hhid = blank(b$Respondent),
			adm2 = blank(b$District),
			location = blank(b$Village),
			plot_id = blank(b[["plot no"]]),
			country = "Bangladesh",
			adm1 = NA_character_,
			crop = crop,
			season = season,
			yield = out,
			variety = NA_character_,
			as_kg = TRUE
		)
	}
	d1 <- rbind(
		bang("Monsoon Rice area cultivated monsoon rice", "Monsoon Rice yield of rice (kg)", "rice", "kharif"),
		bang("Wheat area cultivated wheat", "Wheat yield of wheat", "wheat", "rabi"),
		bang("Maize area cultivated maize", "Maize yield of maize", "maize", NA_character_),
		bang("Pulses area cultivated pulses", "Pulses yield of pulses", "pulse", NA_character_),
		bang("Tobacco area cultivated Tobacco", "Tobacco yield of Tobacco", "tobacco", NA_character_),
		bang("Winter Paddy area cultivated winter paddy", "Winter Paddy yield of winter  paddy", "rice", "boro"),
		bang("Summer paddy area cultivated summer paddy", "Summer paddy yield of summer paddy", "rice", NA_character_)
	)

	f <- ff[grepl("Bihar", basename(ff))]
	h <- carobiner::read.excel(f, sheet="plot")
	bih <- function(yld, crop, season, variety=NULL, as_kg=TRUE) {
		data.frame(
			hhid = blank(h$respondentno),
			adm2 = blank(h$Nameofdistricts),
			location = blank(h$Nodes),
			plot_id = NA_character_,
			country = "India",
			adm1 = "Bihar",
			crop = crop,
			season = season,
			yield = num(h[[yld]]),
			variety = if (is.null(variety)) NA_character_ else blank(h[[variety]]),
			as_kg = as_kg
		)
	}
	d2 <- rbind(
		bih("Yieldkgperhectare_MonsoonRice", "rice", "kharif", "varietyofrice", TRUE),
		bih("YieldKgperHectare_Wheat", "wheat", "rabi", "varietyofwheat", TRUE),
		bih("yieldofmaizeperhectare", "maize", NA_character_, NULL, FALSE),
		bih("yieldofpulsesperhectare", "pulse", NA_character_, "varietyofpulses", FALSE),
		bih("yieldofsummerpaddyperhectare", "rice", NA_character_, "varietyofsummmerpaddy", FALSE)
	)

	f <- ff[grepl("Westbengal", basename(ff), ignore.case=TRUE)]
	w <- carobiner::read.excel(f, sheet="plot")
	wb <- function(yld, crop, season, variety=NULL, as_kg=TRUE) {
		data.frame(
			hhid = blank(w$respondentno),
			adm2 = blank(w$Nameofdistricts),
			location = blank(w$Nodes),
			plot_id = blank(w$plotno),
			country = "India",
			adm1 = "West Bengal",
			crop = crop,
			season = season,
			yield = num(w[[yld]]),
			variety = if (is.null(variety)) NA_character_ else blank(w[[variety]]),
			as_kg = as_kg
		)
	}
	d3 <- rbind(
		wb("yieldofmonsoonriceperhectarekg", "rice", "kharif", "varietyofrice1kanchimansulirada1", TRUE),
		wb("yieldofwheatperhectare", "wheat", "rabi", "varietyofwheat", FALSE),
		wb("yieldofmaizeperhectare", "maize", NA_character_, NULL, FALSE),
		wb("yieldofpulsesperhectare", "pulse", NA_character_, "varietyofpulses", FALSE),
		wb("yieldofsummerpaddyperhectare", "rice", NA_character_, "varietyofsummmerpaddy", FALSE)
	)

	f <- ff[grepl("Nepal", basename(ff))]
	n <- carobiner::read.excel(f, sheet="plot")
	nep <- function(yld, crop, season, as_kg) {
		data.frame(
			hhid = blank(n$form_no),
			adm2 = blank(n$Districts),
			location = blank(n$Nodes),
			plot_id = NA_character_,
			country = "Nepal",
			adm1 = NA_character_,
			crop = crop,
			season = season,
			yield = num(n[[yld]]),
			variety = NA_character_,
			as_kg = as_kg
		)
	}
	d4 <- rbind(
		nep("yieldofricekgperha", "rice", "kharif", TRUE),
		nep("Yieldofwheatkgperha", "wheat", "rabi", TRUE),
		nep("yield_pulses_ha", "pulse", "rabi", FALSE)
	)

	d <- rbind(d1, d2, d3, d4)
	d <- d[!is.na(d$yield) & !is.na(d$hhid), ]
	# Columns that do not say kg are t/ha when the positive values are below 40.
	unk <- !d$as_kg & !is.na(d$yield) & d$yield > 0
	if (any(unk) && median(d$yield[unk]) < 40) d$yield[!d$as_kg] <- d$yield[!d$as_kg] * 1000
	d$as_kg <- NULL
	d$yield_part <- part_of(d$crop)
	d$trial_id <- d$hhid
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE
	d$adm1 <- carobiner::fix_name(d$adm1, "title")
	d$adm2 <- carobiner::fix_name(d$adm2, "title")
	d$location <- carobiner::fix_name(d$location, "title")
	d$variety <- carobiner::fix_name(d$variety, "title")

	carobiner::write_files(path, meta, d)
}
