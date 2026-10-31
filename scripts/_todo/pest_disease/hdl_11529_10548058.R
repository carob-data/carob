# R script for "carob"
# license: GPL (>=3)

## NOTES
# Dry direct-seeded rice weed-management trials at four SRFSI nodes in 2015. Country and district are the site-sheet header.
# Grain yield on the harvest sheet is t/ha and is stored as kg/ha. Oven-dry weed biomass is g/m2 and is stored as kg/ha. weed_biomass in the vocabulary is fresh weight, so the dry weight is a separate variable.
# A dash or "No-spray" is no herbicide. Herbicide amount is kg of active ingredient per ha when the dose cell has a single gram value. Mixed doses are not summed.
# Pendimethalin, oxadiazon, bispyribac-sodium, pyrazosulfuron, fenoxaprop-p-ethyl, ethoxysulfuron, and halosulfuron-methyl are the active ingredients named in the sheets.

## ISSUES
# The season cell is "Rabi (winter)" while the file names say kharif rice 2015. The season cell is kept.
# No coordinates. Fertilizer is recorded as product grade and grams per plot, without a nutrient summary, and is not summed here.
# Variety was not filled in on the harvest or phenology sheets that were read.
# write_files: missing variables: yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: dry_weed_biomass_kg_ha_. invalid terms: herbicide_product: ethoxysulfuron.

## NEW VARIABLES
# dry_weed_biomass_kg_ha_: oven-dry weed biomass at harvest, kg/ha.

## NOT PROCESSED
# Rainfall, land preparation, irrigation, ponding, pesticide logs, soil analysis, and general notes.
# Quadrat weed counts at 15, 35, and 60 days after seeding, and visual scores.
# Fertilizer applications in g/plot.
# Straw weight and panicle components.


carob_script <- function(path) {

"
8.2-Zero tillage rice weed management learning module- -SRFSI Project-ACIAR-CIMMYT

Dry direct-seeded rice in no-till or tilled soil is an alternate option for transplanting rice but due to weed management constraints, farmers hesitate to adopt the direct seeded rice. To address the weed management problems in dry direct seeded rice, different weed management options trial is designed to convince different stakeholders including farmers, input dealers, extension officers, private sectors, researchers, and policymakers.
"

	uri <- "hdl:11529/10548058"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3,
		data_organization = "CIMMYT",
		publication = NA,
		project = "SRFSI",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield;dry_weed_biomass_kg_ha_",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.6
	)

	header_value <- function(cells, label) {
		i <- which(cells == label)
		if (!length(i)) return(NA_character_)
		rest <- cells[(i[1] + 1):length(cells)]
		rest <- rest[!is.na(rest) & nzchar(rest)]
		if (!length(rest)) return(NA_character_)
		rest[1]
	}
	num <- function(x) {
		if (is.null(x)) return(NA_real_)
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA")] <- NA_character_
		as.numeric(x)
	}
	blank_herb <- function(x) {
		x <- trimws(as.character(x))
		x[is.na(x) | x %in% c("", "-", "No-spray", "no-spray")] <- ""
		x
	}
	products <- function(pre, post) {
		src <- paste(pre, post)
		out <- rep("none", length(src))
		for (i in seq_along(src)) {
			s <- src[i]
			parts <- character(0)
			if (grepl("pendimethalin", s, ignore.case=TRUE)) parts <- c(parts, "pendimethalin")
			if (grepl("oxadiazon", s, ignore.case=TRUE)) parts <- c(parts, "oxadiazon")
			if (grepl("bispyribac", s, ignore.case=TRUE)) parts <- c(parts, "bispyribac-sodium")
			if (grepl("pyrazosulfuron", s, ignore.case=TRUE)) parts <- c(parts, "pyrazosulfuron")
			if (grepl("fenoxaprop", s, ignore.case=TRUE)) parts <- c(parts, "fenoxaprop-p-ethyl")
			if (grepl("ethoxysulfuron", s, ignore.case=TRUE)) parts <- c(parts, "ethoxysulfuron")
			if (grepl("halosulfuron", s, ignore.case=TRUE)) parts <- c(parts, "halosulfuron-methyl")
			if (length(parts)) out[i] <- paste(parts, collapse=";")
		}
		out
	}
	one_rate_kg <- function(x) {
		x <- tolower(as.character(x))
		x[is.na(x)] <- ""
		has <- grepl("[0-9]", x)
		multi <- grepl("\\+", x)
		num <- sub("^[^0-9]*([0-9]+).*", "\\1", x)
		num[!has | multi] <- NA_character_
		as.numeric(num) / 1000
	}
	one <- function(f) {
		si <- carobiner::read.excel(f, sheet="2 - Site information", col_names=FALSE)
		cells <- trimws(as.character(unlist(si[1, ])))
		cells[is.na(cells)] <- ""
		gh <- carobiner::read.excel.hdr(f, sheet="16.WeedBio & cropData (harvest)", skip=4, hdr=2)
		gh <- gh[!is.na(gh$Treatment.No), ]
		pre <- blank_herb(gh$Pre.emergence.herbicide)
		post <- blank_herb(gh$Post.emergence.herbicide)
		prod <- products(pre, post)
		rate_pre <- one_rate_kg(gh$Dose.g.a.i.ha.1.and.application.time)
		rate_post <- one_rate_kg(gh$Dose.g.a.i.ha.1.and.application.time.1)
		rate <- ifelse(!is.na(rate_pre) & is.na(rate_post), rate_pre, ifelse(is.na(rate_pre) & !is.na(rate_post), rate_post, NA_real_))
		rate[prod == "none"] <- 0
		ylab <- ifelse(pre == "" & post == "", "no herbicide", trimws(paste(pre, post)))
		ylab[ylab == ""] <- "no herbicide"
		dry <- num(gh$Oven.dry.weed.biomass.at.harvest.g.m2) * 10
		data.frame(
			country = header_value(cells, "Country:"),
			adm2 = header_value(cells, "District:"),
			location = header_value(cells, "Community/Node:"),
			year = as.character(gh$Year),
			season = ifelse(grepl("rabi|winter", gh$Season, ignore.case=TRUE), "rabi", ifelse(grepl("kharif|monsoon", gh$Season, ignore.case=TRUE), "kharif", tolower(trimws(as.character(gh$Season))))),
			trial_code = as.character(gh$Trial.Code),
			plot_id = as.character(gh$Plot.Id),
			treatment = ylab,
			herbicide_product = prod,
			herbicide_used = prod != "none",
			herbicide_amount = rate,
			crop = "rice",
			yield_part = "grain",
			yield = num(gh$Grain.yield.t.ha) * 1000,
			plant_height = num(gh$Plant.height.cm),
			seed_weight = num(gh$TGW),
			plot_area = num(gh$Plot.size.m2.each.plot),
			dry_weed_biomass_kg_ha_ = dry,
			stress = "weeds"
		)
	}

	d <- do.call(rbind, lapply(ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)], one))
	d$location <- carobiner::fix_name(d$location, "title")
	d$adm2 <- carobiner::fix_name(d$adm2, "title")
	d$trial_id <- paste(d$location, d$year, d$trial_code, sep="_")
	d$trial_code <- NULL
	d$year <- NULL
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
