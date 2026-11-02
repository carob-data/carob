# R script for "carob"
# license: GPL (>=3)

## NOTES
# International Low-ODAP grasspea nursery IGYT-LO-2016 at El Batán. Biomass is in the Calculation sheet.
# Country is Mexico and the state and city are Estado de Mexico / Texcoco, from geographicCoverage. The station name El Batán is in the dataset title. No coordinates are in the files.
# Fresh yield (kg/ha) is total biomass weight (g) / harvest area (m2) * 10.
# Dry yield is the Yield_Dry column. It matches fresh yield times (1 - subsample moisture). The separate "yld kg /ha" column does not match that calculation and is not used.
# Subsample moisture is a fraction in the sheet and is stored as a percent.
# Entry names, accessions, and origins are the field-book list. Variety is the accession, with the origin when the book gives one.
# Entries 24 (Sel190) and 25 (local check) are the checks named in the field book. A summary label also says Vetch(Check); the field book calls entry 25 a local check, so the crop is kept as grass pea.
# Row spacing 30 cm is the recommended row width in the field book.
# The nursery year is 2016. Harvest was on 28 Apr, 3 May, 18 May, and 22 May 2017, depending on maturity, and is not recorded per plot.

## ISSUES
# No coordinates.
# The four harvest dates are not assigned to plots.
# Fresh aboveground biomass reaches about 34 t/ha. That is what the weights and areas give, so it was left as recorded.
# write_files: missing variables: latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. high crop yield: grass pea: 33656.338028169. contributor:

## NOT PROCESSED
# The "yld kg /ha" column. It is not the dry yield computed from total weight, area, and subsample moisture.
# Field-book columns STAND, DFLR, DMAT, PTHT, YLD, and BYLD are empty.
# Subsample fresh and dry weights, except as used for moisture. Width and length, except as the area already provides.


carob_script <- function(path) {

"
Evaluation of international Low-ODAP grasspea lines for highlands in El Batán, México (2016)

Several international varieties of Low-ODAP grass pea were evaluated. Some of the variables measured were Total biomass weight (g), Fresh weigth of the subsample (g), Dry weight of the subsample (g) and Harvest area (m²). The experiment was conducted in El Batán, México.
"

	uri <- "hdl:11529/10548671"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Grasspea Yield Trial, Low-ODAP, 2016",
		design = "alpha lattice",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_total",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.5
	)

	# Field book, rows 49-73: entry, accession, origin.
	key <- data.frame(
		entry = as.character(1:25),
		variety = c(
			"299 Morocco", "368 Pakistan", "384 Nepal", "243 Turkey", "269 Algeria",
			"303 Morocco", "311 Bangladesh", "659 Bangladesh", "275 Bulgaria", "728 Bangladesh",
			"1118 Bangladesh", "1123 Bangladesh", "56 Germany", "65 Greece", "22 Greece",
			"1095 Bangladesh", "18 Greece", "17 Greece", "25 Greece", "651 Bangladesh",
			"991 Bangladesh", "973 Bangladesh", "bio520", "Sel190", "local check"
		)
	)

	f <- ff[basename(ff) == "DAT-BV105-OI2016-17-Grasspea-BiomassYield.xlsx"]
	r <- carobiner::read.excel(f, sheet="Calculation")
	r <- r[!is.na(r$Entry), ]

	pick <- function(pat) {
		n <- grep(pat, names(r), value=TRUE)
		as.numeric(r[[n[1]]])
	}
	area <- pick("^Area$")
	fresh <- pick("WTotal") / area * 10
	dry <- pick("Yield_Dry")
	moist <- pick("Humidity")
	moist <- ifelse(!is.na(moist) & moist <= 1, moist * 100, moist)

	d <- data.frame(
		country = "Mexico",
		adm1 = "México",
		adm2 = "Texcoco",
		location = "El Batan",
		trial_id = "IGYT-LO-2016",
		rep = as.integer(r$REP),
		variety_code = as.character(as.integer(r$Entry)),
		plot_id = paste(r$REP, as.integer(r$Entry)),
		crop = "grass pea",
		yield_part = "aboveground biomass",
		yield = fresh,
		dmy_total = dry,
		yield_moisture = moist,
		plot_area = area,
		row_spacing = 30,
		planting_date = "2016",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d <- merge(d, key, by.x="variety_code", by.y="entry", sort=FALSE)
	d$variety_code <- as.character(d$variety_code)
	d$treatment <- d$variety
	carobiner::write_files(path, meta, d)
}
