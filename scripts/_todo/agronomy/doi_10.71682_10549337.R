# R script for "carob"
# license: GPL (>=3)

## NOTES
# Split nitrogen on wheat at Irapuato and Pénjamo. Country is Mexico in the geographic coverage.
# The coverage names the cities and does not give coordinates.
# Cycle 1 is the 2016-2017 season and cycle 2 is 2017-2018. planting_date is the first year of that season.
# Treatments 2 and 4 are both labelled 33-33-33. Treatment 2 is 210 kg N/ha and treatment 4 is 300 kg N/ha.
# Treatment 5 nitrogen is the total in Datos sensor for that site, variety, and sensor year (2017 for cycle 1, 2018 for cycle 2).
# RA is used as grain yield. A 3 m2 harvest area makes the values a kg/ha grain yield, not grams per plot.
# The useful plot is 1.5 x 2 m.

## ISSUES
# write_files: missing variables: on_farm, yield_moisture, latitude, longitude, irrigated, K_fertilizer, P_fertilizer, harvest_date.


## NOT PROCESSED
# Biomass, grains per square metre, grains per spike, protein, height, SPAD, and nitrogen-use efficiency.
# The abbreviation of those columns is not defined in the dictionary.
# Principal-component and genotype-by-environment matrices.
# Field randomization map.


carob_script <- function(path) {

"
Impact of nitrogen split application on wheat grain yield and protein content in Conservation Agriculture

Research on the effect of split nitrogen application on wheat grain yield and quality under conservation agriculture. The study was conducted during the 2016-2017 and 2017-2018 fall-winter cycles in the Bajío region of Guanajuato.
"

	uri <- "doi:10.71682/10549337"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment;variety",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "BD-TrigoFuerte-Baj-2024 b.xlsx"]
	r <- carobiner::read.excel(f, sheet="Matriz ANOVA")

	variety <- c("1"="Cisne F2016", "2"="Alondra F2014", "3"="Luminaria F2012")
	location <- c("1"="Irapuato", "2"="Pénjamo")
	season <- c("1"="2016", "2"="2017")
	sensor_year <- c("1"="2017", "2"="2018")
	treat <- c(
		"1"="50-50-0",
		"2"="33-33-33 (210)",
		"3"="30-70-0",
		"4"="33-33-33 (300)",
		"5"="sensor",
		"6"="control"
	)
	n_fixed <- c("1"=210, "2"=210, "3"=210, "4"=300, "5"=NA, "6"=0)

	# Totals from Datos sensor, kg N/ha, for the optical-sensor treatment only.
	sensor <- data.frame(
		location = rep(c("Irapuato", "Pénjamo"), each=6),
		year = rep(c("2017", "2018"), each=3, times=2),
		variety = rep(c("Cisne", "Alondra", "Luminaria"), times=4),
		n_sensor = c(185, 215, 203, 241, 266, 203, 275, 216, 218, 226, 213, 229)
	)

	code <- as.character(r$Tratamiento)
	d <- data.frame(
		trial_id = paste(location[as.character(r$Localidad)], season[as.character(r$Ciclo)], sep="-"),
		country = "Mexico",
		adm1 = "Guanajuato",
		location = location[as.character(r$Localidad)],
		planting_date = season[as.character(r$Ciclo)],
		crop = "wheat",
		yield_part = "grain",
		variety = variety[as.character(r$Genotipo)],
		treatment = unname(treat[code]),
		rep = as.integer(r$Repetición),
		yield = as.numeric(r$RA),
		N_fertilizer = unname(n_fixed[code]),
		plot_area = 3,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	short <- sub(" F.*", "", d$variety)
	sy <- sensor_year[as.character(r$Ciclo)]
	key <- paste(d$location, short, sy)
	skey <- paste(sensor$location, sensor$variety, sensor$year)
	d$N_fertilizer[d$treatment == "sensor"] <- sensor$n_sensor[match(key, skey)][d$treatment == "sensor"]

	carobiner::write_files(path, meta, d)
}
