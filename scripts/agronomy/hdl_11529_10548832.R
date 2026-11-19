# R script for "carob"
# license: GPL (>=3)

## NOTES
# Paper and supplement (doi:10.1016/j.agee.2021.107812) give coordinates, systems, fertilizer, weed control, maize population and dates; its mean yields match the data.
# Six on-station long-term CA trials (RCBD, 4 reps; HRS 5 from 2007); one row per maize plot and season.
# "Season" is the harvest year (paper 2.2, Table S3).

## ISSUES
# Repeated years kept: DTC 2017 = 2018 and CRS 2009 = 2012 have identical yields (the paper also gives DTC DS-MG 4.0 t/ha in both). Drop one year of each?
# MFTC 2012-2019 has a second CP-MCt set (plot_id suffix "_b"), probably the DS maize-sunnhemp rotation that started in 2012. Relabel?
# "Biomass" is below grain in 56% of rows, so it is not total biomass (probably stover); kept as maize_biomass_ (t/ha assumed).
# Soil and rain values look partly copied or rescaled: soil N/P/K repeated across sites; OrgC 6-11 labelled "%" (taken as g/kg); rainfall in multiples of 100/33 at CRS, HRS, MFTC.
# Other data issues, left as reported: CRS 2011 plots 15 and 17 used twice; one CRS 2010 plot missing; 4 MFTC 2019 plots with zero yield; two CRS seasons with unusual sowing-to-harvest times.
# Irrigation and yield moisture basis not reported; left out.

## NEW VARIABLES
# maize_biomass_: maize "aboveground biomass" as reported, probably stover (kg/ha)
# biochar_used_: biochar applied (DS-MBio at DTC)

## NOT PROCESSED
# Table S1: legume and companion crop populations (no data for those crops)
# Table S2: glyphosate 2.5 l/ha product (concentration unknown)
# biochar rate, fertilizer products and maize variety names: not reported


carob_script <- function(path) {

"
Short-term yield gains or long-term sustainability? A synthesis of conservation agriculture (CA) long-term experiments in Southern Africa

Grain yield data collected from Conservation Agriculture (CA) systems across experiments  of varying experimental duration, established in trial locations of Malawi, Mozambique, Zambia, and Zimbabwe under an increasingly variable climate. Data contains different agro-environmental yield response moderators such as type of crop diversifcation and amount of rainfall and aims to identify cropping systems that may provide both short-term gains and longer-term sustainability.
"

	uri <- "hdl:11529/10548832"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = "doi:10.1016/j.agee.2021.107812",
		project = NA,
		design = "RCBD (4 reps; HRS 5 from 2007)",
		data_type = "experiment",
		treatment_vars = "land_prep_method;planting_implement;residue_prevcrop_used;crop_rotation;intercrops;biochar_used_",
		response_vars = "yield;maize_biomass_",
		notes = NA,
		carob_contributor = "Stella Muthoni",
		carob_LLM = "Claude Opus 5.5 (merged from 3 drafts)",
		carob_date = "2026-11-19",
		carob_completion = 90,
		carob_effort = 1                
	)

	f1 <- ff[basename(ff) == "DATA SA 2005 to 2019.xls"]

	r1a <- carobiner::read.excel(f1, sheet="Variable description")   # codebook: variables, system and location codes
	r1b <- carobiner::read.excel(f1, sheet="Raw Data")               # plot-season data: system, rain, soil, biomass, grain

## d1b from r1b

	# plot numbers are not unique in CRS 2011 and MFTC 2012-2019 (CP-MCt twice)
	pid <- paste(r1b$Location, r1b$Season, r1b$Plot, sep="_")
	pid <- paste0(pid, ifelse(duplicated(pid), "_b", ""))

	d1b <- data.frame(
		Location = r1b$Location,
		harvest_year = r1b$Season,             # harvest year of the season (Table S3)
		treatment = r1b$System,
		rep = as.integer(r1b$Rep),
		plot_id = pid,
		trial_id = paste(r1b$Location, r1b$Season, sep="_"),
		rain = r1b$Rainfall,                   # mm per season; period not stated
		soil_N = r1b$Nitrogen / 1.1,           # kg/ha -> mg/kg, approximation (AGENTS.md)
		soil_P = r1b$Phosphorus,               # mg/kg
		soil_K = r1b$Potassium,                # mg/kg
		soil_clay = r1b$Clay,
		soil_sand = r1b$Sand,
		soil_SOC = r1b$OrgC / 10,              # labelled %, values 6-11: assumed g/kg
		maize_biomass_ = r1b$Biomass * 1000,   # t/ha -> kg/ha (unit assumed)
		yield = r1b$Grain * 1000,              # t/ha -> kg/ha (paper means in t/ha match)
		crop = "maize",
		yield_part = "grain",
		on_farm = FALSE,
		is_survey = FALSE,
		N_splits = 2L                          # basal + topdressing (Table S2)
	)

	# cropping systems (codebook, paper Table 2 and Fig. 4, supplement system descriptions)
	# CP land preparation differs by site, and some companion crops change by year; fixed after merging
	# codebook calls DS-MSfC maize-sunflower-cotton; the paper (2.2, Fig. 3 legend) says sunflower-legume (beans, then cowpea)
	sys <- data.frame(
		treatment = c("BA", "CP", "CP2", "CP-MCt", "DiS", "DiS-M+C", "DiS-M+Mp", "DiS-M+Pp", "DiS-MC", "DS", "DS-M+C", "DS-M+Pp", "DS-MBio", "DS-MC", "DS-MCt", "DS-MCtS", "DS-MG", "DS-MSf", "DS-MSfC", "DS-MSy", "JP", "RI", "RI-M+C"),
		land_prep_method = c("basins", "ploughing", "ridge tillage", "ploughing", rep("none", 17), "ripping", "ripping"),
		land_prep_implement = c("hoe", "mouldboard plough", "hoe", "mouldboard plough", rep("none", 17), "ripper", "ripper"),
		planting_implement = c(rep("manual", 4), rep("dibbling stick", 5), rep("direct seeder", 11), "jab planter", "manual", "manual"),
		residue_prevcrop_used = c(TRUE, FALSE, FALSE, FALSE, rep(TRUE, 19)),
		crop_rotation = c("maize", "maize", "maize", "maize;cotton", "maize", "maize", "maize", "maize", "maize;cowpea", "maize", "maize", "maize", "maize", "maize;cowpea", "maize;cotton", "maize;cotton;crotalaria", "maize;groundnut", "maize;sunflower", "maize;sunflower;cowpea", "maize;soybean", "maize", "maize", "maize"),
		intercrops = c(rep("none", 5), "cowpea", "velvet bean", "pigeon pea", "none", "none", "cowpea", "pigeon pea", rep("none", 10), "cowpea"),
		biochar_used_ = c(rep(FALSE, 12), TRUE, rep(FALSE, 10))
	)

	# sites: paper Table 1 (coordinates, elevation, soils) and Table S2 (fertilizer, weed control)
	site <- data.frame(
		Location = c("HRS", "MFTC", "CRS", "SRS", "DTC", "MRS"),
		location = c("Henderson Research Station", "Monze Farmer Training Centre", "Chitedze Research Station", "Sussundenga Research Station", "Domboshawa Training Centre", "Msekera Research Station"),
		country = c("Zimbabwe", "Zambia", "Malawi", "Mozambique", "Zimbabwe", "Zambia"),
		adm1 = c("Mashonaland Central", "Southern", "Lilongwe", "Manica", "Mashonaland East", "Eastern"),
		latitude = c(-17.5727, -16.2402, -13.9732, -19.3169, -17.6091, -13.6450),
		longitude = c(30.9874, 27.4414, 33.6540, 33.2416, 31.1337, 32.5585),
		elevation = c(1136, 1108, 1145, 608, 1543, 1018),
		soil_texture = c("loamy sand;sand", rep("sandy loam", 5)),
		soil_type = c("Arenosols and Luvisols", "Chromic Lixisols", "Ferruginous Latosols", "Haplic Lixisols", "Rhodic Lixisols", "Luvisols"),
		N_fertilizer = c(12+69, 17+92, 23+69, 12+92, 12+69, 17+92),         # basal + topdressing, kg N/ha
		P_fertilizer = c(23, 33, 21, 23, 23, 33) / 2.29,                    # P2O5 -> P
		K_fertilizer = c(12, 17, 0, 12, 12, 17) / 1.2051,                   # K2O -> K
		herbicide_product = c("none", "glyphosate", "glyphosate", "glyphosate", "none", "glyphosate"),
		weeding_method = c("manual", NA, NA, NA, "manual", NA),
		plant_density = c(44444, 44444, 53000, 44444, 44444, 44444)         # target plants/ha (paper 2.2, Table S1)
	)

	# maize sowing and harvest dates, Table S3 (dd/mm/yyyy in source)
	# sowing year typos fixed, as the source date falls in the previous season, which has its own sowing date: SRS 02/01/2006 -> 2007, CRS 05/01/2014 -> 2015, MRS 02/01/2014 -> 2015
	dates <- data.frame(
		Location = rep(c("HRS", "MFTC", "CRS", "SRS", "DTC", "MRS"), c(15, 14, 12, 9, 10, 8)),
		harvest_year = c(2005:2019, 2006:2019, 2008:2019, 2007:2015, 2010:2019, 2012:2019),
		planting_date = c(
			"2004-12-10", "2005-11-24", "2006-11-27", "2007-11-29", "2008-11-17", "2009-11-23", "2010-11-29", "2011-11-24", "2012-11-06", "2013-11-25", "2014-12-11", "2015-12-21", "2016-11-28", "2017-11-20", "2018-11-26",
			"2005-12-01", "2006-11-23", "2007-11-20", "2008-11-17", "2009-11-19", "2010-11-30", "2011-12-12", "2012-11-28", "2013-11-26", "2014-12-11", "2015-12-21", "2016-11-22", "2017-11-12", "2018-11-29",
			"2007-12-22", "2008-11-25", "2009-12-21", "2010-12-09", "2011-12-31", "2012-11-12", "2013-12-16", "2015-01-05", "2015-12-28", "2016-12-08", "2017-11-28", "2018-12-11",
			"2007-01-02", "2007-11-27", "2008-12-10", "2009-11-22", "2010-11-23", "2011-11-26", "2012-11-26", "2013-12-14", "2014-12-13",
			"2009-11-30", "2010-11-12", "2011-11-06", "2012-12-06", "2013-12-03", "2014-12-10", "2015-12-13", "2016-11-15", "2017-11-17", "2018-11-27",
			"2011-11-30", "2012-12-13", "2013-12-24", "2015-01-02", "2015-12-21", "2016-11-30", "2017-12-05", "2018-12-07"),
		harvest_date = c(
			"2005-05-10", "2006-04-21", "2007-04-20", "2008-04-16", "2009-03-30", "2010-04-08", "2011-04-05", "2012-05-03", "2013-04-04", "2014-04-09", "2015-04-30", "2016-05-04", "2017-04-25", "2018-04-16", "2019-04-24",
			"2006-04-04", "2007-03-29", "2008-04-09", "2009-04-15", "2010-04-13", "2011-04-26", "2012-04-20", "2013-04-09", "2014-04-22", "2015-04-29", "2016-05-08", "2017-04-20", "2018-04-17", "2019-03-25",
			"2008-05-22", "2009-05-22", "2010-06-07", "2011-05-31", "2012-06-03", "2013-05-27", "2014-04-06", "2015-06-08", "2016-05-09", "2017-05-08", "2018-05-12", "2019-06-05",
			"2007-05-08", "2008-04-02", "2009-05-05", "2010-04-11", "2011-04-12", "2012-03-28", "2013-04-09", "2014-04-16", "2015-04-23",
			"2010-04-20", "2011-03-11", "2012-03-23", "2013-04-25", "2014-04-07", "2015-04-14", "2016-05-03", "2017-04-19", "2018-04-10", "2019-04-25",
			"2012-05-23", "2013-05-01", "2014-05-09", "2015-06-02", "2016-05-08", "2017-05-08", "2018-04-23", "2019-05-10")
	)

## combine

	d <- merge(d1b, sys, by="treatment", all.x=TRUE)
	d <- merge(d, site, by="Location", all.x=TRUE)
	d <- merge(d, dates, by=c("Location", "harvest_year"), all.x=TRUE)

	# CP at CRS and DTC is hand-hoe ridge and furrow (supplement)
	i <- d$treatment == "CP" & d$Location %in% c("CRS", "DTC")
	d$land_prep_method[i] <- "ridge tillage"
	d$land_prep_implement[i] <- "hoe"

	# glyphosate at planting (Table S2) is given per site; not set for tilled plots
	i <- d$treatment %in% c("CP", "CP2", "CP-MCt") & d$herbicide_product == "glyphosate"
	d$herbicide_product[i] <- NA
	d$herbicide_used <- d$herbicide_product != "none"

	# companion crops by year (paper 2.2); its year ranges run from first sowing to last harvest, as in Table 1
	h <- d$harvest_year
	i <- d$Location == "HRS" & d$treatment == "RI-M+C"
	d$intercrops[i & h <= 2007] <- "velvet bean"
	d$intercrops[i & h %in% 2008:2013] <- "pigeon pea"
	i <- d$Location == "DTC"
	d$intercrops[i & d$treatment == "DS-M+C" & h <= 2015] <- "pigeon pea"
	d$crop_rotation[i & d$treatment == "DS-MSf" & h >= 2016] <- "maize;cowpea"
	i <- d$Location == "SRS" & h <= 2011
	d$intercrops[i & d$treatment == "DS-M+Pp"] <- "sunflower"
	d$crop_rotation[i & d$treatment == "DS-MSfC"] <- "maize;sunflower;common bean"
	# plots split into rotation halves, unknown which half was sampled: HRS from year 3, CRS DiS-MC from 2009/10
	d$crop_rotation[d$Location == "HRS" & h >= 2007] <- NA
	d$crop_rotation[d$Location == "CRS" & d$treatment == "DiS-MC" & h >= 2010] <- NA
	d$intercropped <- d$intercrops != "none"

	d$geo_from_source <- TRUE
	d$geo_uncertainty <- 1000    # research station extent
	d$Location <- NULL
	d$harvest_year <- NULL

	carobiner::write_files(path, meta, d)
}
