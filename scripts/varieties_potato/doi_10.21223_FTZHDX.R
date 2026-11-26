# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. TTWP (total tuber weight per plot) in the "Fieldbook" sheet is wrong: it is the plot weight
#    multiplied by the number of plants with tubers (NPH); for example, plot 1 has 562 g in the
#    field sheet ("original", column "Peso") and TTWP = 2.81 kg (0.562 x 5). This gives
#    implausible yields (up to 91 t/ha). In "original", "Peso" is the total tuber weight of the
#    plot in grams ("Rendimiento total (g/plta)" = Peso / plants; average tuber weight = Peso / tubers).
#    Yield is therefore computed from "Peso" (Peso / 1000 * 10000 / 3 m2), matching the
#    records by replication and clone (the plot numbers of "original" and "Fieldbook" differ).
# 2. 20 plots have no tuber weight (18 with no plants with tubers, NPH = 0, and 2 with 1-2);
#    the plants probably died (bacterial wilt, "MB", is noted for some plots). Their yield is NA.
#    In 7 other plots with NPH = 0, a tuber weight was recorded (100-316 g) and is used.
# 3. Planting date: the "Minimal" sheet gives 2006-12-12 to 2007-04-02 (used), but the
#    "calculos" sheet gives planting on 27/09/05 and the comment in "Minimal" is
#    "SRspr2005-06" (spring season). To be confirmed with the data provider.
# 4. The description says 38 clones; the data and material list have 32.
# 5. The "Vine cutting / killing" date (2006-12-01) is before planting; not used.

## NOTES
# 1. Screening of the Ranchitos group for early yield and heat tolerance at the CIP experiment
#    station in San Ramon (828 m). Plots of 10 plants in one row (0.3 x 1.0 m, 3 m2).
#    Coordinates, elevation and dates from the "Minimal" sheet.
# 2. Yield is the non-adjusted total tuber yield; marketable yield is not reported.
#    plant_density from the number of plants emerged (NPE), tuber_density from the
#    number of tubers (TNTP).
# 3. It is not stated whether the trial was irrigated; no information on fertilizer use.
#    The "Soil_analysis" sheet is empty and there are no weather data.
#    Not processed: plant vigor, senescence, stolon length, tuber appearance and size (scales).

carob_script <- function(path) {

"
Dataset for: Evaluation of Clones Potatoes of Ranchitos population, San Ramon, 2006

The objective of this study was to assess early yield and heat tolerance for 38 potato clones during 2006 in San Ramon, Junin. A randomized complete block design (RCBD) with three replications was employed.
"

	uri <- "doi:10.21223/FTZHDX"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-26",
		carob_completion = 80,
		carob_effort = 1
	)

	process <- carobiner::get_function("process_cip_lbvars", path, group)
	f <- ff[grep("^PTYL.*\\.xls$", basename(ff))]
	d <- process(f)

	## yield from the field sheet (ISSUES 1 and 2)
	r <- carobiner::read.excel(f, sheet="original")
	cip <- c("C95.276"="CIP396311.1", "C99.551"="CIP399101.1", "CANCHAN"="CIP380389.1",
		"Maria Bonita"="CIP388676.1", "PERRICHOLI"="CIP374080.5", "REICHE"="CIP388611.22",
		"REVOLUCION"="CIP720043", "TOMASA"="CIP720072", "UNICA"="CIP392797.22",
		"ACHIRANA"="CIP720088")
	id <- trimws(as.character(r$Cipnumber))
	i <- id %in% names(cip)
	id[i] <- cip[id[i]]
	## CIP numbers are stored as numbers and read with floating point noise
	## (e.g. "301044.35999999999")
	id[!i] <- paste0("CIP", round(as.numeric(id[!i]), 2))
	r <- data.frame(rep = as.integer(r$Rep), variety_code = id,
		yield = suppressWarnings(as.numeric(r$Peso)) / 1000 * 10000 / d$plot_area[1])

	d$yield <- NULL
	d <- merge(d, r, by=c("rep", "variety_code"), all.x=TRUE)
	stopifnot(nrow(d) == 96)

	## not stated in the data (NOTES 3)
	d$on_farm <- FALSE
	d$irrigated <- NA

	carobiner::write_files(path, meta, d)
}
