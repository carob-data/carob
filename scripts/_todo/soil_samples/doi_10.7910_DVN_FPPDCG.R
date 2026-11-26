# R script for "carob"
# license: GPL (>=3)

## NOTES
# Wet chemistry for 160 samples from the Western Kenya Integrated Ecosystem Management Project. The title places the work in western Kenya.
# Total carbon and acidified carbon are percent. Acidified carbon is soil_SOC. Total nitrogen is percent and is stored as mg/kg (percent * 10000).
# pH is a 1:1 soil:water suspension. Al, B, Cu, Fe, Mn, P, S, and Zn are Mehlich-3 extracts in mg/kg. Exchangeable Ca, Mg, K, Na, and acidity are cmolc/kg.
# Electric conductivity is dS/m, the same number as mS/cm.
# Sila, Shepherd, and Pokhariyal (Geoderma Regional, 2017) and Sila (2016) describe these samples. Plots follow the Land Degradation Surveillance Framework in lower and middle Nyando, lower and middle Yala, and lower Nzoia, at 0-20 cm and 20-50 cm.
# The wet chemistry file has no depth, site, or coordinate column. Samples come in consecutive SSN pairs, one pair for the two depths, and the file does not say which member is the topsoil.
# Units are from Table 1 of Sila (2016), whose ranges match this file.
# Infrared MIR.csv has one absorbance at each wavenumber. The column names run from about 600 to 7500 cm-1. The long table is sample_id, wavenumber, and absorbance.

## ISSUES
# The site table cannot be joined: it has no SSN, and its Northing/Easting columns mix scales without CRS. Approximate Western Kenya basin centroid (Nyando, Yala, Nzoia; geo_from_source = FALSE) is used.
# Phosphorus above 200 mg/kg is kept as reported.
# Two spectra, 2248_545 and 2248_546, have no wet chemistry row. Two wet chemistry rows, 2248_563 and 2248_564, have no spectrum.
# write_files: out of bounds soil_P (1, 467); time/depth (long table is MIR spectra indexed by wavenumber).

## NOT PROCESSED
# Wkiemp site characteristics.csv. Land use, field texture, slope, elevation, Northing, and Easting. No shared sample id.
# Acidified nitrogen, the sum of exchangeable bases, the exchangeable sodium ratio, the exchangeable sodium percentage, and the Ca:Mg ratio.
# Wkiemp site chracteristics.csv: not read. columns: Key, ID, Alternative Block, Site, Cluster, Plot, Date, Photo ID, Northing, Easting, Elevation, Pos error, .... Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Data for soil samples collected from WKIEMP project and analyzed at ICRAF using infrared spectroscopy methods and wet chemistry at Crop Nutrition labs
"

	uri <- "doi:10.7910/DVN/FPPDCG"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRAF; UONBI",
		publication = "doi:10.1016/j.geodrs.2017.04.003",
		project = "WKIEMP",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 75,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Wet Chemistry.csv"]
	r <- read.csv(f, na.strings = c("", "NA"))

	# Approximate Western Kenya study-area centroid (lower/middle Nyando, Yala, and lower Nzoia basins; Sila et al. 2017)
	d <- data.frame(
		sample_id = as.character(r$SSN),
		country = "Kenya",
		location = "Western Kenya",
		latitude = -0.09,
		longitude = 34.76,
		geo_uncertainty = 60000,
		geo_from_source = FALSE,
		soil_N_total = r$Total_Nitrogen * 10000,
		soil_C_total = r$Total_Carbon,
		soil_SOC = r$Carbon.acid,
		soil_pH = r$pH,
		soil_Al = r$Al,
		soil_B = r$B,
		soil_Cu = r$Cu,
		soil_Fe = r$Fe,
		soil_Mn = r$Mn,
		soil_P = r$P,
		soil_P_method = "Mehlich 3",
		soil_S = r$S,
		soil_Zn = r$Zn,
		soil_Na_exch = r$ExNa,
		soil_Ca_exch = r$ExCa,
		soil_Mg_exch = r$ExMg,
		soil_K_exch = r$ExK,
		soil_EC = r$Ecd,
		soil_ex_acidity = r$ExAc,
		is_survey = TRUE,
		on_farm = NA
	)
	d$soil_P_method[is.na(d$soil_P)] <- NA_character_

	f1 <- ff[basename(ff) == "Infrared MIR.csv"]
	r1 <- read.csv(f1, check.names=FALSE, na.strings=c("", "NA"))
	ssn <- as.character(r1$SSN)
	miss <- ssn[!ssn %in% d$sample_id]
	if (length(miss)) {
		add <- d[seq_along(miss), ]
		add[] <- NA
		add$sample_id <- miss
		add$country <- "Kenya"
		add$location <- "Western Kenya"
		add$latitude <- -0.09
		add$longitude <- 34.76
		add$geo_uncertainty <- 60000
		add$geo_from_source <- FALSE
		add$is_survey <- TRUE
		d <- rbind(d, add)
	}
	d$record_id <- as.integer(seq_len(nrow(d)))

	wave <- as.numeric(sub("^w", "", names(r1)[-1]))
	absb <- as.vector(as.matrix(r1[, -1]))
	n <- nrow(r1)
	long <- data.frame(
		record_id = d$record_id[match(rep(ssn, times=length(wave)), d$sample_id)],
		wavenumber = rep(wave, each=n),
		absorbance = absb
	)

	carobiner::write_files(path, meta, d, long)
}
