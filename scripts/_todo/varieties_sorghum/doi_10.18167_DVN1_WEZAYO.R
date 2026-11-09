# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot grain yield is Obsplant GrainYield (kg/ha), not the trial-by-variety means in Observations.csv. Plots with two grain records keep both.
# TotBiom is dmy_total (kg/ha). 1000GrWe is stored in the file as kg and is converted to g/1000 seeds.
# Coordinates are the weather-station coordinates in Ws.csv, linked by Wstrial. Trial latitude and longitude are empty. geo_from_source is TRUE for those station coordinates.
# Country is the site countrycode: BF Burkina Faso, ML Mali, SN Senegal (Country.csv uses French uppercase names).
# NPK applications are kg/ha of an ungraded blend, so N, P, and K are left missing on those trials. Urea is 46% N and DAP is 18% N and 20.1% P. MO and MANURE are OM_amount (kg/ha). A trial with no fertilizer row is 0.

## ISSUES
# Some plots have no grain yield, and some trials have no weather-station coordinates. Flowering values are missing. Row spacing is empty. Irrigation is not in the management file.
# Plant height includes values below 10 cm and above 500 cm. Weather values outside the allowed range were set to missing.
# write_files: missing variables: project. missing variables: yield_moisture, irrigated, harvest_date. NA detected: yield, latitude, longitude. out of bounds: plant_height (2.33, 556.5).

## NOT PROCESSED
# Observations.csv variety means. Biomass components other than TotBiom. Pesticides. Soil files. KO fertilizer. The protocol pdf.
# Radiation duplicates mean temperature on the rows that were checked. windtot and eto are not mapped.
# readme.txt: documentation or a codebook, not an observation table.
# Sorghum_West_Africa_Country.csv: countrycode lookup (BF, ML, SN). The site country code is stored directly.
# Sorghum_West_Africa_Crop.csv: crop code list. The crop is sorghum.
# Sorghum_West_Africa_Days.csv: trial-level DAP and GDD. Plot records use the observation date instead.
# Sorghum_West_Africa_Genesoil.csv and Sorghum_West_Africa_Layersoil.csv: soil-profile parameters (p0, ru, layer water limits). No plot soil sample is linked to a carob soil variable from these codes.
# Sorghum_West_Africa_Lib.csv: French/English label dictionary.
# Sorghum_West_Africa_Observations.csv: trial-by-variety means. Plot grain yield is taken from Obsplant.
# Sorghum_West_Africa_Variety.csv: variety code list. The variety name is already on the plot.

## NEW VARIABLES
# None.


carob_script <- function(path) {

"
Dataset characterizing ten major West African sorghum varieties in twenty-three sites across the sudano-sahelien West Africa

This dataset provides an experimental dataset to calibrate and/or validate sorghum crop growth and development model under genotype x environment interaction. Data were collected in a network of experimentations across the drylands of West Africa to characterize contrasted sorghum varieties of the region, and to have a harmonized dataset of detailed plant growth and development during the crop cycle for proper sorghum models calibration. Crop modellers as well as crop eco-physiologist and sorghum breeders could use this dataset for G*E analysis. Such data could be used for G*E analysis as well as for crop modelling intercomparison exercise.
The database contains location, weather, soil, management techniques, sorghum crop growth and development data and yield data from 23 sites (i.e. location * year * sowing date) located in three countries West African countries: Sénégal (11 sites), Mali (5 sites) and Burkina Faso (7 sites).  These twenty-three sites represent experiments from six locations carried out from 2013 to 2016, and for different sowing dates.
"

	uri <- "doi:10.18167/DVN1/WEZAYO"
	group <- "varieties_sorghum"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_total",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 1.0
	)

	# Timestamps in these files are month/day/year (for example 6/10/2014).
	mdy <- function(x) {
		x <- sub(" .*", "", trimws(as.character(x)))
		x[x %in% c("", "NA")] <- NA
		as.character(as.Date(x, "%m/%d/%Y"))
	}
	utf8 <- function(x) {
		x <- as.character(x)
		bad <- !is.na(x) & !validUTF8(x)
		x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8")
		x
	}

	f1 <- ff[basename(ff) == "Sorghum_West_Africa_Plot.csv"]
	f2 <- ff[basename(ff) == "Sorghum_West_Africa_Plotcrop.csv"]
	f3 <- ff[basename(ff) == "Sorghum_West_Africa_Trial.csv"]
	f4 <- ff[basename(ff) == "Sorghum_West_Africa_Site.csv"]
	f5 <- ff[basename(ff) == "Sorghum_West_Africa_Ws.csv"]
	f6 <- ff[basename(ff) == "Sorghum_West_Africa_Wstrial.csv"]
	f7 <- ff[basename(ff) == "Sorghum_West_Africa_Itk.csv"]
	f8 <- ff[basename(ff) == "Sorghum_West_Africa_Obsplant.csv"]
	f9 <- ff[basename(ff) == "Sorghum_West_Africa_Wdataday.csv"]
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	r5 <- read.csv(f5)
	r6 <- read.csv(f6)
	r7 <- read.csv(f7)
	r8 <- read.csv(f8)
	r9 <- read.csv(f9)

	r1 <- merge(r1, r2[, c("plotcode", "variety")], by = "plotcode", all.x = TRUE)
	r3 <- merge(r3, r4[, c("sitecode", "countrycode", "sitename")], by = "sitecode", all.x = TRUE)
	r5$wscode <- as.character(r5$wscode)
	r6$wscode <- as.character(r6$wscode)
	r3 <- merge(r3, r6[, c("trialcode", "wscode")], by = "trialcode", all.x = TRUE)
	r3 <- merge(r3, r5[, c("wscode", "wslat", "wslong", "wsalt")], by = "wscode", all.x = TRUE)

	cty <- c("BF" = "Burkina Faso", "ML" = "Mali", "SN" = "Senegal")
	r3$country <- unname(cty[r3$countrycode])
	r3$sitename <- utf8(r3$sitename)

	fert <- r7[r7$itklib == "fertilizin", c("trialcode", "Product", "itkval")]
	fert$product <- toupper(trimws(fert$Product))
	fert$itkval <- as.numeric(fert$itkval)
	fert <- aggregate(itkval ~ trialcode + product, fert, sum)
	dose <- function(tr, prod) {
		i <- fert$trialcode == tr & fert$product == prod
		if (!any(i)) 0 else sum(fert$itkval[i])
	}
	tr <- unique(as.character(r1$trialcode))
	fm <- data.frame(trialcode = tr, npk = 0, urea = 0, dap = 0, om = 0)
	for (i in seq_along(tr)) {
		fm$npk[i] <- dose(tr[i], "NPK")
		fm$urea[i] <- dose(tr[i], "UREA")
		fm$dap[i] <- dose(tr[i], "DAP")
		fm$om[i] <- dose(tr[i], "MO") + dose(tr[i], "MANURE")
	}
	fm$N_fertilizer <- ifelse(fm$npk > 0, NA, fm$urea * 0.46 + fm$dap * 0.18)
	fm$P_fertilizer <- ifelse(fm$npk > 0, NA, fm$dap * 0.201)
	fm$K_fertilizer <- ifelse(fm$npk > 0, NA, 0)
	ftype <- function(i) {
		parts <- c(
			if (fm$npk[i] > 0) "NPK",
			if (fm$urea[i] > 0) "urea",
			if (fm$dap[i] > 0) "DAP"
		)
		if (length(parts) == 0) "none" else paste(parts, collapse = ";")
	}
	fm$fertilizer_type <- vapply(seq_len(nrow(fm)), ftype, "")

	sow <- r7[r7$itklib == "sowing", c("trialcode", "itkdate", "itkval")]
	sow <- sow[!duplicated(sow$trialcode), ]
	sow$planting_date <- mdy(sow$itkdate)
	sow$plant_density <- as.numeric(sow$itkval)

	gy <- r8[r8$obsplantlib == "GrainYield", c("plotcode", "obsplantval")]
	names(gy)[2] <- "yield"
	tb <- r8[r8$obsplantlib == "TotBiom", c("plotcode", "obsplantval")]
	tb <- tb[!duplicated(tb$plotcode), ]
	names(tb)[2] <- "dmy_total"
	sw <- r8[r8$obsplantlib == "1000GrWe", c("plotcode", "obsplantval")]
	sw <- sw[!duplicated(sw$plotcode), ]
	sw$seed_weight <- as.numeric(sw$obsplantval) * 1000
	sw <- sw[, c("plotcode", "seed_weight")]

	d <- merge(r1, gy, by = "plotcode", all.x = TRUE)
	d <- merge(d, tb, by = "plotcode", all.x = TRUE)
	d <- merge(d, sw, by = "plotcode", all.x = TRUE)
	d <- merge(d, r3[, c("trialcode", "country", "sitename", "wslat", "wslong", "wsalt", "wscode")], by = "trialcode", all.x = TRUE)
	d <- merge(d, fm[, c("trialcode", "N_fertilizer", "P_fertilizer", "K_fertilizer", "fertilizer_type", "om")], by = "trialcode", all.x = TRUE)
	d <- merge(d, sow[, c("trialcode", "planting_date", "plant_density")], by = "trialcode", all.x = TRUE)

	d <- data.frame(
		trial_id = d$trialcode,
		plot_id = d$plotcode,
		crop = "sorghum",
		variety = d$variety,
		treatment = d$variety,
		block_id = as.character(d$block),
		plot_area = as.numeric(d$surface),
		country = d$country,
		location = d$sitename,
		latitude = d$wslat,
		longitude = d$wslong,
		elevation = d$wsalt,
		geo_from_source = TRUE,
		station_id = as.character(d$wscode),
		planting_date = d$planting_date,
		plant_density = d$plant_density,
		yield = as.numeric(d$yield),
		yield_part = "grain",
		dmy_total = as.numeric(d$dmy_total),
		seed_weight = d$seed_weight,
		N_fertilizer = d$N_fertilizer,
		P_fertilizer = d$P_fertilizer,
		K_fertilizer = d$K_fertilizer,
		fertilizer_type = d$fertilizer_type,
		OM_amount = d$om,
		on_farm = FALSE,
		is_survey = FALSE
	)

	hp <- r8[r8$obsplantlib %in% c("Pl_Height", "height_pc") & !is.na(r8$obsplantval), ]
	lai <- r8[r8$obsplantlib %in% c("LAI", "lai") & !is.na(r8$obsplantval), ]
	h <- data.frame(
		plot_id = hp$plotcode,
		date = mdy(hp$obsplantdate),
		plant_height = as.numeric(hp$obsplantval)
	)
	l <- data.frame(
		plot_id = lai$plotcode,
		date = mdy(lai$obsplantdate),
		LAI = as.numeric(lai$obsplantval)
	)
	long <- merge(h, l, by = c("plot_id", "date"), all = TRUE)
	long <- long[long$plot_id %in% d$plot_id, ]

	r9$tmin <- as.numeric(r9$tmin)
	r9$tmax <- as.numeric(r9$tmax)
	r9$temp <- as.numeric(r9$tmoy)
	r9$prec <- as.numeric(r9$rainfall)
	r9$rhum <- as.numeric(r9$rhmoy)
	r9$rhmn <- as.numeric(r9$rhmin)
	r9$rhmx <- as.numeric(r9$rhmax)
	for (v in c("tmin", "tmax", "temp", "prec", "rhum", "rhmn", "rhmx")) {
		r9[[v]][r9[[v]] == -999] <- NA
	}
	wth <- data.frame(
		station_id = as.character(r9$wscode),
		date = mdy(r9$weatherdate),
		tmin = r9$tmin,
		tmax = r9$tmax,
		temp = r9$temp,
		prec = r9$prec,
		rhum = r9$rhum,
		rhmn = r9$rhmn,
		rhmx = r9$rhmx
	)
	wth <- wth[wth$station_id %in% d$station_id & !is.na(wth$date), ]
	# Drop values outside the weather bounds, including temp of -492 and humidity above 100.
	in_bounds <- function(x, lo, hi) {
		x[!is.na(x) & (x < lo | x > hi)] <- NA
		x
	}
	wth$tmin <- in_bounds(wth$tmin, -50, 60)
	wth$tmax <- in_bounds(wth$tmax, -50, 60)
	wth$temp <- in_bounds(wth$temp, -50, 60)
	wth$rhum <- in_bounds(wth$rhum, 0, 100)
	wth$rhmn <- in_bounds(wth$rhmn, 0, 100)
	wth$rhmx <- in_bounds(wth$rhmx, 0, 100)

	carobiner::write_files(path, meta, d, long = long, wth = wth)
}
