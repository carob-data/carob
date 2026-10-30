# R script for "carob"
# license: GPL (>=3)

## NOTES
# 2018 mangrove Breeding Task Force release. The workbooks are genotype-by-station BLUPs, not plot-level country files.
# One row per genotype and station. Yield, flowering and plant height come from the three BLUP sheets. Plot replicates are not in the files and are not rebuilt.
# The statistics sheets report 3 replicates where a trait was analysed. That is left as a note; there is no replicate column.
# Units are not printed. Yield is stored as recorded (about 150-6,900, kg/ha scale; Sierra Leone stations are much lower than Senegal and Guinea). Flowering values are days. Height values are cm.
# The Ndiaye header is spelled AFricaRice in the workbook and stored as AfricaRice Ndiaye.
# The yield sheet says the low yields at Sierra Leone-Kychom and Sierra Leone-Thormoya, which affected all genotypes, were attributed by the Sierra Leone breeder to severe crab damage, seedlings failing to strive under constant standing water, and low soil fertility. That note is not a genotype row.

## ISSUES
# No coordinates, planting or harvest dates, fertilizer rates, or yield moisture in the workbooks. Georeference the stations. Irrigation is not stated for this mangrove set, so irrigated is left NA.
# Check genotypes are marked only by yellow cell highlighting, which is not a value, so they are not flagged.
# Some genotype-station cells are blank. Sierra Leone-Rokupr has no yield BLUP. Flowering is absent for Gambia-Kaiaf, Guinea-Khayta, Guinea-Koba and Sierra Leone-Kychom. Height is absent for Gambia-Geneir. Rows are kept when another trait is present, so yield is NA on those rows.
# Station names follow the column headers (Geneir, Kaiaf, Khayta, Kychom, Thormoya). Confirm spellings when georeferencing.
# The files do not say which stations were on-farm hot spots. All rows are coded on_farm FALSE from the station names.
# write_files: missing variables: carob_contributor; yield_moisture, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. NA detected: yield.

## NOT PROCESSED
# Statistics-Flowering-2018-Mangrove.xls, Statistics-Height-2018-Mangrove.xls, Statistics-Yield-2018-Mangrove.xls: heritability, mean, LSD, replicates, genotype p-value, genotype variance and residual variance. ANOVA summaries, not variety rows. The same summary rows are repeated at the bottom of each BLUP sheet and are skipped.
# Yield-sheet note naming Sierra Leone-Kychom and Sierra Leone-Thormoya (crab damage, seedlings failing to strive under standing water, low soil fertility), plus the three bullet lines under it. Not a genotype.
# 2018-Mangrove-protocol.pdf (protocol, not a data table).


carob_script <- function(path) {

"
2018 Africa-wide Breeding Task Force Trials for Mangrove

In 2010, the Africa-wide Rice Breeding Task Force was launched by AfricaRice involving National Agricultural Research System (NARS) from about 30 countries. The objectives of the network are to evaluate the stability of traits incorporated in breeding processes and to identify varieties best fit to growth conditions in target regions and to markets. The Task Force also accumulates data on the performance of new elite lines, thereby facilitating varietal release procedures. Furthermore, by exposing breeders from NARS and farmers to these elite lines during the testing phase, dissemination will be facilitated. The activities conducted by the Task Force consists of a series of consecutive trials. Promising breeding lines developed by AfricaRice or by national and international partners, such as IRRI, CIAT and the NARS are nominated for evaluation in one or several rice cultivation environments: rainfed lowland, irrigated lowland, rainfed upland, high elevation and mangrove. All nominated lines should be fixed and accompanied by supporting data on traits incorporated during the breeding process and with information on yield performance. These characteristics are checked at AfricaRice before incorporation into the network. Further details are in the '2018-Mangrove-protocol.pdf'
"

	uri <- "doi:10.7910/DVN/MI7BSZ"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;flowering_days;plant_height",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.5
	)

	norm_key <- function(x) {
		x <- trimws(x)
		sub("^AFricaRice-", "AfricaRice-", x)
	}

	blup_long <- function(f, varname) {
		r <- carobiner::read.excel(f, col_names=FALSE)
		hdr <- norm_key(as.character(unlist(r[1, ])))
		variety <- trimws(as.character(r[[1]]))
		drop_lab <- c("Heritability", "Mean", "LSD", "CV", "STATISTIC", "Statistic", "Genotype")
		keep <- seq_len(nrow(r)) > 1
		keep <- keep & !is.na(variety) & nzchar(variety)
		keep <- keep & !variety %in% drop_lab
		keep <- keep & !startsWith(variety, "-")
		keep <- keep & !startsWith(variety, "Low yields")
		out <- list()
		for (j in seq_len(ncol(r))[-1]) {
			lab <- hdr[j]
			if (is.na(lab) || !nzchar(lab)) next
			if (startsWith(lab, "Combined")) next
			if (grepl("highlighted|Supplementary", lab)) next
			# Header text makes the whole column character. Data rows are numbers or blank.
			vals <- r[[j]][keep]
			if (is.character(vals)) {
				vals <- trimws(vals)
				vals[!nzchar(vals)] <- NA_character_
			}
			vals <- as.numeric(vals)
			piece <- data.frame(
				variety = variety[keep],
				loc_key = lab
			)
			piece[[varname]] <- vals
			out[[length(out) + 1]] <- piece
		}
		if (length(out) == 0) {
			stop("no location columns in ", basename(f))
		}
		x <- do.call(rbind, out)
		if (any(duplicated(x[, c("variety", "loc_key")]))) {
			stop("duplicate variety-location in ", basename(f))
		}
		x
	}

	fy <- ff[basename(ff) == "BLUP-Yield-2018-Mangrove.xls"]
	ffl <- ff[basename(ff) == "BLUP-Flowering-2018-Mangrove.xls"]
	fh <- ff[basename(ff) == "BLUP-Height-2018-Mangrove.xls"]

	d <- blup_long(fy, "yield")
	d <- merge(d, blup_long(ffl, "flowering_days"), by=c("variety", "loc_key"), all=TRUE)
	d <- merge(d, blup_long(fh, "plant_height"), by=c("variety", "loc_key"), all=TRUE)

	sites <- data.frame(
		loc_key = c(
			"AfricaRice-Senegal-Ndiaye", "Gambia-Geneir", "Gambia-Kaiaf",
			"Guinea-Khayta", "Guinea-Koba", "Nigeria-Ondo",
			"Sierra Leone-Kychom", "Sierra Leone-Rokupr", "Sierra Leone-Thormoya"
		),
		country = c(
			"Senegal", "Gambia", "Gambia",
			"Guinea", "Guinea", "Nigeria",
			"Sierra Leone", "Sierra Leone", "Sierra Leone"
		),
		location = c(
			"AfricaRice Ndiaye", "Geneir", "Kaiaf",
			"Khayta", "Koba", "Ondo",
			"Kychom", "Rokupr", "Thormoya"
		)
	)

	bad <- setdiff(unique(d$loc_key), sites$loc_key)
	if (length(bad) > 0) {
		stop("unmapped location: ", paste(bad, collapse=", "))
	}
	d <- merge(d, sites, by="loc_key", all.x=TRUE)

	use <- !(is.na(d$yield) & is.na(d$flowering_days) & is.na(d$plant_height))
	d <- d[use, ]
	d <- d[order(d$country, d$location, d$variety), ]
	rownames(d) <- NULL

	d <- data.frame(
		crop = "rice",
		variety = d$variety,
		treatment = d$variety,
		country = d$country,
		location = d$location,
		yield = d$yield,
		flowering_days = d$flowering_days,
		plant_height = d$plant_height,
		yield_part = "grain",
		trial_id = paste(d$location, "2018"),
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = NA
	)

	carobiner::write_files(path, meta, d)
}
