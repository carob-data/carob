# R script for "carob"
# license: GPL (>=3)

## NOTES
# 2017 mangrove multi-environment trial. Each BLUP workbook is a genotype-by-site matrix, not plot records.
# One row per genotype and site column. Yield is kg/ha, flowering is days, height is cm: site means are about 4500-6200, 75-104 and 98-103, so the figures are not t/ha.
# Institute is kept on the location when the header names one (Mbe AfricaRice, Ndiaye AfricaRice).
# Combined-All is the all-site aggregate. Flowering and height are blank for AfricaRice-Cote d'Ivoire-Mbe. Height is blank for Guinea-Khayta.

## ISSUES
# No coordinates, planting or harvest dates, fertilizer rates, yield moisture, or irrigation flag. Mangrove here is tidal; it is not the irrigated-lowland series. Dataverse productionDate is 2016-12-31; the trial year is the 2017 title.
# on_farm is FALSE because the 2017 text says the farmer adoption trial was removed. The sheets do not flag each hub.
# Check varieties are yellow cell fill only. That fill was not read.
# Statistics sheets say 3 replicates where a site was analysed. Those were not expanded into plot rows.
# Some genotype-by-site cells are blank, so yield is missing where only flowering or height was published. WITA 9 is on the yield sheet only.
# write_files: missing carob_contributor; missing yield_moisture, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.

## NOT PROCESSED
# BLUP-Flowering, BLUP-Height and BLUP-Yield footer rows Heritability, Mean and LSD: the same summaries as the statistics workbooks, not genotypes.
# BLUP column Combined-All: multi-site aggregate, not a location.
# Margin notes (site counts and "Checks are highlighted in yellow"): not records.
# Statistics-Flowering-2017-Mangrove.xls, Statistics-Height-2017-Mangrove.xls, Statistics-Yield-2017-Mangrove.xls: Heritability, Mean, LSD, Replicates, P-value (Genotype), Variance (Genotype), Variance (Residual) by site. ANOVA only. Flowering at Mbe and height at Khayta and Mbe are blank there too.
# Supplementary Materials - Mangrove 2017.pdf: font-encoded print of the tables, not a separate data table.

carob_script <- function(path) {

"
2017 Africa-wide Breeding Task Force Trials for Mangrove

Objective and Requirement of MET by Breeding Task Force:

In 2010, Rice Breeding Task Force in Africa was established aimed at accelerating the breeding process, especially in the later phase of evaluation of promising breeding lines in multi-locations throughout Africa. Breeding lines included in the multi-environment trials (MET) are developed by various institutions such as IRRI, CIAT, NARS in Africa and AfricaRice. The objective of the MET is to identify lines that are suitable for cultivation under an ecological environment in the target region in Africa. To reduce noise and to acquire real genetic differences among test lines through MET, the following conditions must be met for the MET: 1. Experimental field is uniform in soil fertility; 2. Experimental field is very well leveled before seeding or transplanting, to facilitate uniform water and fertilizer management; 3. Fertilizer is evenly applied to every plot, and ideally every plant within a plot; 4. Weed control, either by applying herbicide or hand weeding, is carried out uniformly across the whole trial, and finished in one day per time; 5. Any other operations, whatever necessary for a trial, are uniformly done across the whole trial.

The MET serves as a part of a national testing program. In other words, the MET conducted by BTF is integrated into the corresponding national testing system. Data collected from the MET will be recognized and used by the Varietal Testing and Release Committee of a country where the MET is conducted. This is a measure to shorten breeding cycle and to increase genetic gains. Structure of varietal evaluation series has been changed since the season of 2017.

Explanations of the changes

1. The number of entries is reduced to ensure better conduct of the trials and higher quality data. 1.1 For each sub-region (WCA and ESA), a maximum of 29 new entries per production system (irrigated lowland, rainfed lowland, rainfed upland, mangrove and high-elevation) is considered to enter the BTF (Phase I, former MET) each year. 1.2 After evaluation and data analysis, a maximum of 10 entries is selected for further evaluation (Phase II, former PET). 1.3 After evaluation and data analysis, a maximum of 3 entries is selected for further evaluation (Phase III, former PAT). 2. Materials from Phase I, II and III are combined into a single trial (up to 42 test entries every year). 3. Participatory varietal selection (PVS) may be conducted in the single trial (one replication may be chosen for the PVS). 4. The single trial is conducted in three different locations in each participating country and in priority in the Hubs. 5. The Farmers Adoption Trial (FAT) and Validation AfricaRice Trial (VAT) is removed from the BTF scheme. Where required, further testing of selected entries in farmers' fields will be ideally handled by NARS with the assistance of Rice Agronomy Task Force.
"

	uri <- "doi:10.7910/DVN/SYFAZJ"
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
		notes = "Variety-by-location BLUPs from the 2017 mangrove trial, not plot replicates.",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.4
	)

	blup_long <- function(f, response) {
		r <- carobiner::read.excel(f, col_names=FALSE, .name_repair="minimal")
		hdr <- trimws(as.character(unlist(r[1, ])))
		hdr[is.na(hdr) | !nzchar(hdr)] <- NA_character_
		body <- r[-1, , drop=FALSE]
		variety <- trimws(as.character(body[[1]]))
		stat <- variety %in% c("Heritability", "Mean", "LSD", "LCS", "Replicates", "Statistic", "Genotype")
		stat <- stat | grepl("^(P-value|Variance)", variety)
		keep <- !is.na(variety) & nzchar(variety) & !stat
		variety <- variety[keep]
		body <- body[keep, , drop=FALSE]
		loc <- which(!is.na(hdr) & hdr != "Genotype" & !grepl("^Combined", hdr))
		pieces <- vector("list", length(loc))
		for (i in seq_along(loc)) {
			j <- loc[i]
			val <- body[[j]]
			if (!is.numeric(val)) {
				val <- as.numeric(trimws(as.character(val)))
			}
			pieces[[i]] <- data.frame(
				variety = variety,
				site_label = hdr[j],
				value = val
			)
		}
		x <- do.call(rbind, pieces)
		x <- x[!is.na(x$value), ]
		names(x)[names(x) == "value"] <- response
		x
	}

	site_row <- function(label) {
		parts <- trimws(strsplit(label, "-", fixed=TRUE)[[1]])
		inst <- NA_character_
		if (tolower(parts[1]) %in% c("africarice", "isra")) {
			inst <- parts[1]
			parts <- parts[-1]
		}
		cmap <- c(
			"nigeria" = "Nigeria",
			"senegal" = "Senegal",
			"mali" = "Mali",
			"burkina faso" = "Burkina Faso",
			"guinea" = "Guinea",
			"ghana" = "Ghana",
			"benin" = "Benin",
			"cote d'ivoire" = "C\u00f4te d'Ivoire"
		)
		country <- unname(cmap[tolower(parts[1])])
		if (is.na(country)) {
			stop(paste("unparsed site", label))
		}
		loc <- paste(parts[-1], collapse="-")
		loc <- carobiner::fix_name(loc, "title", skip="du")
		if (!is.na(inst)) {
			loc <- paste0(loc, " (", inst, ")")
		}
		data.frame(site_label=label, country=country, location=loc)
	}

	fy <- ff[basename(ff) == "BLUP-Yield-2017-Mangrove.xls"]
	fh <- ff[basename(ff) == "BLUP-Height-2017-Mangrove.xls"]
	ffl <- ff[basename(ff) == "BLUP-Flowering-2017-Mangrove.xls"]
	y <- blup_long(fy, "yield")
	h <- blup_long(fh, "plant_height")
	fl <- blup_long(ffl, "flowering_days")

	d <- merge(y, h, by=c("variety", "site_label"), all=TRUE)
	d <- merge(d, fl, by=c("variety", "site_label"), all=TRUE)
	sites <- do.call(rbind, lapply(unique(d$site_label), site_row))
	d <- merge(d, sites, by="site_label", all.x=TRUE)

	d <- data.frame(
		trial_id = as.character(as.integer(as.factor(paste(d$country, d$location, "2017")))),
		crop = "rice",
		variety = d$variety,
		treatment = d$variety,
		country = d$country,
		location = d$location,
		yield = as.numeric(d$yield),
		flowering_days = as.numeric(d$flowering_days),
		plant_height = as.numeric(d$plant_height),
		yield_part = "grain",
		on_farm = FALSE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
