# R script for "carob"
# license: GPL (>=3)

## NOTES
# Maize grain yield is taken from the 2018 and 2019 replicate columns next to "Grain yield (t/ha)". Means are not used.
# Treatment codes are the sheet's notation. N0, N120 and N150 are the N rates in the definition block (kg/ha).
# PCA+ is partial CA without residue and subsurface drip. CA+ is residue-retained CA and subsurface drip. CA is residue-retained CA with furrow irrigation. CT is conventional tillage, residue removed, furrow irrigation.
# The definition block says residue retention is 25% of the previous wheat residue. The mass is not given.

## ISSUES
# The description places the trial in north-west India. The sheet has no country, site, or coordinates, and geographicCoverage is empty.
# Phosphorus and potassium rates are not in the sheet. Straw yield is not labeled fresh or dry.
# write_files: missing variables: yield_moisture, country, latitude, longitude, K_fertilizer, P_fertilizer, harvest_date.

## NOT PROCESSED
# Straw yield, biological yield, cobs, yield components, photosynthesis, nitrogen uptake, water productivity, and economics.
# Residue mass.


carob_script <- function(path) {

"
Appraisal of complementarity of subsurface drip fertigation and conservation agriculture for physiological performance and water economy of maize

The Indo-Gangetic Plains (IGP) in north-west (NW) India are facing a severe decline in ground water due to prevalent rice-based cropping systems. To combat this issue, conservation agriculture (CA) with an alternative crop/s, such as maize, is being promoted. Recently, surface drip fertigation has also been evaluated as a viable option to address low-nutrient use efficiency and water scarcity problems for cereals. While the individual benefits of CA and sub-surface drip (SSD) irrigation on water economy are well-established, information regarding their combined effect in cereal-based systems is lacking. Therefore, we conducted a two-year field experiment in maize, under an ongoing CA-based maize-wheat system, to evaluate the complementarity of CA with SSD irrigation through two technological interventions–– CA+ (residue retained CA + SSD), PCA+ (partial CA without residue + SSD) – at different N rates (0, 120 and 150 kg N ha-1) in comparison to traditional furrow irrigated (FI) CA and conventional tillage (CT) at 120 kg N ha-1.
"

	uri <- "hdl:11529/10548914"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;land_prep_method;irrigation_method;residue_prevcrop_used",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "AGWAT_Kiran_Data.xlsx"]
	m <- as.matrix(carobiner::read.excel(f, sheet="Sheet1", col_types="text"))

	# The notation sometimes sits on the row below the factor cells.
	code_rows <- which(!is.na(m[, 6]) & grepl("^N[0-9]", m[, 6]))
	code_rows <- code_rows[code_rows < which(m[, 1] == "Treatment")[1]]
	src <- code_rows
	blank_n <- is.na(m[code_rows, 5]) | !nzchar(m[code_rows, 5])
	src[blank_n] <- code_rows[blank_n] - 1
	tillage <- m[src, 1]
	residue_txt <- m[src, 2]
	irr_txt <- m[src, 3]
	land <- ifelse(grepl("Conventional", tillage), "conventional", "permanent beds")
	residue <- grepl("WR", residue_txt)
	water <- ifelse(grepl("SSD|Subsurface", irr_txt), "subsurface drip", "furrow")
	facts <- data.frame(
		treatment = m[code_rows, 6],
		N_fertilizer = as.numeric(m[src, 5]),
		land_prep_method = land,
		residue_prevcrop_used = residue,
		irrigation_method = water
	)

	y2018 <- which(m == "Yield data 2018", arr.ind=TRUE)[1]
	y2019 <- which(m == "Yield data 2019", arr.ind=TRUE)[1]
	gy <- which(m == "Grain yield (t/ha)", arr.ind=TRUE)
	blocks <- list()
	for (b in seq_len(nrow(gy))) {
		i <- gy[b, 1]
		j <- gy[b, 2]
		hdr <- m[i + 1, ]
		rcols <- which(hdr %in% c("R1", "R2", "R3"))
		rcols <- rcols[rcols >= j]
		rcols <- rcols[seq_len(3)]
		year <- if (!is.na(y2019) && i > y2019) "2019" else "2018"
		rows <- seq.int(i + 2, nrow(m))
		for (k in rows) {
			trt <- m[k, j - 1]
			if (is.na(trt) || !nzchar(trt)) break
			if (grepl("^Yield data", trt)) break
			for (rc in rcols) {
				blocks[[length(blocks) + 1]] <- data.frame(
					treatment = trt,
					rep = as.integer(sub("R", "", hdr[rc])),
					yield = as.numeric(m[k, rc]) * 1000,
					planting_date = year
				)
			}
		}
	}
	d <- do.call(rbind, blocks)
	d <- merge(d, facts, by="treatment", all.x=TRUE)
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$trial_id <- d$planting_date
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
