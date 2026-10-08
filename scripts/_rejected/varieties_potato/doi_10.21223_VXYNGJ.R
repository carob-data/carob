# R script for "carob"
# license: GPL (>=3)

### REJECT - There is no response. Just mentioned in the dictionary but not reproted in data

## NOTES
# virus indexing of potato seed lots sampled at CIP Muguga (Kenya), 2018 (dates from data dictionary)
# 80 lots, each tested for PLRV, PVA, PVM, PVS, PVX and PVY
# one row per lot x virus test
# two lot series ("SAMPLE 1-28", codes SV-180283..310; "1-52", codes SV-180488..539) kept as trial_id 1 and 2
# lab sample codes (SAMPLE_CODE) not kept
# no associated publication found

## NEW VARIABLES
# seed_lot_: seed lot number (LOT_NO); some clones occur in several lots

## ISSUES
# RESULT column (test outcome) described in the data dictionary is not in the data file, so there is no response;
# lab test of seed lots, not a field trial; no data_type value fits, left NA
# lot SAMPLE 25 (Shangi) has no PVA test under its own code; its PVA row is coded SV-180308 (lot SAMPLE 26)
# lot 42 (CIP311109.616) has no PVY test
# variety "SHANGl" fixed to "SHANGI"
# variety_code "CIP39337.164" probably misses a digit (CIP393337.164 in the old germplasm list)
# which clones are Rwanda local, Ethiopian 2x biofortified or Kenyan LTVR x LBHT is not given


carob_script <- function(path) {

"
Dataset for: Cleaning up at least 10 Rwanda local varieties, 30 2x biofortified potato introduced from Ethiopia and 5 LTVR x LBHT clones selected in Kenya for further distribution.

Cleaning up at least 10 Rwanda local varieties, 30 2x biofortified potato introduced from Ethiopia and 5 LTVR x LBHT clones selected in Kenya for further distribution.
"

	uri <- "doi:10.21223/VXYNGJ"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "none",
		response_vars = NA, # test results not in the data
		carob_contributor = "Stella Muthoni",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-10-04",
		carob_completion = 80,
		carob_effort = 0.5
	)

	f1 <- ff[basename(ff) == "12533_BATCH1.xlsx"]
	r1 <- carobiner::read.excel(f1)

	viruses <- c(PLRV="potato leafroll virus", PVA="potato virus A", PVM="potato virus M",
		PVS="potato virus S", PVX="potato virus X", PVY="potato virus Y")

	# one row per lot x virus test
	d <- data.frame(
		trial_id = ifelse(grepl("SAMPLE", r1$LOT_NO), "1", "2"),
		seed_lot_ = trimws(r1$LOT_NO),
		crop = "potato",
		variety_code = trimws(r1$ACCESSION_CIPNUMBER),
		variety = trimws(r1$VARIETY_NAME),
		disease = unname(viruses[r1$TEST_DONE]),
		country = "Kenya",
		location = "Muguga",
		site = "CIP Muguga"
	)
	d$variety[d$variety == "SHANGl"] <- "SHANGI"

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$longitude <- 36.6661 # from geocode
	d$latitude <- -1.1994 # from geocode
	d$geo_from_source <- FALSE

	# not reported
	d$planting_date <- as.character(NA)
	d$harvest_date <- as.character(NA)
	d$irrigated <- as.logical(NA)
	d$N_fertilizer <- as.numeric(NA)
	d$P_fertilizer <- as.numeric(NA)
	d$K_fertilizer <- as.numeric(NA)
	d$yield <- as.numeric(NA) # not measured
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- as.logical(NA)
	d$yield_part <- "tubers"

	carobiner::write_files(path, meta, d)
}
