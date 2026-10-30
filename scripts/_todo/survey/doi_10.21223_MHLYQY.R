# R script for "carob"
# license: GPL (>=3)

## NOTES
# Willingness-to-pay auctions for sweetpotato planting material in Rwanda. Country Rwanda is the geographic coverage. District, sector, cell, and village are columns. District is stored as adm2 and sector as adm3 (Rwanda's province is not in the file). Village is the location. The sheets use capital letters for place names; those are written in title case.
# Each bidding sheet is a round. p1, p2, and p3 are the bids in that round. The sheet does not name the currency, so the bids stay as bid_.
# The crop is sweetpotato, including orange-fleshed varieties, from the description. Beta-carotene was not measured.

## ISSUES
# No coordinates.
# Bid currency is not stated.
# write_files: missing carob_contributor, latitude, longitude. unknown variable: bid_.

## NEW VARIABLES
# bid_: auction bid as recorded. Currency is not on the sheet.
# bid_index_: bid number within the round (1, 2, or 3).
# auction_round_: bidding-sheet round.

## NOT PROCESSED
# monitoring.xls: the monitoring questionnaire (demographics and perceptions), not the auction prices.
# The docx bidding forms and the questionnaire document.


carob_script <- function(path) {

"
Dataset for: Eliciting farmers’ demand for quality and nutritionally enhanced sweetpotato planting material in Rwanda

Sweetpotato (Ipomoea batatas L.) is an important staple and food security crop in Rwanda. It is widely consumed by many households, mostly in boiled form . Per capita sweetpotato production in Rwanda is among the highest in SSA. Sweetpotato, especially the biofortified orange-fleshed varieties with proven efficacy , can play a greater role on the immediate and long-term food and nutrition security in Rwanda. However, improving the productivity of sweetpotato is hindered by several factors, most importantly, the access to quality planting material. The existing material, mainly sourced informally from farmers, is recycled, hence oftentimes heavily infected with yield-degenerating virus diseases and pests (especially weevils). To date, however, projects and NGOs are the major buyers of quality vines from Descentralized Vine Multipliers while the majority of farmers obtain their sweetpotato planting material from own sources or neighboring farmers. This leads to the following research questions: what premium price are farmers willing to pay for quality vines and what are the drivers of the demand for quality vines? Are farmers willing to pay a premium for the high beta carotene nutritious biofortified varieties? The database present the data of the a willingness to pay (WTP) approach using auctions. Based on this design, each participant had a total of 11 bids (3 bids in Round 1 and 2 bids in the subsequent 4 rounds). In addition to estimating the average WTP for quality seed and a biofortified sweetpotato (i.e., OFSP) variety, econometric models will be estimated to examine the factors that affect WTP for quality seed and the biofortified sweetpotato variety.
"

	uri <- "doi:10.21223/MHLYQY"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.35
	)

	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(format(as.Date(x), "%Y-%m-%d"))
		# The xls stores the day as an integer ddmmyyyy, sometimes with a one-digit day.
		if (is.numeric(x)) {
			z <- sprintf("%08d", as.integer(round(x)))
			out <- paste0(substr(z, 5, 8), "-", substr(z, 3, 4), "-", substr(z, 1, 2))
			out[is.na(x)] <- NA_character_
			return(out)
		}
		trimws(as.character(x))
	}
	txt <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z)] <- NA_character_
		z
	}

	files <- ff[grepl("^biding_sheet_[0-9]", basename(ff))]
	wide <- list()
	long <- list()
	for (f in files) {
		r <- carobiner::read.excel(f)
		round <- as.integer(gsub("\\D", "", basename(f)))
		id <- txt(r$farmer_id)
		wide[[length(wide) + 1]] <- data.frame(
			hhid = id,
			adm2 = txt(r$district_name),
			adm3 = txt(r$sector_name),
			location = txt(r$vill_name),
			stringsAsFactors = FALSE
		)
		pcols <- grep("^p[0-9]+$", names(r), value=TRUE)
		for (i in seq_along(pcols)) {
			long[[length(long) + 1]] <- data.frame(
				hhid = id,
				date = as_date(r$date),
				auction_round_ = round,
				bid_index_ = i,
				bid_ = as.numeric(r[[pcols[i]]]),
				stringsAsFactors = FALSE
			)
		}
	}
	d <- do.call(rbind, wide)
	d <- d[!duplicated(d$hhid), ]
	# The sheets store place names in capitals.
	nice <- function(x) {
		ifelse(!is.na(x) & toupper(x) == x, tools::toTitleCase(tolower(x)), x)
	}
	d$adm2 <- nice(d$adm2)
	d$adm3 <- nice(d$adm3)
	d$location <- nice(d$location)
	d$country <- "Rwanda"
	d$crop <- "sweetpotato"
	d$geo_from_source <- FALSE
	d$is_survey <- TRUE
	d$on_farm <- NA
	d$trial_id <- "Rwanda_sweetpotato_WTP"
	dlong <- do.call(rbind, long)
	dlong <- dlong[dlong$hhid %in% d$hhid, ]
	rownames(d) <- NULL
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
