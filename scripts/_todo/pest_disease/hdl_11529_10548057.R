# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-station direct-seeded aman rice herbicide trials, SRFSI, 2015 and 2016. The title says the trials are on station.
# Grain yield is t/ha, stored as kg/ha.
# Pre-emergence and post-emergence herbicides are the treatments. "No pre-emergence", "No post emergence", "No-spray", and a dash are stored as none.
# Pendimethalin is spelled Pendamethalin in the files. Fenoxaprop is spelled Fenaxoprop or Fenoxoprop. Doses are not stored: only pendimethalin states 1000 g a.i./ha, and the others do not give an active-ingredient rate.
# Grain moisture above 100 is not a percent and is not stored. Plant height 0 is empty.
# Fertilizer, weed biomass, and straw are not stored. The workbooks do not share one column layout for those blocks.

## ISSUES
# write_files: missing variables: latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# write_files: invalid terms: herbicide_product: ethoxysulfuron
# ethoxysulfuron is the name in the file. The vocabulary does not list it.

## NOT PROCESSED
# Fertilizer rates. The application columns are not in one layout across the workbooks.
# Weed biomass, straw weight, phenology dates, row spacing, and seed rate.
# Weed counts, visual scores, and species-level biomass.
# Irrigation, ponding, pesticide logs, prices, and daily rainfall.


carob_script <- function(path) {

"
8.1-Direct seeded rice weed management-on-station research trials-SRFSI Project-ACIAR-CIMMYT

Dry direct-seeded rice herbicide trials on research stations, comparing pre-emergence and post-emergence weed-control options.
"

	uri <- "hdl:11529/10548057"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=5,
		data_organization = "CIMMYT",
		publication = NA,
		project = "SRFSI",
		design = NA,
		data_type = "experiment",
		treatment_vars = "herbicide_product",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.8
	)

	as_date <- function(x) {
		if (is.null(x)) return(NA_character_)
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		if (is.numeric(x)) return(as.character(as.Date(x, origin="1899-12-30")))
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		out <- rep(NA_character_, length(x))
		iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
		if (any(iso)) out[iso] <- substr(x[iso], 1, 10)
		out
	}
	grab <- function(df, nm) {
		if (!nm %in% names(df)) return(rep(NA_real_, nrow(df)))
		num(df[[nm]])
	}
	grab_date <- function(df, nm) {
		if (!nm %in% names(df)) return(rep(NA_character_, nrow(df)))
		as_date(df[[nm]])
	}
	num <- function(x) {
		if (is.null(x)) return(NA_real_)
		if (is.numeric(x) && !inherits(x, "POSIXt")) return(as.numeric(x))
		x <- trimws(as.character(x))
		x[x %in% c("", "-", "NA")] <- NA_character_
		as.numeric(x)
	}
	header_value <- function(cells, label) {
		i <- which(cells == label)
		if (!length(i)) return(NA_character_)
		rest <- cells[(i[1] + 1):length(cells)]
		rest <- rest[!is.na(rest) & nzchar(rest)]
		if (!length(rest)) return(NA_character_)
		rest[1]
	}
	pre_ai <- function(x) {
		x <- as.character(x)
		out <- rep(NA_character_, length(x))
		out[grepl("No pre|No-spray|^-$", x)] <- "none"
		out[grepl("Pend", x)] <- "pendimethalin"
		out[grepl("Oxadiazon", x)] <- "oxadiazon"
		out[grepl("Pretilachlor", x)] <- "pretilachlor"
		out
	}
	post_ai <- function(x) {
		x <- as.character(x)
		out <- rep(NA_character_, length(x))
		out[grepl("No post|No-spray|^-$", x)] <- "none"
		out[grepl("Pyrazosulfuron", x)] <- "bispyribac-sodium;pyrazosulfuron"
		out[grepl("Ethoxysulfuron", x)] <- "fenoxaprop-p-ethyl;ethoxysulfuron"
		out[grepl("Halosulfuron", x)] <- "fenoxaprop-p-ethyl;halosulfuron-methyl"
		out[grepl("Bispyribac", x) & is.na(out)] <- "bispyribac-sodium"
		out
	}
	join_ai <- function(a, b) {
		out <- paste(a, b, sep=";")
		out <- gsub("(^none;|;none$)", "", out)
		out[a == "none" & b == "none"] <- "none"
		out[is.na(a) | is.na(b)] <- NA_character_
		out
	}

	one <- function(f) {
		a <- carobiner::read.excel(f, sheet="16.WeedBio & cropData (harvest)", col_names=FALSE)
		hr <- which(as.character(a[[1]]) == "Year")[1]
		h1 <- as.character(unlist(a[hr, ]))
		h2 <- as.character(unlist(a[hr + 1, ]))
		h1[is.na(h1)] <- ""
		h2[is.na(h2)] <- ""
		at <- function(pat, row="h1") {
			src <- if (row == "h2") h2 else h1
			i <- which(grepl(pat, src, ignore.case=TRUE))
			if (!length(i)) return(NA_integer_)
			i[1]
		}
		pull <- function(i) {
			if (is.na(i)) return(rep(NA, nrow(a)))
			a[[i]]
		}
		year <- as.numeric(as.character(a[[1]]))
		ok <- !is.na(year) & year >= 2000 & year <= 2100
		subv <- function(v) v[ok]
		pre <- pre_ai(subv(pull(at("pre-emergence herb"))))
		post <- post_ai(subv(pull(at("Post-emergence herb"))))
		prod <- join_ai(pre, post)
		yi <- which(grepl("Grain yield \\(t/ha\\)", h2))
		yi <- if (length(yi)) yi[1] else NA_integer_
		m1 <- which(grepl("sample1", h2) & grepl("MC%", h2))
		m2 <- which(grepl("sample 2|sample2", h2) & grepl("MC%", h2))
		mc1 <- num(subv(pull(if (length(m1)) m1[1] else NA_integer_)))
		mc2 <- num(subv(pull(if (length(m2)) m2[1] else NA_integer_)))
		mc <- (mc1 + mc2) / 2
		mc[is.na(mc1)] <- mc2[is.na(mc1)]
		mc[is.na(mc2)] <- mc1[is.na(mc2)]
		mc[!is.na(mc) & mc <= 1] <- mc[!is.na(mc) & mc <= 1] * 100
		loc <- as.character(subv(pull(at("^Name of research station"))))
		pid <- as.character(subv(pull(at("^Plot Id$"))))
		trial <- as.character(subv(pull(at("^Trial Code$"))))
		repn <- as.character(subv(pull(at("^Replication$"))))
		abbr <- paste(subv(pull(at("^Main plotTmnt"))), subv(pull(at("^Sub plotTmnt"))))
		abbr[grepl("NA", abbr)] <- prod[grepl("NA", abbr)]
		tgw <- which(grepl("^TGW", h2))
		pht <- which(h2 == "Plant height (cm)")
		psz <- at("^Plot size")
		si <- tryCatch(carobiner::read.excel(f, sheet="2 - Site information", col_names=FALSE), error=function(e) NULL)
		raw_c <- NA_character_
		adm2 <- NA_character_
		if (!is.null(si)) {
			cells <- trimws(as.character(unlist(si[1, ])))
			cells[is.na(cells)] <- ""
			raw_c <- header_value(cells, "Country:")
			adm2 <- header_value(cells, "District:")
		}
		country <- raw_c
		adm1 <- NA_character_
		if (!is.na(raw_c) && toupper(raw_c) %in% c("INDIA", "BIHAR")) {
			country <- "India"
			if (toupper(raw_c) == "BIHAR") adm1 <- "Bihar"
		}
		if (!is.na(raw_c) && raw_c == "West Bengal") {
			country <- "India"
			adm1 <- "West Bengal"
		}
		if (!is.na(raw_c) && toupper(raw_c) == "NEPAL") country <- "Nepal"
		if (!is.na(raw_c) && toupper(raw_c) == "BANGLADESH") country <- "Bangladesh"
		n <- sum(ok)
		data.frame(
			country = country,
			adm1 = adm1,
			adm2 = carobiner::fix_name(adm2, "title"),
			location = loc,
			trial_id = paste(subv(pull(1)), trial, loc),
			plot_id = pid,
			rep = as.integer(repn),
			treatment = abbr,
			herbicide_product = prod,
			herbicide_used = prod != "none",
			crop = "rice",
			yield_part = "grain",
			yield = num(subv(pull(yi))) * 1000,
			yield_moisture = mc,
			seed_weight = num(subv(pull(if (length(tgw)) tgw[1] else NA_integer_))),
			plant_height = num(subv(pull(if (length(pht)) pht[1] else NA_integer_))),
			plot_area = num(subv(pull(psz))),
			stress = "weeds",
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	ff <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	d <- do.call(rbind, lapply(ff, one))
	d$yield_moisture[!is.na(d$yield_moisture) & d$yield_moisture > 100] <- NA_real_
	d$plant_height[!is.na(d$plant_height) & d$plant_height <= 0] <- NA_real_
	d <- d[!is.na(d$herbicide_product), ]
	carobiner::write_files(path, meta, d)
}
