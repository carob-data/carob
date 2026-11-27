# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Two raw wheat blast severity values at ZGS 85 exceed 100% in the source workbook (Rawdata1Y plot 9: 100.44%; Rawdata2Y plot 33: 108.10%); kept as recorded for reviewer inspection.
# Exact harvest dates are not recorded in the workbook; harvest_date is set to the harvest year ("2019", "2020", "2021") following March ZGS 85 disease scoring.
# Both surface irrigation (3 dates) and daily sprinkler irrigation from heading were applied; irrigation_method is set to "sprinkler" (terminag allows a single value) and the 3 surface irrigation dates are recorded in irrigation_dates.

## NEW
# lodging_: visual plot lodging score at 62-65 DAS (%).
# wb_visual_severity_: plot-level visual wheat blast severity (%).
# wb_disease_index_: calculated wheat blast disease index, incidence x severity / 100 (%).
# disease values not in terminag: wheat blast.

## NOTES
# Three-season (2018-19 to 2020-21) split-plot wheat cultivar-mixture trial at RARS/BWMRI Jashore, Bangladesh (Krupnik et al. 2024, doi:10.1016/j.cropro.2024.106831).
# Main plots compare fungicide (6 sprays of Nativo 75 WG = tebuconazole + trifloxystrobin) vs. no fungicide; subplots compare 10 pure stands and mixtures of BARI Gom 26, BARI Gom 30, and BARI Gom 33 (5 replications, 300 plots total).
# Yield is taken from Kg/ha with its matching yield moisture (col 53 Ave, not col 47 TGW moisture); basal + top-dress fertilizers are converted to elemental N, P, K, S, B.
# Plot-level wheat blast (incidence, calculated severity, visual severity, disease index) and Bipolaris leaf blight (BpLB = helminthosporium sativum leaf) severity across ZGS 75, 80, and 85 are stored in long with exact scoring dates.

## NOT PROCESSED
# Rawdata1Y/2Y/3Y: individual plant-level spikelet and flag-leaf scores for the 30 tagged plants per plot (plot-level means across the 30 plants are stored in long).
# Agdata1Y/2Y/3Y: moisture-adjusted and lodging-loss-adjusted yields (measured yield, yield_moisture, and lodging_ are kept instead), border inoculation dates, and BpLB AUDPC (derivable from the 3 dated BpLB observations in long).


carob_script <- function(path) {

"
Replication Data for: Mix and manage: Cultivar mixtures can maintain yield under high wheat blast disease pressure

Wheat blast disease, which began in South America, has now spread to Asia and Africa, posing a serious threat to food security. The first outbreak in Bangladesh occurred in 2016, leading to the release of the blast-resistant wheat variety BARI Gom 33. Although seeds for this variety are limited, it remains crucial due to the ongoing threat of the disease. Our research assessed using BARI Gom 33 in mixture with other varieties under high disease pressure. Over three growing seasons, we tested combinations of BARI Gom 26, BARI Gom 30, and BARI Gom 33 to evaluate disease symptoms and crop yield.
"

	uri <- "hdl:11529/10549179"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = "doi:10.1016/j.cropro.2024.106831",
		project = NA,
		design = "split plot",
		data_type = "experiment",
		treatment_vars = "variety;fungicide_used;fungicide_product",
		response_vars = "yield;seed_weight;heading_days;disease_severity;disease_incidence",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "221002 1st, 2nd and 3rd years data of Cultivar Mixture Trial_Jashore.xlsx"]
	num <- function(x) as.numeric(as.character(x))
	as_date <- function(x) as.character(as.Date(num(x), origin="1899-12-30"))

	# Expand BG26/BG30/BG33 cultivar codes to full names and normalize spacing
	clean_var <- function(x) {
		x <- trimws(as.character(x))
		x <- gsub("BG26", "BARI Gom 26", x, fixed=TRUE)
		x <- gsub("BG30", "BARI Gom 30", x, fixed=TRUE)
		x <- gsub("BG33", "BARI Gom 33", x, fixed=TRUE)
		x <- gsub("(?<=30)\\(", " (", x, perl=TRUE)
		x <- gsub("\\+", " + ", x)
		gsub("\\s+", " ", trimws(x))
	}

	# Extract fungicide application dates (DD-MM-YYYY) from column headers 32..37
	parse_fung_dates <- function(hdrs) {
		m <- regmatches(hdrs, regexpr("[0-9]{2}-[0-9]{2}-[0-9]{4}", hdrs))
		paste(as.character(as.Date(m, format="%d-%m-%Y")), collapse="; ")
	}

	one_year <- function(sheet, rawsheet) {
		r1 <- carobiner::read.excel(f, sheet=sheet, col_names=FALSE)
		if (!grepl("Kg/ha", as.character(r1[3, 50]))) stop("yield column moved in ", sheet)
		n <- nrow(r1)
		rows <- 4:n
		plot <- num(r1[rows, 1])
		keep <- !is.na(plot)
		rows <- rows[keep]
		plot <- plot[keep]

		variety <- clean_var(r1[rows, 4])
		fung <- trimws(as.character(r1[rows, 3]))
		fung_used <- fung == "Fungicide"
		fung_dates_str <- parse_fung_dates(as.character(unlist(r1[3, 32:37])))

		pdate <- as_date(r1[rows, 7])
		tddate <- as_date(r1[rows, 14])
		irr_dates <- paste(as_date(r1[rows, 15]), as_date(r1[rows, 16]), as_date(r1[rows, 17]), sep="; ")
		weed_dates <- paste(as_date(r1[rows, 19]), as_date(r1[rows, 20]), sep="; ")
		trial_yr <- substr(pdate[1], 1, 4)
		harv_yr <- as.character(as.integer(trial_yr) + 1L)

		# Basal: urea 150, TSP 135, MoP 110, gypsum 110, boric acid 6.5 kg/ha; top-dress: urea 75 kg/ha
		d <- data.frame(
			country = "Bangladesh",
			adm1 = "Khulna",
			adm2 = "Jessore",
			location = "Jashore",
			site = "RARS Jashore",
			latitude = num(r1[rows, 5]),
			longitude = num(r1[rows, 6]),
			geo_from_source = TRUE,
			trial_id = paste("Jashore", trial_yr, sep="_"),
			plot_id = as.character(as.integer(plot)),
			rep = as.integer(num(r1[rows, 2])),
			variety = variety,
			fungicide_used = fung_used,
			fungicide_product = ifelse(fung_used, "tebuconazole;trifloxystrobin", "none"),
			fungicide_times = ifelse(fung_used, 6L, 0L),
			fungicide_dates = ifelse(fung_used, fung_dates_str, NA_character_),
			treatment = paste(variety, fung, sep="; "),
			crop = "wheat",
			planting_date = pdate,
			# Exact harvest date not recorded; harvest occurs in spring of the following calendar year
			harvest_date = harv_yr,
			plot_area = num(r1[rows, 38]),
			heading_days = num(r1[rows, 39]),
			seed_weight = num(r1[rows, 44]),
			yield_part = "grain",
			yield = num(r1[rows, 50]),
			# Col 53 is mean grain moisture of the yield sample (col 47 is for the 1000-grain weight sample)
			yield_moisture = num(r1[rows, 53]),
			yield_isfresh = TRUE,
			lodging_ = num(r1[rows, 54]),
			N_fertilizer = (150 + 75) * 0.46,
			N_splits = 2L,
			P_fertilizer = 135 * 0.1923,
			K_fertilizer = 110 * 0.498,
			S_fertilizer = 110 * 0.19,
			B_fertilizer = 6.5 * 0.1748,
			fertilizer_type = "urea;TSP;KCl;gypsum;H3BO3",
			fertilizer_date = paste(pdate, tddate, sep="; "),
			irrigated = TRUE,
			irrigation_method = "sprinkler",
			irrigation_dates = irr_dates,
			weeding_done = TRUE,
			weeding_times = 2L,
			weeding_dates = weed_dates,
			stress = "disease",
			on_farm = FALSE,
			is_survey = FALSE
		)
		d$key_id <- paste(d$trial_id, d$plot_id, sep="_")

		# Long table: plot-level wheat blast and Bipolaris leaf blight (BpLB) across ZGS 75, 80, 85
		r2 <- carobiner::read.excel(f, sheet=rawsheet)
		rp <- as.character(as.integer(num(r2[["Plot No"]])))
		kid <- paste(d$trial_id[1], rp, sep="_")
		sc_dates <- list(
			as.character(as.Date(r2[["bplb_1st_Sc_date"]])),
			as.character(as.Date(r2[["bplb_2nd_Sc_date"]])),
			as.character(as.Date(r2[["bplb_3rd_Sc_date"]]))
		)
		zgs <- c("75", "80", "85")
		blocks <- list()
		for (j in seq_along(zgs)) {
			z <- zgs[j]
			wb_sev <- num(r2[[paste0(z, "wb_sev_%")]])
			wb_inc <- num(r2[[paste0(z, "wb_Inc_%")]])
			wb_vis <- num(r2[[paste0(z, "wb_visu-sev%")]])
			wb_di <- num(r2[[paste0(z, "wb_di_%")]])
			bplb <- num(r2[[paste0(z, "bplb_%")]])

			blocks[[length(blocks) + 1]] <- data.frame(
				key_id = kid,
				order = as.integer(j),
				date = sc_dates[[j]],
				growth_stage = paste("ZGS", z),
				disease = "wheat blast",
				disease_incidence = ifelse(is.na(wb_inc), NA_character_, as.character(round(wb_inc, 2))),
				disease_severity = ifelse(is.na(wb_sev), NA_character_, as.character(round(wb_sev, 2))),
				severity_scale = "0-100%",
				wb_visual_severity_ = wb_vis,
				wb_disease_index_ = wb_di
			)
			blocks[[length(blocks) + 1]] <- data.frame(
				key_id = kid,
				order = as.integer(j),
				date = sc_dates[[j]],
				growth_stage = paste("ZGS", z),
				disease = "helminthosporium sativum leaf",
				disease_incidence = NA_character_,
				disease_severity = ifelse(is.na(bplb), NA_character_, as.character(round(bplb, 2))),
				severity_scale = "0-100%",
				wb_visual_severity_ = NA_real_,
				wb_disease_index_ = NA_real_
			)
		}
		long <- do.call(rbind, blocks)
		long <- long[!is.na(long$disease_severity) | !is.na(long$disease_incidence), ]
		list(d=d, long=long)
	}

	y1 <- one_year("Agdata1Y", "Rawdata1Y")
	y2 <- one_year("Agdata2Y", "Rawdata2Y")
	y3 <- one_year("Agdata3Y", "Rawdata3Y")
	d <- rbind(y1$d, y2$d, y3$d)
	long <- rbind(y1$long, y2$long, y3$long)

	d$record_id <- as.integer(seq_len(nrow(d)))
	long$record_id <- as.integer(match(long$key_id, d$key_id))
	long <- long[!is.na(long$record_id), ]
	d$key_id <- NULL
	long$key_id <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
