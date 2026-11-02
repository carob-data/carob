# R script for "carob"
# license: GPL (>=3)

## NOTES
# Wheat blast cultivar-mixture trial at Jashore, Bangladesh, three seasons. The file name names Jashore. Geographic coverage is Bangladesh. Coordinates are in the agronomy sheets.
# Main plots are fungicide or none. Subplots are the cultivar mixtures. The workbook labels these main plot and subplot.
# Basal fertilizer is the same on every plot: urea 150, TSP 135, MoP 110, gypsum 110, and boric acid 6.5 kg/ha, plus 75 kg/ha urea as a top dressing.
# Element contents are the terminag fractions: urea 46 percent N, TSP 19.23 percent P, KCl 49.8 percent K, gypsum 19 percent S, boric acid 17.48 percent B.
# Yield is the Kg/ha column. The g/HA figure is the harvested sample, not a per-hectare yield. Moisture is the mean of the two grain samples beside that yield.
# Plot-level blast severity is the three wb_sev columns in Rawdata. The 30-plant scores are not copied.

## ISSUES
# Wheat blast is not in the disease vocabulary. It is left as recorded. Harvest date is not in the sheet.
# write_files: missing variables: harvest_date. invalid terms: disease: wheat blast. contributor:

## NOT PROCESSED
# Individual plant spike and leaf scores (30 plants at three counts).
# Lodging-adjusted and moisture-adjusted yield. Measured yield and moisture are kept instead.
# Inoculation, irrigation, and weeding dates.


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
		treatment_vars = "variety;fungicide_used",
		response_vars = "yield;disease_severity",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.7
	)

	f <- ff[grepl("\\.xlsx$", basename(ff))][1]
	num <- function(x) as.numeric(as.character(x))
	as_date <- function(x) as.character(as.Date(num(x), origin="1899-12-30"))

	one_year <- function(sheet, rawsheet) {
		a <- carobiner::read.excel(f, sheet=sheet, col_names=FALSE)
		if (!grepl("Kg/ha", as.character(a[3, 50]))) stop("yield column moved in ", sheet)
		n <- nrow(a)
		plot <- num(a[4:n, 1])
		keep <- !is.na(plot)
		variety <- trimws(as.character(a[4:n, 4]))
		fung <- trimws(as.character(a[4:n, 3]))
		d <- data.frame(
			plot_id = as.character(plot),
			rep = as.integer(num(a[4:n, 2])),
			variety = variety,
			fungicide_used = fung == "Fungicide",
			treatment = paste(variety, fung, sep="; "),
			latitude = num(a[4:n, 5]),
			longitude = num(a[4:n, 6]),
			planting_date = as_date(a[4:n, 7]),
			plot_area = num(a[4:n, 38]),
			heading_days = num(a[4:n, 39]),
			seed_weight = num(a[4:n, 44]),
			yield_moisture = num(a[4:n, 47]),
			yield = num(a[4:n, 50]),
			country = "Bangladesh",
			location = "Jashore",
			crop = "wheat",
			yield_part = "grain",
			stress = "disease",
			N_fertilizer = (150 + 75) * 0.46,
			P_fertilizer = 135 * 0.1923,
			K_fertilizer = 110 * 0.498,
			S_fertilizer = 110 * 0.19,
			B_fertilizer = 6.5 * 0.1748,
			fertilizer_type = "urea; TSP; KCl; gypsum; H3BO3"
		)
		d <- d[keep, ]
		d$trial_id <- paste("Jashore", substr(d$planting_date, 1, 4), sep="_")
		d$record_id <- paste(d$trial_id, d$plot_id, sep="_")

		raw <- carobiner::read.excel(f, sheet=rawsheet)
		sev_names <- grep("wb_sev_%$", names(raw), value=TRUE)
		sev_names <- sev_names[order(sev_names)]
		long <- NULL
		if (length(sev_names) > 0) {
			rp <- as.character(raw[["Plot No"]])
			blocks <- list()
			for (j in seq_along(sev_names)) {
				blocks[[j]] <- data.frame(
					record_id = paste(d$trial_id[1], rp, sep="_"),
					order = j,
					disease = "wheat blast",
					disease_severity = as.character(num(raw[[sev_names[j]]]))
				)
			}
			long <- do.call(rbind, blocks)
			long <- long[!is.na(long$disease_severity), ]
			long <- long[long$record_id %in% d$record_id, ]
		}
		list(d=d, long=long)
	}

	y1 <- one_year("Agdata1Y", "Rawdata1Y")
	y2 <- one_year("Agdata2Y", "Rawdata2Y")
	y3 <- one_year("Agdata3Y", "Rawdata3Y")
	d <- rbind(y1$d, y2$d, y3$d)
	long <- rbind(y1$long, y2$long, y3$long)
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$geo_from_source <- TRUE
	d$trial_id <- paste(d$location, substr(d$planting_date, 1, 4), sep="_")
	key <- paste(d$trial_id, d$plot_id, sep="_")
	long$record_id <- match(long$record_id, key)
	long <- long[!is.na(long$record_id), ]
	d$record_id <- as.integer(seq_len(nrow(d)))
	long$record_id <- as.integer(long$record_id)

	carobiner::write_files(path, meta, d, long=long)
}
