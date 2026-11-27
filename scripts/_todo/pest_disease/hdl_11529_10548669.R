# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Coordinates are not in the workbook; CIMMYT El Batan research station coordinates were georeferenced from the protocol ("Lote G6, El Batan", Texcoco, Estado de Mexico).
# Active ingredient names are mapped from the protocol trade names (Fusilade -> fluazifop-p-butyl, Basagran -> bentazon, sodium salt, Fomesafan -> fomesafen, Pivot -> imazethapyr, ammonium salt); exact product concentrations are not stated, so herbicide product doses (g or ml/ha) cannot be converted to kg a.i./ha.
# The protocol notes two quadrats of 0.50 m x bed width (0.75 m) per plot for weed counts/biomass, but does not confirm whether spreadsheet entries are per quadrat or summed over both quadrats, so counts and dry weights are kept as raw values in the long table rather than converted to per-ha.

## NEW
# herbicide_dose_: product application rate from the protocol (g or ml of formulated product per ha; 0 for unweeded and weed-free checks).
# weed_count_: weed count per plot sample for the given weed_species and date (count per sampling unit).
# weed_dry_weight_: oven-dry weed biomass per plot sample on the given date (g per sampling unit).
# herbicide_product: "fomesafen" is not yet in values_herbicides.csv.

## MISSING
# K_fertilizer, harvest_date (planting_date is year-only "2016").

## NOTES
# Rainfed common bean (cv. Pinto Saltillo) herbicide evaluation trial (RCBD, 2 reps, 10 treatments) on 0.75 m permanent beds at CIMMYT El Batan (Lote G6), 2016.
# Grain yield at 12% moisture is stored in yield (yield_moisture = 12, yield_isfresh = TRUE) alongside yield_dry, dmy_total, dmy_residue, seed_weight (1000-seed dry weight), and harvest_index (%).
# Protocol blanket management: 50 kg/ha seed rate (150,000 seeds/ha), 50 kg N/ha as urea and 40 kg P2O5/ha (17.47 kg P/ha) as TSP applied pre-sowing.
# Repeated weed counts by species (two dates, using field-book Spanish abbreviations plus broadleaf/grass totals H.ANCHA and H.ANGOSTA) and weed dry weights (two dates) are stored in the long table.

## NOT PROCESSED
# Calculations sheet: test weight (g/L), grains per stem, stem dry weight, and single-date 45 DAS summary columns (P_sco_45, H.ACHA45, H.AGOS45) already captured in the weed count/dry weight workbooks.
# Phytotoxicity scores: listed in protocol but absent from all data workbooks.


carob_script <- function(path) {

"
Evaluation of herbicides for weed control in beans in El Batán, México (2016)

Effectiveness of herbicides were compared during 2016 in El Batan, Mexico. The results will be used to make recommendations to farmers for weed control.
"

	uri <- "hdl:11529/10548669"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "herbicide_product;herbicide_dose_;weeding_done",
		response_vars = "yield;yield_dry;dmy_total;dmy_residue;seed_weight;weed_count_;weed_dry_weight_",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	# Treatment table from protocol BV101-16-C-Bean.docx
	key <- data.frame(
		trt = 1:10,
		treatment = c(
			"unweeded check", "weed free", "Prawl (pendimethalin)",
			"Flex BIW (fomesafen)", "Fusilade (fluazifop-p-butyl)",
			"Fusiflex (fomesafen;fluazifop-p-butyl)", "Pivot (imazethapyr) 500",
			"Pivot (imazethapyr) 400", "Pivot (imazethapyr) 300",
			"Basagran (bentazon, sodium salt)"
		),
		herbicide_product = c(
			"none", "none", "pendimethalin", "fomesafen", "fluazifop-p-butyl",
			"fomesafen;fluazifop-p-butyl", "imazethapyr, ammonium salt",
			"imazethapyr, ammonium salt", "imazethapyr, ammonium salt",
			"bentazon, sodium salt"
		),
		herbicide_dose_ = c(0, 0, 2631, 1000, 1000, 1000, 500, 400, 300, 2000),
		herbicide_used = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
		weeding_done = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
	)

	f <- ff[basename(ff) == "DAT-BV101_BEANS-16.xlsx"]
	# Read as text because rows below the data table contain treatment labels in Yield_12%H2O
	r <- carobiner::read.excel(f, sheet="Calculations", col_types="text", na=c("", "NA", "."))
	r <- r[!is.na(r$Rep) & !is.na(r$Trt), ]

	d <- data.frame(
		country = "Mexico",
		adm1 = "México",
		adm2 = "Texcoco",
		location = "El Batán",
		site = "Lote G6",
		# Georeferenced CIMMYT El Batan research station
		latitude = 19.530,
		longitude = -98.849,
		elevation = 2249,
		geo_from_source = FALSE,
		geo_uncertainty = 1000,
		geo_source = "CIMMYT El Batan research station",
		trial_id = "El_Batan_2016",
		plot_id = sprintf("%d%02d", as.integer(r$Rep), as.integer(r$Trt)),
		rep = as.integer(r$Rep),
		crop = "common bean",
		variety = "Pinto Saltillo",
		planting_date = "2016",
		yield_part = "seed",
		yield = as.numeric(r[["Yield_12%H2O"]]),
		yield_moisture = 12,
		yield_isfresh = TRUE,
		yield_dry = as.numeric(r$Yield_Dry),
		dmy_total = as.numeric(r$Biomass),
		dmy_residue = as.numeric(r$Straw),
		seed_weight = as.numeric(r$Thou),
		harvest_index = as.numeric(r$HI),
		# Legend specifies days after emergence
		maturity_days = as.numeric(r$Maturity),
		plot_area = as.numeric(r$Area),
		plot_width = as.numeric(r$WidthArea),
		plot_length = as.numeric(r$LengthArea),
		# Blanket management from protocol BV101-16-C-Bean.docx
		row_spacing = 75,
		seed_rate = 50,
		seed_density = 150000,
		fertilizer_used = TRUE,
		fertilizer_type = "urea;TSP",
		N_fertilizer = 50,
		P_fertilizer = round(40 / 2.29, 2),
		land_prep_method = "permanent beds",
		irrigated = FALSE,
		stress = "weeds",
		on_farm = FALSE,
		is_survey = FALSE,
		trt = as.integer(r$Trt)
	)
	d <- merge(d, key, by="trt", sort=FALSE)
	d$trt <- NULL
	d$record_id <- seq_len(nrow(d))

	# Repeated weed counts by species
	f1 <- ff[basename(ff) == "DAT-BV101-16-WeedCounts_Bean.xlsx"]
	sh_dates <- c("01-Agost-16"="2016-08-01", "19-Ago-16"="2016-08-19")
	counts <- list()
	for (s in names(sh_dates)) {
		w <- carobiner::read.excel(f1, sheet=s, col_names=FALSE, col_types="text", na=c(".", ""))
		hdr <- which(apply(w, 1, function(z) any(toupper(trimws(as.character(z))) == "PARC")))[1]
		nms <- trimws(as.character(unlist(w[hdr, ])))
		body <- w[(hdr + 1):nrow(w), ]
		parc <- as.integer(trimws(body[[1]]))
		ok <- !is.na(parc)
		body <- body[ok, ]
		parc <- parc[ok]
		for (j in seq_len(ncol(body))[-1]) {
			if (is.na(nms[j]) || !nzchar(nms[j])) next
			val <- as.numeric(trimws(body[[j]]))
			keep <- !is.na(val)
			if (!any(keep)) next
			counts[[length(counts) + 1]] <- data.frame(
				plot_id = sprintf("%03d", parc[keep]),
				date = unname(sh_dates[s]),
				weed_species = nms[j],
				weed_count_ = val[keep],
				weed_dry_weight_ = NA_real_
			)
		}
	}

	# Repeated total weed dry weights
	f2 <- ff[basename(ff) == "DAT-BV101-16-WeedDryWeight_Bean.xlsx"]
	w <- carobiner::read.excel(f2, sheet="WeedDryWeight", col_names=FALSE, col_types="text", na=c(".", ""))
	hdr <- which(apply(w, 1, function(z) any(toupper(trimws(as.character(z))) == "PARC")))[1]
	nms <- trimws(as.character(unlist(w[hdr, ])))
	above <- as.numeric(trimws(unlist(w[hdr - 1, ])))
	body <- w[(hdr + 1):nrow(w), ]
	parc_j <- which(toupper(nms) == "PARC")[1]
	parc <- as.integer(trimws(body[[parc_j]]))
	ok <- !is.na(parc)
	for (j in which(grepl("seco|peso", nms, ignore.case=TRUE))) {
		val <- as.numeric(trimws(body[[j]]))
		keep <- ok & !is.na(val)
		if (!any(keep) || is.na(above[j])) next
		counts[[length(counts) + 1]] <- data.frame(
			plot_id = sprintf("%03d", parc[keep]),
			date = as.character(as.Date(above[j], origin="1899-12-30")),
			weed_species = "all weeds",
			weed_count_ = NA_real_,
			weed_dry_weight_ = val[keep]
		)
	}

	long <- do.call(rbind, counts)
	long <- merge(long, d[, c("plot_id", "record_id", "trial_id")], by="plot_id", sort=FALSE)

	carobiner::write_files(path, meta, d, long=long)
}
