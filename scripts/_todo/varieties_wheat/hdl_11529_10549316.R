# R script for "carob"
# license: GPL (>=3)

## NOTES
# Wheat yield trials of BNI lines at El Batan, Obregon, and Toluca. The data dictionary describes LOCATION as the field trial in Mexico. geographicCoverage is empty.
# Grain weight is grams per plot. The yield sheet states the plot as 2 beds by a width and 4 m. Yield is that weight divided by the plot area.
# El Batan beds are 0.75 m, Obregon 0.8 m, Toluca 0.76 m.
# Flowering and maturity are days from sowing where those columns exist. Plant height is cm.
# Experiment 1 and experiment 2 are kept as separate trials. Nitrogen rate is named in the dictionary and is not a column on the yield sheets.

## ISSUES
# No coordinates.
# Quality sheets (protein, test weight) and soil nitrogen sheets are not joined.
# Toluca 2024 and 2025 put the grain-weight label in the column name and the plot size on the next row. Variety is parentage or cross name when that column exists, otherwise the female cross or germplasm name.
# missing variables: yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date

## NOT PROCESSED
# Stem weights. They are grams per 100 stems, and stems per hectare are not given.
# Quality workbooks and soil nitrogen sheets.
# The data-dictionary file, used here for the yield definition.


carob_script <- function(path) {

"
Grain Yield and Grain Protein data of CIMMYT Elite Spring Wheat lines with the BNI short arm translocation (T3BL.3NsbS) from Leymus racemosus.
"

	uri <- "hdl:11529/10549316"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "BNI-wheat",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.8
	)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "Grain weight (g) plot-1")] <- NA_character_
		x[grepl("bed", x, ignore.case=TRUE)] <- NA_character_
		as.numeric(x)
	}
	area_of <- function(txt) {
		txt <- txt[grepl("bed", txt, ignore.case=TRUE)][1]
		beds <- as.numeric(sub(".*?([0-9.]+)\\s*beds.*", "\\1", txt, ignore.case=TRUE))
		dims <- regmatches(txt, gregexpr("[0-9.]+\\s*m", txt, ignore.case=TRUE))[[1]]
		dims <- as.numeric(gsub("[^0-9.]", "", dims))
		beds * dims[1] * dims[2]
	}
	place_year <- function(bn) {
		loc <- NA_character_
		if (grepl("Batan", bn, ignore.case=TRUE)) loc <- "El Batan"
		if (grepl("Obregon", bn, ignore.case=TRUE)) loc <- "Obregon"
		if (grepl("Toluca", bn, ignore.case=TRUE)) loc <- "Toluca"
		yr <- sub(".*_([0-9]{4}).*", "\\1", bn)
		c(loc, yr)
	}
	one <- function(f, sheet) {
		raw <- as.data.frame(carobiner::read.excel(f, sheet=sheet))
		if ("Grain weight (g) plot-1" %in% names(raw)) {
			top <- raw[1, ]
			top[1, ] <- names(raw)
			raw <- rbind(top, raw)
		}
		cells <- as.matrix(raw)
		cells[] <- trimws(as.character(cells))
		at <- function(pattern, fixed=TRUE) {
			m <- matrix(grepl(pattern, cells, fixed=fixed), nrow=nrow(cells))
			which(m, arr.ind=TRUE)
		}
		gw <- at("Grain weight (g) plot-1")
		if (nrow(gw) == 0) return(NULL)
		grow <- gw[1, 1]
		gcol <- gw[1, 2]
		beds_at <- at("bed", fixed=FALSE)
		area <- area_of(cells[beds_at])
		id_names <- c("parentage", "cross name", "f_cross_name", "germplasm name")
		flat <- tolower(cells)
		id_at <- matrix(numeric(), ncol=2)
		for (w in id_names) {
			hit <- which(matrix(flat == w, nrow=nrow(flat)), arr.ind=TRUE)
			if (nrow(hit) > 0) {
				id_at <- hit
				break
			}
		}
		if (nrow(id_at) == 0) return(NULL)
		hrow <- id_at[1, 1]
		vcol <- id_at[1, 2]
		hdr <- trimws(as.character(unlist(raw[hrow, ])))
		repcol <- match(c("Rep", "REP"), hdr)
		repcol <- repcol[!is.na(repcol)][1]
		plotcol <- match(c("Plot", "PLOT NO.", "PLOT CODE"), hdr)
		plotcol <- plotcol[!is.na(plotcol)][1]
		desc <- if (hrow > 1) trimws(as.character(unlist(raw[hrow - 1, ]))) else rep(NA_character_, ncol(raw))
		flcol <- match("Flowering", desc)
		if (is.na(flcol)) flcol <- match("Flowering", hdr)
		mtcol <- match("Maturity", desc)
		if (is.na(mtcol)) mtcol <- match("Maturity", hdr)
		htcol <- match("Plant height", desc)
		if (is.na(htcol)) htcol <- match("Plant height", hdr)
		dat <- raw[(max(hrow, grow) + 1):nrow(raw), ]
		variety <- trimws(as.character(dat[[vcol]]))
		variety[variety %in% c("", "NA", id_names)] <- NA_character_
		keep <- !is.na(variety)
		py <- place_year(basename(f))
		exp <- if (grepl("Exp_2", sheet)) "2" else "1"
		n <- sum(keep)
		flow <- rep(NA_real_, n)
		matu <- rep(NA_real_, n)
		pht <- rep(NA_real_, n)
		if (!is.na(flcol)) flow <- num(dat[[flcol]][keep])
		if (!is.na(mtcol)) matu <- num(dat[[mtcol]][keep])
		if (!is.na(htcol)) pht <- num(dat[[htcol]][keep])
		reps <- rep(NA_integer_, n)
		plots <- rep(NA_character_, n)
		if (!is.na(repcol)) reps <- as.integer(num(dat[[repcol]][keep]))
		if (!is.na(plotcol)) plots <- trimws(as.character(dat[[plotcol]][keep]))
		data.frame(
			country = "Mexico",
			location = py[1],
			trial_id = paste(py[1], py[2], "exp", exp),
			planting_date = py[2],
			crop = "wheat",
			variety = variety[keep],
			treatment = variety[keep],
			rep = reps,
			plot_id = paste(py[1], py[2], exp, plots, reps),
			yield = num(dat[[gcol]][keep]) / area * 10,
			yield_part = "grain",
			plot_area = area,
			flowering_days = flow,
			maturity_days = matu,
			plant_height = pht,
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	sheets <- list(
		"Yield_Trial+_El Batan_2023.xls" = c("BATAN_YIELD_2023_Exp_1", "BATAN_YIELD_2023_Exp_2"),
		"Yield_Trial+_El Batan_2024.xlsx" = c("BATAN_YIELD_2024_Exp_1", "BATAN_YIELD_2024_Exp_2"),
		"Yield_Trial+_El Batan_2025.xlsx" = c("BATAN_YIELD_2025_Exp_1", "BATAN_YIELD_2025_Exp_2"),
		"Yield_Trial+_Obregon_2023_24.xlsx" = c("YIELD_OBREGON_2023_Exp_1", "YIELD_OBREGON_2023_Exp_2"),
		"Yield_Trial+_Obregon_2024_25.xlsx" = c("YIELD_OBREGON_2024_Exp_1", "YIELD_OBREGON_2024_Exp_2"),
		"Yield_Trial+_Toluca_2023.xlsx" = c("YIELD_TOLUCA_2023_Exp_1", "YIELD_TOLUCA_2023_Exp_2"),
		"Yield_Trial+_Toluca_2024.xlsx" = c("Yield_Toluca_2024_Exp_1", "Yield_Toluca_2024_Exp_2"),
		"Yield_Trial+_Toluca_2025.xlsx" = c("Yield_Toluca_2025_Exp_1", "Yield_Toluca_2025_Exp_2")
	)
	blocks <- list()
	for (bn in names(sheets)) {
		f <- ff[basename(ff) == bn]
		for (s in sheets[[bn]]) blocks[[length(blocks) + 1]] <- one(f, s)
	}
	blocks <- blocks[!vapply(blocks, is.null, TRUE)]
	d <- do.call(carobiner::bindr, blocks)
	d <- d[!is.na(d$yield), ]
	d$record_id <- as.integer(seq_len(nrow(d)))
	carobiner::write_files(path, meta, d)
}
