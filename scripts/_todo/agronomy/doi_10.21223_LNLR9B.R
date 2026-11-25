# R script for "carob"
# license: GPL (>=3)

## NOTES
# Cool Farm Tool records for three UNICA potato trials at CIP La Molina (E1-CIP: 2017-07-05 to 2017-10-10; E2-CIP: 2017-10-05 to 2018-01-15) and UNALM (E3-UNALM: 2018-06-13 to 2018-09-13), Lima, Peru.
# Dictionary: crop area is m2; harvested, rejected, seed, and farm-gate amounts are kg. Yield (kg/ha) = harvested kg / m2 * 10000; yield_marketable uses farm-gate ready amount on the same basis. Residue amount is dry matter in t/ha (converted to dmy_residue in kg/ha).
# Mineral fertilizer nutrient fractions come from the product names in Fertiliser inputs (P = P2O5 / 2.29; K = K2O / 1.2051). Phosphoric acid is reported in "units of P2O5", so it is divided by 2.29 without multiplying by 85%. Magnesium nitrate hexahydrate (30 kg product/ha) has no N/Mg % printed in the sheet and is excluded from N_fertilizer.
# Sheep digestate (0.7% N) is an organic input: its mass is stored in OM_amount (0 when not applied) and its N in N_organic (not mineral N_fertilizer).
# Weekly water columns (TW_Week 1..14) are litres per plot (dictionary). Seasonal irrigation_amount (mm) = sum(litres) / plot_area; weekly depths are preserved in the long table.
# "Furrow irrigation or Flooding" in the Cool Farm Tool Water sheet corresponds to furrow irrigation (confirmed by the dataset title and FI assessment codes).
# Soil-characteristics gives Soil type = "Sandy loam" (mapped to soil_texture = "sandy loam"), soil_SOM (%), and soil_pH.
# Estimated station coordinates are added for CIP La Molina (-12.076289, -76.948417) and UNALM (-12.082, -76.943) with geo_from_source = FALSE and geo_uncertainty = 1000 m.

## ISSUES
# Seed amount (57.6 kg on 45 m2; 12800 to 15190 kg/ha) exceeds typical potato seed rates (~2000-3000 kg/ha), likely reflecting a unit or total-trial entry in the Cool Farm Tool sheet. Kept as calculated from the stated dictionary units.
# "sheep digestate" is not in terminag values_OM.csv, so OM_type is omitted and documented here.
# write_files: missing carob_contributor, yield_moisture; out of bounds: seed_rate; unknown variables in long: week_, irrigation_week_mm_.

## NOT PROCESSED
# Crop protection inputs: multiple pesticide applications per plot (rates and active ingredients mostly NA except Vitavax 300 seed treatment); seed_treatment is captured in d.
# Energy and Transport sheets: fuel, storage, and freight inputs for Cool Farm Tool GHG accounting.
# Soil moisture ("Moist") and drainage ("Poor"): Cool Farm Tool categorical classes.
# Fertiliser application method and emissions inhibitors ("None").
# Data_dictionary.xlsx: codebook.

## NEW VARIABLES
# week_: week number after planting (1 to 14) for weekly irrigation records in long table.
# irrigation_week_mm_: weekly irrigation depth (mm) computed as weekly litres / plot_area (m2).


carob_script <- function(path) {

"
Replication Data for: Estimating carbon footprint in potato trials under drip and furrow irrigation

This dataset has been specially created to estimate carbon footprint in potato field trials using Cool Farm Tool (CFT) Online. Includes three field trials were performed at CIP and National Agrarian University La Molina (UNALM) experimental stations in Lima during July 5th to October 10th, 2017 (E1-CIP), October 5th, 2017 to January 15th, 2018 (E2-CIP), and June 13th to September 13th, 2018 (E3-UNALM).
"

	uri <- "doi:10.21223/LNLR9B"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment;irrigation_method",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 80,
		carob_effort = 0.25
	)

	f <- ff[basename(ff) == "CFT_PNIA 2017-2018.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Crop-details")
	r2 <- carobiner::read.excel(f, sheet="Soil-characteristics")
	r3 <- carobiner::read.excel(f, sheet="Fertiliser inputs")
	r4 <- carobiner::read.excel(f, sheet="Water")
	r5 <- carobiner::read.excel(f, sheet="Crop protection inputs")

	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
		trimws(as.character(x))
	}
	num <- function(x) as.numeric(as.character(x))

	npk_one <- function(name, rate, units) {
		out <- c(N=0, P=0, K=0, N_org=0, OM=0)
		if (is.na(name) || is.na(rate)) return(out)
		if (grepl("P2O5", units, fixed=TRUE)) {
			out["P"] <- rate / 2.29
			return(out)
		}
		if (grepl("33%N, 3% P2O5", name, fixed=TRUE)) {
			out["N"] <- rate * 0.33
			out["P"] <- rate * 0.03 / 2.29
		} else if (grepl("33%N", name, fixed=TRUE)) {
			out["N"] <- rate * 0.33
		} else if (grepl("50% K2O", name, fixed=TRUE)) {
			out["K"] <- rate * 0.50 / 1.2051
		} else if (grepl("18%N, 46%P2O5", name, fixed=TRUE)) {
			out["N"] <- rate * 0.18
			out["P"] <- rate * 0.46 / 2.29
		} else if (grepl("45% K2O and 13.5%N", name, fixed=TRUE)) {
			out["N"] <- rate * 0.135
			out["K"] <- rate * 0.45 / 1.2051
		} else if (grepl("15.5%N", name, fixed=TRUE)) {
			out["N"] <- rate * 0.155
		} else if (grepl("0.7% N", name, fixed=TRUE)) {
			# Sheep digestate (0.7% N): organic amendment, not mineral N
			out["N_org"] <- rate * 0.007
			out["OM"] <- rate
		}
		out
	}

	fert_code <- function(name) {
		if (is.na(name)) return(NA_character_)
		if (grepl("Ammonium nitrate", name, fixed=TRUE)) return("AN")
		if (grepl("Potassium sulfate", name, fixed=TRUE)) return("SOP")
		if (grepl("Phosphoric acid", name, fixed=TRUE)) return("PA")
		if (grepl("Diammonium Phosphate", name, fixed=TRUE)) return("DAP")
		if (grepl("Potassium nitrate", name, fixed=TRUE)) return("KNO")
		if (grepl("Calcium nitrate", name, fixed=TRUE)) return("CN")
		NA_character_
	}

	rates <- t(mapply(npk_one, r3$Name, num(r3[["Application rate"]]), as.character(r3[["Fertiliser weight, or units?"]])))
	r3$N <- rates[, "N"]
	r3$P <- rates[, "P"]
	r3$K <- rates[, "K"]
	r3$N_org <- rates[, "N_org"]
	r3$OM <- rates[, "OM"]
	r3$fcode <- vapply(as.character(r3$Name), fert_code, character(1))
	fert_sum <- aggregate(cbind(N, P, K, N_org, OM) ~ `Assessment name`, data=r3, FUN=sum)
	ftypes <- tapply(r3$fcode, trimws(as.character(r3[["Assessment name"]])), function(z) {
		z <- unique(z[!is.na(z)])
		if (length(z) == 0) NA_character_ else paste(z, collapse=";")
	})
	fert_sum$fertilizer_type <- as.character(unname(ftypes[trimws(as.character(fert_sum[["Assessment name"]]))]))

	tw_cols <- names(r4)[grepl("^TW_Week", names(r4))]
	twm <- as.matrix(as.data.frame(lapply(r4[, tw_cols, drop=FALSE], num)))
	litres <- rowSums(twm, na.rm=TRUE)
	litres[rowSums(!is.na(twm)) == 0] <- NA_real_
	method <- trimws(as.character(r4$Method))
	# Cool Farm Tool groups furrow and flooding; title and FI codes confirm furrow irrigation
	imeth <- ifelse(method == "Drip irrigation", "drip",
		ifelse(grepl("Furrow", method, ignore.case=TRUE), "furrow", method))
	water_sum <- data.frame(
		plot_id = trimws(as.character(r4[["Assessment name"]])),
		litres = litres,
		irrigation_method = imeth,
		irrigation_source_distance = num(r4[["Horizontal distance"]])
	)

	area <- num(r1[["Crop area"]])
	assess <- trimws(as.character(r1[["Assessment name"]]))
	experiment <- trimws(as.character(r1[["Experiment  name"]]))
	is_unalm <- grepl("^UNALM", experiment)
	loc <- ifelse(is_unalm, "National Agrarian University La Molina", "CIP La Molina")
	treat <- sub("^E[0-9]+_(DI|FI)_", "", assess)

	d <- data.frame(
		record_id = seq_len(nrow(r1)),
		trial_id = experiment,
		plot_id = assess,
		treatment = treat,
		crop = "potato",
		variety = trimws(as.character(r1$Variety)),
		country = "Peru",
		adm1 = "Lima",
		location = loc,
		latitude = ifelse(is_unalm, -12.082, -12.076289),
		longitude = ifelse(is_unalm, -76.943, -76.948417),
		geo_from_source = FALSE,
		geo_uncertainty = 1000,
		planting_date = as_date(r1[["Planting / sowing date"]]),
		harvest_date = as_date(r1[["Harvest date"]]),
		plot_area = area,
		yield = num(r1[["Harvested amount"]]) / area * 10000,
		yield_marketable = num(r1[["Farm-gate ready amount"]]) / area * 10000,
		yield_part = "tubers",
		yield_isfresh = TRUE,
		seed_rate = num(r1[["Seed amount"]]) / area * 10000,
		dmy_residue = num(r1[["Residue amount"]]) * 1000,
		fertilizer_used = TRUE,
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = TRUE
	)

	# Seed treatment recorded in Crop protection inputs
	st <- r5[grepl("Seed treatment", as.character(r5$Category), ignore.case=TRUE), ]
	if (nrow(st) > 0) {
		st_map <- tapply(trimws(as.character(st$Category)), trimws(as.character(st[["Assessment name"]])), function(z) paste(unique(z), collapse="; "))
		d$seed_treatment <- as.character(unname(st_map[d$plot_id]))
	}

	fert_sum$plot_id <- trimws(as.character(fert_sum[["Assessment name"]]))
	fert_sum[["Assessment name"]] <- NULL
	d <- merge(d, fert_sum, by="plot_id", all.x=TRUE, sort=FALSE)
	names(d)[names(d) == "N"] <- "N_fertilizer"
	names(d)[names(d) == "P"] <- "P_fertilizer"
	names(d)[names(d) == "K"] <- "K_fertilizer"
	names(d)[names(d) == "N_org"] <- "N_organic"
	names(d)[names(d) == "OM"] <- "OM_amount"
	d$OM_used <- !is.na(d$OM_amount) & d$OM_amount > 0

	d <- merge(d, water_sum, by="plot_id", all.x=TRUE, sort=FALSE)
	d$irrigation_amount <- d$litres / d$plot_area
	d$litres <- NULL

	soil_key <- data.frame(
		plot_id = trimws(as.character(r2[["Assessment name"]])),
		soil_texture = tolower(trimws(as.character(r2[["Soil type"]]))),
		soil_pH = num(r2[["Soil pH"]]),
		soil_SOM = num(r2[["Soil organic matter"]])
	)
	d <- merge(d, soil_key, by="plot_id", all.x=TRUE, sort=FALSE)
	d <- d[order(d$record_id), ]
	rownames(d) <- NULL

	# Weekly irrigation depths in long format
	r4_pid <- trimws(as.character(r4[["Assessment name"]]))
	rid_map <- match(r4_pid, d$plot_id)
	area_map <- d$plot_area[rid_map]
	wk_list <- lapply(seq_along(tw_cols), function(k) {
		w <- as.integer(sub("^TW_Week\\s*", "", tw_cols[k]))
		vals <- num(r4[[tw_cols[k]]])
		data.frame(
			record_id = d$record_id[rid_map],
			DAP = w * 7L,
			week_ = w,
			irrigation_week_mm_ = vals / area_map
		)
	})
	dlong <- do.call(rbind, wk_list)
	dlong <- dlong[!is.na(dlong$record_id) & !is.na(dlong$irrigation_week_mm_), ]
	dlong <- dlong[order(dlong$record_id, dlong$week_), ]
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
