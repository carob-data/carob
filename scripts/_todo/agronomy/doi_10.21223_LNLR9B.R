# R script for "carob"
# license: GPL (>=3)

## NOTES
# Cool Farm Tool records for three potato trials of variety UNICA at CIP La Molina and UNALM, Lima. Country Peru and the city Lima are in the description. The Dataverse box is only south -12.10 and west -77, so it is not used as a plot coordinate.
# Dictionary: crop area is m2 and harvested amount, rejected amount, seed amount, and farm-gate amount are kg. Yield (kg/ha) is harvested kg / m2 * 10000. Farm-gate amount is yield_marketable on the same basis. Residue amount is t/ha of dry matter (dictionary unit tn/ha) and is stored as dmy_residue in kg/ha. Every row is 2.09, the value entered in the tool.
# Fertiliser application rate is kg/ha. "Product" means kg product/ha. The nutrient fractions are the percentages printed in the product name. P2O5 is divided by 2.29 and K2O by 1.2051. Phosphoric acid is the one row whose unit cell says "units of P2O5", so that rate is already kg P2O5/ha and is not multiplied by 85%.
# Magnesium nitrate is 30 kg product/ha and the name gives no N percentage, so it is not added to N_fertilizer. Sheep digestate is 0.7% N. That N is included. The product mass is OM_amount. "sheep digestate" is not an OM_type value.
# Weekly water columns are litres (dictionary). irrigation_amount (mm) is the sum of litres divided by the crop area in m2. "Drip irrigation" is drip. "Furrow irrigation or Flooding" is kept as printed because the tool uses one label for both.
# Soil organic matter and pH are the soil sheet. "Sandy" is the tool class, not the texture term sand.

## ISSUES
# CIP La Molina and UNALM need georeferencing.
# Seed kg on the stated m2 is above 3000 kg/ha. The dictionary units are used and the rate is left as calculated.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude. out of bounds: seed_rate (12800, 15189.873). invalid terms: irrigation_method: Furrow irrigation or Flooding.

## NOT PROCESSED
# Crop protection inputs: category, doses, rate (kg/ha), and active ingredient. Several applications per plot, not mapped to one pesticide column.
# Energy and Transport sheets: fuel, storage, and freight for the carbon-footprint tool.
# Soil texture Sandy, soil moisture "Moist", and drainage "Poor": tool classes, not texture or a drainage rate.
# Fertiliser application method and emissions inhibitors.


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
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "CFT_PNIA 2017-2018.xlsx"]
	crop <- carobiner::read.excel(f, sheet="Crop-details")
	soil <- carobiner::read.excel(f, sheet="Soil-characteristics")
	fert <- carobiner::read.excel(f, sheet="Fertiliser inputs")
	water <- carobiner::read.excel(f, sheet="Water")

	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
		trimws(as.character(x))
	}
	num <- function(x) as.numeric(as.character(x))

	npk_one <- function(name, rate, units) {
		out <- c(N=0, P=0, K=0, OM=0)
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
			out["N"] <- rate * 0.007
			out["OM"] <- rate
		}
		out
	}

	rates <- t(mapply(npk_one, fert$Name, num(fert[["Application rate"]]), as.character(fert[["Fertiliser weight, or units?"]])))
	fert$N <- rates[, "N"]
	fert$P <- rates[, "P"]
	fert$K <- rates[, "K"]
	fert$OM <- rates[, "OM"]
	fert_sum <- aggregate(cbind(N, P, K, OM) ~ `Assessment name`, data=fert, FUN=sum)

	tw <- water[, grepl("^TW_Week", names(water))]
	twm <- as.matrix(as.data.frame(lapply(tw, num)))
	litres <- rowSums(twm, na.rm=TRUE)
	litres[rowSums(!is.na(twm)) == 0] <- NA_real_
	method <- trimws(as.character(water$Method))
	water_sum <- data.frame(
		`Assessment name` = water[["Assessment name"]],
		litres = litres,
		irrigation_method = ifelse(method == "Drip irrigation", "drip", method),
		check.names = FALSE,
		stringsAsFactors = FALSE
	)

	area <- num(crop[["Crop area"]])
	assess <- trimws(as.character(crop[["Assessment name"]]))
	experiment <- trimws(as.character(crop[["Experiment  name"]]))
	loc <- ifelse(grepl("^UNALM", experiment), "National Agrarian University La Molina", "CIP La Molina")
	treat <- sub("^E[0-9]+_(DI|FI)_", "", assess)

	d <- data.frame(
		trial_id = experiment,
		plot_id = assess,
		treatment = treat,
		crop = "potato",
		variety = trimws(as.character(crop$Variety)),
		country = "Peru",
		adm1 = "Lima",
		location = loc,
		planting_date = as_date(crop[["Planting / sowing date"]]),
		harvest_date = as_date(crop[["Harvest date"]]),
		plot_area = area,
		yield = num(crop[["Harvested amount"]]) / area * 10000,
		yield_marketable = num(crop[["Farm-gate ready amount"]]) / area * 10000,
		yield_part = "tubers",
		seed_rate = num(crop[["Seed amount"]]) / area * 10000,
		dmy_residue = num(crop[["Residue amount"]]) * 1000,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE,
		irrigated = TRUE,
		stringsAsFactors = FALSE
	)

	d <- merge(d, fert_sum, by.x="plot_id", by.y="Assessment name", all.x=TRUE, sort=FALSE)
	names(d)[names(d) == "N"] <- "N_fertilizer"
	names(d)[names(d) == "P"] <- "P_fertilizer"
	names(d)[names(d) == "K"] <- "K_fertilizer"
	names(d)[names(d) == "OM"] <- "OM_amount"
	d$OM_amount[d$OM_amount == 0] <- NA_real_

	d <- merge(d, water_sum, by.x="plot_id", by.y="Assessment name", all.x=TRUE, sort=FALSE)
	d$irrigation_amount <- d$litres / d$plot_area
	d$litres <- NULL

	soil_key <- data.frame(
		plot_id = trimws(as.character(soil[["Assessment name"]])),
		soil_pH = num(soil[["Soil pH"]]),
		soil_SOM = num(soil[["Soil organic matter"]]),
		stringsAsFactors = FALSE
	)
	d <- merge(d, soil_key, by="plot_id", all.x=TRUE, sort=FALSE)

	carobiner::write_files(path, meta, d)
}
