# R script for "carob"
# license: GPL (>=3)

## NOTES
# Split-split plot potato trial at La Soledad, Chugay, Sanchez Carrion, La Libertad, Peru, 25 October 2023 to 16 April 2024. Variety INIA-325 Poderosa. The description spells the design "split-plit". Plots are 3 x 4 m. Plant spacing 0.3 m and row spacing 1 m.
# FTY, MTY, TEY, and MEY are t/ha. FFBY (faba bean) is already kg/ha. Plot weights in the harvest file are not converted again.
# Poultry manure rates are 0, 1, 2, and 4 t/ha. Zero is the control rate. OM_amount is kg/ha. OM_type is poultry manure.
# Disc / no disc is the fungicide decision tool, not a product or a dose.
# Soil P is ug/g, stored as mg/kg. EC is dS/m, the same number as mS/cm. CaCO3 is percent, stored as g/kg (times 10). Exchangeable cations and CEC are meq/100 g, the same number as cmol/kg. K+ is that cation. The separate K column is about 100 to 130 with the unit cell meq/100 g, so it is not stored as soil_K or soil_K_exch.
# Organic carbon, total carbon, and total N are two depths (0-24 cm and 24-36 cm). Organic carbon g/kg is divided by 10 to percent for soil_SOC. Total N is mg/g and is not soil_N. One N value is "<0,01" and is set to missing. Base saturation is described as a percentage but the unit cell says meq/100 g, so it is omitted. Texture codes Fr. and Fr.L. are not expanded in the dictionary.

## ISSUES
# La Soledad needs georeferencing. The files have no coordinates.
# No mineral fertilizer rate. The control is poultry manure at 0, not an N-P-K rate.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: faba_yield_, equivalent_yield_, marketable_equivalent_yield_, tuber_dry_matter_, soil_K_listed_, soil_C_gkg_, soil_N_mgg_.

## NEW VARIABLES
# faba_yield_: faba bean yield, kg/ha, already in those units.
# equivalent_yield_: total equivalent yield, t/ha times 1000.
# marketable_equivalent_yield_: marketable equivalent yield, t/ha times 1000.
# tuber_dry_matter_: tuber dry matter, percent.
# soil_K_listed_: the K column, unit cell meq/100 g.
# soil_C_gkg_: total carbon, g/kg, by depth.
# soil_N_mgg_: total nitrogen, mg/g, by depth.

## NOT PROCESSED
# Cost production, gross return, benefit-cost ratio, and carbon-footprint columns.
# Harvest-file kilogram weights and tuber counts. The per-hectare yields are used instead.
# OM2 by depth (g/kg). soil_SOM is the single OM1 percent.
# Base saturation, textural class, and the design schematic image.


carob_script <- function(path) {

"
Dataset for: Organic agriculture trial in the locality of La Soledad, Chugay district, La Libertad - Peru (Second season 2023-2024)

An experimental potato trial based on organic agriculture in the second season (October 25th, 2023 to April 16th, 2024) was conducted in a total area of 987 m2 with the support of the 'Asociación  de Productores Agropecuarios y Semilleristas La Poderosa' of the locality of La Soledad, Chugay district, province of Sánchez Carrión, La Libertad. This trial, the improved potato variety INIA-325 Poderosa was used with a split-plit plot design considering three factors: crop intercropping (intercropping with faba bean, variety Señorita and no intercropping), use of a decision support system for fungicide application or 'Juego de Ruedas' (Disc and no disc) and the application of organic amendments (1, 2 and 4 t ha-1 of poultry manure). The combination of these factors generated a total of 12 treatments; and an additional control treatment (conventional practices in the study zone). Four replicates were established, with a total of 52 plots of 3x4 m2 each (distance between plant and furrow of 0.3 m and 1 m, respectively). The variables evaluated were: fresh tuber yield (t ha-1), dry matter content of tubers (%), cost-benefit ratio, carbon footprint (kg CO2e Total) and soil analysis.
"

	uri <- "doi:10.21223/TIOWXR"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "split-split plot",
		data_type = "experiment",
		treatment_vars = "treatment;intercropped;OM_amount",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.55
	)

	num <- function(x) {
		z <- trimws(as.character(x))
		z[grepl("^<", z)] <- NA_character_
		as.numeric(z)
	}
	y <- carobiner::read.excel(ff[grepl("^2_Yield", basename(ff))])
	s <- carobiner::read.excel(ff[grepl("^4_Soil", basename(ff))])
	dm <- carobiner::read.excel(ff[grepl("^3_Dry", basename(ff))])

	oa <- trimws(as.character(y$OA_f3))
	om <- rep(NA_real_, length(oa))
	om[oa == "0"] <- 0
	om[grepl("^1 ", oa)] <- 1000
	om[grepl("^2 ", oa)] <- 2000
	om[grepl("^4 ", oa)] <- 4000
	if (any(is.na(om))) stop("unparsed poultry manure rate")

	inter <- trimws(as.character(y$CI_f1)) == "Intercropping"
	d <- data.frame(
		record_id = as.integer(y$ID),
		trial_id = "LaSoledad_2023",
		plot_id = as.character(y[["Sub-Sub-Plot"]]),
		rep = as.integer(y$Block),
		treatment = trimws(as.character(y$Treatment)),
		crop = "potato",
		variety = "INIA-325 Poderosa",
		country = "Peru",
		adm1 = "La Libertad",
		adm2 = "Sánchez Carrión",
		adm3 = "Chugay",
		location = "La Soledad",
		planting_date = "2023-10-25",
		harvest_date = "2024-04-16",
		plot_area = 12,
		plant_spacing = 30,
		row_spacing = 100,
		intercropped = inter,
		intercrops = ifelse(inter, "faba bean", NA_character_),
		OM_type = "poultry manure",
		OM_amount = om,
		yield = num(y$FTY) * 1000,
		yield_marketable = num(y$MTY) * 1000,
		yield_part = "tubers",
		faba_yield_ = num(y$FFBY),
		equivalent_yield_ = num(y$TEY) * 1000,
		marketable_equivalent_yield_ = num(y$MEY) * 1000,
		geo_from_source = FALSE,
		on_farm = NA,
		is_survey = FALSE
	)

	if (!"DMCt" %in% names(dm)) stop("dry matter column DMCt not found")
	d$tuber_dry_matter_ <- num(dm$DMCt[match(d$record_id, as.integer(dm$ID))])

	s <- s[match(d$record_id, as.integer(s$ID)), ]
	d$soil_pH <- num(s$pH)
	d$soil_EC <- num(s$EC)
	d$soil_CaCO3 <- num(s$CaCO3) * 10
	d$soil_SOM <- num(s$OM1)
	d$soil_P <- num(s$P)
	d$soil_K_listed_ <- num(s$K)
	d$soil_sand <- num(s$Sand)
	d$soil_silt <- num(s$Silt)
	d$soil_clay <- num(s$Clay)
	d$soil_CEC <- num(s$CEC)
	d$soil_Ca_exch <- num(s[["Ca2+"]])
	d$soil_Mg_exch <- num(s[["Mg2+"]])
	d$soil_K_exch <- num(s[["K+"]])
	d$soil_Na_exch <- num(s[["Na+"]])
	d$soil_ex_acidity <- num(s[["Al+3 + H+3"]])
	d$soil_bd <- num(s$BD)

	depth_row <- function(top, bottom, ccol, occol, ncol) {
		data.frame(
			record_id = d$record_id,
			depth_top = top,
			depth_bottom = bottom,
			soil_SOC = num(s[[occol]]) / 10,
			soil_C_gkg_ = num(s[[ccol]]),
			soil_N_mgg_ = num(s[[ncol]])
		)
	}
	dlong <- rbind(
		depth_row(0, 24, "Clevel A", "OClevel A", "Nlevel A"),
		depth_row(24, 36, "Clevel B", "OClevel B", "Nlevel B")
	)

	carobiner::write_files(path, meta, d, long=dlong)
}
