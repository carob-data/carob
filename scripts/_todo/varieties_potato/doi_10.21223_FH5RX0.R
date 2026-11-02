# R script for "carob"
# license: GPL (>=3)

## NOTES
# Only two of the six sites named in the description are in the raw folder: Mariscal Caceres (Conayca) and Sotopampa. Ccasapata, Ccollpaccasa, La Victoria, and Rangra are not here.
# Coordinates, elevation, dates, and plot layout are the Minimal and Installation sheets. Design is RCBD, 3 reps, 10 plants on 2.7 m2, 0.30 m between plants and 0.90 m between rows. The description says one hill per plot and gives Sotopampa as 3754 m. The Sotopampa workbook elevation is 3730 m. The workbook values are used.
# Planting and harvest dates are Begin date and End date. Crop_management also records vine killing (2010-05-13 at Mariscal Caceres) with no product or dose.
# Dictionary: DM is percent dry matter. yield_moisture is 100 minus that value. CH is chlorogenic acid, CafAc is caffeic acid, and Trip is tryptophan, all mg/100 g. TP_DW and TP_FW are the synonyms for total phenolics on a chlorogenic-acid basis, which is a different row from HPLC total phenolics (Tphe) and from chlorogenic acid (CH). TA is not in the dictionary. AntCap is the fieldbook column. A dictionary row named antheraxanthin (ug/100 g, upper limit 100) uses the synonym Ant and a method that mentions an ascorbic-acid standard curve. AntCap values are about 400 to 1060, so they are kept under their own names and not called antheraxanthin.
# Soil is one sample per site, repeated on the plot rows. P, K, Fe, Cu, Zn, B, and Mn are ppm and stored as mg/kg. EC is mmho/cm, the same number as mS/cm. Exchangeable cations and CEC are meq/100 g, the same number as cmol/kg. CaCO3 is percent in the file and is stored as g/kg (times 10). The sand, Lime, and clay percentages sum to 100, so Lime is the silt fraction. Franco arenoso is sandy loam and Franco is loam. Total N has a blank unit cell and is not mapped to soil_N.

## ISSUES
# No tuber yield in the fieldbooks.
# The description's other four sites are absent.
# Conayca exchangeable potassium is 4.87 meq/100 g and total bases is also 4.87, which does not match the sum of the exchangeable cations. Values are left as recorded.
# Some chlorogenic-acid and total-phenolic numbers are above the dictionary upper limit of 100 mg/100 g. They are left as recorded.
# write_files: missing carob_contributor, yield, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. Unknown variables are the tuber chemistry names and soil_N_total_.

## NEW VARIABLES
# lab_sample_: laboratory sample number (1 or 2) for the same plot.
# tuber_chlorogenic_dw_, tuber_chlorogenic_fw_: chlorogenic acid, mg/100 g dry and fresh weight.
# tuber_caffeic_dw_, tuber_caffeic_fw_: caffeic acid, mg/100 g.
# tuber_tryptophan_dw_, tuber_tryptophan_fw_: tryptophan, mg/100 g.
# tuber_TP_dw_, tuber_TP_fw_: total phenolics on a chlorogenic-acid basis. Unit is not on the dictionary scale row.
# tuber_TA_dw_, tuber_TA_fw_: TA columns. The dictionary does not name TA or give a unit.
# antcap_dw_, antcap_fw_: AntCap columns. See NOTES for the antheraxanthin conflict.
# soil_N_total_: total nitrogen. The unit cell is blank.

## NOT PROCESSED
# Summary by clone and Results all: means and statistical summaries.
# Weather_data: header only, no rows.
# Crop_management: planting and harvest dates already used. Vine killing has no product.
# Var List: overlaps the material list.
# Soil soluble ions (calcium, magnesium, potassium, sodium, chloride, carbonate, bicarbonate, nitrate, sulfate): unit cells are blank.
# Soil requester, operator, laboratory code, and field code.


carob_script <- function(path) {

"
Dataset for: Stability analysis for nutritional contents (phenolics) on 40 native potato cultivars -2009

'The analysis of phenolic content in potato cultivars aims to evaluate their nutritional quality and potential as a source of bioactive compounds, known for their health benefits such as antioxidant activity. This study focuses on identifying variations in phenolic content among different genotypes and their interaction with environmental conditions to select varieties with high stability and consistent phenolic concentrations.

Forty native potato cultivars were evaluated to analyze the magnitude and nature of Genotype (G), Environment (E), and GxE interaction effects on phenolic compound concentrations in the tropical highlands of Peru, as well as to study the contribution of soil fertility to the phenolic content in potato tubers. The cultivars were grown in randomized complete block designs with three replications of one hill per plot in six sites in the central Peruvian Andes in 2010: Ccasapata (3765 masl), Sotopampa (3754 masl), Ccollpaccasa (4067 masl), Conayca (4178 masl), La Victoria (3265 masl), and Rangra (3323 masl). Fully matured tubers were harvested at 150 and 180 days and analyzed via spectrophotometry to determine their phenolic content. This analysis explored the effects of genetic and environmental factors on tuber quality.

Statistical analyses included ANOVA to assess differences between genotypes and environments, as well as the AMMI model (Additive Main Effects and Multiplicative Interaction) to study genotype stability and adaptability. This comprehensive approach provides critical information for breeding programs, strategies to conserve native varieties, and the development of cultivars with outstanding nutritional characteristics that meet market demands and contribute to agricultural biodiversity.'
"

	uri <- "doi:10.21223/FH5RX0"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield_moisture;tuber_chlorogenic_dw_;tuber_caffeic_dw_;tuber_tryptophan_dw_;tuber_TP_dw_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.55
	)

	num <- function(x) as.numeric(as.character(x))
	one <- function(sheet, name) {
		hit <- sheet$Value[trimws(as.character(sheet$Factor)) == name]
		if (length(hit) == 0) return(NA_character_)
		x <- hit[1]
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
		trimws(as.character(x))
	}
	soil_val <- function(soil, abbrev) {
		hit <- soil$Data1[trimws(as.character(soil$Abbreviture)) == abbrev]
		if (length(hit) == 0) return(NA_real_)
		num(hit[1])
	}

	files <- ff[grepl("Stability_Field.xls$", basename(ff))]
	parts <- lapply(files, function(f) {
		mini <- carobiner::read.excel(f, sheet="Minimal")
		inst <- carobiner::read.excel(f, sheet="Installation")
		mat <- carobiner::read.excel(f, sheet="Material List")
		fb <- carobiner::read.excel(f, sheet="Fieldbook")
		soil <- carobiner::read.excel(f, sheet="Soil_analysis")

		code <- trimws(as.character(mat[["Institutional number"]]))
		vname <- gsub("\u00a0", " ", as.character(mat[["Clone or variety name"]]), fixed=TRUE)
		vname <- trimws(vname)
		vname[!nzchar(vname)] <- NA_character_
		alias <- data.frame(variety_code=code, variety=vname)
		alias <- alias[!duplicated(alias$variety_code), ]

		fb$variety_code <- trimws(as.character(fb$INSTN))
		fb <- merge(fb, alias, by="variety_code", all.x=TRUE)
		fb$variety[is.na(fb$variety)] <- fb$variety_code[is.na(fb$variety)]

		tex <- trimws(as.character(soil$Data1[trimws(as.character(soil$Abbreviture)) == "STEX"]))
		texture <- c("Franco arenoso"="sandy loam", "Franco"="loam")
		if (!tex %in% names(texture)) stop("unexpected soil texture: ", tex)

		data.frame(
			variety_code = fb$variety_code,
			variety = fb$variety,
			treatment = fb$variety,
			rep = as.integer(fb$REP),
			plot_id = as.character(fb$PLOT),
			lab_sample_ = as.integer(fb$Sample),
			country = one(mini, "Country"),
			adm1 = one(mini, "Admin1"),
			adm2 = one(mini, "Admin2"),
			adm3 = one(mini, "Admin3"),
			location = one(mini, "Locality"),
			elevation = num(one(mini, "Elevation")),
			latitude = num(one(mini, "Latitude")),
			longitude = num(one(mini, "Longitude")),
			planting_date = one(mini, "Begin date"),
			harvest_date = one(mini, "End date"),
			trial_id = paste(one(mini, "Site short name"), substr(one(mini, "Begin date"), 1, 4), sep="_"),
			plot_area = num(one(inst, "Plot size (m2)")),
			plant_spacing = num(one(inst, "Distance between plants (m)")) * 100,
			row_spacing = num(one(inst, "Distance between rows (m)")) * 100,
			seed_density = num(one(inst, "Planting density (plants/Ha)")),
			yield_moisture = 100 - num(fb$DM),
			tuber_TP_dw_ = num(fb$TP_DW),
			tuber_TP_fw_ = num(fb$TP_FW),
			tuber_TA_dw_ = num(fb$TA_DW),
			tuber_TA_fw_ = num(fb$TA_FW),
			tuber_chlorogenic_dw_ = num(fb$CH_DW),
			tuber_chlorogenic_fw_ = num(fb$CH_FW),
			tuber_tryptophan_dw_ = num(fb$Trip_DW),
			tuber_tryptophan_fw_ = num(fb$Trip_FW),
			tuber_caffeic_dw_ = num(fb$CafAc_DW),
			tuber_caffeic_fw_ = num(fb$CafAc_FW),
			antcap_dw_ = num(fb$AntCap_DW),
			antcap_fw_ = num(fb$AntCap_FW),
			soil_pH = soil_val(soil, "SOILPH"),
			soil_EC = soil_val(soil, "SOILEC"),
			soil_CaCO3 = soil_val(soil, "CACO3") * 10,
			soil_SOM = soil_val(soil, "ORGM"),
			soil_N_total_ = soil_val(soil, "NITR"),
			soil_P = soil_val(soil, "PHOS"),
			soil_K = soil_val(soil, "POT"),
			soil_sand = soil_val(soil, "SAND"),
			soil_silt = soil_val(soil, "LIME"),
			soil_clay = soil_val(soil, "CLAY"),
			soil_texture = unname(texture[tex]),
			soil_CEC = soil_val(soil, "SCEC"),
			soil_Ca_exch = soil_val(soil, "EXCA2"),
			soil_Mg_exch = soil_val(soil, "EXMG2"),
			soil_K_exch = soil_val(soil, "EXK"),
			soil_Na_exch = soil_val(soil, "EXNA"),
			soil_ex_acidity = soil_val(soil, "AL3H"),
			soil_BS = soil_val(soil, "BSAT"),
			soil_Fe = soil_val(soil, "SOILFE"),
			soil_Cu = soil_val(soil, "SOILCU"),
			soil_Zn = soil_val(soil, "SOILZN"),
			soil_B = soil_val(soil, "SOILB"),
			soil_Mn = soil_val(soil, "SOILMN")
		)
	})

	d <- do.call(rbind, parts)
	d$crop <- "potato"
	d$geo_from_source <- TRUE
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$record_id <- seq_len(nrow(d))

	carobiner::write_files(path, meta, d)
}
