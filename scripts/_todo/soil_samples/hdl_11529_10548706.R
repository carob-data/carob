# R script for "carob"
# license: GPL (>=3)

## NOTES
# Earthworm abundance and soil physico-chemical properties after 10 years of climate-smart agriculture (CSA) at ICAR-CSSRI, Karnal, Haryana, India (Jat et al., 2022, Agronomy 12: 658, doi:10.3390/agronomy12030658; PDF bundled in deposit).
# Site details from Methods 2.1-2.2: ICAR-CSSRI Karnal (29°42'20.7" N, 76°57'19.79" E, 243 m elevation), RCBD with 3 replicates, 20 m x 50 m plots, started 2009, sampled mid-September 2019.
# The workbook contains 7 management scenarios (Sc1-Sc7, 3 replicates each = 21 plots; paper published 5 of the 7):
#   Sc1: CT + Flood (conventional rice-wheat-fallow, residues removed)
#   Sc2: CTDSR-ZTW + Flood
#   Sc3: ZTDSR-ZTW-ZTMb + Flood (paper Sc2)
#   Sc4: ZTDSR-ZTW-ZTMb + Drip / SDI (paper Sc3)
#   Sc5: ZTM-ZTW-ZTMb + Flood (paper Sc4)
#   Sc6: PBM-PBW-ZTMb + Furrow irrigation
#   Sc7: ZTM-ZTW-ZTMb + Drip / SDI (paper Sc5)
# Extracted replicate-level bulk soil measurements from sheets Earthworm (density per m2 at 0-15 and 15-30 cm), BD (g/cm3 at 0-15 and 15-30 cm), ECpH (1:2 water EC in dS/m = mS/cm, pH at 0-15 cm), Total Carbon (Walkley-Black TOC % at 0-15 cm), N (alkaline KMnO4 available N kg/ha), P (Olsen P kg/ha), K (NH4OAc K kg/ha), and Micro (DTPA Zn, Cu, Mn, Fe in mg/kg at 0-15 cm).
# Available N, P, K in the 0-15 cm soil layer are reported in kg/ha (Table 3 of paper); converted to mg/kg by dividing by 2.24 (standard 0-15 cm furrow-slice conversion at ~1.49 g/cm3 bulk density).

## ISSUES
# Bulk-soil available N, P, K were converted from kg/ha to mg/kg using / 2.24 (0-15 cm furrow-slice mass ~2.24 x 10^6 kg/ha); verify if plot-specific bulk density conversion is preferred.
# write_files: unknown variables earthworm_small_, earthworm_medium_, earthworm_large_, earthworm_density_.

## NEW VARIABLES
# earthworm_small_: earthworms shorter than 2 cm (individuals/m2)
# earthworm_medium_: earthworms 2 to 4 cm (individuals/m2)
# earthworm_large_: earthworms longer than 4 cm (individuals/m2)
# earthworm_density_: total earthworm density (individuals/m2)

## NOT PROCESSED
# Earthworm casts (EWC) chemical properties (cast EC, pH, TOC, N, P, K, micronutrients, enzymes): biogenic cast material rather than bulk soil.
# Infiltration only, Aggregate only (MWD), Enzyme, Penetra Fig used, and earthworm biomass sheets/blocks (mixed summary/transformed layouts).
# agronomy-12-00658.pdf: published paper (used for site metadata, coordinates, methods, and units).


carob_script <- function(path) {

"
A Decade of Climate-Smart Agriculture in Major Agri-Food Systems: Earthworm Abundance and Soil Physico-Biochemical Properties

Earthworm abundance and soil properties after 10 years of climate-smart agriculture practices in a rice-wheat system.
"

	uri <- "hdl:11529/10548706"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CSSRI; CIMMYT",
		publication = "doi:10.3390/agronomy12030658",
		project = "CSISA",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "soil_bd; soil_pH; soil_EC; soil_SOC; soil_N; soil_P; soil_K; soil_Zn; soil_Cu; soil_Mn; soil_Fe; earthworm_density_",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 75,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Agronomy 12-0065_SK.xlsx"]
	rd <- function(sh) carobiner::read.excel(f, sheet=sh, col_names=FALSE, col_types="text")
	num <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		as.numeric(x)
	}

	# All sheets share the same 21-row block: 3 replicates x 7 scenarios (Sc1-Sc7)
	re <- rd("Earthworm ")[5:25, ]
	rb <- rd("BD")[3:23, ]
	rp <- rd("ECpH")[4:24, ]
	rn <- rd("N")[3:23, ]
	rph <- rd("P")[3:23, ]
	rk <- rd("K")[3:23, ]
	rm <- rd("Micro")[4:24, ]
	rc <- rd("Total Carbon")[4:24, ]

	treat <- trimws(as.character(re[[1]]))
	repn <- as.integer(num(re[[3]]))

	# 0-15 cm topsoil (earthworms + bulk soil physical & chemical properties)
	d1 <- data.frame(
		treatment = treat,
		rep = repn,
		depth_top = 0,
		depth_bottom = 15,
		earthworm_small_ = num(re[[4]]),
		earthworm_medium_ = num(re[[5]]),
		earthworm_large_ = num(re[[6]]),
		earthworm_density_ = num(re[[7]]),
		soil_bd = num(rb[[4]]),
		soil_EC = num(rp[[4]]),
		soil_pH = num(rp[[5]]),
		soil_SOC = num(rc[[3]]),
		# Available N, P, K reported in kg/ha for 0-15 cm; / 2.24 -> mg/kg
		soil_N = num(rn[[3]]) / 2.24,
		soil_P = num(rph[[3]]) / 2.24,
		soil_P_method = "Olsen",
		soil_K = num(rk[[3]]) / 2.24,
		soil_Zn = num(rm[[3]]),
		soil_Cu = num(rm[[5]]),
		soil_Mn = num(rm[[7]]),
		soil_Fe = num(rm[[9]])
	)

	# 15-30 cm subsoil (earthworms + bulk density)
	d2 <- data.frame(
		treatment = treat,
		rep = repn,
		depth_top = 15,
		depth_bottom = 30,
		earthworm_small_ = num(re[[8]]),
		earthworm_medium_ = num(re[[9]]),
		earthworm_large_ = num(re[[10]]),
		earthworm_density_ = num(re[[11]]),
		soil_bd = num(rb[[5]])
	)

	d <- carobiner::bindr(d1, d2)
	d$country <- "India"
	d$adm1 <- "Haryana"
	d$adm2 <- "Karnal"
	d$location <- "CSSRI, Karnal"
	# Exact coordinates from Methods 2.1: 29°42'20.7" N, 76°57'19.79" E, 243 m
	d$latitude <- 29.7058
	d$longitude <- 76.9555
	d$elevation <- 243
	d$geo_from_source <- TRUE
	d$date <- "2019-09"
	d$soil_sample_date <- "2019-09"
	d$plot_area <- 1000
	d$plot_length <- 50
	d$plot_width <- 20
	d$trial_id <- "CSSRI_Karnal_CSA_2019"
	d$sample_id <- paste(re[[2]][match(d$treatment, treat)], d$rep, d$depth_top, d$depth_bottom, sep="_")
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d)
}
