# R script for "carob"
# license: GPL (>=3)

## NOTES
# Aus rice panel (~220 genotypes) field trials at IRRI, Los Banos, well-watered (WW) and reproductive-stage
# drought (RS). Genotype MEANS (alpha lattice, 3 reps) from "aus compilation for Dataverse.xlsx", sheets
# 2010DS, 2011DS, 2012DS_all (WW), "2012DS stress early/med/late" (RS, 3 maturity groups with different drought
# onset), and 2018DS (4 experiments: 13rs drought +N+P, 13w WW +N+P, 14w WW +N+0P, 15w WW 0N+0P).
# Plot-level 2019WS data (RCBD, 16 genotypes, RS under rainout shelter) from sheet "harvest" of
# "19WS-Expt.3-data summary.xlsx".
# Units from sheet "descriptors": grain yield and straw mass g/m2 (x10 -> kg/ha); grain yield at 14% moisture
# (Liao et al. 2022, Plant Cell Environ., doi:10.1111/pce.14272, included pdf). "IR ave" = average canopy
# temperature (C) across measurement dates. Root dry weights (g) are from a 4-cm diameter soil core.
# Management (paper): transplanted ~17 DAS into puddled soil, 3-row plots of 3 m, 25 x 20 cm.

## ISSUES
# Fertilizer: paper gives 40-40-40 kg N-P-K/ha basal and ammonium sulphate topdressing "at 40 kg/ha" (product or N?);
#   2018 +N: 100 kg/ha basal + 50 kg/ha AS, +P: 50 kg/ha single superphosphate (product amounts?). Only the
#   2018 0N0P treatment (no fertilizer) is set to 0; other N/P/K left NA. Paper table labels 15w as "WW0N + P"
#   while sheet "descriptors" says 0P+0N; the descriptor is used. Verify.
# Planting dates not given (only season year). Drought onset 39-59 DAS by season.
# Genotype names differ between sheets (e.g. "29 A-2" vs "29 A 2", "ARC 10100" vs "Arc 10100"); 2018DS has no
#   accession numbers. Not harmonized; GS_accno and GS_varname kept as variety_code and variety_alt.
# 2018DS "Biomass" (g/m2) is not consistent with GY14/HI as either total or straw biomass; kept as biomass_.
# Not mapped: RGR (2011, 2012; treatment unclear), Plas RDW, GY/straw reductions (derived), root sheets of the
#   2019WS file (WW/rs 1st-3rd sampling, root length/diameter by class and depth, SDW), and the greenhouse
#   studies (BG02 lysimeter, BG11 basket, Aberdeen herbicide). "Table S1" (accession list/panels) not used.
# 2019WS plot rep not given (plot number kept). 2018DS DTF has "NA" strings (read as NA).
# Some genotype means are negative (yield down to -700 kg/ha, straw down to -3800 kg/ha), so the means are
#   probably model-adjusted (BLUE/BLUP) rather than raw means; left as recorded. Check with the authors.
# No coordinates in data or paper; georeference IRRI experimental farm, Los Banos (other IRRI papers give 14 11'N, 121 15'E).
# write_files remaining: missing coordinates, harvest_date; NA yield; out of bounds yield and dmy_residue (negative);
#   missing values in NP_treatment_ (only defined for 2018); unknown (new) variables.

## NEW VARIABLES
# canopy_temperature_: average canopy temperature (C), drought plots
# root_dw_0_15_, root_dw_15_30_, root_dw_0_30_: root dry weight (g per 4-cm soil core) at that depth (cm)
# root_deep_: root dry weight at 15-30 cm as % of 0-30 cm
# biomass_: 2018DS "Biomass" (kg/ha), definition unclear
# NP_treatment_: 2018DS nutrient treatment (+N+P, +N+0P, 0N+0P)
# maturity_group_: 2012DS drought maturity group (early, med, late)


carob_script <- function(path) {

"
Aus rice root architecture variation contributing to grain yield under drought suggests a key role of nodal root diameter class

The aus rice variety group originated in stress‐prone regions and is a promising source for the development of new stress‐tolerant rice cultivars. In this study, an aus panel (~220 genotypes) was evaluated in field trials under well‐watered and drought conditions and in the greenhouse (basket, herbicide and lysimeter studies) to investigate relationships between grain yield and root architecture, and to identify component root traits behind the composite trait of deep root growth. In the field trials, high and stable grain yield was positively related to high and stable deep root growth (r = 0.16), which may indicate response to within‐season soil moisture fluctuations (i.e., plasticity). When dissecting component traits related to deep root growth (including angle, elongation and branching), the number of nodal roots classified as 'large‐diameter' was positively related to deep root growth (r = 0.24), and showed the highest number of colocated genome‐wide association study (GWAS) peaks with grain yield under drought. The role of large‐diameter nodal roots in deep root growth may be related to their branching potential. Two candidate loci that colocated for yield and root traits were identified that showed distinct haplotype distributions between contrasting yield/stability groups and could be good candidates to contribute to rice improvement.
"

	uri <- "doi:10.7910/DVN/MCP17S"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = "doi:10.1111/pce.14272",
		project = NA,
		design = "alpha lattice (2019: RCBD); 3 replications",
		data_type = "experiment",
		treatment_vars = "variety;stress;NP_treatment_",
		response_vars = "yield;dmy_residue;flowering_days",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 60,
		carob_effort = 0.8
	)

	f1 <- ff[basename(ff) == "aus compilation for Dataverse.xlsx"]

	ids <- function(r, name) {
		data.frame(
			variety = trimws(r[[name]]),
			variety_alt = trimws(r$GS_varname),
			variety_code = ifelse(r$GS_accno == "na", NA, r$GS_accno)
		)
	}

	r1 <- carobiner::read.excel(f1, sheet="2010DS")
	d1s <- data.frame(ids(r1, "Name"), trial_id="2010DS_RS", stress="drought",
		yield = r1[["Grain Yield Stress"]] * 10,
		dmy_residue = r1[["Straw mass stress"]] * 10,
		flowering_days = r1[["DTF stress"]],
		canopy_temperature_ = r1[["2010 IR ave"]])
	d1c <- data.frame(ids(r1, "Name"), trial_id="2010DS_WW", stress="none",
		yield = r1[["Grain Yield Control"]] * 10,
		dmy_residue = r1[["Straw mass control"]] * 10,
		flowering_days = r1[["DTF control"]])

	r2 <- carobiner::read.excel(f1, sheet="2011DS")
	d2s <- data.frame(ids(r2, "Genotype"), trial_id="2011DS_RS", stress="drought",
		yield = r2[["Grain Yield Stress"]] * 10,
		dmy_residue = r2[["Straw mass stress"]] * 10,
		flowering_days = r2[["DTF stress"]],
		canopy_temperature_ = r2[["2011 IR ave"]],
		root_dw_0_30_ = r2[["Root DW 0-30 stress"]],
		root_dw_0_15_ = r2[["Root DW A stress"]],
		root_deep_ = r2[["% Deep Root 15-30 stress"]])
	d2c <- data.frame(ids(r2, "Genotype"), trial_id="2011DS_WW", stress="none",
		yield = r2[["Grain Yield Control"]] * 10,
		dmy_residue = r2[["Straw mass control"]] * 10,
		flowering_days = r2[["DTF control"]])

	r3 <- carobiner::read.excel(f1, sheet="2012DS_all")
	d3c <- data.frame(ids(r3, "Genotype"), trial_id="2012DS_WW", stress="none",
		yield = r3[["Grain Yield Control"]] * 10,
		flowering_days = r3[["DTF control"]],
		root_dw_0_15_ = r3[["RDW A control"]],
		root_dw_15_30_ = r3[["RDW B control"]],
		root_deep_ = r3[["% Deep Root 15-30 control"]])

	get2012s <- function(sheet, grp, name) {
		r <- carobiner::read.excel(f1, sheet=sheet)
		data.frame(ids(r, name), trial_id=paste0("2012DS_RS_", grp), stress="drought", maturity_group_=grp,
			yield = r[["Grain Yield Stress"]] * 10,
			dmy_residue = r[["Straw mass stress"]] * 10,
			flowering_days = r$DTF,
			canopy_temperature_ = r[["IR ave"]],
			root_dw_0_15_ = r[["Root DW 0-15 cm"]],
			root_dw_15_30_ = r[["Root DW 15-30 cm"]],
			root_deep_ = r[[grep("deep roots", names(r), ignore.case=TRUE)]])
	}
	d3e <- get2012s("2012DS stress early", "early", "Genotype")
	d3m <- get2012s("2012DS stress med", "med", "Genotype")
	d3l <- get2012s("2012DS stress late", "late", "Name")

	r4 <- carobiner::read.excel(f1, sheet="2018DS", na=c("", "NA"))
	d4 <- data.frame(
		variety = trimws(r4$Genotype),
		trial_id = r4$Expt,
		stress = ifelse(grepl("rs", r4$Expt), "drought", "none"),
		biomass_ = r4$Biomass * 10,
		flowering_days = r4$DTF,
		yield = r4[["GY 14"]] * 10,
		harvest_index = r4$HI * 100
	)
	np <- c("13rs-18DS"="+N+P", "13w-18DS"="+N+P", "14w-18DS"="+N+0P", "15w-18DS"="0N+0P")
	d4$NP_treatment_ <- np[d4$trial_id]

	f5 <- ff[basename(ff) == "19WS-Expt.3-data summary.xlsx"]
	r5 <- carobiner::read.excel(f5, sheet="harvest", skip=1)
	d5 <- data.frame(
		trial_id = paste0("2019WS_", r5$Expt),
		stress = ifelse(r5$Expt == "3rs", "drought", "none"),
		variety = trimws(r5$Genotype),
		plot_id = as.character(r5$Plot),
		yield = r5[["grain yield (g/m2)"]] * 10,
		dmy_residue = r5[["Biomass(g/m2)"]] * 10,
		harvest_index = r5$HI * 100
	)

	d <- carobiner::bindr(d1s, d1c, d2s, d2c, d3c, d3e, d3m, d3l, d4, d5)

	d$season <- ifelse(grepl("WS", d$trial_id), "wet", "dry")
	d$planting_date <- ifelse(grepl("18DS", d$trial_id), "2018", substr(d$trial_id, 1, 4))
	d$treatment <- d$trial_id
	d$N_fertilizer <- ifelse(d$NP_treatment_ %in% "0N+0P", 0, NA)
	d$P_fertilizer <- ifelse(d$NP_treatment_ %in% c("0N+0P", "+N+0P"), 0, NA)
	d$K_fertilizer <- ifelse(d$NP_treatment_ %in% "0N+0P", 0, NA)

	d$crop <- "rice"
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$flooded <- d$stress == "none"
	d$planting_method <- "transplanting"
	d$transplanting_days <- 17
	d$row_spacing <- 25
	d$plant_spacing <- 20
	d$plot_length <- 3

	d$country <- "Philippines"
	d$adm1 <- "Laguna"
	d$location <- "IRRI, Los Banos"

	carobiner::write_files(path, meta, d)
}
