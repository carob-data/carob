# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Elemental N, P, and K application rates (kg/ha) for the five fertilization
#    treatments (SF, PL, PS, SS1, SS2) are not provided in the workbook; SS1
#    and SS2 are two sewage sludge rates whose exact quantities are only
#    reported in the associated publication (doi:10.1016/j.agee.2024.109431).
# 2. Sixteen of the 100 plot-year soil records (in 2013 and 2023) have no
#    matching rows in "Macrofauna Densities" (whether zero fauna or not
#    sampled is not stated).
# 3. Five pairs of rows in "Macrofauna Densities" (in SF1..3MULCH16) share the
#    same Sample.bis and taxonomic assignment with different abundances (kept
#    as separate rows via `order`).
# 4. Coordinates are not in the data files; approximated from the SOERE PRO
#    La Mare experimental station (Sainte-Marie, Réunion) with 2 km uncertainty.

## NEW
# fertilization_modality_: fertilization treatment code (PL, PS, SF, SS1, SS2)
# subsample_id_: matrix-specific macrofauna subsample identifier (raw Sample.bis)
# sample_matrix_: sampled compartment ("soil" or "mulch")
# fauna_phylum_: macroinvertebrate phylum
# fauna_class_: macroinvertebrate class
# fauna_order_: macroinvertebrate order
# fauna_family_: macroinvertebrate family
# fauna_genus_: macroinvertebrate genus
# fauna_species_: macroinvertebrate species
# fauna_common_: macroinvertebrate common name
# fauna_trophic_group_: macroinvertebrate trophic group
# fauna_abundance_: macroinvertebrate density (individuals/m2)
# OM_type values not in terminag: "pig slurry"

## MISSING
# yield, yield_moisture, yield_part, irrigated, N_fertilizer, P_fertilizer,
# K_fertilizer, planting_date, harvest_date.

## NOTES
# Long-term sugarcane fertilization trial (SOERE PRO Réunion, La Mare station,
# RCBD with 5 blocks x 5 treatments, plots 28 m x 9 m, 6 rows at 1.5 m spacing).
# Wide table `d` holds the 100 plot x year soil records (2013, 2016, 2019, 2023),
# with `soil_pH_KCl` computed from `pH.H2O - delta.pH`. Long table `long` holds
# all 638 TSBF soil and mulch macroinvertebrate records linked by `record_id`.

## NOT PROCESSED
# Macrofauna Densities: Abundance_indiv_625cm2 (exact 1/16 scaling of
# Abundance_indiv_m2), Site (French plot code redundant with Modality + Replicate).


carob_script <- function(path) {

"
Replication Data for: Does fertilization type drive soil and mulch macroinvertebrate communities in a sugarcane agroecosystem? Evidences from a 10-year field trial

Macroinvertebrates play a central role in processes supporting soil fertility. In the framework of the ecological intensification of agriculture, the choice of management practices should be guided by their ability to support these beneficial organisms supplying ecosystem services. This study aims at investigating the specific effect of partial substitution of synthetic fertilizers by locally produced organic fertilizers at a similar level of major nutrient inputs on macroinvertebrate’s abundance and diversity in sugarcane agroecosystems, on a Nitisol. Invertebrates visible to the naked eye were sampled in 2013, 2016, 2019 and 2023 using the standardized TSBF method on a long term experimental field trial in Réunion island. The individuals were identified and soil samples were analyzed for physico-chemical properties. Despite the low response of macroinvertebrates to the fertilizer type, total macroinvertebrate abundance increased over time, especially isopods and earthworms. The input of organic carbon via the return of litter to the soil surface and the root turnover after each harvest enables soil macroinvertebrates to be more abundant even after the replanting tillage. Mulching and root turnover are therefore important levers to consider for promoting macroinvertebrates in sugarcane agroecosystems.
"

	uri <- "doi:10.18167/DVN1/VDUXQP"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.agee.2024.109431",
		project = "SOERE PRO",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "fertilization_modality_;OM_type;OM_used",
		response_vars = "fauna_abundance_;soil_pH;soil_SOC;soil_N_total;soil_P;soil_CEC",
		carob_LLM = "Gemini 3.1 Pro",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Dataverse_Macrofauna_240229.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Soil parameters")
	r2 <- carobiner::read.excel(f, sheet="Macrofauna Densities", na="NA(notassigned)")

	mod <- trimws(as.character(r1$Modality))
	om_map <- c(
		PL = "poultry manure",
		PS = "pig slurry",
		SF = "none",
		SS1 = "sludge",
		SS2 = "sludge"
	)

	d <- data.frame(
		record_id = seq_len(nrow(r1)),
		trial_id = paste0("SOERE_PRO_", as.integer(r1$Year)),
		LTE_name = "SOERE PRO Réunion",
		sample_id = trimws(as.character(r1$Sample)),
		crop = "sugarcane",
		date = as.character(as.integer(r1$Year)),
		soil_sample_date = as.character(as.integer(r1$Year)),
		treatment = mod,
		fertilization_modality_ = mod,
		fertilizer_used = TRUE,
		OM_used = mod != "SF",
		OM_type = unname(om_map[mod]),
		rep = as.integer(r1$Replicate),
		block_id = as.character(as.integer(r1$Replicate)),
		plot_id = paste0(mod, "_", as.integer(r1$Replicate)),
		soil_pH = as.numeric(r1[["pH.H2O"]]),
		soil_pH_KCl = as.numeric(r1[["pH.H2O"]]) - as.numeric(r1[["delta.pH"]]),
		soil_SOC = as.numeric(r1$Corg),
		soil_N_total = as.numeric(r1$Ntot) * 1000,
		soil_P = as.numeric(r1$Pavail),
		soil_P_method = "Olsen-Dabin",
		soil_CEC = as.numeric(r1$CEC),
		soil_K_exch = as.numeric(r1$Kexch),
		soil_Na_exch = as.numeric(r1$Naexch),
		soil_Ca_exch = as.numeric(r1$Caexch),
		soil_Mg_exch = as.numeric(r1$Mgexch)
	)

	## Field trial design from Field trial design_Macrofauna.pdf (28 m x 9 m, 6 rows)
	d$plot_length <- 28
	d$plot_width <- 9
	d$plot_area <- 252
	d$row_spacing <- 150
	d$soil_type <- "Nitisol"

	## Location from Dataverse geographicCoverage (La Mare, Sainte-Marie, Réunion)
	d$country <- "Réunion"
	d$location <- "Sainte-Marie"
	d$site <- "Station expérimentale de La Mare"
	d$latitude <- -20.896
	d$longitude <- 55.533
	d$geo_uncertainty <- 2000
	d$geo_source <- "Mapcarta / SOERE PRO La Mare, Sainte-Marie"
	d$geo_from_source <- FALSE
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	clean <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA(notassigned)", "NA")] <- NA_character_
		x
	}

	rec_map <- stats::setNames(d$record_id, d$sample_id)
	long <- data.frame(
		record_id = unname(rec_map[trimws(as.character(r2$Sample))]),
		order = seq_len(nrow(r2)),
		subsample_id_ = trimws(as.character(r2$Sample.bis)),
		sample_matrix_ = tolower(trimws(as.character(r2$Matrix))),
		fauna_phylum_ = clean(r2$Phylum),
		fauna_class_ = clean(r2$Class),
		fauna_order_ = clean(r2$Order),
		fauna_family_ = clean(r2$Family),
		fauna_genus_ = clean(r2$Gender),
		fauna_species_ = clean(r2$Species),
		fauna_common_ = clean(r2$Common.name),
		fauna_trophic_group_ = clean(r2$Trophic.group),
		fauna_abundance_ = as.numeric(r2$Abundance_indiv_m2)
	)

	carobiner::write_files(path, meta, d, long=long)
}
