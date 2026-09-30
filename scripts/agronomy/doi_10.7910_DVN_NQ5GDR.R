# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil biology field trials in Colombia 1990-2001: 
            # litter decomposition, N fixation, soil N and microbial biomass under pastures and crops; 
            # maize yield response to urea and tree prunings (TSBF)
# Green manure- assumed foliage
# greenhouse experiments (SITIO == "CIAT - Invernaderos") are not standardized & where mixed greenhouse rows are dropped
                       # Green house only: 11.MBIO_INOC_INV.csv, 14.MBIO_FIJA_N_INV.csv
                       # Mixed fields & greenhouse: f5, f12
                       # lookup tables with greenhouse entries: f2, f4

## Suggested new terms
# agrozone (agroecological zone of the site; here "savanna" or "hillside")
# residue_placement_: litterbag placement (surface, incorporated, initial sample)
# plant_part_: plant part in litterbag
# sampling_no_: evaluation (sampling) number
# bag_batch_: litterbag distribution number
# plant_no_: tree number within plot
# plant_species: plant species as given in the data (CULT)

## ISSUES
# f5 PARTE: 20 rows have "Suelo" (soil), which is not a plant part; set to NA
# f17 HUME treated as grain moisture at harvest
# out of bounds left as is: residue_C (191-617 mg/g, valid_max 10), residue_K, leaf_N, grain_N, soil_N_total, soil_NO3 (low values incl. 0), tree plant_height (up to 578 cm)
# treatments apply to one trial each: N rates RFN60, OM_used RFN42, inoculated RFN49; crop NA for tree species, prunings and RFN53 land uses
# yield only for RFN60 maize (Feb 2001); other plots have no yield
# crop terms not in terminag: centrosema, desmodium, gamba grass, pinto peanut, tropical kudzu


carob_script <- function(path) {

"
Long-term field experiments carried out in savannas and hillsides agroecosystems by Soil Biology (PE-2: Soils project of CIAT)

The soil biology (PE-2: Soils project of CIAT) had several field experiments in both tropical savannas and
hillsides agroecosystems. The data collected from 1990 to 2000 were organized systematically.
"

	uri <- "doi:10.7910/DVN/NQ5GDR"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "CIAT",
		publication = "hdl:10568/71571;hdl:10568/54288;hdl:10568/56187",
		project = "TSBF; Soil Biology (PE-2)",
		design = "Multiple long-term experiments on forage/legume cover fixation assessed via 15N isotope dilution.",
		data_type = "experiment",   # on-station and on-farm
		treatment_vars = "crop;N_fertilizer;N_organic;OM_used;inoculated",
		response_vars = "yield;dmy_total;dmy_storage;dmy_stems;dmy_roots;grain_N;root_N;root_K",
		notes = NA,
		carob_contributor = "Stella Muthoni",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-09-22",
		carob_completion = 80,
		carob_effort = 5
	)

	f2 <- ff[basename(ff) == "02.MBIO_SITIOS.csv"]         # sites
	f4 <- ff[basename(ff) == "04.MBIO_TRATAMIENTOS.csv"]   # site x trial x treatment x species
	f5 <- ff[basename(ff) == "05.MBIO_DESC_HOJ_RAI.csv"]   # decomposition of dead leaves or roots - N/P/K/Ca/Mg by plant part
	f7 <- ff[basename(ff) == "07.MBIO_ANAL_SUE.csv"]       # soil analysis - total N, organic matter
	f8 <- ff[basename(ff) == "08.MBIO_FIJA_N.csv"]         # nitrogen fixation
	f10 <- ff[basename(ff) == "10.MBIO_BALANCE.csv"]       # whole-plant N partitioning by part
	f12 <- ff[basename(ff) == "12.MBIO_15N_SUE.csv"]       # 15N atom percent in soil
	f13 <- ff[basename(ff) == "13.MBIO_POTE_MIN.csv"]      # mineralization potential - ammonium/nitrate at day 0 and day 7
	f15 <- ff[basename(ff) == "15.MBIO_NIVE_N.csv"]        # N level - ammonium/nitrate in soil
	f16 <- ff[basename(ff) == "16.MBIO_BIOM_MIC.csv"]      # microbial biomass - carbon in fumigated/nonfumigated soil
	f17 <- ff[basename(ff) == "17.MBIO_PRODUCCI.csv"]      # production - yield, dry matter by plant part, N/P/K content
	f18 <- ff[basename(ff) == "18.MBIO_PLANTAS.csv"]       # plants - diameter, height

	r2 <- read.csv(f2)
	names(r2) <- c("zone","country","adm1","adm2","location","site","site_description")

	r4 <- read.csv(f4)
	names(r4) <- c("site","trial","treatment","species","treatment_description","species_description")

	r5 <- read.csv(f5)
	names(r5) <- c("trial_code","site","trial","date","treatment","species","rep","subtreatment",
	               "part","eval_number","distribution_number","days_after_distribution","dry_matter",
	               "ash","dry_matter_corrected","dry_matter_residual","n_pct","p_pct","k_pct","ca_pct",
	               "mg_pct","c_pct","organic_matter","lignin","digestible_om","hemicellulose","tannin","phenols",
	               "tannin_soluble","tannin_insoluble")

	r7 <- read.csv(f7)
	names(r7) <- c("trial_code","site","trial","date","treatment","species","rep","depth_range","n_total","organic_matter")

	r8 <- read.csv(f8)
	names(r8) <- c("trial_code","site","trial","date","treatment","species","rep",
	               "dry_matter_legume","dry_matter_grass","n_legume_pct","n_grass_pct",
	               "n_total_legume","n_total_grass","atom15_legume","atom15_grass","atom15_savanna",
	               "atom15_excess_legume","atom15_excess_grass","atom15_excess_savanna",
	               "n_atm_derived_grass","n_atm_derived_savanna","n_fixed_grass","n_fixed_savanna",
	               "legume_pct","n_from_fertilizer_grass","n_from_soil_grass")

	r10 <- read.csv(f10)
	names(r10) <- c("trial_code","site","trial","date","treatment","species","rep",
	                "sampling_area","stem","leaf","husk","cob","straw","grain",
	                "dry_matter_total","dry_matter_total_m2",
	                "pct_stem","pct_leaf","pct_husk","pct_cob","pct_straw","pct_grain",
	                "n_stem","n_leaf","n_husk","n_cob","n_straw","n_grain","n_total",
	                "n_plant","n_plant_m2",
	                "atom15_stem","atom15_leaf","atom15_husk","atom15_cob","atom15_straw","atom15_grain",
	                "atom15_n_total","atom15_excess","atom15_excess_n_total","atom15_excess_plant",
	                "pct_n_green_manure","n_green_manure","n_green_manure_m2")

	r12 <- read.csv(f12)
	names(r12) <- c("trial_code","site","trial","date","treatment","species","rep",
	                "depth_range","amount_15n_applied","amount_15n_excess_applied","sampling_area",
	                "atom15_fertilizer","wet_weight_soil_frame","wet_weight_soil_subsample",
	                "dry_weight_soil_subsample","dry_weight_soil_frame","dry_weight_soil_m2",
	                "soil_moisture","n_pct","n_total","atom15","atom15_excess","atom15_excess_g","recovery_15n")

	r13 <- read.csv(f13)
	names(r13) <- c("trial_code","site","trial","date","treatment","species","rep",
	                "eval_number","depth_range","nh4_day0","no3_day0","nh4_day7","no3_day7",
	                "nh4_dry_day0","no3_dry_day0","nh4_dry_day7","no3_dry_day7",
	                "n_total_dry_day0","n_diff_day7_day0","n_mineralization_potential")

	r15 <- read.csv(f15)
	names(r15) <- c("trial_code","site","trial","date","treatment","species","rep",
	                "depth_range","wet_weight_soil","dry_weight_soil",
	                "nh4","no3","nh4_wet","no3_wet","nh4_dry","no3_dry","n_total_dry")

	r16 <- read.csv(f16)
	names(r16) <- c("trial_code","site","trial","date","treatment","species","rep",
	                "depth_range","wet_weight_soil","dry_weight_soil",
	                "c_fumigated","c_nonfumigated","c_difference","microbial_c_wet","microbial_c_dry")

	r17 <- read.csv(f17)
	names(r17) <- c("trial_code","site","trial","date","treatment","species","rep",
	                "sampling_area","cane_wet","cane_wet_subsample","husk_wet","husk_wet_subsample",
	                "ear_wet","cane_dry_subsample","husk_dry_subsample",
	                "ear_dry_matter","root_dry_matter","cane_dry_matter","husk_dry_matter","cob_dry_matter",
	                "root_dry_matter_ha","cane_dry_matter_ha","husk_dry_matter_ha","cob_dry_matter_ha",
	                "grain","moisture","grain_at_12pct","yield",
	                "n_grain","n_cane","p_cane","k_cane","n_root","p_root","k_root")

	r18 <- read.csv(f18)
	names(r18) <- c("trial_code","site","trial","date","treatment","species",
	                "plant_number","days_after_transplant","plant_diameter","plant_height")

	# drop greenhouse records
	gh <- "CIAT - Invernaderos"
	r2 <- r2[!r2$site %in% gh, ]
	r4 <- r4[!r4$site %in% gh, ]
	r5 <- r5[!r5$site %in% gh, ]
	r12 <- r12[!r12$site %in% gh, ]

	# sites (joins to the other d* by "site")
	# Location lat, lon are extracted from carobiner::geocode or Google maps
	loc <- gsub("\\s+", " ", r2$location)
	d2 <- data.frame(
		country = r2$country,
		adm1 = r2$adm1,
		adm2 = r2$adm2,
		location = loc,
		site = r2$site,
		agrozone = c(Llanos="savanna", Laderas="hillside")[r2$zone],        # suggested term
		longitude = c("Estacion Experimental - Carimagua" = -71.3358,       #Google
		              "Estacion Experimental - S.Quilichao" = -76.4940,     #Google
		              "Finca Matazul" = -72.6036,                           #Google
		              "Vereda Pescador" = -76.5473)[loc],                   #Geocode
		latitude = c("Estacion Experimental - Carimagua" = 4.5716,
		             "Estacion Experimental - S.Quilichao" = 3.0804,
		             "Finca Matazul" = 4.1709,
		             "Vereda Pescador" = 2.8043)[loc],
		geo_from_source = FALSE
	)

	# treatments (joins to data d* by site, trial_name, treatment, plant_species)
	d4 <- data.frame(
		site = r4$site,
		trial_name = r4$trial,
		treatment = r4$treatment,
		plant_species = r4$species
	)
	# N rates only for TSBF trial (RFN60)
	td <- r4$treatment_description
	i <- grepl("N Inorg kg/ha", td)
	d4$N_fertilizer[i] <- as.numeric(sub("N Inorg.*", "", td[i]))
	d4$fertilizer_type[i] <- ifelse(d4$N_fertilizer[i] > 0, "urea", "none")
	d4$N_organic[i] <- as.numeric(sub(".*\\+ ([0-9.]+)N Org.*", "\\1", td[i]))
	d4$OM_used[i] <- d4$N_organic[i] > 0
	d4$OM_type[i] <- ifelse(d4$OM_used[i], "foliage", "none")   # leaf prunings
	# cowpea green manure (Satelite)
	j <- r4$trial %in% "Satelite"
	d4$OM_used[j] <- grepl("Con caupi", r4$treatment[j])
	d4$OM_type[j] <- ifelse(d4$OM_used[j], "foliage", "none")
	# rhizobium inoculation of trees (Leguminosa arborea); "#NAME?" is Excel for "+ N"
	d4$treatment[d4$treatment == "#NAME?"] <- "+ N"
	j <- r4$trial %in% "Leguminosa arborea"
	d4$inoculated[j] <- grepl("^Con ", d4$treatment[j])

	# litterbag decomposition
	pp <- c(Hoja = "leaves", Tallo = "stems", "Hoja + Tallo" = "leaves;stems")   # "Suelo" to NA
	pl <- c(Cobertura = "surface", Incorporado = "incorporated", "Muestra Inicial" = "initial sample")
	d5 <- data.frame(
		trial_id = r5$trial_code,
		site = r5$site,
		trial_name = r5$trial,
		treatment = r5$treatment,
		plant_species = r5$species,
		block_id = as.character(r5$rep),
		date = as.character(as.Date(r5$date, "%d-%b-%y")),
		residue_placement_ = pl[r5$subtreatment],
		plant_part_ = pp[r5$part],
		sampling_no_ = r5$eval_number,
		bag_batch_ = r5$distribution_number,
		residue_N = r5$n_pct * 10,
		residue_P = r5$p_pct * 10,
		residue_K = r5$k_pct * 10,
		residue_Ca = r5$ca_pct * 10,
		residue_Mg = r5$mg_pct * 10,
		residue_C = r5$c_pct * 10
	)


	# soil N and organic matter
	d7 <- data.frame(
		trial_id = r7$trial_code,
		site = r7$site,
		trial_name = r7$trial,
		treatment = r7$treatment,
		plant_species = r7$species,
		block_id = as.character(r7$rep),
		date = as.character(as.Date(r7$date, "%d-%b-%y")),
		depth_top = as.numeric(sub("-.*", "", r7$depth_range)),
		depth_bottom = as.numeric(sub(".*-", "", r7$depth_range)),
		soil_N_total = r7$n_total,
		soil_SOM = r7$organic_matter
	)

	# N fixation
	d8 <- data.frame(
		trial_id = r8$trial_code,
		site = r8$site,
		trial_name = r8$trial,
		treatment = r8$treatment,
		plant_species = r8$species,
		block_id = as.character(r8$rep),
		date = as.character(as.Date(r8$date, "%d-%b-%y")),
		N_fixation = r8$n_fixed_grass   # grass as reference
	)


	# N balance at harvest; kg per sampling area to kg/ha
	d10 <- data.frame(
		trial_id = r10$trial_code,
		site = r10$site,
		trial_name = r10$trial,
		treatment = r10$treatment,
		plant_species = r10$species,
		block_id = as.character(r10$rep),
		date = as.character(as.Date(r10$date, "%d-%b-%y")),
		crop = c(Arroz = "rice", Maiz = "maize")[r10$species],
		sampled_plot_area = r10$sampling_area,
		dmy_stems = r10$stem * 10000 / r10$sampling_area,
		dmy_leaves = r10$leaf * 10000 / r10$sampling_area,
		dmy_residue = r10$straw * 10000 / r10$sampling_area,   # rice straw
		dmy_storage = r10$grain * 10000 / r10$sampling_area,
		dmy_total = r10$dry_matter_total * 10000 / r10$sampling_area,
		leaf_N = r10$n_leaf * 10,
		grain_N = r10$n_grain * 10
	)

	# 15N in soil
	d12 <- data.frame(
		trial_id = r12$trial_code,
		site = r12$site,
		trial_name = r12$trial,
		treatment = r12$treatment,
		plant_species = r12$species,
		block_id = as.character(r12$rep),
		date = as.character(as.Date(r12$date, "%d-%b-%y")),
		depth_top = as.numeric(sub("-.*", "", r12$depth_range)),
		depth_bottom = as.numeric(sub(".*-", "", r12$depth_range)),
		soil_N_total = r12$n_pct * 10000
	)

	# N mineralization, 7-day incubation
	d13 <- data.frame(
		trial_id = r13$trial_code,
		site = r13$site,
		trial_name = r13$trial,
		treatment = r13$treatment,
		plant_species = r13$species,
		block_id = as.character(r13$rep),
		date = as.character(as.Date(r13$date, "%d-%b-%y")),
		depth_top = as.numeric(sub("-.*", "", r13$depth_range)),
		depth_bottom = as.numeric(sub(".*-", "", r13$depth_range)),
		sampling_no_ = r13$eval_number,
		soil_NH4 = r13$nh4_dry_day0,
		soil_NO3 = r13$no3_dry_day0
	)

	# soil mineral N
	d15 <- data.frame(
		trial_id = r15$trial_code,
		site = r15$site,
		trial_name = r15$trial,
		treatment = r15$treatment,
		plant_species = r15$species,
		block_id = as.character(r15$rep),
		date = as.character(as.Date(r15$date, "%d-%b-%y")),
		depth_top = as.numeric(sub("-.*", "", r15$depth_range)),
		depth_bottom = as.numeric(sub(".*-", "", r15$depth_range)),
		soil_NH4 = r15$nh4_dry,
		soil_NO3 = r15$no3_dry
	)

	# microbial biomass C
	d16 <- data.frame(
		trial_id = r16$trial_code,
		site = r16$site,
		trial_name = r16$trial,
		treatment = r16$treatment,
		plant_species = r16$species,
		block_id = as.character(r16$rep),
		date = as.character(as.Date(r16$date, "%d-%b-%y")),
		depth_top = as.numeric(sub("-.*", "", r16$depth_range)),
		depth_bottom = as.numeric(sub(".*-", "", r16$depth_range)),
		soil_MBC = r16$microbial_c_dry
	)

	# maize; yield only in 2001
	h <- !is.na(r17$yield)
	d17 <- data.frame(
		trial_id = r17$trial_code,
		site = r17$site,
		trial_name = r17$trial,
		treatment = r17$treatment,
		plant_species = r17$species,
		block_id = as.character(r17$rep),
		date = as.character(as.Date(r17$date, "%d-%b-%y")),
		crop = "maize",
		harvest_date = ifelse(h, as.character(as.Date(r17$date, "%d-%b-%y")), NA),
		yield = r17$yield,
		yield_moisture = ifelse(h, 12.5, NA),
		yield_part = ifelse(h, "grain", NA),
		storage_moisture = r17$moisture,   # grain, at harvest
		sampled_plot_area = r17$sampling_area,
		dmy_stems = r17$cane_dry_matter_ha,
		dmy_roots = r17$root_dry_matter_ha,
		grain_N = r17$n_grain * 10,
		root_N = r17$n_root * 10,
		root_K = r17$k_root * 10
	)

	# tree growth
	d18 <- data.frame(
		trial_id = r18$trial_code,
		site = r18$site,
		trial_name = r18$trial,
		treatment = r18$treatment,
		plant_species = r18$species,
		date = as.character(as.Date(r18$date, "%d-%b-%y")),
		plant_no_ = r18$plant_number,
		DAP = r18$days_after_transplant,   # transplanted seedlings
		plant_height = r18$plant_height
	)

	# measurements are on different dates per file: stack, don't merge
	long <- carobiner::bindr(d5, d7, d8, d12, d13, d15, d16, d18)
	long$trial_id <- paste(long$trial_id, long$site, sep = "_")

	# harvests: one row per plot and harvest, go to d
	hv <- carobiner::bindr(d10, d17)
	hv$trial_id <- paste(hv$trial_id, hv$site, sep = "_")

	# plots
	k <- c("trial_id", "site", "trial_name", "treatment", "plant_species", "block_id")
	d <- unique(rbind(long[, k], hv[, k]))
	d <- merge(d, d4, by = c("site", "trial_name", "treatment", "plant_species"), all.x = TRUE)
	d <- merge(d, d2, by = "site", all.x = TRUE)
	d$plot_id <- as.character(seq_len(nrow(d)))

	long <- merge(long, d[, c(k, "plot_id")], by = k)
	long <- long[, !names(long) %in% k]
	d <- merge(d, hv, by = k, all.x = TRUE)

	# plant_species to crop; trees, prunings and "SZ"/"Forest" (NA) left NA
	cr <- c(Maiz = "maize", Arroz = "rice", Soya = "soybean", Caupi = "cowpea", Yuca = "cassava",
	        B.brizantha = "brachiaria", B.decubens = "brachiaria", B.decumbens = "brachiaria",
	        B.dictyoneura = "brachiaria", B.humidicola = "brachiaria",
	        A.gayanus = "gamba grass", A.pintoi = "pinto peanut", C.acutifolium = "centrosema",
	        D.ovalifolium = "desmodium", P.phaseoloides = "tropical kudzu",
	        S.capitata = "stylosanthes", S.guianensis = "stylosanthes",
	        Pasto = "pasture", "Sabana nativa" = "pasture",   # native savanna
	        "B.decumbens + P.phaseoloides" = "brachiaria_tropical kudzu",
	        "B.dictyoneura + A.pintoi" = "brachiaria_pinto peanut",
	        "B.dictyoneura + C.acutifolium" = "brachiaria_centrosema",
	        "B.dictyoneura + S.capitata" = "brachiaria_stylosanthes",
	        "B.humidicola + A.pintoi" = "brachiaria_pinto peanut")
	d$crop <- cr[d$plant_species]

	d$on_farm <- d$location %in% c("Finca Matazul", "Vereda Pescador")
	d$is_survey <- FALSE

	d$irrigated <- NA
	d$planting_date <- as.character(NA)
	d$P_fertilizer <- as.numeric(NA)
	d$K_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d, long = long)
}

