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
	r4 <- read.csv(f4)
	r5 <- read.csv(f5)
	r7 <- read.csv(f7)
	r8 <- read.csv(f8)
	r10 <- read.csv(f10)
	r12 <- read.csv(f12)
	r13 <- read.csv(f13)
	r15 <- read.csv(f15)
	r16 <- read.csv(f16)
	r17 <- read.csv(f17)
	r18 <- read.csv(f18)
	
	# drop greenhouse records
	gh <- "CIAT - Invernaderos"
	r2 <- r2[!r2$SITIO %in% gh, ]
	r4 <- r4[!r4$SITIO %in% gh, ]
	r5 <- r5[!r5$SITIO %in% gh, ]
	r12 <- r12[!r12$SITIO %in% gh, ]

	# sites (joins to the other d* by "site")
	# Location lat, lon are extracted from carobiner::geocode or Google maps
	loc <- gsub("\\s+", " ", r2$UBICACION)
	d2 <- data.frame(
	  country = r2$PAIS,
	  adm1 = r2$DEPARTAMENTO,
	  adm2 = r2$CIUDAD,
	  location = loc,
	  site = r2$SITIO,
	  agrozone = c(Llanos="savanna", Laderas="hillside")[r2$ZONA],        # suggested term
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
	  site = r4$SITIO,
	  trial_name = r4$ENSAYO,
	  treatment = r4$TRAT,
	  plant_species = r4$CULT
	)
	# N rates only for TSBF trial (RFN60)
	td <- r4$DESC_TRAT
	i <- grepl("N Inorg kg/ha", td)
	d4$N_fertilizer[i] <- as.numeric(sub("N Inorg.*", "", td[i]))
	d4$fertilizer_type[i] <- ifelse(d4$N_fertilizer[i] > 0, "urea", "none")
	d4$N_organic[i] <- as.numeric(sub(".*\\+ ([0-9.]+)N Org.*", "\\1", td[i]))
	d4$OM_used[i] <- d4$N_organic[i] > 0
	d4$OM_type[i] <- ifelse(d4$OM_used[i], "foliage", "none")   # leaf prunings
	# cowpea green manure (Satelite)
	j <- r4$ENSAYO %in% "Satelite"
	d4$OM_used[j] <- grepl("Con caupi", r4$TRAT[j])
	d4$OM_type[j] <- ifelse(d4$OM_used[j], "foliage", "none")
	# rhizobium inoculation of trees (Leguminosa arborea); "#NAME?" is Excel for "+ N"
	d4$treatment[d4$treatment == "#NAME?"] <- "+ N"
	j <- r4$ENSAYO %in% "Leguminosa arborea"
	d4$inoculated[j] <- grepl("^Con ", d4$treatment[j])

	# litterbag decomposition
	pp <- c(Hoja = "leaves", Tallo = "stems", "Hoja + Tallo" = "leaves;stems")   # "Suelo" to NA
	pl <- c(Cobertura = "surface", Incorporado = "incorporated", "Muestra Inicial" = "initial sample")
	d5 <- data.frame(
	  trial_id = r5$CODIGO,
	  site = r5$SITIO,
	  trial_name = r5$ENSAYO,
	  treatment = r5$TRAT,
	  plant_species = r5$CULT,
	  block_id = as.character(r5$REPI),
	  date = as.character(as.Date(r5$FECH_EVA, "%d-%b-%y")),
	  residue_placement_ = pl[r5$SUBTRAT],
	  plant_part_ = pp[r5$PARTE],
	  sampling_no_ = r5$EVAL,
	  bag_batch_ = r5$DISTRIBUCION,
	  residue_N = r5$N * 10,
	  residue_P = r5$P * 10,
	  residue_K = r5$K * 10,
	  residue_Ca = r5$CA * 10,
	  residue_Mg = r5$MG * 10,
	  residue_C = r5$C * 10
	)
	
	
	# soil N and organic matter
	d7 <- data.frame(
	  trial_id = r7$CODIGO,
	  site = r7$SITIO,
	  trial_name = r7$ENSAYO,
	  treatment = r7$TRAT,
	  plant_species = r7$CULT,
	  block_id = as.character(r7$REPI),
	  date = as.character(as.Date(r7$FECH_EVA, "%d-%b-%y")),
	  depth_top = as.numeric(sub("-.*", "", r7$PROF)),
	  depth_bottom = as.numeric(sub(".*-", "", r7$PROF)),
	  soil_N_total = r7$N_TOT,
	  soil_SOM = r7$MO
	)
	
	# N fixation
	d8 <- data.frame(
	  trial_id = r8$CODIGO,
	  site = r8$SITIO,
	  trial_name = r8$ENSAYO,
	  treatment = r8$TRAT,
	  plant_species = r8$CULT,
	  block_id = as.character(r8$REPI),
	  date = as.character(as.Date(r8$FECH_EVA, "%d-%b-%y")),
	  N_fixation = r8$N_FIJ_GRAM   # grass as reference
	)
	
	
	# N balance at harvest; kg per sampling area to kg/ha
	d10 <- data.frame(
	  trial_id = r10$CODIGO,
	  site = r10$SITIO,
	  trial_name = r10$ENSAYO,
	  treatment = r10$TRAT,
	  plant_species = r10$CULT,
	  block_id = as.character(r10$REPI),
	  date = as.character(as.Date(r10$FECH_EVA, "%d-%b-%y")),
	  crop = c(Arroz = "rice", Maiz = "maize")[r10$CULT],
	  sampled_plot_area = r10$AREA_MUE,
	  dmy_stems = r10$TALLO * 10000 / r10$AREA_MUE,
	  dmy_leaves = r10$HOJA * 10000 / r10$AREA_MUE,
	  dmy_residue = r10$TAMO * 10000 / r10$AREA_MUE,   # rice straw
	  dmy_storage = r10$GRANO * 10000 / r10$AREA_MUE,
	  dmy_total = r10$MATE_SEC_TOT * 10000 / r10$AREA_MUE,
	  leaf_N = r10$N_HOJ * 10,
	  grain_N = r10$N_GRA * 10
	)
	
	# 15N in soil
	d12 <- data.frame(
	  trial_id = r12$CODIGO,
	  site = r12$SITIO,
	  trial_name = r12$ENSAYO,
	  treatment = r12$TRAT,
	  plant_species = r12$CULT,
	  block_id = as.character(r12$REPI),
	  date = as.character(as.Date(r12$FECH_EVA, "%d-%b-%y")),
	  depth_top = as.numeric(sub("-.*", "", r12$PROF)),
	  depth_bottom = as.numeric(sub(".*-", "", r12$PROF)),
	  soil_N_total = r12$N * 10000
	)
	
	# N mineralization, 7-day incubation
	d13 <- data.frame(
	  trial_id = r13$CODIGO,
	  site = r13$SITIO,
	  trial_name = r13$ENSAYO,
	  treatment = r13$TRAT,
	  plant_species = r13$CULT,
	  block_id = as.character(r13$REPI),
	  date = as.character(as.Date(r13$FECH_EVA, "%d-%b-%y")),
	  depth_top = as.numeric(sub("-.*", "", r13$PROF)),
	  depth_bottom = as.numeric(sub(".*-", "", r13$PROF)),
	  sampling_no_ = r13$EVAL,
	  soil_NH4 = r13$NNH4_SS_T0,
	  soil_NO3 = r13$NNO3_SS_T0
	)

	# soil mineral N
	d15 <- data.frame(
	  trial_id = r15$CODIGO,
	  site = r15$SITIO,
	  trial_name = r15$ENSAYO,
	  treatment = r15$TRAT,
	  plant_species = r15$CULT,
	  block_id = as.character(r15$REPI),
	  date = as.character(as.Date(r15$FECH_EVA, "%d-%b-%y")),
	  depth_top = as.numeric(sub("-.*", "", r15$PROF)),
	  depth_bottom = as.numeric(sub(".*-", "", r15$PROF)),
	  soil_NH4 = r15$NNH4_SS,
	  soil_NO3 = r15$NNO3_SS
	)
	
	# microbial biomass C
	d16 <- data.frame(
	  trial_id = r16$CODIGO,
	  site = r16$SITIO,
	  trial_name = r16$ENSAYO,
	  treatment = r16$TRAT,
	  plant_species = r16$CULT,
	  block_id = as.character(r16$REPI),
	  date = as.character(as.Date(r16$FECH_EVA, "%d-%b-%y")),
	  depth_top = as.numeric(sub("-.*", "", r16$PROF)),
	  depth_bottom = as.numeric(sub(".*-", "", r16$PROF)),
	  soil_MBC = r16$C_MIC_SS
	)
	
	# maize; yield only in 2001
	h <- !is.na(r17$RENDIMIENTO)
	d17 <- data.frame(
	  trial_id = r17$CODIGO,
	  site = r17$SITIO,
	  trial_name = r17$ENSAYO,
	  treatment = r17$TRAT,
	  plant_species = r17$CULT,
	  block_id = as.character(r17$REPI),
	  date = as.character(as.Date(r17$FECH_EVA, "%d-%b-%y")),
	  crop = "maize",
	  harvest_date = ifelse(h, as.character(as.Date(r17$FECH_EVA, "%d-%b-%y")), NA),
	  yield = r17$RENDIMIENTO,
	  yield_moisture = ifelse(h, 12.5, NA),
	  yield_part = ifelse(h, "grain", NA),
	  storage_moisture = r17$HUME,   # grain, at harvest
	  sampled_plot_area = r17$AREA_MUE,
	  dmy_stems = r17$MATE_SEC_CAN_HEC,
	  dmy_roots = r17$MATE_SEC_RAI_HEC,
	  grain_N = r17$N_GRA * 10,
	  root_N = r17$N_RAI * 10,
	  root_K = r17$K_RAI * 10
	)
	
	# tree growth
	d18 <- data.frame(
	  trial_id = r18$CODIGO,
	  site = r18$SITIO,
	  trial_name = r18$ENSAYO,
	  treatment = r18$TRAT,
	  plant_species = r18$CULT,
	  date = as.character(as.Date(r18$FECH_EVA, "%d-%b-%y")),
	  plant_no_ = r18$NUME_PLA,
	  DAP = r18$DIA_DES_TRA,   # transplanted seedlings
	  plant_height = r18$ALTU_PLA
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

