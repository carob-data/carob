# R script for "carob"
# license: GPL (>=3)

## NOTES
# Survey of seed tubers from farmer fields in Bolívar, Chimborazo, and Tungurahua, then grown out at CIP-Quito. The title names Ecuador. Coordinates, altitude, community, seed source, and field area are on the geographic sheet. geo_from_source is TRUE for those farmer-field coordinates.
# Dictionary localidad codes: Culebrillas 1, Guapungoto Tishiringui field 1 is 2, San Simon 3, Calerita de Santa Rosa field 1 is 4, field 2 is 5, Ballagan 6, Tamboloma 7, Yachil 8, Guabug 9, Guapungoto Tishiringui field 2 is 10, La Delicia 11. The geographic sheet does not number the two Calerita fields, so sheet order is used (first row field 1). Province is filled down the geographic sheet.
# Variety codes: Dolores 1, Única 2, INIAP-Fripapa 3, Chaucha Roja 4, INIAP-Gabriela 5, INIAP-Yana Shungo 6. The dictionary writes Yanashungo as one word. The geographic sheet spelling is used.
# Numero repeats within a field, up to three times, so plot_id is the sheet row. tuber_code_ keeps Numero.
# Seed-surface and progeny percentages are the long table. Two black-scurf cells are written 0,1 and stored as 0.1.
# Plant height is meters on the dictionary and is stored in cm. One value is 0 and one is 108.5 m.
# Progeny weights are grams per plant. The Quito planting has no plot area or spacing in the file, so they are not converted to kg/ha. The farmer field area is the source field, not that planting.
# The abstract says the tubers were planted from October 2010 through December 2011. The planting date stored is 2010-10.

## ISSUES
# No fertilizer, irrigation, harvest date, or CIP-Quito coordinates. The abstract gives CIP-Quito as 3058 m.
# plant_height has one value recorded as 108.5 (m -> 10850 cm, likely entered in cm); kept as recorded.
# Several diseases and pests in the long table (Andean potato latent virus, Andean potato mottle virus, potato yellow vein virus, black leg, black scurf, fusarium, silver scurf, powdery scab, Andean potato weevil, potato tuber moth, white grub, wireworm, flea beetle) are not yet in terminag values_disease / values_pest; kept in disease / pest_species per Carob guidelines so maintainers can add them.

## NOT PROCESSED
# Brote. It is not in the dictionary.
# CIC. The dictionary calls it a quality index and the formula only says see S_Rhizoc.
# Dictionary.xlsx, other than the codes and units used above.


## NEW VARIABLES
# tuber_code_: tuber number on the sheet. It is not unique inside a field.
# source_field_area_: area of the farmer field (m2).
# seed_tuber_weight_: seed tuber fresh weight (g).
# sprout_number_: sprouts on the seed tuber.
# emerged_: 1 if the plant had emerged at 45 days.
# stem_number_: stems per plant.
# misshapen_: 1 if the seed tuber was misshapen.
# mechanical_damage_: 1 if the seed tuber was mechanically damaged.
# immature_: 1 if the seed tuber was immature.
# tubers_class1_, tubers_class2_, tubers_class3_, tubers_class4_: progeny tuber counts per plant (>80 g, 61-80 g, 41-60 g, <40 g).
# weight_class1_, weight_class2_, weight_class3_, weight_class4_: progeny tuber fresh weight per plant in those size classes (g/plant).
# stage_: seed (order = 1) or progeny (order = 2).
# defect_: physiological/mechanical tuber defect (cracking, misshapen, mechanical damage) in long table.
# defect_pct_: percentage severity or incidence for physiological/mechanical tuber defects.


carob_script <- function(path) {

"
Dataset for: Replication Data for: Potato crop health quality and yield losses in Ecuador

A survey and a field experiment were performed in Ecuador to: (1) determine the incidence and severity of seed borne pests affecting the seed health quality, (2) understand how altitude and seed sources affect seed health quality, and (3) identify the influence of the seed health quality on the yield. The survey was performed, in 2010, in three provinces: Bolivar, Chimborazo and Tungurahua at an altitude between 2751 and 3634 meters above the sea level (m.a.s.l). It consisted on collecting tubers from farmers' fields, and assesing the prevalence of seed borne pests, and mechanical and physiological problems per tuber collected. These tubers were later planted (October 2010-December 2011) in a field experiment at CIP-Quito (altitude of 3058 m.a.s.l). Growth variables, yield and production quality were registered per tuber at harvest. The general characteristics of farmer fields (e.g., geographic information, farmer's seed sources, potato varieties and field sizes) are described in the \"Fields_Geographic_locations\" sheet, and the detailed information about the survey and field experiment is available in the \"Losses_yield_determination\" sheet.
"

	uri <- "doi:10.21223/P3/VREYMX"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "variety",
		response_vars = "plant_height;seed_tuber_weight_;weight_class1_",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 65,
		carob_effort = 0.9
	)

	f1 <- ff[basename(ff) == "Losses yield determination.xls"]
	r1 <- carobiner::read.excel(f1, col_types="text")
	f2 <- ff[basename(ff) == "Field geographic locations.xlsx"]
	r2 <- carobiner::read.excel(f2)
	num <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x %in% c("NA", ".")] <- NA_character_
		as.numeric(x)
	}
	flag <- function(x) as.integer(num(x))
	dms <- function(x) {
		x <- trimws(as.character(x))
		hem <- substr(x, 1, 1)
		p <- strsplit(trimws(gsub("[^0-9.]+", " ", x)), " +")
		deg <- vapply(p, function(z) {
			z <- as.numeric(z)
			z[1] + z[2] / 60 + z[3] / 3600
		}, numeric(1))
		ifelse(hem %in% c("S", "W"), -deg, deg)
	}

	prov <- trimws(as.character(r2$Province))
	prov[!nzchar(prov)] <- NA_character_
	for (i in seq_along(prov)[-1]) {
		if (is.na(prov[i])) prov[i] <- prov[i - 1]
	}
	# localidad 1 to 11, in the dictionary order, pointing at geographic-sheet rows
	geo_row <- c(1, 3, 2, 5, 6, 8, 10, 11, 7, 4, 9)
	gi <- geo_row[as.integer(r1$Localidad)]
	varmap <- c(
		"1"="Dolores",
		"2"="Única",
		"3"="INIAP-Fripapa",
		"4"="Chaucha Roja",
		"5"="INIAP-Gabriela",
		"6"="INIAP-Yana Shungo"
	)
	variety <- unname(varmap[trimws(r1$Variedad)])
	loc <- as.integer(r1$Localidad)
	community <- as.character(r2[["Community name"]])
	area_col <- grep("^Area", names(r2), value=TRUE)[1]

	d <- data.frame(
		trial_id = paste0("F", loc),
		crop = "potato",
		variety = variety,
		treatment = variety,
		plot_id = paste(loc, seq_len(nrow(r1)), sep="-"),
		tuber_code_ = trimws(r1$Numero),
		country = "Ecuador",
		adm1 = carobiner::fix_name(prov[gi], "title"),
		location = carobiner::fix_name(community[gi], "title"),
		elevation = as.numeric(r2$Altitude[gi]),
		latitude = dms(r2$Latitude)[gi],
		longitude = dms(r2$Longitude)[gi],
		seed_source = trimws(as.character(r2[["Seed source"]][gi])),
		source_field_area_ = as.numeric(r2[[area_col]][gi]),
		planting_date = "2010-10",
		seed_tuber_weight_ = num(r1$Peso),
		sprout_number_ = num(r1$NumBrote),
		emerged_ = flag(r1$Emerg),
		stem_number_ = num(r1$Num_tallos),
		plant_height = num(r1$Altura) * 100,
		misshapen_ = flag(r1$Deformidad),
		mechanical_damage_ = flag(r1$D_mecanico),
		immature_ = flag(r1$Pelona),
		tubers_class1_ = num(r1$Num_prim),
		tubers_class2_ = num(r1$Num_seg),
		tubers_class3_ = num(r1$Num_ter),
		tubers_class4_ = num(r1$Num_cuar),
		weight_class1_ = num(r1$P_prim),
		weight_class2_ = num(r1$P_seg),
		weight_class3_ = num(r1$P_ter),
		weight_class4_ = num(r1$P_cuar),
		stress = "pests;disease",
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = TRUE
	)
	d$record_id <- seq_len(nrow(d))

	# Seed-tuber virus ELISA (0/1 -> 0/100% incidence), disease, pest, and defect records in long table
	virus_specs <- list(
		list("PVX", "potato virus X"),
		list("PVY", "potato virus Y"),
		list("PVS", "potato virus S"),
		list("PLRV", "potato leafroll virus"),
		list("PVA", "potato virus A"),
		list("APLV", "Andean potato latent virus"),
		list("APMoV", "Andean potato mottle virus"),
		list("PYVV", "potato yellow vein virus")
	)
	vparts <- lapply(virus_specs, function(sp) {
		val <- flag(r1[[sp[[1]]]])
		keep <- !is.na(val)
		data.frame(
			record_id = d$record_id[keep],
			order = 1L,
			stage_ = "seed",
			disease = sp[[2]],
			disease_incidence = as.character(val[keep] * 100)
		)
	})

	# kind: "disease", "pest", or "defect"; measure: "severity" or "incidence"
	specs <- list(
		list("Pie_Negro", "seed", 1L, "disease", "black leg", "severity"),
		list("Agrietamiento", "seed", 1L, "defect", "cracking", "severity"),
		list("Rhizoctoniasis", "seed", 1L, "disease", "black scurf", "severity"),
		list("Fusariosis", "seed", 1L, "disease", "fusarium", "severity"),
		list("Mancha_plateada", "seed", 1L, "disease", "silver scurf", "severity"),
		list("Ronia", "seed", 1L, "disease", "powdery scab", "severity"),
		list("Gusano_blanco", "seed", 1L, "pest", "Andean potato weevil", "severity"),
		list("Polilla", "seed", 1L, "pest", "potato tuber moth", "severity"),
		list("Cutzo", "seed", 1L, "pest", "white grub", "severity"),
		list("Gusano_alambre", "seed", 1L, "pest", "wireworm", "severity"),
		list("Pulguilla", "seed", 1L, "pest", "flea beetle", "severity"),
		list("S_Rhizoc", "progeny", 2L, "disease", "black scurf", "severity"),
		list("S_Fusarium", "progeny", 2L, "disease", "fusarium", "severity"),
		list("S_Gusano_blanco", "progeny", 2L, "pest", "Andean potato weevil", "severity"),
		list("S_Cutzo", "progeny", 2L, "pest", "white grub", "severity"),
		list("S_Agrietamiento", "progeny", 2L, "defect", "cracking", "severity"),
		list("I_Deformes", "progeny", 2L, "defect", "misshapen", "incidence"),
		list("I_Danio_Mec", "progeny", 2L, "defect", "mechanical damage", "incidence"),
		list("I_Erwinia", "progeny", 2L, "disease", "black leg", "incidence"),
		list("I_Rhizoc", "progeny", 2L, "disease", "black scurf", "incidence"),
		list("I_Fusarium", "progeny", 2L, "disease", "fusarium", "incidence"),
		list("I_Gusano_blanco", "progeny", 2L, "pest", "Andean potato weevil", "incidence"),
		list("I_Cutzo", "progeny", 2L, "pest", "white grub", "incidence"),
		list("I_Agrietamiento", "progeny", 2L, "defect", "cracking", "incidence")
	)
	parts <- lapply(specs, function(sp) {
		val <- r1[[sp[[1]]]]
		if (sp[[1]] == "Rhizoctoniasis") val[trimws(val) == "0,1"] <- "0.1"
		pct <- num(val)
		keep <- !is.na(pct)
		out <- data.frame(
			record_id = d$record_id[keep],
			order = sp[[3]],
			stage_ = sp[[2]]
		)
		if (sp[[4]] == "disease") {
			out$disease <- sp[[5]]
			if (sp[[6]] == "severity") {
				out$disease_severity <- as.character(pct[keep])
				out$severity_scale <- "0-100"
			} else {
				out$disease_incidence <- as.character(pct[keep])
			}
		} else if (sp[[4]] == "pest") {
			out$pest_species <- sp[[5]]
			if (sp[[6]] == "severity") {
				out$pest_severity <- as.character(pct[keep])
			} else {
				out$pest_incidence <- as.integer(round(pct[keep]))
			}
		} else {
			out$defect_ <- sp[[5]]
			out$defect_pct_ <- pct[keep]
		}
		out
	})
	long <- do.call(carobiner::bindr, c(vparts, parts))
	rownames(d) <- NULL
	rownames(long) <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
