# R script for "carob"
# license: GPL (>=3)

## NOTES
# Organic cropping system at WUR-proeflocatie Vredepeel. The spatial coverage in the deposit is Vredepeel and southeast Netherlands (Zuidoost Nederland). No coordinates are given.
# On sheet GEWAS, marketable yield is already kg/ha. Dry-matter content is kg dry matter per kg fresh. yield_moisture is 100 times (1 minus that fraction).
# ploegen is stored as land_prep_method ploughing.
# Astilbe and Boshaag are not crop names in the vocabulary, so those rows are not in the table. Grasklaver is stored as clover. The source does not name the grass species.
# The description says 2001-2016. The sheet also has 2000.

## ISSUES
# No coordinates. Manure rates are not in this table.
# Fresh vegetables have dry-matter fractions that put yield_moisture above 90.
# write_files: , latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. out of bounds: yield_moisture (12.9, 92.4). NA detected: yield.


## NOT PROCESSED
# MEST, BODEM, NEMATODEN, NAWERKING, NO3 GRONDWATER, NEERSLAGOVERSCHOT, GRONDWATERSTAND, OS 1998-2016, and the compost overview.
# GEWAS rows for Astilbe and Boshaag.
# Quality parameters and the nutrient-content columns of the harvested product.


carob_script <- function(path) {

"
Biologische teelt op een zuidelijke zandgrond: opbrengst, bemesting, bodemkwaliteit en stikstofverliezen

In deze dataset en het bijbehorende rapport worden de resultaten van het biologische bedrijfssysteem op WUR-proeflocatie Vredepeel in de periode 2001-2016 gepresenteerd met focus op gewasopbrengst, bodemkwaliteit, bemesting en stikstofuitspoeling.
"

	uri <- "doi:10.17026/dans-x3n-64rp"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "WUR",
		publication = "doi:10.18174/440225",
		project = "Bodemkwaliteit op zand",
		design = NA,
		data_type = "compilation",
		treatment_vars = "none",
		response_vars = "yield",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.55
	)

	f <- ff[grepl("Biologisch_Definitief.xlsx$", basename(ff))]
	g <- carobiner::read.excel(f, sheet="GEWAS", skip=7, col_names=FALSE)
	g <- g[!is.na(g[[2]]), ]
	name <- trimws(as.character(g[[5]]))
	keep <- !(name %in% c("Astilbe", "Boshaag jr1", "Boshaag jr2"))
	g <- g[keep, ]
	name <- name[keep]

	crop <- c(
		"Aardappel" = "potato", "Waspeen" = "carrot", "Peen" = "carrot",
		"Suikerbiet" = "sugar beet", "Snijmaïs" = "silage maize",
		"Korrelmais" = "maize", "Triticale" = "triticale",
		"Conservenerwt" = "pea", "Zomergerst" = "barley", "Prei" = "leek",
		"Broccoli" = "broccoli", "Stamslaboon" = "green bean",
		"Luzerne" = "lucerne", "Grasklaver" = "clover"
	)
	cn <- crop[name]
	if (any(is.na(cn))) stop("crop not mapped: ", paste(unique(name[is.na(cn)]), collapse=", "))
	part <- c(
		potato = "tubers", carrot = "roots", "sugar beet" = "roots",
		"silage maize" = "aboveground biomass", maize = "grain",
		triticale = "grain", pea = "seed", barley = "grain",
		leek = "aboveground biomass", broccoli = "flowers",
		"green bean" = "pod", lucerne = "aboveground biomass",
		clover = "aboveground biomass"
	)
	till <- trimws(as.character(g[[6]]))
	dm <- as.numeric(g[[14]])

	d <- data.frame(
		trial_id = paste0("BIO_", g[[2]]),
		country = "Netherlands",
		location = "Vredepeel",
		plot_id = as.character(g[[3]]),
		crop = unname(cn),
		yield = as.numeric(g[[7]]),
		yield_moisture = (1 - dm) * 100,
		yield_part = unname(part[cn]),
		land_prep_method = ifelse(till == "ploegen", "ploughing", NA_character_),
		intercropped = cn == "clover",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
