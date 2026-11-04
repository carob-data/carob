# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm fertilizer demonstrations under ATASP-1 in three Nigerian staple crop processing zones, 2016 season (metadata).
# Yield is already kg/ha (GHvYld_C_kgha).
# "NPK 60:30:30" is treated as 60 kg N, 30 kg P2O5 and 30 kg K2O per ha (Nigeria fertilizer-rate notation). P and K stored as elements.

## ISSUES
# get_data() can time out on dataverse.icrisat.org over IPv6; files were retrieved with IPv4 (curl -4).
# Fertilizer rates for "NPK", "NPK+Urea", "NPK+Urea+FYM" and poultry manure are not in the table. Do not invent them.
# Urea and FYM amounts are not reported.
# Planting/harvest dates are not in the table (only the 2016 season in the abstract).
# Communities (76) are not georeferenced. Human should georeference; LGA names G/Mallam and M/Madori need expanding (Garun Mallam; Malam Madori).
# Description is a .docx; not parsed here.

carob_script <- function(path) {

"
Enhancing farmers’ access to technology and market for increased sorghum productivity in the selected staple crop processing zones

The Agricultural Transformation Agenda Support Program Phase 1 (AT ASP-1) of the Federal Government of Nigeria was launched m 2015 as a follow up to the previous. Agricultural  Transformation Agenda (ATA). It is expected to create in 120.000 jobs along the value chain of priority commodities and add additional 20 million metric tons of food. Project activities included thematic training, on-farm technology  demonstrations, community seed production and formation of Innovation Platforms for market linkages. The project has made remarkable progress in enhancing access to quality seeds and other inputs to over 34.300 farmers while expanding knowledge of best-bet productron technologies in over 100 communities across three staple crop processing zones (SCPZ). During the 2016 cropping season, farmers produced over 70,268 Mt of grains valued at £i9.135billion (US$29M). The use of improved varieties increased yields by 32%. 42% and 64% in Bida-Badeggi. Kano-Jigawa and Sokoto-Kebbi SCPZ. respectively.
"

	uri <- "doi:10.21421/D2/ORJQWZ"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "ICRISAT",
		publication = NA,
		project = "ATASP-1",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 50,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Data file of Icrisat on farm fertilizer demonstration..xlsx"]
	r <- carobiner::read.excel(f)

	trt <- trimws(r$Treatment)
	trt[trt == "NPK+ Urea"] <- "NPK+Urea"
	trt[trt == "Poutry Manure"] <- "Poultry manure"
	trt[tolower(trt) == "poultry manure"] <- "Poultry manure"

	fert <- rep(NA_character_, length(trt))
	fert[trt == "NPK"] <- "NPK"
	fert[trt == "NPK 60:30:30"] <- "NPK"
	fert[trt == "NPK+Urea"] <- "NPK; urea"
	fert[trt == "NPK+Urea+FYM"] <- "NPK; urea"
	fert[trt == "Poultry manure"] <- NA_character_

	om <- rep(NA_character_, length(trt))
	om[trt == "NPK+Urea+FYM"] <- "farmyard manure"
	om[trt == "Poultry manure"] <- "poultry manure"

	# 60:30:30 as N : P2O5 : K2O kg/ha
	N <- rep(NA_real_, length(trt))
	P <- rep(NA_real_, length(trt))
	K <- rep(NA_real_, length(trt))
	i <- trt == "NPK 60:30:30"
	N[i] <- 60
	P[i] <- 30 / 2.29
	K[i] <- 30 / 1.2051

	d <- data.frame(
		country = "Nigeria",
		adm1 = carobiner::fix_name(r$State, "title"),
		adm2 = carobiner::fix_name(r$LGA, "title"),
		location = r$Community,
		variety = r$Variety,
		treatment = trt,
		fertilizer_type = fert,
		OM_type = om,
		OM_used = !is.na(om),
		N_fertilizer = N,
		P_fertilizer = P,
		K_fertilizer = K,
		yield = as.numeric(r$GHvYld_C_kgha),
		crop = "sorghum",
		yield_part = "grain",
		planting_date = "2016",
		on_farm = TRUE,
		is_survey = FALSE
	)

	d$trial_id <- paste(d$adm1, d$adm2, d$location, sep = "_")
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
