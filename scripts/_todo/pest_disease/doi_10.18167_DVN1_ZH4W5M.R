# R script for "carob"
# license: GPL (>=3)

## NOTES
# 240 plot-level weed surveys (Dimbokro-Bongouanou, 2011). One row per species
# observed (rating > 0). Floristique is already on the 1-9 scale; converted to
# % cover. Latin names from Données d'origine (EPPO code + species).

## ISSUES
# Abstract says 140 surveys / 389 species; Facteurs has 240 plots. Crop is
# only "vivrier" (food crop) per plot; cassava/maize/okra not distinguished.
# Coords estimated for the Dimbokro-Bongouanou area. write_files: missing
# yield, yield_moisture, yield_part, N/P/K_fertilizer, planting_date,
# harvest_date (survey in pest_disease group). Crop is NA (only "vivrier").



## NOT PROCESSED
# CDI-AWA-2011-VIV-AD-DOC.txt: not read. columns: NA. Observation records are missing from d until this file is standardized.
# CDI-AWA-2011-VIV-AD-FLO.txt: not read. columns: "CDI-AWA-01" "CDI-AWA-02" "CDI-AWA-03" "CDI-AWA-04" "CDI-AWA-05" "CDI-AWA-06" "CDI-AWA-07" "CDI-AWA-08" "CDI-AWA-09" "CDI-AWA-10" "CDI-AWA-11" "CDI-AWA-12" "CDI-AWA-13" "CDI-AWA-14" "CDI-AWA-15" "CDI-AWA-16" "CDI-AWA-17" "CDI-AWA-18" "CDI-AWA-19" "CDI-AWA-20" "CDI-AWA-21" "CDI-AWA-22" "CDI-AWA-23" "CDI-AWA-24" "CDI-AWA-25" "CDI-AWA-26" "CDI-AWA-27" "CDI-AWA-28" "CDI-AWA-29" "CDI-AWA-30" "CDI-AWA-31" "CDI-AWA-32" "CDI-AWA-33" "CDI-AWA-34" "CDI-AWA-35" "CDI-AWA-36" "CDI-AWA-37" "CDI-AWA-38" "CDI-AWA-39" "CDI-AWA-40" "CDI-AWA-41" "CDI-AWA-42" "CDI-AWA-43" "CDI-AWA-44" "CDI-AWA-45" "CDI-AWA-46" "CDI-AWA-47" "CDI-AWA-48" "CDI-AWA-49" "CDI-AWA-50" "CDI-AWA-51" "CDI-AWA-52" "CDI-AWA-53" "CDI-AWA-54" "CDI-AWA-55" "CDI-AWA-56" "CDI-AWA-57" "CDI-AWA-58" "CDI-AWA-59" "CDI-AWA-60" "CDI-AWA-61" "CDI-AWA-62" "CDI-AWA-63" "CDI-AWA-64" "CDI-AWA-65" "CDI-AWA-66" "CDI-AWA-67" "CDI-AWA-68" "CDI-AWA-69" "CDI-AWA-70" "CDI-AWA-71" "CDI-AWA-72" "CDI-AWA-73" "CDI-AWA-74" "CDI-AWA-75" "CDI-AWA-76" "CDI-AWA-77" "CDI-AWA-78" "CDI-AWA-79" "CDI-AWA-80" "CDI-AWA-81" "CDI-AWA-82" "CDI-AWA-83" "CDI-AWA-84" "CDI-AWA-85" "CDI-AWA-86" "CDI-AWA-87" "CDI-AWA-88" "CDI-AWA-89" "CDI-AWA-90" "CDI-AWA-91" "CDI-AWA-92" "CDI-AWA-93" "CDI-AWA-94" "CDI-AWA-95" "CDI-AWA-96" "CDI-AWA-97" "CDI-AWA-98" "CDI-AWA-99" "CDI-AWA-100" "CDI-AWA-101" "CDI-AWA-102" "CDI-AWA-103" "CDI-AWA-104" "CDI-AWA-105" "CDI-AWA-106" "CDI-AWA-107" "CDI-AWA-108" "CDI-AWA-109" "CDI-AWA-110" "CDI-AWA-111" "CDI-AWA-112" "CDI-AWA-113" "CDI-AWA-114" "CDI-AWA-115" "CDI-AWA-116" "CDI-AWA-117" "CDI-AWA-118" "CDI-AWA-119" "CDI-AWA-120" "CDI-AWA-121" "CDI-AWA-122" "CDI-AWA-123" "CDI-AWA-124" "CDI-AWA-125" "CDI-AWA-126" "CDI-AWA-127" "CDI-AWA-128" "CDI-AWA-129" "CDI-AWA-130" "CDI-AWA-131" "CDI-AWA-132" "CDI-AWA-133" "CDI-AWA-134" "CDI-AWA-135" "CDI-AWA-136" "CDI-AWA-137" "CDI-AWA-138" "CDI-AWA-139" "CDI-AWA-140" "CDI-AWA-141" "CDI-AWA-142" "CDI-AWA-143" "CDI-AWA-144" "CDI-AWA-145" "CDI-AWA-146" "CDI-AWA-147" "CDI-AWA-148" "CDI-AWA-149" "CDI-AWA-150" "CDI-AWA-151" "CDI-AWA-152" "CDI-AWA-153" "CDI-AWA-154" "CDI-AWA-155" "CDI-AWA-156" "CDI-AWA-157" "CDI-AWA-158" "CDI-AWA-159" "CDI-AWA-160" "CDI-AWA-161" "CDI-AWA-162" "CDI-AWA-163" "CDI-AWA-164" "CDI-AWA-165" "CDI-AWA-166" "CDI-AWA-167" "CDI-AWA-168" "CDI-AWA-169" "CDI-AWA-170" "CDI-AWA-171" "CDI-AWA-172" "CDI-AWA-173" "CDI-AWA-174" "CDI-AWA-175" "CDI-AWA-176" "CDI-AWA-177" "CDI-AWA-178" "CDI-AWA-179" "CDI-AWA-180" "CDI-AWA-181" "CDI-AWA-182" "CDI-AWA-183" "CDI-AWA-184" "CDI-AWA-185" "CDI-AWA-186" "CDI-AWA-187" "CDI-AWA-188" "CDI-AWA-189" "CDI-AWA-190" "CDI-AWA-191" "CDI-AWA-192" "CDI-AWA-193" "CDI-AWA-194" "CDI-AWA-195" "CDI-AWA-196" "CDI-AWA-197" "CDI-AWA-198" "CDI-AWA-199" "CDI-AWA-200" "CDI-AWA-201" "CDI-AWA-202" "CDI-AWA-203" "CDI-AWA-204" "CDI-AWA-205" "CDI-AWA-206" "CDI-AWA-207" "CDI-AWA-208" "CDI-AWA-209" "CDI-AWA-210" "CDI-AWA-211" "CDI-AWA-212" "CDI-AWA-213" "CDI-AWA-214" "CDI-AWA-215" "CDI-AWA-216" "CDI-AWA-217" "CDI-AWA-218" "CDI-AWA-219" "CDI-AWA-220" "CDI-AWA-221" "CDI-AWA-222" "CDI-AWA-223" "CDI-AWA-224" "CDI-AWA-225" "CDI-AWA-226" "CDI-AWA-227" "CDI-AWA-228" "CDI-AWA-229" "CDI-AWA-230" "CDI-AWA-231" "CDI-AWA-232" "CDI-AWA-233" "CDI-AWA-234" "CDI-AWA-235" "CDI-AWA-236" "CDI-AWA-237" "CDI-AWA-238" "CDI-AWA-239" "CDI-AWA-240". Observation records are missing from d until this file is standardized.
# CDI-AWA-2011-VIV-FAC.txt: not read. columns: "CDI-AWA-01" "CDI-AWA-02" "CDI-AWA-03" "CDI-AWA-04" "CDI-AWA-05" "CDI-AWA-06" "CDI-AWA-07" "CDI-AWA-08" "CDI-AWA-09" "CDI-AWA-10" "CDI-AWA-11" "CDI-AWA-12" "CDI-AWA-13" "CDI-AWA-14" "CDI-AWA-15" "CDI-AWA-16" "CDI-AWA-17" "CDI-AWA-18" "CDI-AWA-19" "CDI-AWA-20" "CDI-AWA-21" "CDI-AWA-22" "CDI-AWA-23" "CDI-AWA-24" "CDI-AWA-25" "CDI-AWA-26" "CDI-AWA-27" "CDI-AWA-28" "CDI-AWA-29" "CDI-AWA-30" "CDI-AWA-31" "CDI-AWA-32" "CDI-AWA-33" "CDI-AWA-34" "CDI-AWA-35" "CDI-AWA-36" "CDI-AWA-37" "CDI-AWA-38" "CDI-AWA-39" "CDI-AWA-40" "CDI-AWA-41" "CDI-AWA-42" "CDI-AWA-43" "CDI-AWA-44" "CDI-AWA-45" "CDI-AWA-46" "CDI-AWA-47" "CDI-AWA-48" "CDI-AWA-49" "CDI-AWA-50" "CDI-AWA-51" "CDI-AWA-52" "CDI-AWA-53" "CDI-AWA-54" "CDI-AWA-55" "CDI-AWA-56" "CDI-AWA-57" "CDI-AWA-58" "CDI-AWA-59" "CDI-AWA-60" "CDI-AWA-61" "CDI-AWA-62" "CDI-AWA-63" "CDI-AWA-64" "CDI-AWA-65" "CDI-AWA-66" "CDI-AWA-67" "CDI-AWA-68" "CDI-AWA-69" "CDI-AWA-70" "CDI-AWA-71" "CDI-AWA-72" "CDI-AWA-73" "CDI-AWA-74" "CDI-AWA-75" "CDI-AWA-76" "CDI-AWA-77" "CDI-AWA-78" "CDI-AWA-79" "CDI-AWA-80" "CDI-AWA-81" "CDI-AWA-82" "CDI-AWA-83" "CDI-AWA-84" "CDI-AWA-85" "CDI-AWA-86" "CDI-AWA-87" "CDI-AWA-88" "CDI-AWA-89" "CDI-AWA-90" "CDI-AWA-91" "CDI-AWA-92" "CDI-AWA-93" "CDI-AWA-94" "CDI-AWA-95" "CDI-AWA-96" "CDI-AWA-97" "CDI-AWA-98" "CDI-AWA-99" "CDI-AWA-100" "CDI-AWA-101" "CDI-AWA-102" "CDI-AWA-103" "CDI-AWA-104" "CDI-AWA-105" "CDI-AWA-106" "CDI-AWA-107" "CDI-AWA-108" "CDI-AWA-109" "CDI-AWA-110" "CDI-AWA-111" "CDI-AWA-112" "CDI-AWA-113" "CDI-AWA-114" "CDI-AWA-115" "CDI-AWA-116" "CDI-AWA-117" "CDI-AWA-118" "CDI-AWA-119" "CDI-AWA-120" "CDI-AWA-121" "CDI-AWA-122" "CDI-AWA-123" "CDI-AWA-124" "CDI-AWA-125" "CDI-AWA-126" "CDI-AWA-127" "CDI-AWA-128" "CDI-AWA-129" "CDI-AWA-130" "CDI-AWA-131" "CDI-AWA-132" "CDI-AWA-133" "CDI-AWA-134" "CDI-AWA-135" "CDI-AWA-136" "CDI-AWA-137" "CDI-AWA-138" "CDI-AWA-139" "CDI-AWA-140" "CDI-AWA-141" "CDI-AWA-142" "CDI-AWA-143" "CDI-AWA-144" "CDI-AWA-145" "CDI-AWA-146" "CDI-AWA-147" "CDI-AWA-148" "CDI-AWA-149" "CDI-AWA-150" "CDI-AWA-151" "CDI-AWA-152" "CDI-AWA-153" "CDI-AWA-154" "CDI-AWA-155" "CDI-AWA-156" "CDI-AWA-157" "CDI-AWA-158" "CDI-AWA-159" "CDI-AWA-160" "CDI-AWA-161" "CDI-AWA-162" "CDI-AWA-163" "CDI-AWA-164" "CDI-AWA-165" "CDI-AWA-166" "CDI-AWA-167" "CDI-AWA-168" "CDI-AWA-169" "CDI-AWA-170" "CDI-AWA-171" "CDI-AWA-172" "CDI-AWA-173" "CDI-AWA-174" "CDI-AWA-175" "CDI-AWA-176" "CDI-AWA-177" "CDI-AWA-178" "CDI-AWA-179" "CDI-AWA-180" "CDI-AWA-181" "CDI-AWA-182" "CDI-AWA-183" "CDI-AWA-184" "CDI-AWA-185" "CDI-AWA-186" "CDI-AWA-187" "CDI-AWA-188" "CDI-AWA-189" "CDI-AWA-190" "CDI-AWA-191" "CDI-AWA-192" "CDI-AWA-193" "CDI-AWA-194" "CDI-AWA-195" "CDI-AWA-196" "CDI-AWA-197" "CDI-AWA-198" "CDI-AWA-199" "CDI-AWA-200" "CDI-AWA-201" "CDI-AWA-202" "CDI-AWA-203" "CDI-AWA-204" "CDI-AWA-205" "CDI-AWA-206" "CDI-AWA-207" "CDI-AWA-208" "CDI-AWA-209" "CDI-AWA-210" "CDI-AWA-211" "CDI-AWA-212" "CDI-AWA-213" "CDI-AWA-214" "CDI-AWA-215" "CDI-AWA-216" "CDI-AWA-217" "CDI-AWA-218" "CDI-AWA-219" "CDI-AWA-220" "CDI-AWA-221" "CDI-AWA-222" "CDI-AWA-223" "CDI-AWA-224" "CDI-AWA-225" "CDI-AWA-226" "CDI-AWA-227" "CDI-AWA-228" "CDI-AWA-229" "CDI-AWA-230" "CDI-AWA-231" "CDI-AWA-232" "CDI-AWA-233" "CDI-AWA-234" "CDI-AWA-235" "CDI-AWA-236" "CDI-AWA-237" "CDI-AWA-238" "CDI-AWA-239" "CDI-AWA-240". Observation records are missing from d until this file is standardized.
# CDI-AWA-2011-VIV-PA-FLO.txt: not read. columns: "CDI-AWA-01" "CDI-AWA-02" "CDI-AWA-03" "CDI-AWA-04" "CDI-AWA-05" "CDI-AWA-06" "CDI-AWA-07" "CDI-AWA-08" "CDI-AWA-09" "CDI-AWA-10" "CDI-AWA-11" "CDI-AWA-12" "CDI-AWA-13" "CDI-AWA-14" "CDI-AWA-15" "CDI-AWA-16" "CDI-AWA-17" "CDI-AWA-18" "CDI-AWA-19" "CDI-AWA-20" "CDI-AWA-21" "CDI-AWA-22" "CDI-AWA-23" "CDI-AWA-24" "CDI-AWA-25" "CDI-AWA-26" "CDI-AWA-27" "CDI-AWA-28" "CDI-AWA-29" "CDI-AWA-30" "CDI-AWA-31" "CDI-AWA-32" "CDI-AWA-33" "CDI-AWA-34" "CDI-AWA-35" "CDI-AWA-36" "CDI-AWA-37" "CDI-AWA-38" "CDI-AWA-39" "CDI-AWA-40" "CDI-AWA-41" "CDI-AWA-42" "CDI-AWA-43" "CDI-AWA-44" "CDI-AWA-45" "CDI-AWA-46" "CDI-AWA-47" "CDI-AWA-48" "CDI-AWA-49" "CDI-AWA-50" "CDI-AWA-51" "CDI-AWA-52" "CDI-AWA-53" "CDI-AWA-54" "CDI-AWA-55" "CDI-AWA-56" "CDI-AWA-57" "CDI-AWA-58" "CDI-AWA-59" "CDI-AWA-60" "CDI-AWA-61" "CDI-AWA-62" "CDI-AWA-63" "CDI-AWA-64" "CDI-AWA-65" "CDI-AWA-66" "CDI-AWA-67" "CDI-AWA-68" "CDI-AWA-69" "CDI-AWA-70" "CDI-AWA-71" "CDI-AWA-72" "CDI-AWA-73" "CDI-AWA-74" "CDI-AWA-75" "CDI-AWA-76" "CDI-AWA-77" "CDI-AWA-78" "CDI-AWA-79" "CDI-AWA-80" "CDI-AWA-81" "CDI-AWA-82" "CDI-AWA-83" "CDI-AWA-84" "CDI-AWA-85" "CDI-AWA-86" "CDI-AWA-87" "CDI-AWA-88" "CDI-AWA-89" "CDI-AWA-90" "CDI-AWA-91" "CDI-AWA-92" "CDI-AWA-93" "CDI-AWA-94" "CDI-AWA-95" "CDI-AWA-96" "CDI-AWA-97" "CDI-AWA-98" "CDI-AWA-99" "CDI-AWA-100" "CDI-AWA-101" "CDI-AWA-102" "CDI-AWA-103" "CDI-AWA-104" "CDI-AWA-105" "CDI-AWA-106" "CDI-AWA-107" "CDI-AWA-108" "CDI-AWA-109" "CDI-AWA-110" "CDI-AWA-111" "CDI-AWA-112" "CDI-AWA-113" "CDI-AWA-114" "CDI-AWA-115" "CDI-AWA-116" "CDI-AWA-117" "CDI-AWA-118" "CDI-AWA-119" "CDI-AWA-120" "CDI-AWA-121" "CDI-AWA-122" "CDI-AWA-123" "CDI-AWA-124" "CDI-AWA-125" "CDI-AWA-126" "CDI-AWA-127" "CDI-AWA-128" "CDI-AWA-129" "CDI-AWA-130" "CDI-AWA-131" "CDI-AWA-132" "CDI-AWA-133" "CDI-AWA-134" "CDI-AWA-135" "CDI-AWA-136" "CDI-AWA-137" "CDI-AWA-138" "CDI-AWA-139" "CDI-AWA-140" "CDI-AWA-141" "CDI-AWA-142" "CDI-AWA-143" "CDI-AWA-144" "CDI-AWA-145" "CDI-AWA-146" "CDI-AWA-147" "CDI-AWA-148" "CDI-AWA-149" "CDI-AWA-150" "CDI-AWA-151" "CDI-AWA-152" "CDI-AWA-153" "CDI-AWA-154" "CDI-AWA-155" "CDI-AWA-156" "CDI-AWA-157" "CDI-AWA-158" "CDI-AWA-159" "CDI-AWA-160" "CDI-AWA-161" "CDI-AWA-162" "CDI-AWA-163" "CDI-AWA-164" "CDI-AWA-165" "CDI-AWA-166" "CDI-AWA-167" "CDI-AWA-168" "CDI-AWA-169" "CDI-AWA-170" "CDI-AWA-171" "CDI-AWA-172" "CDI-AWA-173" "CDI-AWA-174" "CDI-AWA-175" "CDI-AWA-176" "CDI-AWA-177" "CDI-AWA-178" "CDI-AWA-179" "CDI-AWA-180" "CDI-AWA-181" "CDI-AWA-182" "CDI-AWA-183" "CDI-AWA-184" "CDI-AWA-185" "CDI-AWA-186" "CDI-AWA-187" "CDI-AWA-188" "CDI-AWA-189" "CDI-AWA-190" "CDI-AWA-191" "CDI-AWA-192" "CDI-AWA-193" "CDI-AWA-194" "CDI-AWA-195" "CDI-AWA-196" "CDI-AWA-197" "CDI-AWA-198" "CDI-AWA-199" "CDI-AWA-200" "CDI-AWA-201" "CDI-AWA-202" "CDI-AWA-203" "CDI-AWA-204" "CDI-AWA-205" "CDI-AWA-206" "CDI-AWA-207" "CDI-AWA-208" "CDI-AWA-209" "CDI-AWA-210" "CDI-AWA-211" "CDI-AWA-212" "CDI-AWA-213" "CDI-AWA-214" "CDI-AWA-215" "CDI-AWA-216" "CDI-AWA-217" "CDI-AWA-218" "CDI-AWA-219" "CDI-AWA-220" "CDI-AWA-221" "CDI-AWA-222" "CDI-AWA-223" "CDI-AWA-224" "CDI-AWA-225" "CDI-AWA-226" "CDI-AWA-227" "CDI-AWA-228" "CDI-AWA-229" "CDI-AWA-230" "CDI-AWA-231" "CDI-AWA-232" "CDI-AWA-233" "CDI-AWA-234" "CDI-AWA-235" "CDI-AWA-236" "CDI-AWA-237" "CDI-AWA-238" "CDI-AWA-239" "CDI-AWA-240". Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"
Weed surveys in food crops in Center-East Côte d'Ivoire (2011-2014)

Inventory of weeds of food crops (cassava, maize, okra) in the Center-East of Côte d'Ivoire from 2011 to 2014. Rating in abundance index 1-5 transformed into scale 1-9. This dataset covers 140 plant surveys for 389 weed species.
"

	uri <- "doi:10.18167/DVN1/ZH4W5M"
	group <- "pest_disease"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = "Amatrop",
		design = "weed abundance survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 70,
		carob_effort = 0.5
	)

	f1 <- ff[basename(ff) == "CDI-AWA-2011-VIV-AD.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="Facteurs")
	r2 <- carobiner::read.excel(f1, sheet="Floristique")
	r3 <- carobiner::read.excel(f1, sheet="Données d'origine")

	get_row <- function(label) unlist(r1[r1[[1]] == label, -1])

	irrig <- tolower(get_row("Irrigation"))
	d1b <- data.frame(
		trial_id = names(r1)[-1],
		date = as.character(as.integer(as.numeric(get_row("Annee")))),
		country = "Côte d'Ivoire",
		location = "Dimbokro-Bongouanou",
		irrigated = irrig %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	# Midpoint of Dimbokro and Bongouanou. Author should refine.
	d1b$longitude <- -4.45
	d1b$latitude <- 6.65
	d1b$geo_uncertainty <- 40000
	d1b$geo_source <- "estimated from Dimbokro-Bongouanou"
	d1b$geo_from_source <- FALSE
	d1b$on_farm <- TRUE
	d1b$is_survey <- TRUE

	header_row <- which(r3[[2]] == "code EPPO")[1]
	species_lookup <- data.frame(
		code = r3[[2]][(header_row + 2):nrow(r3)],
		weed_species = r3[[1]][(header_row + 2):nrow(r3)]
	)
	species_lookup <- species_lookup[!is.na(species_lookup$code) & nzchar(trimws(as.character(species_lookup$code))), ]

	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)
	present <- which(!is.na(r2[, -1]) & r2[, -1] > 0, arr.ind=TRUE)
	d <- data.frame(
		trial_id = names(r2)[-1][present[, "col"]],
		code = r2[[1]][present[, "row"]],
		ground_cover = rating_to_cover[match(as.numeric(r2[, -1][present]), 1:9)]
	)
	d <- merge(d, species_lookup, by="code", all.x=TRUE)
	# keep EPPO code when Latin name is missing so rows stay distinct
	miss <- is.na(d$weed_species) | !nzchar(trimws(as.character(d$weed_species)))
	d$weed_species[miss] <- d$code[miss]
	d$code <- NULL
	d$crop <- NA_character_
	d <- merge(d, d1b, by="trial_id", all.x=TRUE)

	carobiner::write_files(path, meta, d)
}
