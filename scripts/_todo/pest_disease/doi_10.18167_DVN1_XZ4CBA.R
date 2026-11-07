# R script for "carob"
# license: GPL (>=3)

## NOTES
# 210 plot surveys in south-west Madagascar (1999). One row per species with a
# rating > 0. Floristique notes are the DOC 1-9 scale (Ab/Dom already transformed).
# Enherbement is that same 1-9 note. weed_species is the 5-letter code.
# mais/manioc is recorded as one crop, maize_cassava.

## ISSUES
# No coordinates. DOC names Tuléar; Lieu is Ankazoabo, Antseva, Manombo, Sakaraha,
# or Vineta. Five floristic cells are 11 or 22, outside the DOC 1-9 table, so
# ground_cover is NA there. write_files: missing variables: yield,
# yield_moisture, yield_part, latitude, longitude, K_fertilizer, N_fertilizer,
# P_fertilizer, planting_date, harvest_date.


## NOT PROCESSED
# Facteurs not mapped: ages, Assolement, Type_culture, Climat, Date_semis,
# Date_observation, Drainage, Fumure (qualitative), Intensification,
# Intensite_sarclage, Labour, Profondeur_sol, Structure, Type_sol, Zone.
# Méta Données, FLO, FAC, and PA repeat the workbook or the DOC.
# MAD-JA2-1999-DIV-AD-FLO.txt: not read. columns: "MAD-JA2-001" "MAD-JA2-002" "MAD-JA2-003" "MAD-JA2-004" "MAD-JA2-005" "MAD-JA2-006" "MAD-JA2-007" "MAD-JA2-008" "MAD-JA2-009" "MAD-JA2-010" "MAD-JA2-011" "MAD-JA2-012" "MAD-JA2-013" "MAD-JA2-014" "MAD-JA2-015" "MAD-JA2-016" "MAD-JA2-017" "MAD-JA2-018" "MAD-JA2-019" "MAD-JA2-020" "MAD-JA2-021" "MAD-JA2-022" "MAD-JA2-023" "MAD-JA2-024" "MAD-JA2-025" "MAD-JA2-026" "MAD-JA2-027" "MAD-JA2-028" "MAD-JA2-029" "MAD-JA2-030" "MAD-JA2-031" "MAD-JA2-032" "MAD-JA2-033" "MAD-JA2-034" "MAD-JA2-035" "MAD-JA2-036" "MAD-JA2-037" "MAD-JA2-038" "MAD-JA2-039" "MAD-JA2-040" "MAD-JA2-041" "MAD-JA2-042" "MAD-JA2-043" "MAD-JA2-044" "MAD-JA2-045" "MAD-JA2-046" "MAD-JA2-047" "MAD-JA2-048" "MAD-JA2-049" "MAD-JA2-050" "MAD-JA2-051" "MAD-JA2-052" "MAD-JA2-053" "MAD-JA2-054" "MAD-JA2-055" "MAD-JA2-056" "MAD-JA2-057" "MAD-JA2-058" "MAD-JA2-059" "MAD-JA2-060" "MAD-JA2-061" "MAD-JA2-062" "MAD-JA2-063" "MAD-JA2-064" "MAD-JA2-065" "MAD-JA2-066" "MAD-JA2-067" "MAD-JA2-068" "MAD-JA2-069" "MAD-JA2-070" "MAD-JA2-071" "MAD-JA2-072" "MAD-JA2-073" "MAD-JA2-074" "MAD-JA2-075" "MAD-JA2-076" "MAD-JA2-077" "MAD-JA2-078" "MAD-JA2-079" "MAD-JA2-080" "MAD-JA2-081" "MAD-JA2-082" "MAD-JA2-083" "MAD-JA2-084" "MAD-JA2-085" "MAD-JA2-086" "MAD-JA2-087" "MAD-JA2-088" "MAD-JA2-089" "MAD-JA2-090" "MAD-JA2-091" "MAD-JA2-092" "MAD-JA2-093" "MAD-JA2-094" "MAD-JA2-095" "MAD-JA2-096" "MAD-JA2-097" "MAD-JA2-098" "MAD-JA2-099" "MAD-JA2-100" "MAD-JA2-101" "MAD-JA2-102" "MAD-JA2-103" "MAD-JA2-104" "MAD-JA2-105" "MAD-JA2-106" "MAD-JA2-107" "MAD-JA2-108" "MAD-JA2-109" "MAD-JA2-110" "MAD-JA2-111" "MAD-JA2-112" "MAD-JA2-113" "MAD-JA2-114" "MAD-JA2-115" "MAD-JA2-116" "MAD-JA2-117" "MAD-JA2-118" "MAD-JA2-119" "MAD-JA2-120" "MAD-JA2-121" "MAD-JA2-122" "MAD-JA2-123" "MAD-JA2-124" "MAD-JA2-125" "MAD-JA2-126" "MAD-JA2-127" "MAD-JA2-128" "MAD-JA2-129" "MAD-JA2-130" "MAD-JA2-131" "MAD-JA2-132" "MAD-JA2-133" "MAD-JA2-134" "MAD-JA2-135" "MAD-JA2-136" "MAD-JA2-137" "MAD-JA2-138" "MAD-JA2-139" "MAD-JA2-140" "MAD-JA2-141" "MAD-JA2-142" "MAD-JA2-143" "MAD-JA2-144" "MAD-JA2-145" "MAD-JA2-146" "MAD-JA2-147" "MAD-JA2-148" "MAD-JA2-149" "MAD-JA2-150" "MAD-JA2-151" "MAD-JA2-152" "MAD-JA2-153" "MAD-JA2-154" "MAD-JA2-155" "MAD-JA2-156" "MAD-JA2-157" "MAD-JA2-158" "MAD-JA2-159" "MAD-JA2-160" "MAD-JA2-161" "MAD-JA2-162" "MAD-JA2-163" "MAD-JA2-164" "MAD-JA2-165" "MAD-JA2-166" "MAD-JA2-167" "MAD-JA2-168" "MAD-JA2-169" "MAD-JA2-170" "MAD-JA2-171" "MAD-JA2-172" "MAD-JA2-173" "MAD-JA2-174" "MAD-JA2-175" "MAD-JA2-176" "MAD-JA2-177" "MAD-JA2-178" "MAD-JA2-179" "MAD-JA2-180" "MAD-JA2-181" "MAD-JA2-182" "MAD-JA2-183" "MAD-JA2-184" "MAD-JA2-185" "MAD-JA2-186" "MAD-JA2-187" "MAD-JA2-188" "MAD-JA2-189" "MAD-JA2-190" "MAD-JA2-191" "MAD-JA2-192" "MAD-JA2-193" "MAD-JA2-194" "MAD-JA2-195" "MAD-JA2-196" "MAD-JA2-197" "MAD-JA2-198" "MAD-JA2-199" "MAD-JA2-200" "MAD-JA2-201" "MAD-JA2-202" "MAD-JA2-203" "MAD-JA2-204" "MAD-JA2-205" "MAD-JA2-206" "MAD-JA2-207" "MAD-JA2-208" "MAD-JA2-209" "MAD-JA2-210". Observation records are missing from d until this file is standardized.
# MAD-JA2-1999-DIV-FAC.txt: not read. columns: "MAD-JA2-001" "MAD-JA2-002" "MAD-JA2-003" "MAD-JA2-004" "MAD-JA2-005" "MAD-JA2-006" "MAD-JA2-007" "MAD-JA2-008" "MAD-JA2-009" "MAD-JA2-010" "MAD-JA2-011" "MAD-JA2-012" "MAD-JA2-013" "MAD-JA2-014" "MAD-JA2-015" "MAD-JA2-016" "MAD-JA2-017" "MAD-JA2-018" "MAD-JA2-019" "MAD-JA2-020" "MAD-JA2-021" "MAD-JA2-022" "MAD-JA2-023" "MAD-JA2-024" "MAD-JA2-025" "MAD-JA2-026" "MAD-JA2-027" "MAD-JA2-028" "MAD-JA2-029" "MAD-JA2-030" "MAD-JA2-031" "MAD-JA2-032" "MAD-JA2-033" "MAD-JA2-034" "MAD-JA2-035" "MAD-JA2-036" "MAD-JA2-037" "MAD-JA2-038" "MAD-JA2-039" "MAD-JA2-040" "MAD-JA2-041" "MAD-JA2-042" "MAD-JA2-043" "MAD-JA2-044" "MAD-JA2-045" "MAD-JA2-046" "MAD-JA2-047" "MAD-JA2-048" "MAD-JA2-049" "MAD-JA2-050" "MAD-JA2-051" "MAD-JA2-052" "MAD-JA2-053" "MAD-JA2-054" "MAD-JA2-055" "MAD-JA2-056" "MAD-JA2-057" "MAD-JA2-058" "MAD-JA2-059" "MAD-JA2-060" "MAD-JA2-061" "MAD-JA2-062" "MAD-JA2-063" "MAD-JA2-064" "MAD-JA2-065" "MAD-JA2-066" "MAD-JA2-067" "MAD-JA2-068" "MAD-JA2-069" "MAD-JA2-070" "MAD-JA2-071" "MAD-JA2-072" "MAD-JA2-073" "MAD-JA2-074" "MAD-JA2-075" "MAD-JA2-076" "MAD-JA2-077" "MAD-JA2-078" "MAD-JA2-079" "MAD-JA2-080" "MAD-JA2-081" "MAD-JA2-082" "MAD-JA2-083" "MAD-JA2-084" "MAD-JA2-085" "MAD-JA2-086" "MAD-JA2-087" "MAD-JA2-088" "MAD-JA2-089" "MAD-JA2-090" "MAD-JA2-091" "MAD-JA2-092" "MAD-JA2-093" "MAD-JA2-094" "MAD-JA2-095" "MAD-JA2-096" "MAD-JA2-097" "MAD-JA2-098" "MAD-JA2-099" "MAD-JA2-100" "MAD-JA2-101" "MAD-JA2-102" "MAD-JA2-103" "MAD-JA2-104" "MAD-JA2-105" "MAD-JA2-106" "MAD-JA2-107" "MAD-JA2-108" "MAD-JA2-109" "MAD-JA2-110" "MAD-JA2-111" "MAD-JA2-112" "MAD-JA2-113" "MAD-JA2-114" "MAD-JA2-115" "MAD-JA2-116" "MAD-JA2-117" "MAD-JA2-118" "MAD-JA2-119" "MAD-JA2-120" "MAD-JA2-121" "MAD-JA2-122" "MAD-JA2-123" "MAD-JA2-124" "MAD-JA2-125" "MAD-JA2-126" "MAD-JA2-127" "MAD-JA2-128" "MAD-JA2-129" "MAD-JA2-130" "MAD-JA2-131" "MAD-JA2-132" "MAD-JA2-133" "MAD-JA2-134" "MAD-JA2-135" "MAD-JA2-136" "MAD-JA2-137" "MAD-JA2-138" "MAD-JA2-139" "MAD-JA2-140" "MAD-JA2-141" "MAD-JA2-142" "MAD-JA2-143" "MAD-JA2-144" "MAD-JA2-145" "MAD-JA2-146" "MAD-JA2-147" "MAD-JA2-148" "MAD-JA2-149" "MAD-JA2-150" "MAD-JA2-151" "MAD-JA2-152" "MAD-JA2-153" "MAD-JA2-154" "MAD-JA2-155" "MAD-JA2-156" "MAD-JA2-157" "MAD-JA2-158" "MAD-JA2-159" "MAD-JA2-160" "MAD-JA2-161" "MAD-JA2-162" "MAD-JA2-163" "MAD-JA2-164" "MAD-JA2-165" "MAD-JA2-166" "MAD-JA2-167" "MAD-JA2-168" "MAD-JA2-169" "MAD-JA2-170" "MAD-JA2-171" "MAD-JA2-172" "MAD-JA2-173" "MAD-JA2-174" "MAD-JA2-175" "MAD-JA2-176" "MAD-JA2-177" "MAD-JA2-178" "MAD-JA2-179" "MAD-JA2-180" "MAD-JA2-181" "MAD-JA2-182" "MAD-JA2-183" "MAD-JA2-184" "MAD-JA2-185" "MAD-JA2-186" "MAD-JA2-187" "MAD-JA2-188" "MAD-JA2-189" "MAD-JA2-190" "MAD-JA2-191" "MAD-JA2-192" "MAD-JA2-193" "MAD-JA2-194" "MAD-JA2-195" "MAD-JA2-196" "MAD-JA2-197" "MAD-JA2-198" "MAD-JA2-199" "MAD-JA2-200" "MAD-JA2-201" "MAD-JA2-202" "MAD-JA2-203" "MAD-JA2-204" "MAD-JA2-205" "MAD-JA2-206" "MAD-JA2-207" "MAD-JA2-208" "MAD-JA2-209" "MAD-JA2-210". Observation records are missing from d until this file is standardized.
# MAD-JA2-1999-DIV-PA-FLO.txt: not read. columns: "MAD-JA2-001" "MAD-JA2-002" "MAD-JA2-003" "MAD-JA2-004" "MAD-JA2-005" "MAD-JA2-006" "MAD-JA2-007" "MAD-JA2-008" "MAD-JA2-009" "MAD-JA2-010" "MAD-JA2-011" "MAD-JA2-012" "MAD-JA2-013" "MAD-JA2-014" "MAD-JA2-015" "MAD-JA2-016" "MAD-JA2-017" "MAD-JA2-018" "MAD-JA2-019" "MAD-JA2-020" "MAD-JA2-021" "MAD-JA2-022" "MAD-JA2-023" "MAD-JA2-024" "MAD-JA2-025" "MAD-JA2-026" "MAD-JA2-027" "MAD-JA2-028" "MAD-JA2-029" "MAD-JA2-030" "MAD-JA2-031" "MAD-JA2-032" "MAD-JA2-033" "MAD-JA2-034" "MAD-JA2-035" "MAD-JA2-036" "MAD-JA2-037" "MAD-JA2-038" "MAD-JA2-039" "MAD-JA2-040" "MAD-JA2-041" "MAD-JA2-042" "MAD-JA2-043" "MAD-JA2-044" "MAD-JA2-045" "MAD-JA2-046" "MAD-JA2-047" "MAD-JA2-048" "MAD-JA2-049" "MAD-JA2-050" "MAD-JA2-051" "MAD-JA2-052" "MAD-JA2-053" "MAD-JA2-054" "MAD-JA2-055" "MAD-JA2-056" "MAD-JA2-057" "MAD-JA2-058" "MAD-JA2-059" "MAD-JA2-060" "MAD-JA2-061" "MAD-JA2-062" "MAD-JA2-063" "MAD-JA2-064" "MAD-JA2-065" "MAD-JA2-066" "MAD-JA2-067" "MAD-JA2-068" "MAD-JA2-069" "MAD-JA2-070" "MAD-JA2-071" "MAD-JA2-072" "MAD-JA2-073" "MAD-JA2-074" "MAD-JA2-075" "MAD-JA2-076" "MAD-JA2-077" "MAD-JA2-078" "MAD-JA2-079" "MAD-JA2-080" "MAD-JA2-081" "MAD-JA2-082" "MAD-JA2-083" "MAD-JA2-084" "MAD-JA2-085" "MAD-JA2-086" "MAD-JA2-087" "MAD-JA2-088" "MAD-JA2-089" "MAD-JA2-090" "MAD-JA2-091" "MAD-JA2-092" "MAD-JA2-093" "MAD-JA2-094" "MAD-JA2-095" "MAD-JA2-096" "MAD-JA2-097" "MAD-JA2-098" "MAD-JA2-099" "MAD-JA2-100" "MAD-JA2-101" "MAD-JA2-102" "MAD-JA2-103" "MAD-JA2-104" "MAD-JA2-105" "MAD-JA2-106" "MAD-JA2-107" "MAD-JA2-108" "MAD-JA2-109" "MAD-JA2-110" "MAD-JA2-111" "MAD-JA2-112" "MAD-JA2-113" "MAD-JA2-114" "MAD-JA2-115" "MAD-JA2-116" "MAD-JA2-117" "MAD-JA2-118" "MAD-JA2-119" "MAD-JA2-120" "MAD-JA2-121" "MAD-JA2-122" "MAD-JA2-123" "MAD-JA2-124" "MAD-JA2-125" "MAD-JA2-126" "MAD-JA2-127" "MAD-JA2-128" "MAD-JA2-129" "MAD-JA2-130" "MAD-JA2-131" "MAD-JA2-132" "MAD-JA2-133" "MAD-JA2-134" "MAD-JA2-135" "MAD-JA2-136" "MAD-JA2-137" "MAD-JA2-138" "MAD-JA2-139" "MAD-JA2-140" "MAD-JA2-141" "MAD-JA2-142" "MAD-JA2-143" "MAD-JA2-144" "MAD-JA2-145" "MAD-JA2-146" "MAD-JA2-147" "MAD-JA2-148" "MAD-JA2-149" "MAD-JA2-150" "MAD-JA2-151" "MAD-JA2-152" "MAD-JA2-153" "MAD-JA2-154" "MAD-JA2-155" "MAD-JA2-156" "MAD-JA2-157" "MAD-JA2-158" "MAD-JA2-159" "MAD-JA2-160" "MAD-JA2-161" "MAD-JA2-162" "MAD-JA2-163" "MAD-JA2-164" "MAD-JA2-165" "MAD-JA2-166" "MAD-JA2-167" "MAD-JA2-168" "MAD-JA2-169" "MAD-JA2-170" "MAD-JA2-171" "MAD-JA2-172" "MAD-JA2-173" "MAD-JA2-174" "MAD-JA2-175" "MAD-JA2-176" "MAD-JA2-177" "MAD-JA2-178" "MAD-JA2-179" "MAD-JA2-180" "MAD-JA2-181" "MAD-JA2-182" "MAD-JA2-183" "MAD-JA2-184" "MAD-JA2-185" "MAD-JA2-186" "MAD-JA2-187" "MAD-JA2-188" "MAD-JA2-189" "MAD-JA2-190" "MAD-JA2-191" "MAD-JA2-192" "MAD-JA2-193" "MAD-JA2-194" "MAD-JA2-195" "MAD-JA2-196" "MAD-JA2-197" "MAD-JA2-198" "MAD-JA2-199" "MAD-JA2-200" "MAD-JA2-201" "MAD-JA2-202" "MAD-JA2-203" "MAD-JA2-204" "MAD-JA2-205" "MAD-JA2-206" "MAD-JA2-207" "MAD-JA2-208" "MAD-JA2-209" "MAD-JA2-210". Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Weed survey in various cropping systems in Madagascar (1999)

Characterization of weed communities in the cotton rotation (cotton, maize, cassava, groundnut) in South West Madagascar, within the framework of the PSO (Projet Sud-Ouest). Phytoecological study in abundance scale 1-9. This dataset covers 210 plant surveys for 140 weed species.
"

	uri <- "doi:10.18167/DVN1/XZ4CBA"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIRAD",
		publication = NA,
		project = "Projet Sud-Ouest",
		design = "weed abundance survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species;ground_cover;weed_cover",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "MAD-JA2-1999-DIV-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Facteurs")
	r2 <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1[r1[[1]] == label, -1]))

	# MAD-JA2-1999-DIV-AD-DOC.txt: 1-9 note to % cover, same table as Benin 2013.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(
		arachide = "groundnut", cotonnier = "cotton", "mais/manioc" = "maize_cassava"
	)
	prev_map <- c(
		arachide = "groundnut", cotonnier = "cotton", "jachère" = "none",
		"maïs" = "maize", mais = "maize", manioc = "cassava",
		"manioc-maïs" = "cassava;maize"
	)

	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	enh <- as.integer(as.numeric(get_row("Enherbement")))
	d1b <- data.frame(
		trial_id = names(r1)[-1],
		crop = unname(crop_map[tolower(trimws(as.character(get_row("Culture_origine"))))]),
		previous_crop = unname(prev_map[tolower(trimws(as.character(get_row("Precedent"))))]),
		date = as.character(as.integer(as.numeric(get_row("Annee")))),
		country = "Madagascar", # geographicCoverage
		location = trimws(as.character(get_row("Lieu"))),
		weed_cover = rating_to_cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	d1b$intercropped <- grepl("_", d1b$crop)
	d1b$on_farm <- TRUE
	d1b$is_survey <- TRUE
	d1b$geo_from_source <- FALSE

	present <- which(!is.na(r2[, -1]) & r2[, -1] > 0, arr.ind=TRUE)
	scores <- as.integer(as.numeric(r2[, -1][present]))
	d <- data.frame(
		trial_id = names(r2)[-1][present[, "col"]],
		weed_species = trimws(as.character(r2[[1]][present[, "row"]])),
		ground_cover = rating_to_cover[match(scores, 1:9)]
	)
	d <- merge(d, d1b, by="trial_id", all.x=TRUE)

	carobiner::write_files(path, meta, d)
}
