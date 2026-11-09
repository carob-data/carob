# R script for "carob"
# license: GPL (>=3)

## NOTES
# 198 plot surveys in south-west Madagascar (1998). One row per species with a
# rating > 0. Floristique and Enherbement are the DOC 1-9 notes (not percents);
# both use the DOC percent table. weed_species is the 5-letter code.

## ISSUES
# No coordinates in the file. DOC names Tuléar; Lieu is Ankazoabo, Antseva,
# Manombo, Sakaraha, or Vineta. Culture_origine2 "légumineuses" is not a crop
# species. write_files: missing variables: yield, yield_moisture, yield_part,
# latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date,
# harvest_date. invalid terms: intercrops: légumineuses.


## NOT PROCESSED
# Facteurs not mapped: Parcelle, ages, Altitude, Assolement, Type_culture, Climat,
# Date de semis, Date labour, Drainage, Etat de surface, Facteur hydrique, Fumure
# (qualitative, no nutrient rate), soil surface and texture, crop height and cover,
# labour and weeding intensity, Pente, Profondeur, Zone. Méta Données, FLO, FAC,
# and PA repeat the workbook or the DOC.
# MAD-JAR-1998-DIV-AD-FLO.txt: not read. columns: "MAD-JAR-001" "MAD-JAR-002" "MAD-JAR-003" "MAD-JAR-004" "MAD-JAR-005" "MAD-JAR-006" "MAD-JAR-007" "MAD-JAR-008" "MAD-JAR-009" "MAD-JAR-010" "MAD-JAR-011" "MAD-JAR-012" "MAD-JAR-013" "MAD-JAR-014" "MAD-JAR-015" "MAD-JAR-016" "MAD-JAR-017" "MAD-JAR-018" "MAD-JAR-019" "MAD-JAR-020" "MAD-JAR-021" "MAD-JAR-022" "MAD-JAR-023" "MAD-JAR-024" "MAD-JAR-025" "MAD-JAR-026" "MAD-JAR-027" "MAD-JAR-028" "MAD-JAR-029" "MAD-JAR-030" "MAD-JAR-031" "MAD-JAR-032" "MAD-JAR-033" "MAD-JAR-034" "MAD-JAR-035" "MAD-JAR-036" "MAD-JAR-037" "MAD-JAR-038" "MAD-JAR-039" "MAD-JAR-040" "MAD-JAR-041" "MAD-JAR-042" "MAD-JAR-043" "MAD-JAR-044" "MAD-JAR-045" "MAD-JAR-046" "MAD-JAR-047" "MAD-JAR-048" "MAD-JAR-049" "MAD-JAR-050" "MAD-JAR-051" "MAD-JAR-052" "MAD-JAR-053" "MAD-JAR-054" "MAD-JAR-055" "MAD-JAR-056" "MAD-JAR-057" "MAD-JAR-058" "MAD-JAR-059" "MAD-JAR-060" "MAD-JAR-061" "MAD-JAR-062" "MAD-JAR-063" "MAD-JAR-064" "MAD-JAR-065" "MAD-JAR-066" "MAD-JAR-067" "MAD-JAR-068" "MAD-JAR-069" "MAD-JAR-070" "MAD-JAR-071" "MAD-JAR-072" "MAD-JAR-073" "MAD-JAR-074" "MAD-JAR-075" "MAD-JAR-076" "MAD-JAR-077" "MAD-JAR-078" "MAD-JAR-079" "MAD-JAR-080" "MAD-JAR-081" "MAD-JAR-082" "MAD-JAR-083" "MAD-JAR-084" "MAD-JAR-085" "MAD-JAR-086" "MAD-JAR-087" "MAD-JAR-088" "MAD-JAR-089" "MAD-JAR-090" "MAD-JAR-091" "MAD-JAR-092" "MAD-JAR-093" "MAD-JAR-094" "MAD-JAR-095" "MAD-JAR-096" "MAD-JAR-097" "MAD-JAR-098" "MAD-JAR-099" "MAD-JAR-100" "MAD-JAR-101" "MAD-JAR-102" "MAD-JAR-103" "MAD-JAR-104" "MAD-JAR-105" "MAD-JAR-106" "MAD-JAR-107" "MAD-JAR-108" "MAD-JAR-109" "MAD-JAR-110" "MAD-JAR-111" "MAD-JAR-112" "MAD-JAR-113" "MAD-JAR-114" "MAD-JAR-115" "MAD-JAR-116" "MAD-JAR-117" "MAD-JAR-118" "MAD-JAR-119" "MAD-JAR-120" "MAD-JAR-121" "MAD-JAR-122" "MAD-JAR-123" "MAD-JAR-124" "MAD-JAR-125" "MAD-JAR-126" "MAD-JAR-127" "MAD-JAR-128" "MAD-JAR-129" "MAD-JAR-130" "MAD-JAR-131" "MAD-JAR-132" "MAD-JAR-133" "MAD-JAR-134" "MAD-JAR-135" "MAD-JAR-136" "MAD-JAR-137" "MAD-JAR-138" "MAD-JAR-139" "MAD-JAR-140" "MAD-JAR-141" "MAD-JAR-142" "MAD-JAR-143" "MAD-JAR-144" "MAD-JAR-145" "MAD-JAR-146" "MAD-JAR-147" "MAD-JAR-148" "MAD-JAR-149" "MAD-JAR-150" "MAD-JAR-151" "MAD-JAR-152" "MAD-JAR-153" "MAD-JAR-154" "MAD-JAR-155" "MAD-JAR-156" "MAD-JAR-157" "MAD-JAR-158" "MAD-JAR-159" "MAD-JAR-160" "MAD-JAR-161" "MAD-JAR-162" "MAD-JAR-163" "MAD-JAR-164" "MAD-JAR-165" "MAD-JAR-166" "MAD-JAR-167" "MAD-JAR-168" "MAD-JAR-169" "MAD-JAR-170" "MAD-JAR-171" "MAD-JAR-172" "MAD-JAR-173" "MAD-JAR-174" "MAD-JAR-175" "MAD-JAR-176" "MAD-JAR-177" "MAD-JAR-178" "MAD-JAR-179" "MAD-JAR-180" "MAD-JAR-181" "MAD-JAR-182" "MAD-JAR-183" "MAD-JAR-184" "MAD-JAR-185" "MAD-JAR-186" "MAD-JAR-187" "MAD-JAR-188" "MAD-JAR-189" "MAD-JAR-190" "MAD-JAR-191" "MAD-JAR-192" "MAD-JAR-193" "MAD-JAR-194" "MAD-JAR-195" "MAD-JAR-196" "MAD-JAR-197" "MAD-JAR-198". Observation records are missing from d until this file is standardized.
# MAD-JAR-1998-DIV-FAC.txt: not read. columns: "MAD-JAR-001" "MAD-JAR-002" "MAD-JAR-003" "MAD-JAR-004" "MAD-JAR-005" "MAD-JAR-006" "MAD-JAR-007" "MAD-JAR-008" "MAD-JAR-009" "MAD-JAR-010" "MAD-JAR-011" "MAD-JAR-012" "MAD-JAR-013" "MAD-JAR-014" "MAD-JAR-015" "MAD-JAR-016" "MAD-JAR-017" "MAD-JAR-018" "MAD-JAR-019" "MAD-JAR-020" "MAD-JAR-021" "MAD-JAR-022" "MAD-JAR-023" "MAD-JAR-024" "MAD-JAR-025" "MAD-JAR-026" "MAD-JAR-027" "MAD-JAR-028" "MAD-JAR-029" "MAD-JAR-030" "MAD-JAR-031" "MAD-JAR-032" "MAD-JAR-033" "MAD-JAR-034" "MAD-JAR-035" "MAD-JAR-036" "MAD-JAR-037" "MAD-JAR-038" "MAD-JAR-039" "MAD-JAR-040" "MAD-JAR-041" "MAD-JAR-042" "MAD-JAR-043" "MAD-JAR-044" "MAD-JAR-045" "MAD-JAR-046" "MAD-JAR-047" "MAD-JAR-048" "MAD-JAR-049" "MAD-JAR-050" "MAD-JAR-051" "MAD-JAR-052" "MAD-JAR-053" "MAD-JAR-054" "MAD-JAR-055" "MAD-JAR-056" "MAD-JAR-057" "MAD-JAR-058" "MAD-JAR-059" "MAD-JAR-060" "MAD-JAR-061" "MAD-JAR-062" "MAD-JAR-063" "MAD-JAR-064" "MAD-JAR-065" "MAD-JAR-066" "MAD-JAR-067" "MAD-JAR-068" "MAD-JAR-069" "MAD-JAR-070" "MAD-JAR-071" "MAD-JAR-072" "MAD-JAR-073" "MAD-JAR-074" "MAD-JAR-075" "MAD-JAR-076" "MAD-JAR-077" "MAD-JAR-078" "MAD-JAR-079" "MAD-JAR-080" "MAD-JAR-081" "MAD-JAR-082" "MAD-JAR-083" "MAD-JAR-084" "MAD-JAR-085" "MAD-JAR-086" "MAD-JAR-087" "MAD-JAR-088" "MAD-JAR-089" "MAD-JAR-090" "MAD-JAR-091" "MAD-JAR-092" "MAD-JAR-093" "MAD-JAR-094" "MAD-JAR-095" "MAD-JAR-096" "MAD-JAR-097" "MAD-JAR-098" "MAD-JAR-099" "MAD-JAR-100" "MAD-JAR-101" "MAD-JAR-102" "MAD-JAR-103" "MAD-JAR-104" "MAD-JAR-105" "MAD-JAR-106" "MAD-JAR-107" "MAD-JAR-108" "MAD-JAR-109" "MAD-JAR-110" "MAD-JAR-111" "MAD-JAR-112" "MAD-JAR-113" "MAD-JAR-114" "MAD-JAR-115" "MAD-JAR-116" "MAD-JAR-117" "MAD-JAR-118" "MAD-JAR-119" "MAD-JAR-120" "MAD-JAR-121" "MAD-JAR-122" "MAD-JAR-123" "MAD-JAR-124" "MAD-JAR-125" "MAD-JAR-126" "MAD-JAR-127" "MAD-JAR-128" "MAD-JAR-129" "MAD-JAR-130" "MAD-JAR-131" "MAD-JAR-132" "MAD-JAR-133" "MAD-JAR-134" "MAD-JAR-135" "MAD-JAR-136" "MAD-JAR-137" "MAD-JAR-138" "MAD-JAR-139" "MAD-JAR-140" "MAD-JAR-141" "MAD-JAR-142" "MAD-JAR-143" "MAD-JAR-144" "MAD-JAR-145" "MAD-JAR-146" "MAD-JAR-147" "MAD-JAR-148" "MAD-JAR-149" "MAD-JAR-150" "MAD-JAR-151" "MAD-JAR-152" "MAD-JAR-153" "MAD-JAR-154" "MAD-JAR-155" "MAD-JAR-156" "MAD-JAR-157" "MAD-JAR-158" "MAD-JAR-159" "MAD-JAR-160" "MAD-JAR-161" "MAD-JAR-162" "MAD-JAR-163" "MAD-JAR-164" "MAD-JAR-165" "MAD-JAR-166" "MAD-JAR-167" "MAD-JAR-168" "MAD-JAR-169" "MAD-JAR-170" "MAD-JAR-171" "MAD-JAR-172" "MAD-JAR-173" "MAD-JAR-174" "MAD-JAR-175" "MAD-JAR-176" "MAD-JAR-177" "MAD-JAR-178" "MAD-JAR-179" "MAD-JAR-180" "MAD-JAR-181" "MAD-JAR-182" "MAD-JAR-183" "MAD-JAR-184" "MAD-JAR-185" "MAD-JAR-186" "MAD-JAR-187" "MAD-JAR-188" "MAD-JAR-189" "MAD-JAR-190" "MAD-JAR-191" "MAD-JAR-192" "MAD-JAR-193" "MAD-JAR-194" "MAD-JAR-195" "MAD-JAR-196" "MAD-JAR-197" "MAD-JAR-198". Observation records are missing from d until this file is standardized.
# MAD-JAR-1998-DIV-PA-FLO.txt: not read. columns: "MAD-JAR-001" "MAD-JAR-002" "MAD-JAR-003" "MAD-JAR-004" "MAD-JAR-005" "MAD-JAR-006" "MAD-JAR-007" "MAD-JAR-008" "MAD-JAR-009" "MAD-JAR-010" "MAD-JAR-011" "MAD-JAR-012" "MAD-JAR-013" "MAD-JAR-014" "MAD-JAR-015" "MAD-JAR-016" "MAD-JAR-017" "MAD-JAR-018" "MAD-JAR-019" "MAD-JAR-020" "MAD-JAR-021" "MAD-JAR-022" "MAD-JAR-023" "MAD-JAR-024" "MAD-JAR-025" "MAD-JAR-026" "MAD-JAR-027" "MAD-JAR-028" "MAD-JAR-029" "MAD-JAR-030" "MAD-JAR-031" "MAD-JAR-032" "MAD-JAR-033" "MAD-JAR-034" "MAD-JAR-035" "MAD-JAR-036" "MAD-JAR-037" "MAD-JAR-038" "MAD-JAR-039" "MAD-JAR-040" "MAD-JAR-041" "MAD-JAR-042" "MAD-JAR-043" "MAD-JAR-044" "MAD-JAR-045" "MAD-JAR-046" "MAD-JAR-047" "MAD-JAR-048" "MAD-JAR-049" "MAD-JAR-050" "MAD-JAR-051" "MAD-JAR-052" "MAD-JAR-053" "MAD-JAR-054" "MAD-JAR-055" "MAD-JAR-056" "MAD-JAR-057" "MAD-JAR-058" "MAD-JAR-059" "MAD-JAR-060" "MAD-JAR-061" "MAD-JAR-062" "MAD-JAR-063" "MAD-JAR-064" "MAD-JAR-065" "MAD-JAR-066" "MAD-JAR-067" "MAD-JAR-068" "MAD-JAR-069" "MAD-JAR-070" "MAD-JAR-071" "MAD-JAR-072" "MAD-JAR-073" "MAD-JAR-074" "MAD-JAR-075" "MAD-JAR-076" "MAD-JAR-077" "MAD-JAR-078" "MAD-JAR-079" "MAD-JAR-080" "MAD-JAR-081" "MAD-JAR-082" "MAD-JAR-083" "MAD-JAR-084" "MAD-JAR-085" "MAD-JAR-086" "MAD-JAR-087" "MAD-JAR-088" "MAD-JAR-089" "MAD-JAR-090" "MAD-JAR-091" "MAD-JAR-092" "MAD-JAR-093" "MAD-JAR-094" "MAD-JAR-095" "MAD-JAR-096" "MAD-JAR-097" "MAD-JAR-098" "MAD-JAR-099" "MAD-JAR-100" "MAD-JAR-101" "MAD-JAR-102" "MAD-JAR-103" "MAD-JAR-104" "MAD-JAR-105" "MAD-JAR-106" "MAD-JAR-107" "MAD-JAR-108" "MAD-JAR-109" "MAD-JAR-110" "MAD-JAR-111" "MAD-JAR-112" "MAD-JAR-113" "MAD-JAR-114" "MAD-JAR-115" "MAD-JAR-116" "MAD-JAR-117" "MAD-JAR-118" "MAD-JAR-119" "MAD-JAR-120" "MAD-JAR-121" "MAD-JAR-122" "MAD-JAR-123" "MAD-JAR-124" "MAD-JAR-125" "MAD-JAR-126" "MAD-JAR-127" "MAD-JAR-128" "MAD-JAR-129" "MAD-JAR-130" "MAD-JAR-131" "MAD-JAR-132" "MAD-JAR-133" "MAD-JAR-134" "MAD-JAR-135" "MAD-JAR-136" "MAD-JAR-137" "MAD-JAR-138" "MAD-JAR-139" "MAD-JAR-140" "MAD-JAR-141" "MAD-JAR-142" "MAD-JAR-143" "MAD-JAR-144" "MAD-JAR-145" "MAD-JAR-146" "MAD-JAR-147" "MAD-JAR-148" "MAD-JAR-149" "MAD-JAR-150" "MAD-JAR-151" "MAD-JAR-152" "MAD-JAR-153" "MAD-JAR-154" "MAD-JAR-155" "MAD-JAR-156" "MAD-JAR-157" "MAD-JAR-158" "MAD-JAR-159" "MAD-JAR-160" "MAD-JAR-161" "MAD-JAR-162" "MAD-JAR-163" "MAD-JAR-164" "MAD-JAR-165" "MAD-JAR-166" "MAD-JAR-167" "MAD-JAR-168" "MAD-JAR-169" "MAD-JAR-170" "MAD-JAR-171" "MAD-JAR-172" "MAD-JAR-173" "MAD-JAR-174" "MAD-JAR-175" "MAD-JAR-176" "MAD-JAR-177" "MAD-JAR-178" "MAD-JAR-179" "MAD-JAR-180" "MAD-JAR-181" "MAD-JAR-182" "MAD-JAR-183" "MAD-JAR-184" "MAD-JAR-185" "MAD-JAR-186" "MAD-JAR-187" "MAD-JAR-188" "MAD-JAR-189" "MAD-JAR-190" "MAD-JAR-191" "MAD-JAR-192" "MAD-JAR-193" "MAD-JAR-194" "MAD-JAR-195" "MAD-JAR-196" "MAD-JAR-197" "MAD-JAR-198". Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Weed survey in various cropping systems in Madagascar (1998)

Characterization of weed communities in the cotton rotation (cotton, maize, cassava, groundnut) in South West Madagascar, within the framework of the PSO (Projet Sud-Ouest). Phytoecological study in abundance scale 1-9. This dataset covers 198 plant surveys for 120 weed species.
"

	uri <- "doi:10.18167/DVN1/O1EHV2"
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

	f <- ff[basename(ff) == "MAD-JAR-1998-DIV-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Facteurs")
	r2 <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1[r1[[1]] == label, -1]))

	# MAD-JAR-1998-DIV-AD-DOC.txt, same 1-9 note to % cover table as Benin 2013.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(
		arachide = "groundnut", cotonnier = "cotton", mais = "maize", manioc = "cassava"
	)
	prev_map <- c(
		arachide = "groundnut", cotonnier = "cotton", "jachère" = "none",
		"maïs" = "maize", mais = "maize", manioc = "cassava",
		"manioc-maïs" = "cassava;maize"
	)
	assoc <- tolower(trimws(as.character(get_row("Culture_origine2"))))
	assoc[is.na(assoc) | !nzchar(assoc) | assoc %in% c("na", "-")] <- NA_character_
	# légumineuses is a group, not a species name in the sheet.
	assoc_map <- c("maïs" = "maize", mais = "maize", "légumineuses" = "légumineuses")

	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	enh <- as.integer(as.numeric(get_row("Enherbement")))
	d1b <- data.frame(
		trial_id = names(r1)[-1],
		crop = unname(crop_map[tolower(trimws(as.character(get_row("Culture_origine"))))]),
		intercrops = unname(assoc_map[assoc]),
		previous_crop = unname(prev_map[tolower(trimws(as.character(get_row("Précédent"))))]),
		date = as.character(as.integer(as.numeric(get_row("Année")))),
		country = "Madagascar", # geographicCoverage
		location = trimws(as.character(get_row("Lieu"))),
		weed_cover = rating_to_cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	d1b$intercropped <- !is.na(d1b$intercrops)
	d1b$on_farm <- TRUE
	d1b$is_survey <- TRUE
	# No coordinates in the workbook.
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
