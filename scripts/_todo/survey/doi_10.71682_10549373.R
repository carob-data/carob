# R script for "carob"
# license: GPL (>=3)

## NOTES
# Mechanization survey of 112 farmers in Honduras, 2023. The legend says crop area is hectares.
# One row per crop the farmer reported planting. Maize, frijol, and cafe are maize, common bean, and coffee.
# Topography words in the legend are Plano, Laderas, and Mixto. The dataset description calls these plains, hillsides, and mixed terrain.
# Departments, municipalities, and communities are in the file. There are no coordinates. Country is Honduras in the geographic coverage and in the legend.

## ISSUES
# No yield and no coordinates.
# write_files: missing variables: latitude, longitude. unknown variable: crop_area_ha_ (crop?). unknown variable: tool_type_ (soil_type?). unknown variables: land_topography_.

## NEW VARIABLES
# crop_area_ha_: area of the crop, hectares.
# land_topography_: plains, hillside, or mixed, from the plot topography question.
# tool_type_: class of tools used for that crop.

## NOT PROCESSED
# Named hand tools, machines, harvest and postharvest equipment, tractor access, and perceived constraints.


carob_script <- function(path) {

"
Agricultural mechanization in Honduras: characterization, challenges, and opportunities for maize, beans, and coffee production systems.

CIMMYT and Universidad Zamorano carried out a study across five departments of Honduras (Lempira, Copán, Intibucá, Olancho, and El Paraíso) to characterize farmers’ use of tools and machinery in maize, bean, and coffee systems, and to identify key challenges and opportunities for mechanization. In total, 112 structured surveys of farmers were conducted.
"

	uri <- "doi:10.71682/10549373"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		response_vars = "crop_area_ha_",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.4
	)

	f <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	r <- carobiner::read.excel(f, sheet="Mecanizacion_Honduras_data")
	topo <- c("Plano"="plain", "Laderas"="hillside", "Mixto"="mixed")
	one <- function(crop, flag, area, top, tool) {
		keep <- tolower(trimws(as.character(r[[flag]]))) == "si"
		tp <- trimws(as.character(r[[top]]))
		data.frame(
			hhid = as.character(r$ID),
			adm1 = trimws(as.character(r$Departamento)),
			adm2 = trimws(as.character(r$Municipio)),
			location = trimws(as.character(r$Comunidad)),
			crop = crop,
			crop_area_ha_ = as.numeric(r[[area]]),
			land_topography_ = unname(topo[tp]),
			tool_type_ = trimws(as.character(r[[tool]])),
			keep = keep
		)
	}
	d <- rbind(
		one("maize", "Siembra_maiz", "Superficie_maiz", "Topografia_maiz", "Tipo_herramientas_maiz"),
		one("common bean", "Siembra_frijol", "Superficie_frijol", "Topografia_frijol", "Tipo_herramientas_frijol"),
		one("coffee", "Siembra_cafe", "Superficie_cafe", "Topografia_cafe", "Tipo_herramientas_cafe")
	)
	d <- d[d$keep, ]
	d$keep <- NULL
	d$tool_type_[d$tool_type_ %in% c("", "NA")] <- NA_character_
	d$country <- "Honduras"
	d$trial_id <- d$hhid
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE
	d$adm1 <- carobiner::fix_name(d$adm1, "title")
	d$adm2 <- carobiner::fix_name(d$adm2, "title")
	d$location <- carobiner::fix_name(d$location, "title")

	carobiner::write_files(path, meta, d)
}
