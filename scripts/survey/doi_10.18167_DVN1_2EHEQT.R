# R script for "carob"


carob_script <- function(path) {
  
"Data on rainfed rice production and management were collected for cropping seasons 2006–2007, 2007–2008, 2008–2009 and 2009–2010 around five villages of the BV-Lac programme on lowland and hillside farmer's fields under conservation agriculture in the Lake Alaotra region of Madagascar,  resulting in 3803 site x management x soil x season combinations. (2020-10-07)"
  
  uri <-  "doi:10.18167/DVN1/2EHEQT"
  group <- "survey" 
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
    publication= "doi:10.1017/S0014479714000155",
    data_organization = "CIRAD",
    carob_contributor="Cedric Ngakou",
    carob_date="2023-10-18",
    carob_effort = NA,
    data_type="survey",
	response_vars = "yield",
	# perhaps this is stratification (-like)?
   treatment_vars= "landscape_position;land_prep_method;OM_amount",
    project="Doctorant du Sud;ABACO",
    carob_modified_by = "Siyabusa Mkuhlani", 
    carob_last_modified = "2024-07-05"
  )
  
  r <- readxl::read_excel(ff[basename(ff)=="2006-2010_database_bvlac_bruelle_v01.20201009.xlsx"],sheet=1) |> as.data.frame()
  
  d <- data.frame(
    location = r$village,
    soil_type = r$soil,
    land_prep_method = r$tillage_system,
	landscape_position = r$soil,
    planting_date = as.character(r$sow_date),
    OM_amount = r$manure,
    N_fertilizer = r$nitrogen,
    #Assessment shows N, as 11%. Most common rice fert with 11% N in Madagascar is
    #11:22:16 (N, P2O5, K2O). https://agritrop.cirad.fr/539499/1/document_539499.pdf
	# P = P2O5 / 2.29; K = K2O / 1.2051
    P_fertilizer = (0.22 / 2.29) * r$npk,
    K_fertilizer = (0.16 / 1.2051) * r$npk,  
    yield = r$yield,
	yield_moisture = NA_real_,
	yield_isfresh = TRUE,
    rain = r$rain_cycle,
	trial_id = r$id_field
  )
 
 
    d$land_prep_method <- gsub("till", "conventional", d$land_prep_method)
    # it is not defined what "CA" (conservation agriculture)  means. Is it "none" (no tillage) or "reduced tillage"?
	d$land_prep_method <- gsub("ca", "reduced tillage", d$land_prep_method)
 
  d$country <- "Madagascar"
  d$crop <- "rice"
  d$yield_part <- "grain" 
  d$on_farm <- TRUE
  # Title, keywords, and the paper: rainfed lowland and hillside rice. The file has rainfall and no irrigation column.
  d$irrigated <- FALSE
  d$inoculated <- FALSE
  d$is_survey <- FALSE
  d$fertilizer_type <- "urea;NPK"
  d$OM_type <- "none" 
  d$OM_used <- FALSE
  i <- d$OM_amount > 0
  d$OM_type[i] <- "horse manure"
  d$OM_used[i] <- TRUE
  
  d$herbicide_used <- r$glypho == "Y"
  d$herbicide_product <- "none"
  d$herbicide_product[d$herbicide_used] <- "glyphosate"
 
  # Coordinates are not in the data. Ambohitsilaozana and Ambongabe are the villages of those names beside Lake Alaotra.
  # Antsahamamy (Sahamamy) is the rural municipality in Amparafaravola district, about 15 km north of Amparafaravola. 17°31'S, 48°18'E.
  # Ampitatsimo (Ampitasimo) is the commune in Ambatondrazaka district. The reported position is the box 17°46'-47'S, 48°22'-23'E; the centre of that box is used.
  # Ambohimiarina also names a place about 400 km away, and no village of that name was found near the lake, so its coordinates are left empty.
  geo <- data.frame(
		location=c("Antsahamamy", "Ambohimiarina", "Ambohitsilaozana", "Ambongabe", "Ampitatsimo"),
		latitude=c(-17.51667, -17.93201, -17.7014, -17.7067, -17.775),
		longitude=c(48.3, 48.36667, 48.4657, 48.1885, 48.375),
		geo_uncertainty = c(2000, 75000, NA, NA, 2000),
		geo_from_source = FALSE
	)

  d <- merge(d, geo, by="location", all.x=TRUE)
  # GADM NAME_1. The region is also written Alaotra-Mangoro.
  d$adm1 <- "Alaotra Mangoro"
  
  carobiner::write_files(meta, d, path=path)
  
}

