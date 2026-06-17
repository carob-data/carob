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
    modified_by = "Siyabusa Mkuhlani", 
    last_modified = "2024-07-05"
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
  d$irrigated <- TRUE
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
 
  geo <- data.frame(
		location=c("Antsahamamy", "Ambohimiarina", "Ambohitsilaozana", "Ambongabe", "Ampitatsimo"),
		latitude=c(-18.9185, -21.3561, -17.7014, -17.7067, -18.6729),
		longitude=c(47.5592 , 47.5680 , 48.4657 , 48.1885 , 47.4564),
		geo_from_source = FALSE
	)
  
  d <- merge(d, geo, by="location", all.x=TRUE)
  
  carobiner::write_files(meta, d, path=path)
  
}

