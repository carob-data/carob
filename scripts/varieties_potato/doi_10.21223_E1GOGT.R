carob_script <- function(path) {
  
  "
  Replication Data for: Identification of Elite Potato Clones with Resistance to Late Blight Through Participatory Varietal Selection in Peru

  Potato is the most important crop in Peru and late blight is the main disease affecting the crop. However, new varieties that are resistant to late blight often lack other traits that farmers and consumers prefer. Using participatory varietal selection, this study seeks to identify clones with high potential to become varieties with resistance to late blight but also feature a high marketable tuber yield and other preferred agronomic traits. During 2016–2017, 36 clones previously selected for high levels of resistance to late blight from population B developed by the International Potato Center, and three varieties used as controls (INIA 302 Amarilis moderately resistant, INIA303 Canchan and Yungay susceptible to late blight), were evaluated in five Peruvian locations. At harvest, five clones were selected based on (i) evaluations made by farmers through Participatory Varietal Selection, (ii) analysis of mixed models and Best Linear Unbiased Predictors for tuber yield, (iii) low glycoalkaloid content in tubers, and (iv) good organoleptic quality. These clones were evaluated again during 2017–2018 in four locations. Resistance to late blight and good marketable tuber yields were identified as the most important criteria for the selection of a new potato variety. The clones CIP308488.92, CIP308495.227 and CIP308478.59 were selected as promising clones having resistance to late blight and tuber yield superior to the local varieties, INIA-303 Canchan, Yungay, as well as good organoleptic quality and low glycoalkaloid content. These clones can be suggested for variety release in similar agroecological environments.
  "
  
  uri <- "doi:10.21223/E1GOGT"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield_marketable;glycoalkaloid_total_;tuber_flavor",
    notes =NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-10",
    carob_completion = 80,
    carob_effort = 4.5
)
  
  ## Source files
  f1 <- ff[basename(ff) == "Table 01 B3C3 potato clones with resistance to LB.xlsx"]
  f2 <- ff[basename(ff) == "Table 02 Sites for experiments in 2016–2017 and 2017–2018.xlsx"]
  f3 <- ff[basename(ff) == "Table 03 Clones selected in 2016–2017 and tested in 2017–2018.xlsx"]
  f7 <- ff[basename(ff) == "Table 10 Glycoalkaloid content in B3C3 clones 2016–2017.xlsx"]
  f8 <- ff[basename(ff) == "Table 11 Organoleptic test in B3C3 clones by locality at harvest with PVS methodology.xlsx"]
  
  ## Read source data
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3)
  r7 <- carobiner::read.excel(f7)
  r8 <- carobiner::read.excel(f8)
  
  ## Coordinates from r2 (3 main regions)
  geo <- data.frame(
    location = c("Cajamarca", "La Libertad", "Huancavelica"),
    latitude = c(-7.1638, -7.7814, -12.7537),
    longitude = c(-78.5000, -77.8683, -74.8129),
    adm1 = c("Cajamarca", "La Libertad", "Huancavelica"),
    geo_from_source = FALSE
  )
  
  ## Data from r3 (5 selected clones) - 3 locations
  d_cja <- data.frame(
    trial_id = "E1GOGT_Cajamarca",
    variety = r3$Clone,
    yield_marketable = as.numeric(r3$`Marketable tuber yield (t ha−1) CJA`) * 1000,
    glycoalkaloid_total_ = as.numeric(r3$`Total glycoalkaloid content (mg/100 g fresh weight) CJA`),
    tuber_flavor = as.numeric(r3$`Flavour CJA`),
    location = "Cajamarca",
    country = "Peru",
    crop = "potato",
    on_farm = TRUE,
    is_survey = FALSE,
    yield_part = "tubers",
    yield = NA,
    yield_moisture = NA,
    yield_isfresh = NA,
    irrigated = NA,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    planting_date = NA,
    harvest_date = NA
  )
  
  d_llb <- data.frame(
    trial_id = "E1GOGT_LaLibertad",
    variety = r3$Clone,
    yield_marketable = as.numeric(r3$`Marketable tuber yield (t ha−1) LLB`) * 1000,
    glycoalkaloid_total_ = as.numeric(r3$`Total glycoalkaloid content (mg/100 g fresh weight) LLB`),
    tuber_flavor = as.numeric(r3$`Flavour LLB`),
    location = "La Libertad",
    country = "Peru",
    crop = "potato",
    on_farm = TRUE,
    is_survey = FALSE,
    yield_part = "tubers",
    yield = NA,
    yield_moisture = NA,
    yield_isfresh = NA,
    irrigated = NA,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    planting_date = NA,
    harvest_date = NA
  )
  
  d_hva <- data.frame(
    trial_id = "E1GOGT_Huancavelica",
    variety = r3$Clone,
    yield_marketable = as.numeric(r3$`Marketable tuber yield (t ha−1) HVA`) * 1000,
    glycoalkaloid_total_ = as.numeric(r3$`Total glycoalkaloid content (mg/100 g fresh weight) HVA`),
    tuber_flavor = as.numeric(r3$`Flavour HVA`),
    location = "Huancavelica",
    country = "Peru",
    crop = "potato",
    on_farm = TRUE,
    is_survey = FALSE,
    yield_part = "tubers",
    yield = NA,
    yield_moisture = NA,
    yield_isfresh = NA,
    irrigated = NA,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    planting_date = NA,
    harvest_date = NA
  )
  
  ## Combine all locations
  d <- carobiner::bindr(d_cja, d_llb, d_hva)
  
  ## Merge coordinates
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  carobiner::write_files(path, meta, d)
}