
carob_script <- function(path) {
  
  "
Dataset for: Three new healthy and sustainable potato varieties for family farming systems with resistance to late blight and high quality for the frying industry.

The CIP-PODEROSA CROCANTE, CIP-PODEROSA POLLERA and CIP-PODEROSA WATIA potato varieties, belong to the breeding population CIP's named B, Sub-population B3, cycle 2 (B3C2), obtained from crossing of advanced clones of cycle 1 (B3C1) . The parental clones have high levels of yield, resistance to late blight Phytophthora infestans (Mont) de Bary and frying quality in French fries. The crosses were made by CIP at the Santa Ana experimental station in Huancayo, Peru (3288 m a.s.l., 12°0'33.9'S, 75°13'41.7'W) during 1995 and 1996. CIP-PODEROSA CROCANTE variety (CIP396026.101) was generated  from the crossing of advanced clones CIP393046.7 as female and CIP393079.4 as male. CIP-PODEROSA POLLERA (CIP395123.6) has as parents to  advanced clone CIP392633.4 as female and CIP393280.64 as male, the male progenitor was released as a variety in Costa Rica under the name of Pasqui  and in Bangladesh as BARI Alu-53 due to its high yield, resistance to late blight (LB) and excellent quality for frying. The variety CIP-PODEROSA WATIA (CIP396034.101) was generated from  crossing of advanced clones CIP393042.50 as female and CIP393280.64 as male . The CIP-PODEROSA CROCANTE and CIP-PODEROSA POLLERA varieties have high resistance to LB, high tuber yield, excellent quality for French fries and lower acrylamide content than levels recommended by European Union (500 µgkg-1 of potato chips)   (http: //data.europa.eu/eli/reg/2017/2158/oj), and the CIP-PODEROSA WATIA variety, in addition to the characteristics mentioned for the other varieties, has excellent quality for baking. Evaluation of these varieties for resistance to LB was carried out between 1998 and 2008, under field conditions in Oxapampa (1814 m a.s.l., 12°34′3″ S, 75°24′14″ W) and Comas (2550 m a.s.l. , 11°37′09″S 75°05′22″W), two places with optimal environmental conditions of temperature between 15 to 20°C, more than 80% relative humidity and more than 1500 mm of annual precipitation, which allowed having a high disease pressure, AUDPC and sAUDPC were used as indicators of resistance. Tuber yield also was evaluated at La Molina (241 m a.s.l., 12°4′40″ S, 76° 54′40″ W), Huaral (188 m a.s.l., 1°30′2.74″ S, 77°12′32.62″ W) and Huancayo (3249 m a.s.l., 2°4′0″ S, 75°13′0″ W), where the presence of LB is minimal. French fries frying quality, baking quality, dry matter content, and reducing sugars were determined at the CIP Postharvest Laboratory in La Molina, Lima, Peru. Tubers were stored under room conditions in La Molina, Lima, at 15-16°C for three months after harvest. Names of the varieties were selected in a nationwide contest with the participation of all actors of potato value chain. Names Crocante and Pollera are related to the characteristics of the varieties, Crocante for its crispness and Pollera because it was used together with grilled chicken, a typical dish in Peru. The name Watia was selected  in honor of its quality for baking and has origin in the Quechua word 'Watia' which means baked. Documents for registration as new varieties have been sent to the seed authority in Peru.  These new varieties are healthy both or farmers as for consumers. In case of farmers, they and their families will have less exposure to fungicides to control LB, and in case of consumers, they will have potato tubers  with less fungicide  residues,  and low content of acrylamides, and glycoalkaloids. These varieties are sustainable to be included in family farming systems for their comparative advantages that will preserve the environment, increase the income of small and medium scale farmers, allowing them to improve their living standards.
"
  uri <- "doi:10.21223/J9NLVP"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 1,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield;yield_marketable;fries_color;tuber_flavor;tuber_texture",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-10-16",
    carob_completion = 70,
    carob_effort = 2.5
  )
  
  f1 <- ff[basename(ff) == "01_Data_Adaptation_and_efficiency_trials_three_new_potato_varieties.xlsx"]
  r1 <- carobiner::read.excel(f1, na = c("", "-"))
  
  # coordinates estimated from Google Maps
  geo <- data.frame(
    location = c("Chinchao", "Chugay", "Jauja", "La Paccha", "Majes", "Quilcas", "Santa Rita", "Yanac"),
    latitude = c(-9.77, -7.78, -11.775, -6.63706, -16.3625, -11.9375, -16.49, -11.60639),
    longitude = c(-76.08, -77.87, -75.497, -78.808, -72.1911, -75.2593, -72.09, -76.40528),
    geo_from_source = FALSE
  )

  d <- data.frame(
    trial_id = paste0("J9NLVP_", r1$LOCALITY, "_", r1$YEAR),
    plot_id = as.character(r1$Plot),
    rep = as.integer(r1$Rep),
    variety = r1$Clone,
    location = gsub("Majes[12]", "Majes", r1$LOCALITY),
    country = "Peru",
    crop = "potato",
    on_farm = NA,
    is_survey = FALSE,
    irrigated = NA,
    planting_date = sub("-.*", "", r1$YEAR), 
    harvest_date = sub(".*-", "", r1$YEAR),   
    yield_part = "tubers",
    yield = r1$TTYA * 1000,
    yield_marketable = r1$MTYA * 1000,
    yield_moisture = 100 - r1$`Dry matter %`,
    yield_isfresh = TRUE,
    N_fertilizer = NA,
    P_fertilizer = NA,
    K_fertilizer = NA,
    fries_color = r1$`French Fry color scale At Harvest`,
    tuber_flavor = rowMeans(r1[, c("Baked Flavor M1_1", "Baked Flavor M1_2", "Baked Flavor M1_3")]),
    tuber_texture = rowMeans(r1[, c("Baked Texture M1_1", "Baked Texture M1_2", "Baked Texture M1_3")]),
    # New variable
    reducing_sugars_ = r1$`Reducing sugars %`,
    acrylamide_ = r1$`Acrylamides ug/Kg french fries`
  )
  
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  carobiner::write_files(path, meta, d)
}