# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Estudio de Mercado para variedades resistentes, baculovirus y feromonas en Cajamarca, Cusco, Huánuco, Junin y La Libertad.

El Estudio de Mercado para variedades de papa resistentes, baculovirus y feromonas, se realizó en los departamentos Huánuco, Junin, Cuzco, La Libertad y Cajamarca, en cada departamento se consideró a cuatro comunidades de 25 encuestados cada una, haciendo un total de 500 agricultores encuestados. Las encuestas fueron tomadas del mes de Setiembre-Noviembre del 2003.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/UISCBD"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Hogares de pequeños productores de papa. Registro individual de producción por agricultor. Cada registro corresponde a un agricultor participante en una temporada agrícola determinada e incluye variables de producción, variedades y productos para controlar las plagas entre otros.; universe: Hogares de productores de papa en los departamentos de Cajamarca, Cusco, Huánuco, Junin y La Libertad. Los 500 agricultores participantes de los distritos seleccionados de los departamentos del Perú, en el 2003.; timeMethod: De corte transversal; dataCollector: International Potato Center; samplingProcedure: El propósito fue seleccionar 5 departamentos Cajamarca, Cusco, Huánuco, Junin y La Libertad. En cada departamento se seleccionaron 4 distritos y en cada distrito se encuestaron 25 agricultores, lo que hace un total de 500 hogares.; collectionMode: Encuestadores seleccionaron 20 familias por cada Distrito, lo que hacen un total de 500 hogares. Cada encuesta contiene un total de 78 preguntas.; researchInstrument: Encuestas de preguntas estructuradas a cada hogar del agricultor.",
		
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = NA,
		
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "",
		
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variables
		# that describe management for all treatments or other observations that were not related to the aim of 
		# the trial (e.g. the presence of a disease).
		response_vars = "", 

		# notes for the end-user
		notes = "",

		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "01_Encuesta Datos - Estudio de Mercado para variedades resistentes, baculovirus y feromonas.xlsx"]
	f2 <- ff[basename(ff) == "02_Data dictionary.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		adm1 = carobiner::fix_name(r1[["Encuestador"]], "title"),
		adm2 = carobiner::fix_name(r1[["Provincia"]], "title"),
		variety = r1[["p12variedad"]],
		rep = r1[["p34_ plata repel"]]
	)
##r1: "Codigo", "Institucion del encuestador", "Fecha de entrevista", "Nombre del encuestado", "Relación Jefe de familia", "Sexo", "Departamento", "Distrito", "Comunidad", "horaS", "mintS", "segS", "horaW", "mintW", "segW", "Altitud", "Hora_Inicio", "Hora_Final", "P1cultivo1", "p1import1", "p1ventaja1", "p1areacul1", "p1cantcul1(kg)", "p1tenencia1", "p1riego1", "p1cultivo2", "p1import2", "p1ventaja2", "p1areacul2", "p1cantcul2(kg)", "p1tenencia2", "p1riego2", "p1cultivo3", "p1import3", "p1ventaja3", "p1areacul3", "p1cantcul3(kg)", "p1tenencia3", "p1riego3", "p1cultivo4", "p1import4", "p1ventaja4", "p1areacul4", "p1cantcul4(kg)", "p1tenencia4", "p1riego4", "p1cultivo5", "p1import5", "p1ventaja5", "p1areacul5", "p1cantcul5(kg)", "p1tenencia5", "p1riego5", "p1ventpastculti", "p1areapasto culti", "p1tenencia pasto culti", "p1riegopastoculti", "p1ventdescanso", "p1areadescanso", "p1tenenciadescanso", "p1riegodescanso", "p1ventbosque", "p1areabosque", "p1tenenciabosqe", "p1riegobosque", "p1venterizo", "p1areriazo", "p1tenenciaeraizo", "p1reigoeriazo", "p2terreno", "p3nombre_lugar1", "p3_ubic_1", "p3_suelo_1", "p3_distan_1", "p3nombre_lugar2", "p3_ubic_2", "p3_suelo_2", "p3_distan_2", "p3nombre_lugar3", "p3_ubic_3", "p3_suelo_3", "p3_distan_3", "p3nombre_lugar4", "p3_ubic_4", "p3_suelo_4", "p3_distan_4", "p4helada", "p4helad_cuanto", "p4helad_frec", "p4inund", "p4inund_cuanto", "p4inund_frec", "p4sequia", "p4sequia_cuanto", "p4sequia_frec", "p4graniz", "p4graniz_cuanto", "p4graniz_frec", "p5comunal", "p5_porq", "p6 alpartir", "p6_veces", "p7arreglo", "p7_otro", "p8_consumo", "p8_venta", "p8_semilla", "p8_otro", "p9vende", "p9_mercado", "p9_distrito", "p9_provincia", "p10_siembra_chica", "p10_cosecha_chica", "p10_siembra_grand", "p10_cosecha_grand", "p11_antes", "p11_dos+antes", "p11_siguiente", "p12_cuantas", "p12_porque", "p12_como", "p12_porq mezcla", "p12_porq no mezcla", "p13 almacena", "p14_consum_inicio", "p14_consum_fin", "p14_semill_inicio", "p14_semill_fin", "p14_vent_inicio", "p14_vent_fin", "p15_donde_semill", "p15_otrolugar", "p15_como _sem", "p15_luz_sem", "p15_donde_consum", "p15_como_cons", "p16_prob1", "p16_prob2", "p16_prob3", "p16_prob4", "p16_prob5", "p17_nombre1", "p17_ daño1", "p17_impor1", "p17_tec1", "p17_nombre2", "p17_ daño2", "p17_impor2", "p17_tec2", "p17_nombre3", "p17_ daño3", "p17_impor3", "p17_tec3", "p17_nombre4", "p17_ daño4", "p17_impor4", "p17_tec4", "p17_nombre5", "p17_ daño5", "p17_impor5", "p17_tec5", "p18_polilla", "p18_frec_polilla", "p18_dañ_polilla", "p18_gorgojo", "p18_frec_gorgojo", "p18_dañ_gorgojo", "p18_rancha", "p18_frec_rancha", "p18_dañ_rancha", "p18_pudri", "p18_frec_pudri", "p18_dañ_pudri", "p18_otro", "p18_cual_otro", "p18_frec_otro", "p18_dañ_otro", "p19_var1", "p19_cant_sem1", "p19_cant_cos1", "p19_prec_venta1", "p19_cant_dañ1", "p19_var2", "p19_cant_sem2", "p19_cant_cos2", "p19_prec_venta2", "p19_cant_dañ2", "p19_var3", "p19_cant_sem3", "p19_cant_cos3", "p19_prec_venta3", "p19_cant_dañ3", "p19_var4", "p19_cant_sem4", "p19_cant_cos4", "p19_prec_venta4", "p19_cant_dañ4", "p19_cant_sem_tot", "p19_cant_cos_tot", "p19_cant_dañ_tot", "p19_daño1", "p19_daño2", "p19_otro", "p20_polilla", "p20_frec_polilla", "p20_dañ_polilla", "p20_gorgojo", "p20_frec_gorgojo", "p20_dañ_gorgojo", "p20_rancha", "p20_frec_rancha", "p20_dañ_rancha", "p20_pudri", "p20_frec_pudri", "p20_dañ_pudri", "p20_otro", "p20_cual_otro", "p20_frec_otro", "p20_dañ_otro", "p21_var1", "p21_cant_alm1", "p21_cant_dañ1", "p21_plg1", "p21_otro1", "p21_var2", "p21_cant_alm2", "p21_cant_dañ2", "p21_plg2", "p21_otr2", "p21_var3", "p21_cant_alm3", "p21_cant_dañ3", "p21_plg3", "p21_otr3", "p21_var4", "p21_cant_alm4", "p21_cant_dañ4", "p21_plg4", "p21_otr4", "p21_cant_alm_tot", "p21_cant_dañ_tot", "p22_prod1_poll", "p22_ventj1_prod1_poll", "p22_ventj2_prod1_poll", "p22_desj1_prod1_poll", "p22_desvj2_prod1_poll...250", "p22_prod2_poll", "p22_ventj1_prod2_poll", "p22_ventj2_prod2_poll", "p22_desj1_prod2_poll", "p22_desvj2_prod2_poll", "p22_prod3_poll", "p22_ventj1_prod3_poll", "p22_ventj2_prod3_poll", "p22_desj1_prod3_poll", "p22_desvj2_prod3_poll", "p22_prod4_poll", "p22_ventj1_prod4_poll", "p22_ventj2_prod4_poll", "p22_desj1_prod4_poll", "p22_desvj2_prod1_poll...265", "p22_prod1_gorg", "p22_ventj1_prod1_gorg", "p22_ventj2_prod1_gorg", "p22_desj1_prod1_gorg", "p22_desvj2_prod1_gorg...270", "p22_prod2_gorg", "p22_ventj1_prod2_gorg", "p22_ventj2_prod2_gorg", "p22_desj1_prod2_gorg", "p22_desvj2_prod2_gorg", "p22_prod3_gorg", "p22_ventj1_prod3_gorg", "p22_ventj2_prod3_gorg", "p22_desj1_prod3_gorg", "p22_desvj2_prod3_gorg", "p22_prod4_gorg", "p22_ventj1_prod4_gorg", "p22_ventj2_prod4_gorg", "p22_desj1_prod4_gorg", "p22_desvj2_prod1_gorg...285", "p23_prod1", "p23_aplic_prod1", "p23_cant_prod1", "p23_unidad_prod1", "p23_prc_prod1", "p23_prod2", "p23_aplic_prod2", "p23_cant_prod2", "p23_unidad_prod2", "p23_prc_prod2", "p23_prod3", "p23_aplic_prod3", "p23_cant_prod3", "p23_unidad_prod3", "p23_prc_prod3", "p23_prod4", "p23_aplic_prod4", "p23_cant_prod4", "p23_unidad_prod4", "p23_prc_prod4", "p24_comoaplica", "p24_otro", "p24_prc_moch", "p25_prod1_poll", "p25_ventj1_prod1_poll", "p25_ventj2_prod1_poll", "p25_desj1_prod1_poll", "p25_desvj2_prod1_poll...313", "p25_prod2_poll", "p25_ventj1_prod2_poll", "p25_ventj2_prod2_poll", "p25_desj1_prod2_poll", "p25_desvj2_prod2_poll", "p25_prod3_poll", "p25_ventj1_prod3_poll", "p25_ventj2_prod3_poll", "p25_desj1_prod3_poll", "p25_desvj2_prod3_poll", "p25_prod4_poll", "p25_ventj1_prod4_poll", "p25_ventj2_prod4_poll", "p25_desj1_prod4_poll", "p25_desvj2_prod1_poll...328", "p25_prod1_gorg", "p25_ventj1_prod1_gorg", "p25_ventj2_prod1_gorg", "p25_desj1_prod1_gorg", "p25_desvj2_prod1_gorg...333", "p25_prod2_gorg", "p25_ventj1_prod2_gorg", "p25_ventj2_prod2_gorg", "p25_desj1_prod2_gorg", "p25_desvj2_prod2_gorg", "p25_prod3_gorg", "p25_ventj1_prod3_gorg", "p25_ventj2_prod3_gorg", "p25_desj1_prod3_gorg", "p25_desvj2_prod3_gorg", "p25_prod4_gorg", "p25_ventj1_prod4_gorg", "p25_ventj2_prod4_gorg", "p25_desj1_prod4_gorg", "p25_desvj2_prod1_gorg...348", "p26_especie polilla", "p27_polilla dominante", "p28 plaguicidas almacen", "p28_prod1", "p28_aplic_prod1", "p28_cant_prod1", "p28_unidad_prod1", "p28_prc_prod1", "p28_prod2", "p28_aplic_prod2", "p28_cant_prod2", "p28_unidad_prod2", "p28_prc_prod2", "p28_prod3", "p28_aplic_prod3", "p28_cant_prod3", "p28_unidad_prod3", "p28_prc_prod3", "p28_prod4", "p28_aplic_prod4", "p28_cant_prod4", "p28_unidad_prod4", "p28_prc_prod4", "p29_cant_papa_tratada", "p30_como_aplica", "p30_alq_moch", "p30_alq_otro", "p31_Daño_pestic", "p31_Daño_pestic_fam", "p31_Daño_pestic_comun", "p31_Daño_pestic_animal", "p31_Daño_pestic_ambiente", "p31_Pestic_no_daña", "p32_Prob_salud en 5 años", "p32_producto problema", "p33_roja", "p33_amarilla", "p33_azul", "p34_aporque", "p34_poque aporque", "p34_cosecha tiempo", "p34_porque_cos tiempo", "p34_cubre en campo", "p34_porque cubre", "p34_elima residuos", "p34_como_elimina", "p34_limpia almacen", "p34_porque limpia", "p34_tuberc sano", "p34_porque tuberc", "p34_porque repel", "p35_ atrae polilla", "p35_nombre subs", "p36 feromona", "p36_quien", "p37 uso trampa", "p37_como", "p38_que opina", "p39_donde consigue", "p40_desea usar", "p41_usar_ A", "p41_usar_B", "p41_usar_C", "p41_usar_D", "p41_otra razon", "p42_ pagaria", "p42_poque_paga", "p43_enferma polilla", "p43_cuales_enferma", "p44 baculovirus", "p44_ de quien", "p45_usa baculovirus", "p45_como usa", "p45_donde consigue", "p46_porque no usa", "p47_usaria", "p47_usaria_a", "p47_usaría_b", "p47_usaría_otro", "p48_pagaría bacul", "p48_porque_ paga 1", "p48_porque_paga2", "p49_cuanto gastaria", "p50_varied_res_polilla", "p50_cual_var...434", "p50_var_res_gorg", "p50_cual_var...436", "p51_var1", "p51_ventj1_var1", "p51_ventj2_var1", "p51_ventj3_var1", "p51_ventj4_var1", "p51_dentj1_var1", "p51_dentj2_var1", "p51_dentj3_var1", "p51_dentj4_var1", "p51_var2", "p51_ventj1_var2", "p51_ventj2_var2", "p51_ventj3_var2", "p51_ventj4_var2", "p51_dentj1_var2", "p51_dentj2_var2", "p51_dentj3_var2", "p51_dentj4_var2", "p51_var3", "p51_ventj1_var3", "p51_ventj2_var3", "p51_ventj3_var3", "p51_ventj4_var3", "p51_dentj1_var3", "p51_dentj2_var3", "p51_dentj3_var3", "p51_dentj4_var3", "p51_var4", "p51_ventj1_var4", "p51_ventj2_var4", "p51_ventj3_var4", "p51_ventj4_var4", "p51_dentj1_var4", "p51_dentj2_var4", "p51_dentj3_var4", "p51_dentj4_var4", "p51_var5", "p51_ventj1_var5", "p51_ventj2_var5", "p51_ventj3_var5", "p51_ventj4_var5", "p51_dentj1_var5", "p51_dentj2_var5", "p51_dentj3_var5", "p51_dentj4_var5", "p52_var1", "p52_don_var1", "p52_como_var1", "p52_var2", "p52_don_var2", "p52_como_var2", "p52_var3", "p52_don_var3", "p52_como_var3", "p52_var4", "p52_don_var4", "p52_como_var4", "p52_var5", "p52_don_var5", "p52_como_var5", "p53_var_mercado", "p53_var1", "p53_var2", "p53_var3", "p53_var4", "p53_var5", "p54_var1_pasado", "p54_raz1_var1", "p54_raz2_var1", "p54_raz3_var1", "p54_raz4_var1", "p54_raz5_var1", "p54_var2_pasado", "p54_raz1_var2", "p54_raz2_var2", "p54_raz3_var2", "p54_raz4_var2", "p54_raz5_var2", "p54_var3_pasado", "p54_raz1_var3", "p54_raz2_var3", "p54_raz3_var3", "p54_raz4_var3", "p54_raz5_var3", "p54_var4_pasado", "p54_raz1_var4", "p54_raz2_var4", "p54_raz3_var4", "p54_raz4_var4", "p54_raz5_var4", "p54_var5_pasado", "p54_raz1_var5", "p54_raz2_var5", "p54_raz3_var5", "p54_raz4_var5", "p54_raz5_var5", "p55_varieda_futuro", "p55_var1", "p55_raz1_var1", "p55_raz2_var1", "p55_raz3_var1", "p55_raz4_var1", "p55_raz5_var1", "p55_var2_futuro", "p55_raz1_var2", "p55_raz2_var2", "p55_raz3_var2", "p55_raz4_var2", "p55_raz5_var2", "p55_var3_futuro", "p55_raz1_var3", "p55_raz2_var3", "p55_raz3_var3", "p55_raz4_var3", "p55_raz5_var3", "p55_var4_futuro", "p55_raz1_var4", "p55_raz2_var4", "p55_raz3_var4", "p55_raz4_var4", "p55_raz5_var4", "p55_var5_futuro", "p55_raz1_var5", "p55_raz2_var5", "p55_raz3_var5", "p55_raz4_var5", "p55_raz5_var5", "p56_rend", "p56_color", "p56_tamaño", "p56_calidad", "p56_adapt", "p56_ranch", "p56_insc", "p56_mcdo", "p56_precoc", "p56_otro", "p56_cual_otro", "p57_vent_desv1", "p57_vent_desv2", "p57_vent_desv3", "p58_semilla_a", "p58_semilla_b", "p58_semilla_c", "p58_semilla_d", "p58_semilla_e", "p58_semilla_f", "p58_semilla_g", "p58_semilla_h", "p58_semilla_I", "p58_semilla_j", "p58_semilla_k", "p58_semilla_l", "p58_semilla_otro", "p59 gusta_var_poll", "p59_pq_gustapoll 1", "p59_pq_gustapoll 2", "p60_gusta_prob_poll", "p60_como_poll", "p61_gusta_var_gg", "p61_pq_gusta 1", "p61_pq_gusta 2", "p62_gusta_prob_gg", "p62_como_gg", "p63_rend", "p63_colr", "p63_tamñ", "p63_calid", "p63_adapt", "p63_ranch", "p63_insec", "p63_otro", "p63_cual_otra", "p64_gusta a", "p64_xqa", "p64_gusta b", "p64_xqb", "p65_paga mas", "p65_xq", "p66_comp_sem", "p66_xq", "p67_cuanto_paga", "p68_cuanto_mas paga", "p69_no mezcla", "p69_si mescla", "p70_var_resiste_a", "p70_var_resiste_b", "p71_siembra_a", "p71_siembra_b", "p71_siembra_c", "p71_siembra_d", "p71_siembra_e", "p71_siembra_f", "p71_siembra_g", "p71_simbra_h", "p71_cualidades", "p71_defec", "p72 tecnicos", "p72_inst1", "p72_año_inst1", "p72_tipo_inst1", "p72_inst2", "p72_año_inst2", "p72_tipo_inst2", "p72_inst3", "p72_año_inst3", "p72_tipo_inst3", "p72_inst4", "p72_año_inst4", "p72_tipo_inst4", "p72_inst5", "p72_año_inst5", "p72_tipo_inst5", "p73_nom", "p73_instr", "p73_edad", "p73_hijos", "p74_agri", "p74_gan", "p74_art", "p74_com", "p74_obr", "p74_empl", "p74_otro", "p75_tot", "p75_hom", "p75_ins_hom", "p75_muj", "p75_ins)_muj", "p76_migra", "p76_quien", "p76_veces", "p76_tiempo", "p77_gan1", "p77_xq_gan1", "p77_cab_gan1", "p77_gan2", "p77_xq_gan2", "p77_cab_gan2", "p77_gan3", "p77_xq_gan3", "p77_cab_gan3", "p77_gan4", "p77_xq_gan4", "p77_cab_gan4", "p77_gan5", "p77_xq_gan5", "p77_cab_gan5", "p78_confia_a", "p78_confia b", "p78_confia c", "p78_confia d", "p78_confia e", "p78_cojnfia f", "p78_confia g", "p78_confia h", "p78_confia I", "p78_confia j", "p78_confia k", "p78_confia l", "P78_que Inst"


	d2 <- data.frame()
##r2: "Variable name", "Descripcion", "Valor_Variables", "Observations"


## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor( ____ )))
	
## about the data (TRUE/FALSE)
	d$on_farm <- 
	d$is_survey <- 
	d$irrigated <-
	
## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
    d$crop_rotation <- "crop1;crop2;crop3_crop4"
	
## each site must have corresponding longitude and latitude
## if the raw data do not provide them you can estimate them from the location/adm data 
## see carobiner::geocode
	d$longitude <- 
	d$latitude <- 
# are the coordinates from the source (data/publication) or estimated by you?	
	d$geo_from_source <- TRUE/FALSE


## time can be year ("2023", four characters), year-month ("2023-07", 7 characters) or date ("2023-07-21", 10 characters).
## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
   d$S_fertilizer <- 
   d$lime <- 
## normalize names 
   d$fertlizer_type <- 

## for legumes   
   d$inoculated <- TRUE or FALSE
   d$inoculant <- "name of inoculant"
   
### in general, add comments to your script if computations are
### based on information gleaned from metadata, a publication, 
### or when they are not immediately obvious for other reasons

### Yield

	yield <- r$yield_tonha * 1000
	#what plant part does yield refer to?
	d$yield_part <- "tubers"
	d$yield_moisture <- r$moisture * 100

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
# Also record fresh and/or dry weight production of other organs (or "residue" or "total")
# if the data allow for that 

	d$fwy_storage <- r$yield_tonha * 1000
	d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
	d$dmy_totat <- r$dry_biomass
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

