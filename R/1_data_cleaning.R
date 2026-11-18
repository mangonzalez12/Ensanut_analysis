library("stats") 
library("tidyverse")
library("psych")
library("dplyr")
library("VIM") #imputation     


#Read dataset 
data <- read.csv("../data/data_13nov.csv", header = TRUE, skip = 1) #previous data data_dep.csv(n=1165)
data <- data[1:1163, ] #Remove empty rows
rownames(data) = data$Folio.del.participante #Set rownames with Folio del participante
data =data
#Print dimensions
print("Dataset has these # of patients")
dim(data)[1]
print("Dataset has these # of variables")
dim(data)[2]


# Apply to all character columns that should be numeric
data[] <- lapply(data, function(col) {
  if (is.character(col)) {
    fixed <- gsub(",", ".", col)
    # Only convert if it actually looks numeric
    num <- suppressWarnings(as.numeric(fixed))
    if (sum(is.na(num)) <= sum(is.na(col))) num else col
  } else {
    col
  }
})


#Set variable to numeric
data$IMC.Prom.mmHg <- as.numeric(gsub(",", ".", data$IMC.Prom.mmHg))# IMC.Prom.mmHg
data$GLUCOSA.mg.dL <- as.numeric(gsub(",", ".", data$GLUCOSA.mg.dL))# GLUCOSA.mg.dL
data$HDL.mg.dL <- as.numeric(gsub(",", ".", data$HDL.mg.dL)) #HDL.mg.dL
data$LDL.mg.dL <- as.numeric(gsub(",", ".", data$LDL.mg.dL)) #LDL.mg.dL
data$COLESTEROL.TOTAL.mg.dL <- as.numeric(gsub(",", ".", data$COLESTEROL.TOTAL.mg.dL)) #COLESTEROL.TOTAL.mg.dL
data$TRIGLICÉRIDOS.mg.dL <- as.numeric(gsub(",", ".", data$TRIGLICÉRIDOS.mg.dL)) #TRIGLICÉRIDOS.mg.dL
#data$Categoría.edad <- class(data$Categoría.edad)

#Cambio de nombre de variable de interes (Grupos de dx a Diagnóstico)
data <- data %>%
  rename(Diagnosis = Grupos.de.dx,
          resistencia.a.insulina..RI..con.HOMA.IR..0.sin.RI..1.conRI = resistencia.a.insulina..RI..con.HOMA.IR..1.sin.RI..2.conRI)


#Extract variables to use
###"""
#Variables antropométricas y presión arterial
#●	IMC 
#●	Circunferencia de cintura 
#●	Presión Arterial Sistólica
#●	PA Diastólica

#Antecedentes personales 
#●	Diagnóstico previo de diabetes (columna J)
#●	Diagnóstico previo de prediabetes (columna I)
#●	Diagnóstico previo de hipertensión (columna AF) 
#●	Diagnóstico previo de enfermedad renal crónica (columna AZ)

#Bioquímicos
#●	Glucosa mg/dl
#●	Insulina HOMA-IR
#●	HbA1C %
#●	Ácido úrico mg/dl
#●	Creatinina mg/dl

#Perfil de lípidos 
#●	HDL mg/dl
#●	LDL mg/dl
#●	Colesterol total mg/dl
#●	Triglicéridos mg/dl

#Diagnósticos 
#●	HOMA-IR (columna EQ) (usar como cuantitativa)
#●	DIABETES (columna EM)
#●	PREDIABETES (columna EO)
#●	Resistencia a insulina (columna ER) (dicotómica usando el punto de corte descrito arriba) 
#●	Índice TG/HDL (revisar punto de corte de riesgo descrito arriba) (columna ES)

#Variables de la dieta
#●	Kcal/día (columna EB)
#●	Carbohidratos g/día (columna EC)
#●	Lípidos g/día (columna ED)
#●	Proteínas g/día (columna EE)
#●	dAGEs KU/día (columna DV)
#●	dAGEs/kcal/día (columna EF)
#Variables de la dieta por semana
#●  Kcal/semana
#●  Carbohidratos g/semana
#●  Lípidos g/semana
#●  Proteínas g/semana
#●  dAGEs KU/semana
#●	porciones NOVA 1 (columna EI)
#●	porciones NOVA 2 (columna EJ)
#●	porciones NOVA 3 (columna EK)
#●	porciones NOVA 4 (columna EL)

#Variables confusoras
#Edad (columna D)
#Sexo (columna E)
#Número de cigarrillos por día (columna BM)
#Consumo de alcohol (columna BR)
#Horas de sueño (columna CA)
#Estrato (columna F)
#Ingreso por hogar (columna H) 

###"""

selected_vars <- c("IMC.Prom.mmHg", #IMC
                   "CC.Prom..cm.",  #Circunferencia de cintura
                   "TAS.Prom.mmHg", #Tensión Arterial Sistólica
                   "TAD.Prom.mmHg", #Tensión Arterial Diastólica
                   
                   #Antecedentes personales
                   "Diabetes..1.sí.2.no.",                               #Diagnóstico previo de diabetes (columna J)
                   "Prediabetes..1.sí.2.no.9.no.sabe.sr.sin.respuesta.", #Diagnóstico previo de prediabetes (columna I)
                   "HTA..1.sí..2.gestacional..3.no.",                    #Diagnóstico previo de hipertensión (columna AF) 
                   "insuf.renal.o.ERC..1.sí..2.no.",                     #Diagnóstico previo de enfermedad renal crónica (columna AZ)
                   
                   #Bioqúimicos
                   "GLUCOSA.mg.dL",       #Glucosa mg/dl
                   "INSULINA.µUI.ML",     #Insulina.  PENDIENTE
                   "HOMA.IR",             #HOMA-IR
                   "HbA1C..",             #HbA1C %
                   "ÁCIDO.ÚRICO.mg.dL",  #Ácido úrico. 
                   "CREATININA.mg.dL",    #Creatinina
                   
                   #Perfil de lípidos
                   "HDL.mg.dL",               #HDL mg/dl
                   "LDL.mg.dL",               #LDL mg/dl
                   "COLESTEROL.TOTAL.mg.dL",  #Colesterol total mg/dl
                   "TRIGLICÉRIDOS.mg.dL",     #Triglicéridos mg/dl
                   
                   #Diagnósticos
                   #"HOMA.IR",                                                    #HOMA-IR
                   "DIABETES..0.sin.diabetes..1.con.diabetes",                   #DIABETES (columna EM)
                   "PREDIABETES..0.sin.prediabetes..1.con..prediabetes..todos.los.que.tienen.dx.previo.de.prediabetes...los.nuevos.diagnosticados.con.HbA1C.entre.5.7.6.4..",                                          #PREDIABETES (columna EO)
                   "resistencia.a.insulina..RI..con.HOMA.IR..0.sin.RI..1.conRI",       #Resistencia a insulina (columna ER)
                   "índice.Triglicéridos.HDL",                                         #Índice TG/HDL
                   "reciente.diagnóstico.de.diabetes.con.HbA1C",                       #Reciente diagnóstico de diabetes con HbA1C (columna EN)
                   "reciente.diagnóstico.de.prediabetes.con.HbA1C..ver.dx.variables.", #Reciente diagnóstico de prediabetes con HbA1C (columna EP)
                   
                   
                  #Variables de la dieta por día
                  "Kcal.día",              #Kcal/día (columna EB)
                  "Carbohidratos..g..día", #Carbohidratos g/día (columna EC)
                  "Lípidos..g..día",       #Lípidos g/día (columna ED)
                  "Proteínas..g..día",     #Proteínas g/día (columna EE)
                  "dAGES.Día",             #dAGEs KU/día (columna DV)  
                  "dAGEs.Kcal.día",        #dAGEs/kcal/día (columna EF)
                  #Variables de la dieta por semana
                  "Kcal.SEM",              #Kcal/semana
                  "Carbohidratos..g..sem", #Carbohidratos g/semana
                  "Lípidos..g..sem",       #Lípidos g/semana     
                  "Proteínas..g..sem",     #Proteínas g/semana 
                  "dAGEs.Kcal.sem",        #dAGEs KU/semana
                  "dAGEs.totales",         #dAGEs totales
                  
                  #Variables NOVA
                  "NOVA.1",                #NOVA 1
                  "NOVA.2",                #NOVA 2
                  "NOVA.3",                #NOVA 3
                  "NOVA.4",                #NOVA 4
                  
                  #Variables NOVA %
                  "X..de.kcal.NOVA.1",     #NOVA 1 %
                  "X..de.kcal.NOVA.2",     #NOVA 2 %
                  "X..de.kcal.NOVA.3",     #NOVA 3 %
                  "X..de.kcal.NOVA.4",     #NOVA 4 %
                  
                  #Variables confusoras
                  "Edad..años.",           #Edad (columna E)
                  "sexo.1.H.2.M",          #Sexo (columna F)
                  "num.cigarros.día",      #Número de cigarrillos por día (columna BM)
                  "consumo.alcohol..1.diario..2.semanal..3.mensual..4.anual..5.no.consumo.en.el.últ.año..6.nunca..9.no.responde.",                                               #Consumo de alcohol (columna BR)
                  "horas.sueño.en.1.día..ver.categorías.en.cuestionario",#Horas de sueño (columna CA)
                  "Estrato.1.rural..2.urbano..3.metropolitano",          #Estrato (columna F)
                  "Ingresos.x.hogar",                                    #Ingreso por hogar (columna H)
                  "Diagnosis",                                         #Diagnóstico de No syndrome, Metabolic Syndrome y diabetes
                  "Región.del.país.1.norte..2.centro..3.sur",            #Regiones del pais
                  "Terciles.índice.bienestar",                           #Terciles nivel socioeconomico
                  "Quintiles.índice.bienestar",                           #Quintiles nivel socioecononomico
                  "Nivel.de.actividad.física.según.los.MET.1.AF.BAJA..2.AF.MODERADA..3.AF.ALTA", #Nivel de AF categorica
                  "Categoría.edad",                                        #Quintiles edad
                  "GRUPOS.dAGEs..1.bajo.consumo..2.moderado.consumo..3.elevado.consumo.", #Niveles AGEs
                  
                  #Variables de tratamiento de fármacos
                  "hiperTG.fármacos",                                           #Fármacos hipolipemiantes
                  "Tx.diabetes..1.insulina..2.pastillas..3.ambas..4.ninguno.",  #Fármacos hiperglucemia
                  "tx.HTA..1.sí..2.no."                                         #Fármacos hipertensión
                  )


#Definir variables categoricas como character
char_vars <- c(#Antecedentes personales
                   "Diabetes..1.sí.2.no.",                               #Diagnóstico previo de diabetes (columna J)
                   "Prediabetes..1.sí.2.no.9.no.sabe.sr.sin.respuesta.", #Diagnóstico previo de prediabetes (columna I)
                   "HTA..1.sí..2.gestacional..3.no.",                    #Diagnóstico previo de hipertensión (columna AF) 
                   "insuf.renal.o.ERC..1.sí..2.no.",                     #Diagnóstico previo de enfermedad renal crónica (columna AZ)
   
                   #Diagnósticos
                   "DIABETES..0.sin.diabetes..1.con.diabetes",                   #DIABETES (columna EM)
                   "PREDIABETES..0.sin.prediabetes..1.con..prediabetes..todos.los.que.tienen.dx.previo.de.prediabetes...los.nuevos.diagnosticados.con.HbA1C.entre.5.7.6.4..",                                          #PREDIABETES (columna EO)
                   "resistencia.a.insulina..RI..con.HOMA.IR..0.sin.RI..1.conRI",       #Resistencia a insulina (columna ER)
                   "reciente.diagnóstico.de.diabetes.con.HbA1C",                       #Reciente diagnóstico de diabetes con HbA1C (columna EN)
                   "reciente.diagnóstico.de.prediabetes.con.HbA1C..ver.dx.variables.", #Reciente diagnóstico de prediabetes con HbA1C (columna EP)
                   
                   
                  #Variables confusoras
                  #"Edad..años.",           #Edad (columna D)
                  "sexo.1.H.2.M",          #Sexo (columna E)
                  "consumo.alcohol..1.diario..2.semanal..3.mensual..4.anual..5.no.consumo.en.el.últ.año..6.nunca..9.no.responde.",                                               #Consumo de alcohol (columna BR)
                  "horas.sueño.en.1.día..ver.categorías.en.cuestionario",#Horas de sueño (columna CA)
                  "Estrato.1.rural..2.urbano..3.metropolitano",          #Estrato (columna F)
                  "Ingresos.x.hogar",                                    #Ingreso por hogar (columna H)
                  "Diagnosis",                                         #Diagnóstico de No syndrome, Metabolic Syndrome y diabetes
                  "Región.del.país.1.norte..2.centro..3.sur",            #Regiones del pais
                  "Terciles.índice.bienestar",                           #Terciles nivel socioeconomico
                  "Quintiles.índice.bienestar",                           #Quintiles nivel socioeconomico
                  "Nivel.de.actividad.física.según.los.MET.1.AF.BAJA..2.AF.MODERADA..3.AF.ALTA", #Nivel de AF categorica
                  "Categoría.edad",
                  "GRUPOS.dAGEs..1.bajo.consumo..2.moderado.consumo..3.elevado.consumo.", #Niveles AGEs
                                    
                  #Variables de tratamiento de fármacos
                  "hiperTG.fármacos",                                           #Fármacos hipolipemiantes
                  "Tx.diabetes..1.insulina..2.pastillas..3.ambas..4.ninguno.",  #Fármacos hiperglucemia
                  "tx.HTA..1.sí..2.no."                                         #Fármacos hipertensión
                  )


# Shows which selected_vars don't exist in data
selected_vars[!selected_vars %in% colnames(data)]

#Filter dataset with selected variables
df <- data[,selected_vars]
df

# Convert to character (categorical variables)
df[char_vars] <- lapply(df[char_vars], as.character)


#Check dimensions of dataframe (rows and columns) #Rows = observations/cases 
dim(df)                                           #Columns = #Variables
#Covert back to data
data = df


#########################################################PRIMER FILTRO

# Define normal ranges for each variable (Definición de rangos normales para cada variable)
#normal_ranges <- list(
  # Anthropometric
  #IMC.Prom.mmHg           = c(10, 80),       # BMI kg/m²
  #CC.Prom..cm.            = c(30, 200),      # Waist circumference cm
  #TAS.Prom.mmHg           = c(50, 300),      # Systolic BP mmHg
  #TAD.Prom.mmHg           = c(30, 200),      # Diastolic BP mmHg

  # Biochemical
  #GLUCOSA.mg.dL      = c(40, 600),     # mg/dL
  #INSULINA.µUI.ML    = c(1, 300),      # µUI/mL
  #HOMA.IR            = c(0.1, 20),     # HOMA-IR
  #HbA1C..            = c(3, 18),       # %
  #ÁCIDO.ÚRICO.mg.dL  = c(1, 15),       # mg/dL
  #CREATININA.mg.dL   = c(0.2, 15),     # mg/dL
  
  # Lipid profile
  #HDL.mg.dL              = c(10, 120),    # mg/dL
  #LDL.mg.dL              = c(30, 300),   # mg/dL
  #COLESTEROL.TOTAL.mg.dL = c(80, 400),   # mg/dL
  #TRIGLICÉRIDOS.mg.dL    = c(30, 1000),  # mg/dL
  
  # Indice de triglicéridos
  #índice.Triglicéridos.HDL = c(0.2, 20),
  
  # Dieta variables por día
  #Kcal.día              = c(500, 8000),   # kcal/dia
  #Carbohidratos..g..día = c(50, 1000),    # g/dia
  #Lípidos..g..día       = c(10, 300),     # g/dia
  #Proteínas..g..día     = c(20, 300),     # g/dia
  #dAGES.Día             = c(1000, 50000), # KU/dia
  #dAGEs.Kcal.día        = c(2, 6),        # KU/kcal/dia
  #Variables de la dieta por semana
  #Kcal.SEM              = c(3500, 56000), # Kcal/semana
  #Carbohidratos..g..sem = c(350, 7000),   # Carbohidratos g/semana
  #Lípidos..g..sem       = c(70, 2100),    # Lípidos g/semana     
  #Proteínas..g..sem     = c(140, 2100),   # Proteínas g/semana 
  #dAGEs.Kcal.sem        = c(14, 36)       # dAGEs KU/semana
  
  #Variables NOVA sin filtro 1
  #NOVA.1                = c(0, 70),       # % or servings
  #NOVA.2                = c(0, 50),
  #NOVA.3                = c(0, 35),
  #NOVA.4                = c(0, 49)
#)

# Apply ranges: values outside [min, max] become NA
#for (var in names(normal_ranges)) { # Iterando para cada variable (variable) presente en NORMAL RANGES
#  if (var %in% names(df)) {         # SI, la variable (columna) esta presente en la matriz df       
#    lo <- normal_ranges[[var]][1] # Valor minimo del rango
#    hi <- normal_ranges[[var]][2] # Valor máximo del rango
#    df[[var]][df[[var]] < lo | df[[var]] > hi] <- NA
#  }
#}

#Guardar archivo modificado con filtro 1
#write.csv(df, "../Output/data_1er_filtro.csv")


#Functions to define variables

#  ""reciente.diagnóstico.de.diabetes.con.HbA1C""

data <- data %>%
  mutate(reciente.diagnóstico.de.diabetes.con.HbA1C = case_when(
    HbA1C.. >= 6.5 ~ "Diabetes",
    TRUE ~ "No_diabetes"
  ))


#.  ""DIABETES..0.sin.diabetes..1.con.diabetes""


data <- data %>%
  mutate(DIABETES..0.sin.diabetes..1.con.diabetes = case_when(
    Diabetes..1.sí.2.no. == 1 ~ 1,
    reciente.diagnóstico.de.diabetes.con.HbA1C == "Diabetes" ~ 1,
    TRUE ~ 0
  ))

#  ""reciente.diagnóstico.de.prediabetes.con.HbA1C..ver.dx.variables.""

data <- data %>%
  mutate(reciente.diagnóstico.de.prediabetes.con.HbA1C..ver.dx.variables. = case_when(
    HbA1C.. >= 5.7 & HbA1C.. < 6.4 ~ "Pre_Diabetes",
    HbA1C.. >= 6.5 ~ "Diabetes",
    TRUE ~ "No_Diabetes"
  )) 


#. ""PREDIABETES..0.sin.prediabetes..1.con..prediabetes..todos.los.que.tienen.dx.previo.de.prediabetes...los.nuevos.diagnosticados.con.HbA1C.entre.5.7.6.4..""

data <- data %>%
  mutate(PREDIABETES..0.sin.prediabetes..1.con..prediabetes..todos.los.que.tienen.dx.previo.de.prediabetes...los.nuevos.diagnosticados.con.HbA1C.entre.5.7.6.4.. = case_when(
    Prediabetes..1.sí.2.no.9.no.sabe.sr.sin.respuesta. == 1 ~ 1,
    reciente.diagnóstico.de.prediabetes.con.HbA1C..ver.dx.variables. == "Pre_Diabetes" ~ 1,
    TRUE ~ 0
  ))



#  ""Resistencia a la Insulina HOMA-IR >= 2.4""

#. resistencia.a.insulina..RI..con.HOMA.IR..0.sin.RI..1.conRI. 

data <- data %>%
  mutate(resistencia.a.insulina..RI..con.HOMA.IR..0.sin.RI..1.conRI. = case_when(
    HOMA.IR >= 2.4 ~ 1, #Resistencia a la insulina =1
    TRUE ~ 0 # No resistencia a la insulina =0
  ))

# Índice Triglicéridos/HDL
data <- data %>%
  mutate(índice.Triglicéridos.HDL = TRIGLICÉRIDOS.mg.dL / HDL.mg.dL)


# Diagnostico (Normal,Metabolic Syndrome, Pre diabetes, Diabetes definition)
data <- data %>%
  mutate(
    comp_cc = case_when(
      sexo.1.H.2.M == 1 & CC.Prom..cm. >= 102 ~ 1, #Hombre y CC mayor o igual a 102
      sexo.1.H.2.M == 2 & CC.Prom..cm. >= 88  ~ 1, #Mujer y CC mayor o igual a 88
      TRUE ~ 0
    ),
    comp_trig = case_when(
      TRIGLICÉRIDOS.mg.dL >= 150 ~ 1, #Trigliceridos mayor o igual a 150
      hiperTG.fármacos == 1  ~ 1,     #Fármacos hiperlipemiantes
      TRUE ~ 0
    ),
    comp_hdl = case_when(
      sexo.1.H.2.M == 1 & HDL.mg.dL < 40 ~ 1, #Hombre y HDL menor a 40
      sexo.1.H.2.M == 2 & HDL.mg.dL < 50 ~ 1, #Mujer y HDL menor a 50
      TRUE ~ 0
    ),
    comp_pa = case_when(
      TAS.Prom.mmHg >= 130 ~ 1,      #Tensión Sistólica mayor o igual a 130
      TAD.Prom.mmHg >= 85  ~ 1,      #Tensión Diastólica mayor o igual a 85
      tx.HTA..1.sí..2.no. == 1 ~ 1,  #Fármacos hipertensión
      TRUE ~ 0
    ),
    
    comp_glu = case_when(
      DIABETES..0.sin.diabetes..1.con.diabetes == 1 ~ 0,
      GLUCOSA.mg.dL >= 100 ~ 1,
      HbA1C.. < 6.5 ~ 1,
      Tx.diabetes..1.insulina..2.pastillas..3.ambas..4.ninguno. == 2 ~ 1,
      TRUE ~ 0
    ),
    
    n_componentes = rowSums(
      cbind(comp_cc, comp_trig, comp_hdl, comp_pa, comp_glu), #Sumatoria de componentes del Sindrome Metabólico
      na.rm = TRUE
    ),

    Diagnosis = case_when(
      Diabetes..1.sí.2.no. == 1 ~ "Diabetes_questionnaire",           # Personas con diabetes
      reciente.diagnóstico.de.diabetes.con.HbA1C == "Diabetes" ~ "Diabetes_HB1AC",        # Personas con diabetes
      n_componentes >= 3                            ~ "Metabolic_Syndromehigh",           # Personas con Sindrome metabólico alto
      TRUE                                          ~ "Metabolic_Syndromelow"              # Personas Sindrome metabólico bajo (normales)
    ),
    Diagnosis2 = case_when(
      Diabetes..1.sí.2.no. == 1 ~ "Diabetes_questionnaire",
      reciente.diagnóstico.de.diabetes.con.HbA1C == "Diabetes" ~ "Diabetes_HB1AC", # Personas con diabetes HB1AC
      TRUE                                          ~ "No_diabetes"                # Personas sin diabetes metabólico (normales)
  ))



#Borrar variables generadas de diagnóstico
del_vars <- c("comp_cc", "comp_trig", "comp_hdl", "comp_pa", "comp_glu", "n_componentes")
data <- data %>% select(-all_of(del_vars))


#Rename variables
data <- data %>%
  rename(
    IMC                  = IMC.Prom.mmHg,
    CC                   = CC.Prom..cm.,
    TAS                  = TAS.Prom.mmHg,
    TAD                  = TAD.Prom.mmHg,
    Diabetes_dx          = Diabetes..1.sí.2.no.,
    Prediabetes_dx       = Prediabetes..1.sí.2.no.9.no.sabe.sr.sin.respuesta.,
    HTA                  = HTA..1.sí..2.gestacional..3.no.,
    Insuf_renal          = insuf.renal.o.ERC..1.sí..2.no.,
    Glucosa              = GLUCOSA.mg.dL,
    Insulina             = INSULINA.µUI.ML,
    HOMA_IR              = HOMA.IR,
    HbA1c                = HbA1C..,
    Acido_urico          = ÁCIDO.ÚRICO.mg.dL,
    Creatinina           = CREATININA.mg.dL,
    HDL                  = HDL.mg.dL,
    LDL                  = LDL.mg.dL,
    Colesterol_total     = COLESTEROL.TOTAL.mg.dL,
    Trigliceridos        = TRIGLICÉRIDOS.mg.dL,
    DIABETES             = DIABETES..0.sin.diabetes..1.con.diabetes,
    PREDIABETES          = PREDIABETES..0.sin.prediabetes..1.con..prediabetes..todos.los.que.tienen.dx.previo.de.prediabetes...los.nuevos.diagnosticados.con.HbA1C.entre.5.7.6.4..,
    Resist_insulina      = resistencia.a.insulina..RI..con.HOMA.IR..0.sin.RI..1.conRI,
    Indice_TG_HDL        = índice.Triglicéridos.HDL,
    Dx_diabetes_HbA1c    = reciente.diagnóstico.de.diabetes.con.HbA1C,
    Dx_prediabetes_HbA1c = reciente.diagnóstico.de.prediabetes.con.HbA1C..ver.dx.variables.,
    Kcal_dia             = Kcal.día,
    Carbohidratos_dia    = Carbohidratos..g..día,
    Lipidos_dia          = Lípidos..g..día,
    Proteinas_dia        = Proteínas..g..día,
    dAGEs_dia            = dAGES.Día,
    dAGEs_Kcal_dia       = dAGEs.Kcal.día,
    dAGES_totales        = dAGEs.totales,         #dAGEs totales
    Kcal_sem             = Kcal.SEM,
    Carbohidratos_sem    = Carbohidratos..g..sem,
    Lipidos_sem          = Lípidos..g..sem,
    Proteinas_sem        = Proteínas..g..sem,
    dAGEs_Kcal_sem       = dAGEs.Kcal.sem,
    NOVA1                = NOVA.1,
    NOVA2                = NOVA.2,
    NOVA3                = NOVA.3,
    NOVA4                = NOVA.4,
    NOVA1_per             = X..de.kcal.NOVA.1,
    NOVA2_per            = X..de.kcal.NOVA.2,
    NOVA3_per            = X..de.kcal.NOVA.3,
    NOVA4_per            = X..de.kcal.NOVA.4,
    Edad                 = Edad..años.,
    Sexo                 = sexo.1.H.2.M,
    Cigarros_dia         = num.cigarros.día,
    Alcohol              = consumo.alcohol..1.diario..2.semanal..3.mensual..4.anual..5.no.consumo.en.el.últ.año..6.nunca..9.no.responde.,
    Horas_sueno          = horas.sueño.en.1.día..ver.categorías.en.cuestionario,
    Estrato              = Estrato.1.rural..2.urbano..3.metropolitano,
    Ingresos             = Ingresos.x.hogar,
    Region               = Región.del.país.1.norte..2.centro..3.sur,
    Tertiles             = Terciles.índice.bienestar,
    Quintiles            = Quintiles.índice.bienestar,
    Resistencia_0_1      = resistencia.a.insulina..RI..con.HOMA.IR..0.sin.RI..1.conRI.,
    Actividad_física     = Nivel.de.actividad.física.según.los.MET.1.AF.BAJA..2.AF.MODERADA..3.AF.ALTA,
    Edad_cat             = Categoría.edad,
    AGES_grupos          = GRUPOS.dAGEs..1.bajo.consumo..2.moderado.consumo..3.elevado.consumo.,
    TG_tx                = hiperTG.fármacos,                                           
    Gluc_tx              = Tx.diabetes..1.insulina..2.pastillas..3.ambas..4.ninguno.,  
    HTA_tx               = tx.HTA..1.sí..2.no.                                         
  )


# Rename variables (English)
data <- data %>%
  rename(
    BMI                    = IMC,
    WaistCirc              = CC,
    SBP                    = TAS,
    DBP                    = TAD,
    Diabetes_dx            = Diabetes_dx,
    Prediabetes_dx         = Prediabetes_dx,
    Hypertension           = HTA,
    RenalInsuff            = Insuf_renal,
    Glucose                = Glucosa,
    Insulin                = Insulina,
    HOMA_IR                = HOMA_IR,
    HbA1c                  = HbA1c,
    UricAcid               = Acido_urico,
    Creatinine             = Creatinina,
    HDL                    = HDL,
    LDL                    = LDL,
    TotalCholesterol       = Colesterol_total,
    Triglycerides          = Trigliceridos,
    DIABETES               = DIABETES,
    PREDIABETES            = PREDIABETES,
    InsulinResist          = Resist_insulina,
    TG_HDL_Index           = Indice_TG_HDL,
    Dx_Diabetes_HbA1c      = Dx_diabetes_HbA1c,
    Dx_Prediabetes_HbA1c   = Dx_prediabetes_HbA1c,
    Kcal_day               = Kcal_dia,
    Carbs_day              = Carbohidratos_dia,
    Lipids_day             = Lipidos_dia,
    Protein_day            = Proteinas_dia,
    dAGEs_day              = dAGEs_dia,
    dAGEs_Kcal_day         = dAGEs_Kcal_dia,
    dAGES_total            = dAGES_totales,
    Kcal_week              = Kcal_sem,
    Carbs_week             = Carbohidratos_sem,
    Lipids_week            = Lipidos_sem,
    Protein_week           = Proteinas_sem,
    dAGEs_Kcal_week        = dAGEs_Kcal_sem,
    NOVA1                  = NOVA1,
    NOVA2                  = NOVA2,
    NOVA3                  = NOVA3,
    NOVA4                  = NOVA4,
    NOVA1_pct              = NOVA1_per,
    NOVA2_pct              = NOVA2_per,
    NOVA3_pct              = NOVA3_per,
    NOVA4_pct              = NOVA4_per,
    Age                    = Edad,
    Sex                    = Sexo,
    Cigarettes_day         = Cigarros_dia,
    Alcohol                = Alcohol,
    Sleep_hours            = Horas_sueno,
    Stratum                = Estrato,
    HouseholdIncome        = Ingresos,
    Region                 = Region,
    Tertiles               = Tertiles,
    Quintiles              = Quintiles,
    InsulinResist_bin      = Resistencia_0_1,
    PhysicalActivity       = Actividad_física,
    Age_catg               = Edad_cat,
    AGES                   = AGES_grupos,
    TG_tx                  = TG_tx,                                          
    Gluc_tx                = Gluc_tx,  
    HTA_tx                 = HTA_tx   
  )



# Recode Region to Norte, Centro and Sur
data$Region <- dplyr::recode(data$Region,
                             "1" = "North",
                             "2" = "Center",
                             "3" = "South")
#Recode Estrato to Rural, Urbano and Metropolitano
data$Stratum <- dplyr::recode(data$Stratum,
                             "1" = "Rural",
                             "2" = "Urban",
                             "3" = "Metropolitan",
                             .missing = "No_data")
#Recode Sexo to Hombre y Mujer
data$Sex <- dplyr::recode(data$Sex,
                           "1" = "Male",
                           "2" = "Female")

#Recode Actividad física
data$PhysicalActivity <- dplyr::recode(data$PhysicalActivity,
                                       "1" = "Low",
                                       "2" = "Moderate",
                                       "3" = "High",
                                       .missing = "No_data")
#Recode Edad_cat
data$Age_catg         <- dplyr::recode(data$Age_catg,
                                       "1"  = "Quartile1",
                                       "2"  = "Quartile2",
                                       "3"  = "Quartile3",
                                       "4"  = "Quartile4")

#Recode AGES_groups 
data$AGES             <-dplyr::recode(data$AGES,
                                      "1"   = "Low",
                                      "2"   = "Moderate",
                                      "3"   = "High")


#Exploratory check
table(data$Diagnosis)
Metabolic_Syndromelow = subset(data, Diagnosis=="Metabolic_Syndromelow")
Metabolic_Syndromehigh = subset(data, Diagnosis=="Metabolic_Syndromehigh")
dim(Metabolic_Syndromelow)


hist(Metabolic_Syndromelow$LDL)
hist(Metabolic_Syndromelow$HDL)
hist(Metabolic_Syndromelow$TotalCholesterol)
hist(Metabolic_Syndromelow$UricAcid)
hist(Metabolic_Syndromelow$Triglycerides)
hist(Metabolic_Syndromelow$HbA1c)
hist(Metabolic_Syndromelow$HOMA_IR)


hist(Metabolic_Syndromehigh$LDL)
hist(Metabolic_Syndromehigh$HDL)
hist(Metabolic_Syndromehigh$TotalCholesterol)
hist(Metabolic_Syndromehigh$UricAcid)
hist(Metabolic_Syndromehigh$Triglycerides)
hist(Metabolic_Syndromehigh$HbA1c)
hist(Metabolic_Syndromehigh$HOMA_IR)



#Write file with all processed variables
write.csv(data, "../Output/clean_data_63 variables.csv")


#Final selection of variables for downstream analysis (11 variables) para clustering
final_selected_vars <- c("IMC",
                         "CC",
                         #"TAS",
                         #"TAD",
                         #"Glucosa",
                         #"Insulina",
                         #"HbA1c",
                         #"HDL",
                         #"LDL",
                         #"Colesterol_total",
                         #"Trigliceridos",
                         "Kcal_dia",
                         "Carbohidratos_dia",
                         "Lipidos_dia",
                         "Proteinas_dia",
                         "dAGEs_dia",
                         "NOVA1",
                         "NOVA2",
                         "NOVA3",
                         "NOVA4")


# Final selection of variables for downstream analysis (11 variables) for clustering
final_selected_vars <- c("BMI",
                         "WaistCirc",
                         #"SBP",
                         #"DBP",
                         #"Glucose",
                         #"Insulin",
                         #"HbA1c",
                         #"HDL",
                         #"LDL",
                         #"TotalCholesterol",
                         #"Triglycerides",
                         "Kcal_day",
                         "Carbs_day",
                         "Lipids_day",
                         "Protein_day",
                         "dAGEs_day",
                         "NOVA1",
                         "NOVA2",
                         "NOVA3",
                         "NOVA4")


df_downstream_analysis <- subset(data, select = final_selected_vars)
df_downstream_analysis


#Imputation with K nearest neighbors from VIM package
imp_df <- VIM::kNN(df_downstream_analysis, k = 5, imp_var = FALSE)
rownames(imp_df) <- rownames(df_downstream_analysis) 

#Function to scale data 
normalize_vars <- function(df) {
  col_sums <- colSums(df, na.rm = TRUE)
  avg_sum  <- mean(col_sums, na.rm = TRUE)
  sweep(df, 2, col_sums, "/") * avg_sum
}

#Scale using standard deviations
scale_zscore <- function(df) {
  num_cols <- sapply(df, is.numeric)
  df[, num_cols] <- scale(df[, num_cols])  # base R: centers and scales each column
  return(df)
}


#Normalize data
df_norm <- normalize_vars(imp_df)
#Scale data to standard deviations
df_sc <- scale_zscore(df_norm)

#Write data for heatmap clustering
write.csv(df_sc, "../Output/clean_data_for_clustering.csv")
