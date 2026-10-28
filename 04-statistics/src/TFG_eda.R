#eda

# ============================================================
# 0. PAQUETES
# ============================================================

paquetes <- c("tidyverse", "janitor", "readxl", "scales")

instalar <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if(length(instalar) > 0) install.packages(instalar)

library(tidyverse)
library(janitor)
library(readxl)
library(scales)


# ============================================================
# 1. CARGAR DATOS
# ============================================================

# Cambia el nombre del archivo por el tuyo
datosM <- datos_limpios_final

colnames(datosM)<-c("ciudad"   ,    "sexo"     ,    "carrera"  ,    "curso" ,       "nacionalidad" ,"modalidad",    "aspectos_imp" , "sector"      
                    ,"vivir_lejos"     ,   "ia"   ,        "encontrar_empleo"   , "salario"     , "oportunidades_pais"    ,     "trabajar_IT_ES" ,  "problemas_tupais"   , "positivos_tupais" )

# ============================================================
# 2. INDICAR VARIABLES PRINCIPALES
# ============================================================

# Cambia estos nombres según aparezcan en names(datos)
var_ciudad  <- "ciudad"       # o "donde_estudias"
var_sexo    <- "sexo"       # o "sexo"
var_carrera <- "carrera"      # o "carrera"
var_curso   <- "curso"       # o "curso"
var_ia      <- "ia"          # preocupación por la IA, escala 0-10


# Preguntas de respuesta única
single_vars <- c(
  "modalidad",
  "sector",
  "vivir_lejos",
  "encontrar_empleo",
  "salario",
  "oportunidades_pais",
  "trabajar_IT_ES"
)

# Preguntas de respuesta múltiple
multi_vars <- c(
  "aspectos_imp",
  "problemas_tupais",
  "positivos_tupais"
)


# ============================================================
# 3. CREAR VARIABLES AGRUPADAS
# ============================================================

datosM <- datosM |> 
  mutate(
    pais = case_when(
      str_detect(str_to_lower(as.character(.data[[var_ciudad]])), 
                 "valladolid|espana|españa|spain") ~ "España",
      str_detect(str_to_lower(as.character(.data[[var_ciudad]])), 
                 "bologna|bolonia|italia|italy") ~ "Italia",
      TRUE ~ as.character(.data[[var_ciudad]])
    ),
    
    sexo = as.character(.data[[var_sexo]]),
    
    carrera_original = as.character(.data[[var_carrera]]),
    
    carrera = case_when(
      str_detect(str_to_lower(carrera_original), "estad|stat|scienze") ~ "Estadística",
      str_detect(str_to_lower(carrera_original), "inform") ~ "Informática",
      str_detect(str_to_lower(carrera_original), "doble") ~ "Doble grado",
      TRUE ~ carrera_original
    ),
    
    curso_original = as.character(.data[[var_curso]]),
    
    curso = case_when(
      str_detect(str_to_lower(curso_original), "^1|1º|prim") ~ "Primero",
      TRUE ~ "Otro curso"
    ),
    
    ia_num = suppressWarnings(
      as.numeric(str_replace(as.character(.data[[var_ia]]), ",", "."))
    )
  )


# ============================================================
# 4. FUNCIONES PARA TABLAS
# ============================================================

limpiar_respuesta <- function(x) {
  x |> 
    as.character() |> 
    str_squish() |> 
    na_if("")
}


# Frecuencias totales para preguntas de respuesta única
freq_total <- function(df, var) {
  df |> 
    mutate(respuesta = limpiar_respuesta(.data[[var]])) |> 
    count(respuesta, name = "n") |> 
    mutate(
      total = sum(n),
      porcentaje = round(100 * n / total, 1)
    ) |> 
    arrange(desc(n))
}


# Frecuencias por grupos para respuesta única
freq_by_many <- function(df, var, grupos) {
  df |> 
    mutate(respuesta = limpiar_respuesta(.data[[var]])) |> 
    count(across(all_of(grupos)), respuesta, name = "n") |> 
    group_by(across(all_of(grupos))) |> 
    mutate(
      total = sum(n),
      porcentaje = round(100 * n / total, 1)
    ) |> 
    ungroup() |> 
    arrange(across(all_of(grupos)), desc(porcentaje))
}


# Frecuencias totales para preguntas de respuesta múltiple
multi_total <- function(df, var, sep = "\\s*,\\s*|\\s*;\\s*") {
  
  total_personas <- nrow(df)
  
  df |> 
    transmute(respuesta = limpiar_respuesta(.data[[var]])) |> 
    filter(!is.na(respuesta)) |> 
    separate_rows(respuesta, sep = sep) |> 
    mutate(respuesta = limpiar_respuesta(respuesta)) |> 
    filter(!is.na(respuesta)) |> 
    count(respuesta, sort = TRUE, name = "n") |> 
    mutate(
      total_personas = total_personas,
      porcentaje = round(100 * n / total_personas, 1)
    )
}


# Frecuencias por grupos para preguntas de respuesta múltiple
multi_by_many <- function(df, var, grupos, sep = "\\s*,\\s*|\\s*;\\s*") {
  
  denominadores <- df |> 
    count(across(all_of(grupos)), name = "total_personas")
  
  df |> 
    transmute(across(all_of(grupos)),
              respuesta = limpiar_respuesta(.data[[var]])) |> 
    filter(!is.na(respuesta)) |> 
    separate_rows(respuesta, sep = sep) |> 
    mutate(respuesta = limpiar_respuesta(respuesta)) |> 
    filter(!is.na(respuesta)) |> 
    count(across(all_of(grupos)), respuesta, name = "n") |> 
    left_join(denominadores, by = grupos) |> 
    mutate(
      porcentaje = round(100 * n / total_personas, 1)
    ) |> 
    arrange(across(all_of(grupos)), desc(porcentaje))
}


# Resumen de la variable IA
resumen_ia <- function(df, grupos = NULL) {
  
  if(is.null(grupos)) {
    df |> 
      summarise(
        n = sum(!is.na(ia_num)),
        media = round(mean(ia_num, na.rm = TRUE), 2),
        mediana = round(median(ia_num, na.rm = TRUE), 2),
        sd = round(sd(ia_num, na.rm = TRUE), 2),
        minimo = min(ia_num, na.rm = TRUE),
        maximo = max(ia_num, na.rm = TRUE)
      )
  } else {
    df |> 
      group_by(across(all_of(grupos))) |> 
      summarise(
        n = sum(!is.na(ia_num)),
        media = round(mean(ia_num, na.rm = TRUE), 2),
        mediana = round(median(ia_num, na.rm = TRUE), 2),
        sd = round(sd(ia_num, na.rm = TRUE), 2),
        minimo = min(ia_num, na.rm = TRUE),
        maximo = max(ia_num, na.rm = TRUE),
        .groups = "drop"
      ) |> 
      arrange(desc(media))
  }
}

# ============================================================
# 5. RESULTADOS DESCRIPTIVOS
# ============================================================

# Tamaño de la muestra por estratos principales
muestra <- datosM |> 
  count(pais, carrera, sexo, curso, name = "n") |> 
  arrange(pais, carrera, sexo, curso)

muestra


# Distribución general de país, carrera, sexo y curso
freq_total(datosM, "pais")
freq_total(datosM, "carrera")
freq_total(datosM, "sexo")
freq_total(datosM, "curso")


# IA total y por grupos
ia_total <- resumen_ia(datosM)
ia_pais <- resumen_ia(datosM, "pais")
ia_carrera <- resumen_ia(datosM, "carrera")
ia_sexo <- resumen_ia(datosM, "sexo")
ia_pais_carrera <- resumen_ia(datosM, c("pais", "carrera"))

ia_total
ia_pais
ia_carrera
ia_sexo
ia_pais_carrera

# ============================================================
# 6. PREGUNTAS DE RESPUESTA ÚNICA
# ============================================================

# Asegurarse de que solo se usan variables que existen
single_vars <- intersect(single_vars, names(datosM))
multi_vars  <- intersect(multi_vars, names(datosM))

single_vars
multi_vars


# Tablas generales
tablas_single_total <- map(single_vars, ~freq_total(datosM, .x)) |> 
  set_names(single_vars)

tablas_single_total


# Tablas por país
tablas_single_pais <- map(single_vars, ~freq_by_many(datosM, .x, "pais")) |> 
  set_names(single_vars)

tablas_single_pais


# Tablas por país y carrera
tablas_single_pais_carrera <- map(single_vars, ~freq_by_many(datosM, .x, c("pais", "carrera"))) |> 
  set_names(single_vars)

tablas_single_pais_carrera


# Tablas por sexo
tablas_single_sexo <- map(single_vars, ~freq_by_many(datosM, .x, "sexo")) |> 
  set_names(single_vars)

tablas_single_sexo


# Tablas por curso
tablas_single_curso <- map(single_vars, ~freq_by_many(datosM, .x, "curso")) |> 
  set_names(single_vars)

tablas_single_curso


# ============================================================
# 7. PREGUNTAS DE RESPUESTA MÚLTIPLE
# ============================================================

# Tablas generales
tablas_multi_total <- map(multi_vars, ~multi_total(datosM, .x)) |> 
  set_names(multi_vars)

tablas_multi_total


# Tablas por país
tablas_multi_pais <- map(multi_vars, ~multi_by_many(datosM, .x, "pais")) |> 
  set_names(multi_vars)

tablas_multi_pais


# Tablas por país y carrera
tablas_multi_pais_carrera <- map(multi_vars, ~multi_by_many(datosM, .x, c("pais", "carrera"))) |> 
  set_names(multi_vars)

tablas_multi_pais_carrera


# Tablas por sexo
tablas_multi_sexo <- map(multi_vars, ~multi_by_many(datosM, .x, "sexo")) |> 
  set_names(multi_vars)

tablas_multi_sexo


# Tablas por curso
tablas_multi_curso <- map(multi_vars, ~multi_by_many(datosM, .x, "curso")) |> 
  set_names(multi_vars)

tablas_multi_curso


# ============================================================
# 8. DIFERENCIAS DE PROPORCIONES ESPAÑA VS ITALIA
# ============================================================

dif_pais_single <- function(df, var) {
  
  tab <- freq_by_many(df, var, "pais") |> 
    select(pais, respuesta, porcentaje)
  
  res <- tab |> 
    pivot_wider(
      names_from = pais,
      values_from = porcentaje,
      values_fill = 0
    )
  
  if(!all(c("España", "Italia") %in% names(res))) {
    return(res)
  }
  
  res |> 
    mutate(
      variable = var,
      dif_abs = round(abs(.data[["España"]] - .data[["Italia"]]), 1)
    ) |> 
    select(variable, respuesta, España, Italia, dif_abs) |> 
    arrange(desc(dif_abs))
}


dif_pais_multi <- function(df, var) {
  
  tab <- multi_by_many(df, var, "pais") |> 
    select(pais, respuesta, porcentaje)
  
  res <- tab |> 
    pivot_wider(
      names_from = pais,
      values_from = porcentaje,
      values_fill = 0
    )
  
  if(!all(c("España", "Italia") %in% names(res))) {
    return(res)
  }
  
  res |> 
    mutate(
      variable = var,
      dif_abs = round(abs(.data[["España"]] - .data[["Italia"]]), 1)
    ) |> 
    select(variable, respuesta, España, Italia, dif_abs) |> 
    arrange(desc(dif_abs))
}


top_dif_single <- map_dfr(single_vars, ~dif_pais_single(datosM, .x))
top_dif_multi  <- map_dfr(multi_vars, ~dif_pais_multi(datosM, .x))

top_diferencias_pais <- bind_rows(top_dif_single, top_dif_multi) |> 
  arrange(desc(dif_abs))

top_diferencias_pais


# ============================================================
# 9. FUNCIONES PARA GRÁFICOS
# ============================================================

dir.create("graficos", showWarnings = FALSE)


grafico_single <- function(df, var, grupo = "pais") {
  
  tab <- freq_by_many(df, var, grupo) |> 
    filter(!is.na(respuesta))
  
  ggplot(tab, aes(x = reorder(respuesta, porcentaje),
                  y = porcentaje,
                  fill = .data[[grupo]])) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(
      title = paste("Distribución de", var, "por", grupo),
      x = NULL,
      y = "% dentro del grupo",
      fill = grupo
    ) +
    theme_minimal()
}


grafico_multi <- function(df, var, grupo = "pais") {
  
  tab <- multi_by_many(df, var, grupo) |> 
    filter(!is.na(respuesta))
  
  ggplot(tab, aes(x = reorder(respuesta, porcentaje),
                  y = porcentaje,
                  fill = .data[[grupo]])) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(
      title = paste("Respuestas múltiples en", var, "por", grupo),
      x = NULL,
      y = "% de personas que marcaron la opción",
      fill = grupo
    ) +
    theme_minimal()
}


# ============================================================
# 10. GRÁFICOS PRINCIPALES
# ============================================================

# Modalidad de trabajo preferida por país
if("modalidad" %in% names(datosM)) {
  g_modalidad <- grafico_single(datosM, "modalidad", "pais")
  print(g_modalidad)
  ggsave("graficos/modalidad_por_pais.png", g_modalidad, width = 9, height = 5)
}


# Sector preferido por país
if("sector" %in% names(datosM)) {
  g_sector <- grafico_single(datosM, "sector", "pais")
  print(g_sector)
  ggsave("graficos/sector_por_pais.png", g_sector, width = 9, height = 5)
}


# Disposición a vivir fuera por país
if("vivir_lejos" %in% names(datosM)) {
  g_vivir_lejos <- grafico_single(datosM, "vivir_lejos", "pais")
  print(g_vivir_lejos)
  ggsave("graficos/vivir_lejos_por_pais.png", g_vivir_lejos, width = 9, height = 5)
}


# Oportunidades laborales España vs Italia
if("oportunidades_pais" %in% names(datosM)) {
  g_oportunidades <- grafico_single(datosM, "oportunidades_pais", "pais")
  print(g_oportunidades)
  ggsave("graficos/oportunidades_por_pais.png", g_oportunidades, width = 9, height = 5)
}


# Consideraría trabajar en Italia/España
if("trabajar_IT_ES" %in% names(datosM)) {
  g_trabajar <- grafico_single(datosM, "trabajar_IT_ES", "pais")
  print(g_trabajar)
  ggsave("graficos/trabajar_it_es_por_pais.png", g_trabajar, width = 9, height = 5)
}


# Expectativa de encontrar empleo
if("encontrar_empleo" %in% names(datosM)) {
  g_empleo <- grafico_single(datosM, "encontrar_empleo", "pais")
  print(g_empleo)
  ggsave("graficos/encontrar_empleo_por_pais.png", g_empleo, width = 9, height = 5)
}


# Salario esperado
if("salario" %in% names(datosM)) {
  g_salario <- grafico_single(datosM, "salario", "pais")
  print(g_salario)
  ggsave("graficos/salario_por_pais.png", g_salario, width = 9, height = 5)
}


# Aspectos importantes en un futuro trabajo
if("aspectos_imp" %in% names(datosM)) {
  g_aspectos <- grafico_multi(datosM, "aspectos_imp", "pais")
  print(g_aspectos)
  ggsave("graficos/aspectos_importantes_por_pais.png", g_aspectos, width = 10, height = 6)
}


# Problemas del mercado laboral
if("problemas_tupais" %in% names(datosM)) {
  g_problemas <- grafico_multi(datosM, "problemas_tupais", "pais")
  print(g_problemas)
  ggsave("graficos/problemas_por_pais.png", g_problemas, width = 10, height = 6)
}


# Aspectos positivos del mercado laboral
if("positivos_tupais" %in% names(datosM)) {
  g_positivos <- grafico_multi(datosM, "positivos_tupais", "pais")
  print(g_positivos)
  ggsave("graficos/positivos_por_pais.png", g_positivos, width = 10, height = 6)
}


# Preocupación por la IA: boxplot por país
g_ia_pais <- ggplot(datosM, aes(x = pais, y = ia_num)) +
  geom_boxplot() +
  labs(
    title = "Preocupación por la IA según país",
    x = NULL,
    y = "Preocupación por la IA, escala 0-10"
  ) +
  theme_minimal()

print(g_ia_pais)
ggsave("graficos/ia_por_pais.png", g_ia_pais, width = 7, height = 5)


# Media de preocupación por IA según país y carrera
ia_pais_carrera_graf <- datosM |> 
  group_by(pais, carrera) |> 
  summarise(
    media_ia = mean(ia_num, na.rm = TRUE),
    n = sum(!is.na(ia_num)),
    .groups = "drop"
  )

g_ia_carrera <- ggplot(ia_pais_carrera_graf,
                       aes(x = carrera, y = media_ia, fill = pais)) +
  geom_col(position = "dodge") +
  labs(
    title = "Preocupación media por la IA según país y carrera",
    x = NULL,
    y = "Media de preocupación, escala 0-10",
    fill = "País"
  ) +
  theme_minimal()

print(g_ia_carrera)
ggsave("graficos/ia_media_pais_carrera.png", g_ia_carrera, width = 8, height = 5)



