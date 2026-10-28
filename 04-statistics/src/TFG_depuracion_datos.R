library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(janitor)
library(writexl)


bolonia <- read_excel("percezione.xlsx")
valladolid <- read_excel("percepcion_vall.xlsx")

colnames(valladolid)<-c("tiempo", "ciudad", "sexo", "carrera", "curso", "modalidad", "aspectosIMP", "sector", "lejos", "ia","encontrar", "mensual", "pais", "trabajarIT", "problemas", "positivos")
colnames(bolonia)<-c("tiempo", "ciudad", "sexo", "carrera", "curso", "nacionalidad", "modalidad", "aspectosIMP", "sector", "lejos", "ia","encontrar", "mensual", "pais", "trabajarIT", "problemas", "positivos")


bolonia<-bolonia[,-1]
valladolid<-valladolid[,-1]

conteo <- bolonia %>%
  count(carrera, curso, sexo, name = "n")
conteo

conteo2 <- valladolid %>%
  count(carrera, curso, sexo, name = "n")
conteo2


datos <- bind_rows(
  bolonia %>% mutate(across(everything(), as.character)),
  valladolid %>% mutate(across(everything(), as.character))
)
datos$ia<-as.numeric(datos$ia)




###################################################traduccion

# Función para normalizar textos
normaliza <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("[’`´]", "'") %>%
    str_squish()
}

# Diccionario italiano -> español
dic_traduccion <- c(
  # Lugar
  "Bologna" = "Bolonia",
  "Altro" = "Otro",
  
  # Sexo
  "Uomo" = "Hombre",
  "Donna" = "Mujer",
  "Preferisco non rispondere" = "Prefiero no decirlo",
  
  # Carrera
  "Scienze statistiche" = "Estadística",
  "Informatica" = "Ingeniería Informática",
  
  # Nacionalidad
  "Italiana" = "Italiana",
  "Cinese"="China",
  "albanese"="Albanesa",
  "Bangladino"="Banglades",
  
  # Modalidad de trabajo
  "In presenza" = "Presencial",
  "Smart working (telelavoro)" = "Teletrabajo",
  "Ibrido" = "Híbrida",
  "Indifferente" = "Me es indiferente",
  
  # Aspectos valorados en futuro trabajo
  "Stipendio elevato" = "Salario alto",
  "Stabilità lavorativa" = "Estabilidad laboral",
  "Flessibilità oraria" = "Flexibilidad horaria",
  "Opportunità di carriera" = "Posibilidades de promoción profesional",
  "Un lavoro che mi risulti interessante" = "Que el trabajo me resulte interesante",
  
  # Sector
  "Settore pubblico" = "Sector público",
  "Settore pubblico." = "Sector público",
  "Settore privato" = "Sector privado",
  "Settore privato." = "Sector privado",
  "Lavoro autonomo / Freelance" = "Autónomo",
  "Non lo so" = "No lo tengo claro",
  
  # Disposición a vivir fuera
  "Sì, anche all'estero" = "Sí, incluso en el extranjero",
  "Si, anche all'estero" = "Sí, incluso en el extranjero",
  "Sì, ma in un'altra regione dell'Italia" = "Sí, en otra comunidad autónoma",
  "Si, ma in un'altra regione dell'Italia" = "Sí, en otra comunidad autónoma",
  "Sì, ma all'interno della mia regione" = "Sí, pero dentro de mi comunidad autónoma",
  "Si, ma all'interno della mia regione" = "Sí, pero dentro de mi comunidad autónoma",
  "No, preferisco lavorare vicino al mio luogo di origine" = "No, prefiero trabajar cerca de mi lugar de origen",
  
  # Expectativa de encontrar empleo
  "Sì, senza troppa difficoltà" = "Sí, sin demasiada dificultad",
  "Si, senza troppa difficoltà" = "Sí, sin demasiada dificultad",
  "Sì, ma sarà abbastanza difficile" = "Sí, pero me costará bastante",
  "Si, ma sarà abbastanza difficile" = "Sí, pero me costará bastante",
  "Non ne sono sicuro" = "No lo tengo claro",
  "No, credo che sarà molto difficile" = "No, creo que será muy difícil",
  
  # Salario esperado
  "1000€-1500€" = "1000€ - 1500€",
  "1500€-2000€" = "1500€ - 2000€",
  "2000€-2500€" = "2000€ - 2500€",
  "1000€ -1500€" = "1000€ - 1500€",
  "1500€ - 2000€" = "1500€ - 2000€",
  "2000€ - 2500€" = "2000€ - 2500€",
  
  # País con mejores oportunidades
  "Spagna" = "En España",
  "Italia" = "En Italia",
  "Simili in entrambi i paesi" = "Similares en ambos países",
  "Non ho abbastanza informazioni per confrontare" = "No tengo suficiente información para comparar",
  
  # Consideraría trabajar en España / Italia
  "Sì, sicuramente" = "Sí, definitivamente",
  "Lo considererei se le condizioni fossero migliori" = "Lo consideraría si las condiciones fueran mejores",
  "No, preferisco restare nel mio paese" = "No, prefiero quedarme en mi país",
  "Non ci ho mai pensato" = "No lo había pensado",
  "Preferirei scegliere un altro paese in cui trasferirmi" = "Preferiría escoger otro país al que mudarme",
  
  # Problemas / aspectos positivos del mercado laboral
  "Livello salariale" = "Nivel salarial",
  "Opportunità di lavoro" = "Oportunidades de empleo",
  "Facilità di accesso al primo impiego" = "Facilidad para acceder al primer empleo",
  "Tutela e diritti dei lavoratori" = "Protección y derechos laborales",
  "Conciliazione tra vita lavorativa e vita personale" = "Conciliación entre vida laboral y personal"
)

# Normalizamos también los nombres del diccionario
names(dic_traduccion) <- normaliza(names(dic_traduccion))

# Función para traducir una celda
traducir_celda <- function(x) {
  if (is.na(x)) return(NA_character_)
  
  x_norm <- normaliza(x)
  
  # Caso 1: respuesta simple exacta
  if (x_norm %in% names(dic_traduccion)) {
    return(unname(dic_traduccion[x_norm]))
  }
  
  # Caso 2: respuesta múltiple separada por comas o punto y coma
  partes <- str_split(x_norm, "\\s*[,;]\\s*")[[1]]
  partes <- partes[partes != ""]
  
  if (length(partes) > 1) {
    partes_traducidas <- ifelse(
      partes %in% names(dic_traduccion),
      dic_traduccion[partes],
      partes
    )
    
    return(paste(partes_traducidas, collapse = ", "))
  }
  
  # Si no está en el diccionario, se deja igual
  return(x_norm)
}

# Aplicar traducción a todas las columnas de texto
datos_es <- datos %>%
  mutate(
    across(
      where(~ is.character(.x) | is.factor(.x)),
      ~ vapply(.x, traducir_celda, character(1))
    )
  )
#View(datos_es)

#############################eliminacion 


datos_limpios <- datos_es %>%
  filter(
    # Mantener solo Bolonia y Valladolid
    `ciudad` %in% c("Bolonia", "Valladolid"),
    
    # Mantener solo las carreras válidas
    `carrera` %in% c(
      "Estadística",
      "Ingeniería Informática",
      "Programa de Doble Titulación Oficial: Grado en Ingeniería Informática y Grado en Estadística"
      
    ),
    
    # Eliminar los que prefieren no decir sexo
    `sexo` %in% c("Hombre", "Mujer")
  )


################################comprobacion


# Tabla de tamaños mínimos necesarios
objetivo <- tribble(
  ~ciudad,      ~carrera,                  ~curso,        ~sexo,    ~n_objetivo,
  
  "Bolonia",    "Estadística",             "Primer año",  "Hombre",  8,
  "Bolonia",    "Estadística",             "Primer año",  "Mujer",   6,
  "Bolonia",    "Estadística",             "Otro curso",  "Hombre",  19,
  "Bolonia",    "Estadística",             "Otro curso",  "Mujer",   13,
  
  "Bolonia",    "Ingeniería Informática",  "Primer año",  "Hombre",  12,
  "Bolonia",    "Ingeniería Informática",  "Primer año",  "Mujer",   3,
  "Bolonia",    "Ingeniería Informática",  "Otro curso",  "Hombre",  31,
  "Bolonia",    "Ingeniería Informática",  "Otro curso",  "Mujer",   8,
  
  "Valladolid", "Estadística",             "Primer año",  "Hombre",  2,
  "Valladolid", "Estadística",             "Primer año",  "Mujer",   1,
  "Valladolid", "Estadística",             "Otro curso",  "Hombre",  5,
  "Valladolid", "Estadística",             "Otro curso",  "Mujer",   3,
  
  "Valladolid", "Ingeniería Informática",  "Primer año",  "Hombre",  12,
  "Valladolid", "Ingeniería Informática",  "Primer año",  "Mujer",   2,
  "Valladolid", "Ingeniería Informática",  "Otro curso",  "Hombre",  28,
  "Valladolid", "Ingeniería Informática",  "Otro curso",  "Mujer",   5,
  
  "Valladolid", "Programa de Doble Titulación Oficial: Grado en Ingeniería Informática y Grado en Estadística",             "Primer año",  "Hombre",  2,
  "Valladolid", "Programa de Doble Titulación Oficial: Grado en Ingeniería Informática y Grado en Estadística",             "Primer año",  "Mujer",   1,
  "Valladolid", "Programa de Doble Titulación Oficial: Grado en Ingeniería Informática y Grado en Estadística",             "Otro curso",  "Hombre",  5,
  "Valladolid", "Programa de Doble Titulación Oficial: Grado en Ingeniería Informática y Grado en Estadística",             "Otro curso",  "Mujer",   2
) 

##########################################3Conteo real dataset

conteo_datos <- datos_limpios %>%
  mutate(
    curso = case_when(
      curso %in% c("1º") ~ "Primer año",
      curso %in% c("2º", "3º", "4º", "5º") ~ "Otro curso",
      TRUE ~ curso
    )
  ) %>%
  count(ciudad, carrera, curso, sexo, name = "n_real")

#View(conteo_datos)

##################################3 Comparación entre objetivo y datos reales
comparacion <- objetivo %>%
  left_join(conteo_datos, by = c("ciudad", "carrera", "curso", "sexo")) %>%
  mutate(
    n_real = replace_na(n_real, 0),
    faltan = pmax(n_objetivo - n_real, 0),
    sobran = pmax(n_real - n_objetivo, 0),
    cumple = n_real >= n_objetivo
  )

comparacion





# 1. Personas que NO estudian doble grado y han puesto 5º curso
fallo_curso <- datos_limpios %>%
  filter(
    str_detect(curso, "5") &
      !str_detect(carrera, regex("doble", ignore_case = TRUE))
  )
fallo_curso


# 2. Respuestas repetidas entre problemas y positivos


# Añadir un identificador de individuo/fila
datos_con_id <- datos_limpios %>%
  mutate(id_individuo = row_number())

# Función para separar respuestas múltiples
separar_respuestas <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  
  str_split(x, ",")[[1]] %>%
    str_squish() %>%
    setdiff("No lo tengo claro")
}

# Detectar individuos con respuestas repetidas en problemas y positivos
fallo_problemas_positivos <- datos_con_id %>%
  rowwise() %>%
  mutate(
    lista_problemas = list(separar_respuestas(problemas)),
    lista_positivos = list(separar_respuestas(positivos)),
    repetidas = list(intersect(lista_problemas, lista_positivos)),
    n_repetidas = length(repetidas)
  ) %>%
  ungroup() %>%
  filter(n_repetidas > 0) %>%
  mutate(
    respuestas_repetidas = sapply(repetidas, paste, collapse = ", ")
  ) %>%
  select(
    id_individuo,
    ciudad,
    carrera,
    curso,
    sexo,
    problemas,
    positivos,
    respuestas_repetidas,
    n_repetidas
  )

fallo_problemas_positivos

ids_eliminar <- fallo_problemas_positivos$id_individuo


datos_limpios_final <- datos_con_id %>%
  filter(!id_individuo %in% ids_eliminar) %>%
  select(-id_individuo)
#View(datos_limpios_final)


conteo_datos_final <- datos_limpios_final %>%
  mutate(
    curso = case_when(
      curso %in% c("1º") ~ "Primer año",
      curso %in% c("2º", "3º", "4º", "5º") ~ "Otro curso",
      TRUE ~ curso
    )
  ) %>%
  count(ciudad, carrera, curso, sexo, name = "n_real")


###################################3 Comparación entre objetivo y datos reales
comparacion_final <- objetivo %>%
  left_join(conteo_datos_final, by = c("ciudad", "carrera", "curso", "sexo")) %>%
  mutate(
    n_real = replace_na(n_real, 0),
    faltan = pmax(n_objetivo - n_real, 0),
    sobran = pmax(n_real - n_objetivo, 0),
    cumple = n_real >= n_objetivo
  )

comparacion

colnames(datos_limpios_final)<-c("pais"   ,    "sexo"     ,    "carrera"  ,    "curso" ,       "nacionalidad" ,"modalidad",    "aspectos_imp" , "sector"      
                    ,"vivir_lejos"     ,   "ia"   ,        "encontrar_empleo"   , "salario"     , "oportunidades_pais"    ,     "trabajar_IT_ES" ,  "problemas_tupais"   , "positivos_tupais" )


######################################################################agrupacion curso
datos_limpios_final[datos_limpios_final$curso!="1º",]$curso<-"Otro curso"
datos_limpios_final[datos_limpios_final$curso=="1º",]$curso<-"Primer curso"


########################################################################doble grado y pais
datos_limpios_final[datos_limpios_final$carrera=="Programa de Doble Titulación Oficial: Grado en Ingeniería Informática y Grado en Estadística",]$carrera<-"Doble Grado"
datos_limpios_final[datos_limpios_final$pais=="Valladolid",]$pais<-"España"
datos_limpios_final[datos_limpios_final$pais=="Bolonia",]$pais<-"Italia"

########################################################################unificar
datos_limpios_final[datos_limpios_final$sector=="No lo tengo claro.",]$sector<-"No lo tengo claro"
datos_limpios_final[datos_limpios_final$sector=="Autónomo.",]$sector<-"Autónomo"
datos_limpios_final[datos_limpios_final$sector=="Sector público.",]$sector<-"Sector público"
datos_limpios_final[datos_limpios_final$sector=="Sector privado.",]$sector<-"Sector privado"

datos_limpios_final[datos_limpios_final$vivir_lejos=="Sí, incluso en el extranjero.",]$vivir_lejos<-"Sí, incluso en el extranjero"
datos_limpios_final[datos_limpios_final$vivir_lejos=="Sí, en otra comunidad autónoma.",]$vivir_lejos<-"Sí, en otra comunidad autónoma"
datos_limpios_final[datos_limpios_final$vivir_lejos=="No, prefiero trabajar cerca de mi lugar de origen.",]$vivir_lejos<-"No, prefiero trabajar cerca de mi lugar de origen"
datos_limpios_final[datos_limpios_final$vivir_lejos=="Sí, pero dentro de mi comunidad autónoma.",]$vivir_lejos<-"Sí, pero dentro de mi comunidad autónoma"

datos_depurados<-datos_limpios_final

conteo3 <- datos_depurados %>%
  count(pais, carrera, curso, name = "n")
conteo3

library(writexl)

########################################################################guardar datos depurados
write_xlsx(datos_depurados, "datos_depurados.xlsx")
