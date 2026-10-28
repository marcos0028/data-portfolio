
library(readxl)
library(ggplot2)
library(scales)
library(tidyverse)
library(ggrepel)

estimaciones_modalidad <- read_excel("estimaciones_modalidad.xlsx")
estimaciones_sector <- read_excel("estimaciones_sector.xlsx")
estimaciones_vivir_lejos<-read_excel("estimaciones_vivir_lejos.xlsx")
estimaciones_encontrar_empleo<-read_excel("estimaciones_encontrar_empleo.xlsx")
estimaciones_salario<-read_excel("estimaciones_salario.xlsx")
estimaciones_oportunidades_pais<-read_excel("estimaciones_oportunidades_pais.xlsx")
estimaciones_trabajarItEs<-read_excel("estimaciones_trabajarItEs.xlsx")

estimaciones_ia_estratos<-read_excel("estimaciones_ia_estratos.xlsx")
estimaciones_ia_pais_carrera<-read_excel("estimaciones_ia_pais_carrera.xlsx")
estimaciones_ia_pais<-read_excel("estimaciones_ia_pais.xlsx")

estimaciones_aspectos_imp<-read_excel("estimaciones_aspectos_imp.xlsx")
estimaciones_problemas_pais<-read_excel("estimaciones_problemas_pais.xlsx")
estimaciones_positivo_pais<-read_excel("estimaciones_positivo_pais.xlsx")


#######################################################
# 1. LISTA DE TABLAS DE ESTIMACIONES
#######################################################

lista_estimaciones <- list(
  modalidad = list(
    data = estimaciones_modalidad,
    titulo = "Modalidad de trabajo preferida"
  ),
  sector = list(
    data = estimaciones_sector,
    titulo = "Sector laboral preferido"
  ),
  vivir_lejos = list(
    data = estimaciones_vivir_lejos,
    titulo = "Disposición a vivir lejos del lugar de origen"
  ),
  encontrar_empleo = list(
    data = estimaciones_encontrar_empleo,
    titulo = "Expectativa de encontrar empleo"
  ),
  salario = list(
    data = estimaciones_salario,
    titulo = "Salario esperado en el primer empleo"
  ),
  oportunidades_pais = list(
    data = estimaciones_oportunidades_pais,
    titulo = "País con mejores oportunidades laborales"
  ),
  trabajarItEs = list(
    data = estimaciones_trabajarItEs,
    titulo = "Disposición a trabajar en España o Italia"
  )
)
limpiar_estimaciones <- function(data) {
  
  data %>%
    mutate(
      nivel = as.character(nivel),
      pais = as.character(pais),
      carrera = as.character(carrera),
      curso = as.character(curso),
      opcion = as.character(opcion),
      nh = as.numeric(nh),
      Nh = as.numeric(Nh),
      estimacion = as.numeric(estimacion),
      LI = as.numeric(LI),
      LS = as.numeric(LS),
      estimacion_pct = estimacion * 100,
      LI_pct = LI * 100,
      LS_pct = LS * 100
    )
}

crear_grafico_global_pais <- function(data, titulo) {
  
  data %>%
    filter(nivel == "Global pais") %>%
    ggplot(aes(
      x = opcion,
      y = estimacion_pct,
      color = pais
    )) +
    geom_point(
      position = position_dodge(width = 0.5),
      size = 3
    ) +
    geom_errorbar(
      aes(ymin = LI_pct, ymax = LS_pct),
      position = position_dodge(width = 0.5),
      width = 0.15,
      linewidth = 0.8
    ) +
    labs(
      x = NULL,
      y = "Estimación (%)",
      color = "País"
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 30, hjust = 1),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

crear_grafico_pais_carrera <- function(data, titulo) {
  
  data %>%
    filter(nivel == "Global pais-carrera") %>%
    ggplot(aes(
      x = opcion,
      y = estimacion_pct,
      color = pais
    )) +
    geom_point(
      position = position_dodge(width = 0.5),
      size = 2.8
    ) +
    geom_errorbar(
      aes(ymin = LI_pct, ymax = LS_pct),
      position = position_dodge(width = 0.5),
      width = 0.15,
      linewidth = 0.7
    ) +
    facet_wrap(~ carrera) +
    labs(
      x = NULL,
      y = "Estimación (%)",
      color = "País"
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 35, hjust = 1),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

crear_grafico_estratos_cursos <- function(data, titulo) {
  
  data %>%
    filter(nivel == "Estrato") %>%
    mutate(
      grupo = paste(pais, carrera, sep = " - "),
      curso = factor(curso, levels = c("Primer curso", "Otro curso"))
    ) %>%
    ggplot(aes(
      x = estimacion_pct,
      y = grupo
    )) +
    geom_line(
      aes(group = grupo),
      linewidth = 0.8,
      color = "grey70"
    ) +
    geom_point(
      aes(color = curso),
      size = 3.2
    ) +
    geom_errorbarh(
      aes(
        xmin = LI_pct,
        xmax = LS_pct,
        color = curso
      ),
      height = 0.18,
      linewidth = 0.7
    ) +
    facet_wrap(~ opcion) +
    scale_x_continuous(
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      x = "Estimación (%)",
      y = NULL,
      color = "Curso"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}



for (nombre in names(lista_estimaciones)) {
  
  # Extraer tabla y título
  data_original <- lista_estimaciones[[nombre]]$data
  titulo <- lista_estimaciones[[nombre]]$titulo
  
  # Limpiar tabla
  data_limpia <- limpiar_estimaciones(data_original)
  
  # Crear gráficos
  grafico_global_pais <- crear_grafico_global_pais(
    data = data_limpia,
    titulo = titulo
  )
  
  grafico_pais_carrera <- crear_grafico_pais_carrera(
    data = data_limpia,
    titulo = titulo
  )
  
  grafico_estratos_cursos <- crear_grafico_estratos_cursos(
    data = data_limpia,
    titulo = titulo
  )

  # Guardar gráfico global por país
  ggsave(
    filename = paste0("graficos_unica_respuesta", nombre, ".png"),
    plot = grafico_global_pais,
    width = 9,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  
  # Guardar gráfico por país y carrera
  ggsave(
    filename = paste0("graficos_unica_respuesta", nombre, ".png"),
    plot = grafico_pais_carrera,
    width = 10,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  
 
}



# Guardar gráfico por estratos y cursos
ggsave(
  filename = paste0("graficos_unica_respuesta", nombre, ".png"),
  plot = grafico_estratos_cursos,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)


#############################################################33
###########################################respuesta multiple
lista_estimaciones_multi <- list(
  aspectos_imp = list(
    data = estimaciones_aspectos_imp,
    titulo = "Aspectos más valorados en el trabajo"
  ),
  problemas_pais = list(
    data = estimaciones_problemas_pais,
    titulo = "Principales problemas del mercado laboral"
  ),
  positivo_pais = list(
    data = estimaciones_positivo_pais,
    titulo = "Aspectos positivos del mercado laboral"
  )
)

limpiar_estimaciones_multi <- function(data) {
  
  data %>%
    mutate(
      nivel = as.character(nivel),
      pais = as.character(pais),
      carrera = as.character(carrera),
      curso = as.character(curso),
      opcion = as.character(opcion),
      nh = as.numeric(nh),
      Nh = as.numeric(Nh),
      estimacion = as.numeric(estimacion),
      LI = as.numeric(LI),
      LS = as.numeric(LS),
      estimacion_pct = estimacion * 100,
      LI_pct = LI * 100,
      LS_pct = LS * 100
    )
}

crear_grafico_global_pais_multi <- function(data, titulo) {
  
  data %>%
    filter(nivel == "Global pais") %>%
    mutate(opcion_wrap = str_wrap(opcion, width = 25)) %>%
    ggplot(aes(
      x = opcion_wrap,
      y = estimacion_pct,
      color = pais
    )) +
    geom_point(
      position = position_dodge(width = 0.5),
      size = 3
    ) +
    geom_errorbar(
      aes(ymin = LI_pct, ymax = LS_pct),
      position = position_dodge(width = 0.5),
      width = 0.15,
      linewidth = 0.8
    ) +
    labs(
      x = NULL,
      y = "Estimación (%)",
      color = "País"
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.text.x = element_text(angle = 30, hjust = 1),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

crear_grafico_pais_carrera_multi <- function(data, titulo) {
  
  data %>%
    filter(nivel == "Global pais-carrera") %>%
    mutate(opcion_wrap = str_wrap(opcion, width = 25)) %>%
    ggplot(aes(
      x = opcion_wrap,
      y = estimacion_pct,
      color = pais
    )) +
    geom_point(
      position = position_dodge(width = 0.5),
      size = 2.8
    ) +
    geom_errorbar(
      aes(ymin = LI_pct, ymax = LS_pct),
      position = position_dodge(width = 0.5),
      width = 0.15,
      linewidth = 0.7
    ) +
    facet_wrap(~ carrera) +
    labs(
      x = NULL,
      y = "Estimación (%)",
      color = "País"
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.text.x = element_text(angle = 35, hjust = 1),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}


crear_grafico_estratos_cursos_multi <- function(data, titulo) {
  
  data %>%
    filter(nivel == "Estrato") %>%
    mutate(
      grupo = paste(pais, carrera, sep = " - "),
      curso = factor(curso, levels = c("Primer curso", "Otro curso")),
      opcion_wrap = str_wrap(opcion, width = 25)
    ) %>%
    ggplot(aes(
      x = estimacion_pct,
      y = grupo
    )) +
    geom_line(
      aes(group = grupo),
      linewidth = 0.8,
      color = "grey70"
    ) +
    geom_point(
      aes(color = curso),
      size = 3.2
    ) +
    geom_errorbarh(
      aes(
        xmin = LI_pct,
        xmax = LS_pct,
        color = curso
      ),
      height = 0.18,
      linewidth = 0.7
    ) +
    facet_wrap(~ opcion_wrap) +
    scale_x_continuous(
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      x = "Estimación (%)",
      y = NULL,
      color = "Curso"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

#dir.create("graficos_multirrespuesta", showWarnings = FALSE)
#dir.create("graficos_multirrespuesta/global_pais", showWarnings = FALSE)
#dir.create("graficos_multirrespuesta/pais_carrera", showWarnings = FALSE)
#dir.create("graficos_multirrespuesta/estratos_cursos", showWarnings = FALSE)

graficos_global_pais_multi <- list()
graficos_pais_carrera_multi <- list()
graficos_estratos_cursos_multi <- list()

for (nombre in names(lista_estimaciones_multi)) {
  
  data_original <- lista_estimaciones_multi[[nombre]]$data
  titulo <- lista_estimaciones_multi[[nombre]]$titulo
  
  data_limpia <- limpiar_estimaciones_multi(data_original)
  
  # Crear gráficos
  graficos_global_pais_multi[[nombre]] <- crear_grafico_global_pais_multi(
    data = data_limpia,
    titulo = titulo
  )
  
  graficos_pais_carrera_multi[[nombre]] <- crear_grafico_pais_carrera_multi(
    data = data_limpia,
    titulo = titulo
  )
  
  graficos_estratos_cursos_multi[[nombre]] <- crear_grafico_estratos_cursos_multi(
    data = data_limpia,
    titulo = titulo
  )
  
  # Guardar gráfico global por país
  ggsave(
    filename = paste0("graficos_multirrespuesta", nombre, ".png"),
    plot = graficos_global_pais_multi[[nombre]],
    width = 10,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  
  # Guardar gráfico país-carrera
  ggsave(
    filename = paste0("graficos_multirrespuesta", nombre, ".png"),
    plot = graficos_pais_carrera_multi[[nombre]],
    width = 11,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  
  # Guardar gráfico estratos-cursos
  ggsave(
    filename = paste0("graficos_multirrespuesta", nombre, ".png"),
    plot = graficos_estratos_cursos_multi[[nombre]],
    width = 13,
    height = 8,
    dpi = 300,
    bg = "white"
  )
}



estimaciones_ia_estratos <- estimaciones_ia_estratos %>%
  mutate(
    pais = as.character(pais),
    carrera = as.character(carrera),
    curso = as.character(curso),
    nh = as.numeric(nh),
    Nh = as.numeric(Nh),
    estimacion = as.numeric(estimacion),
    LI = as.numeric(LI),
    LS = as.numeric(LS)
  )

estimaciones_ia_pais_carrera <- estimaciones_ia_pais_carrera %>%
  mutate(
    pais = as.character(pais),
    carrera = as.character(carrera),
    estimacion = as.numeric(estimacion),
    LI = as.numeric(LI),
    LS = as.numeric(LS)
  )

estimaciones_ia_pais <- estimaciones_ia_pais %>%
  mutate(
    pais = as.character(pais),
    estimacion = as.numeric(estimacion),
    LI = as.numeric(LI),
    LS = as.numeric(LS)
  )

dir.create("graficos_ia", showWarnings = FALSE)

grafico_ia_pais <- estimaciones_ia_pais %>%
  ggplot(aes(
    x = pais,
    y = estimacion,
    color = pais
  )) +
  geom_point(size = 4) +
  geom_errorbar(
    aes(ymin = LI, ymax = LS),
    width = 0.12,
    linewidth = 0.9
  ) +
  geom_text_repel(
    aes(label = round(estimacion, 2)),
    size = 4,
    show.legend = FALSE,
    direction = "y",
    nudge_y = 0.4,
    min.segment.length = 0
  ) +
  coord_cartesian(ylim = c(0, 10), clip = "off") +
  labs(
    title = "Preocupación media por la IA según el país",
    subtitle = "Estimación de la media en una escala de 0 a 10",
    x = NULL,
    y = "Media estimada",
    color = "País"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "none",
    plot.margin = margin(10, 25, 10, 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

grafico_ia_pais

ggsave(
  filename = "graficos_ia/grafico_ia_pais.png",
  plot = grafico_ia_pais,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

grafico_ia_pais_carrera <- estimaciones_ia_pais_carrera %>%
  mutate(
    grupo = paste(pais, carrera, sep = " - ")
  ) %>%
  ggplot(aes(
    x = estimacion,
    y = reorder(grupo, estimacion),
    color = pais
  )) +
  geom_point(size = 3.2) +
  geom_errorbarh(
    aes(xmin = LI, xmax = LS),
    height = 0.18,
    linewidth = 0.8
  ) +
  geom_text_repel(
    aes(label = round(estimacion, 2)),
    size = 3.5,
    direction = "x",
    nudge_x = 0.35,
    hjust = 0,
    show.legend = FALSE,
    min.segment.length = 0
  ) +
  coord_cartesian(xlim = c(0, 10.8), clip = "off") +
  labs(
    title = "Preocupación media por la IA según país y carrera",
    subtitle = "Estimación de la media en una escala de 0 a 10",
    x = "Media estimada",
    y = NULL,
    color = "País"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    plot.margin = margin(10, 35, 10, 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

grafico_ia_pais_carrera

ggsave(
  filename = "graficos_ia/grafico_ia_pais_carrera.png",
  plot = grafico_ia_pais_carrera,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)


grafico_ia_estratos_cursos <- estimaciones_ia_estratos %>%
  mutate(
    grupo = paste(pais, carrera, sep = " - "),
    curso = factor(curso, levels = c("Primer curso", "Otro curso"))
  ) %>%
  ggplot(aes(
    x = estimacion,
    y = grupo
  )) +
  geom_line(
    aes(group = grupo),
    linewidth = 0.8,
    color = "grey70"
  ) +
  geom_point(
    aes(color = curso),
    size = 3.4
  ) +
  geom_errorbarh(
    aes(
      xmin = LI,
      xmax = LS,
      color = curso
    ),
    height = 0.18,
    linewidth = 0.8
  ) +
  geom_text_repel(
    aes(
      label = round(estimacion, 2),
      color = curso
    ),
    size = 3.2,
    direction = "x",
    nudge_x = 0.35,
    hjust = 0,
    show.legend = FALSE,
    min.segment.length = 0
  ) +
  coord_cartesian(xlim = c(0, 10.8), clip = "off") +
  labs(
    title = "Preocupación media por la IA según estrato y curso",
    subtitle = "Cada línea une Primer curso y Otro curso dentro del mismo país y carrera",
    x = "Media estimada",
    y = NULL,
    color = "Curso"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    plot.margin = margin(10, 40, 10, 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

grafico_ia_estratos_cursos

ggsave(
  filename = "graficos_ia/grafico_ia_estratos_cursos.png",
  plot = grafico_ia_estratos_cursos,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)
