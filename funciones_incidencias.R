# nolint start: line_length_linter, object_name_linter


# Funciones para manejo de incidencias y responsables

#' Carga las opciones de incidencias para un responsable específico
#' @param nombre_responsable Nombre del responsable
#' @return Vector con las descripciones de incidencias
cargar_opciones_responsable <- function(nombre_responsable){
  df_incidencias <- data.frame(
    motivo_no_levante = 0:26,
    descripcion = c(
      "Levantado",
      "Rotura con retorno a circuito",
      "Rotura sin retorno a circuito",
      "Horas permiso auxiliar",
      "Horas permiso chofer",
      "Horas permiso aux y chof",
      "Medidas gremiales",
      "Habilitado tarde (Mantenimiento)",
      "Demora en cantera",
      "Camion a Lavadero",
      "No Levantado por Feria",
      "Sobrepeso",
      "Auto",
      "Calle Cerrada",
      "Tapa Bloqueda",
      "Persona en el Interior del Cont.",
      "Capacidad del Camion y/o Tiempo",
      "Sin ticket de cantera",
      "Contenedor Roto (choque, desfonde, etc.)",
      "Fuego",
      "Contenedor No Está",
      "Contenedor Fuera de Alcance",
      "Contenedor Volcado",
      "Contenedor Cruzado",
      "Buzonera Girada",
      "Otros",
      "Viaje suspendido"
    ),
    accion_requerida = c("Sin_accion",        # Levantado
                         "Levantar",          # Rotura con retorno a circuito
                         "Levantar",          # Rotura sin retorno a circuito
                         "Levantar",          # Horas permiso auxiliar
                         "Levantar",          # Horas permiso chofer
                         "Levantar",          # Horas permiso aux y chof
                         "Levantar",          # Medidas gremiales
                         "Levantar",          # Habilitado tarde (Mantenimiento)
                         "Levantar",          # Demora en cantera
                         "Levantar",          # Camion a Lavadero
                         "Levantar",          # No Levantado por Feria 
                         "Requiere_accion",   # Sobrepeso
                         "Levantar",          # Auto
                         "Requiere_accion",   # Calle Cerrada
                         "Requiere_accion",   # Tapa Bloqueda
                         "Levantar",          # Persona en el Interior del Cont.
                         "Levantar",          # Capacidad del Camion y/o Tiempo
                         "Levantar",          # Sin ticket de cantera
                         "Requiere_accion",   # Contenedor Roto
                         "Levantar",          # Fuego
                         "Requiere_accion",   # Contenedor No Está
                         "Requiere_accion",   # Contenedor Fuera de Alcance
                         "Requiere_accion",   # Contenedor Volcado
                         "Requiere_accion",   # Contenedor Cruzado
                         "Requiere_accion",   # Buzonera Girada
                         "Requiere_accion",   # Otros
                         "Levantar"           # Viaje suspendido
    ),
    responsable = c(
      "Sin responsable",        # Levantado
      "Operativa",          # Rotura con retorno a circuito
      "Operativa",          # Rotura sin retorno a circuito
      "Operativa",          # Horas permiso auxiliar
      "Operativa",          # Horas permiso chofer
      "Operativa",          # Horas permiso aux y chof
      "Operativa",          # Medidas gremiales
      "Operativa",          # Habilitado tarde (Mantenimiento)
      "Operativa",          # Demora en cantera
      "Operativa",          # Camion a Lavadero
      "Operativa",          # No Levantado por Feria 
      "Grua",               # Sobrepeso
      "Seguimiento SPP",    # Auto
      "Seguimiento SPP",    # Calle Cerrada
      "Mantenimiento",      # Tapa Bloqueda
      "Operativa",          # Persona en el Interior del Cont.
      "Operativa",          # Capacidad del Camion y/o Tiempo
      "Operativa",          # Sin ticket de cantera
      "Mantenimiento",      # Contenedor Roto
      "Operativa",          # Fuego
      "Seguimiento SPP",    # Contenedor No Está
      "Pluma",              # Contenedor Fuera de Alcance
      "Pluma",              # Contenedor Volcado
      "Pluma",              # Contenedor Cruzado
      "Pluma",              # Buzonera Girada
      "Seguimiento SPP",    # Otros
      "Operativa"           # Viaje suspendido
    )
  )
  
  df_incidencias <- df_incidencias %>% 
    filter(responsable == nombre_responsable) %>% 
    pull(descripcion)
  
  return(df_incidencias)
}

#' Carga las opciones de condiciones disponibles
#' @return Vector con las descripciones de condiciones
cargar_opciones_condiciones <- function(){
  df_condiciones <- data.frame(
    id_condicion = 0:7,
    descripcion_condicion = c(
      "Todos",
      "Basura afuera",
      "Dos ciclos",
      "Escombro",
      "Fuera de lugar",
      "Poda",
      "Requiere limpieza",
      "Requiere Mantenimiento"
    )
  ) %>% 
    pull(descripcion_condicion)
  
  return(df_condiciones)
}

# Variables globales para opciones de incidencias
opciones <- cargar_opciones_responsable("Pluma")
opciones_filtradas_grua <- opciones[opciones != "Contenedor Volcado"] 


# nolint end