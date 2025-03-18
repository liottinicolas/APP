
# Funciones relacionadas a las incidencias
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
                       "Requiere_accion",   # Sobrepeso                  # Grua
                       "Levantar",          # Auto                       # Auto, hacer planilla x mes para ver si repite
                       "Requiere_accion",   # Calle Cerrada
                       "Requiere_accion",   # Tapa Bloqueda               # mantenimiento? correo a
                       "Levantar",          # Persona en el Interior del Cont.
                       "Levantar",          # Capacidad del Camion y/o Tiempo
                       "Levantar",          # Sin ticket de cantera
                       "Requiere_accion",   # Contenedor Roto (choque, desfonde, etc.)  # mantenimiento y operativa
                       "Levantar",   # Fuego
                       "Requiere_accion",   # Contenedor No Está                  # SPP
                       "Requiere_accion",   # Contenedor Fuera de Alcance              # PLUMA
                       "Requiere_accion",   # Contenedor Volcado                  # PLUMA
                       "Requiere_accion",   # Contenedor Cruzado                  # PLUMA
                       "Requiere_accion",   # Buzonera Girada                     # PLUMA
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
    "Grua",   # Sobrepeso                  # Grua
    "Seguimiento SPP",          # Auto                       # Auto, hacer planilla x mes para ver si repite
    "Seguimiento SPP",   # Calle Cerrada
    "Mantenimiento",   # Tapa Bloqueda               # mantenimiento? correo a
    "Operativa",          # Persona en el Interior del Cont.
    "Operativa",          # Capacidad del Camion y/o Tiempo
    "Operativa",          # Sin ticket de cantera
    "Mantenimiento",   # Contenedor Roto (choque, desfonde, etc.)  # mantenimiento y operativa
    "Operativa",   # Fuego
    "Seguimiento SPP",   # Contenedor No Está                  # SPP
    "Pluma",   # Contenedor Fuera de Alcance              # PLUMA
    "Pluma",   # Contenedor Volcado                  # PLUMA
    "Pluma",   # Contenedor Cruzado                  # PLUMA
    "Pluma",   # Buzonera Girada                     # PLUMA
    "Seguimiento SPP",   # Otros
    "Operativa"           # Viaje suspendido
  )
)

cargar_opciones_operativa <- function() {
  df_incidencias %>% filter(responsable != "Seguimiento SPP" & responsable != "Sin responsable") %>% pull(descripcion)
}

cargar_opciones_spp <- function() {
  df_incidencias %>% filter(responsable == "Seguimiento SPP") %>% pull(descripcion)
}

