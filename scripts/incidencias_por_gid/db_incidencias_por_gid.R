source("scripts/incidencias_por_gid/funciones_db_incidencias_por_gid.R")

# Definir la ruta donde se guardará el archivo .rds del histórico
archivords_historico_incidencias_completas <- paste0(ruta_proyecto, "/scripts/incidencias_por_gid/historico_incidencias_completas.rds")

# Cargar el histórico anterior si existe, de lo contrario usar solo los datos nuevos
historico_incidencias_completas <- if (file.exists(archivords_historico_incidencias_completas)) {
  # Cargar el histórico previamente guardado
  readRDS(archivords_historico_incidencias_completas)
} else {
  character(0)
  }

## Si existe el historial.
## No es la primera vez, entonces hay que revisar si se agrega o no
if(length(historico_incidencias_completas) > 0){
  fecha_maxima_estado_diario <- fecha_mas_alta_estado_diario()
  fecha_maxima_historicoIncidencias <- max(historico_incidencias_completas$Dia_incidencia)
  
  # sI HAY QUE AGREGAR ELEMENTOS
  # Aca verifico que la ultima fecha del df con las direcciones, sea igual a la ultima fecha del estadoDiario.
  if(fecha_maxima_historicoIncidencias + 1 < fecha_maxima_estado_diario){
    fecha_inicio <- fecha_maxima_historicoIncidencias + 1
    
    # Si es igual
    while (fecha_inicio <= fecha_maxima_estado_diario) {
      
      ## cargo el informe diario.
      informe_por_dia <- unir_incidencias_por_gid(day(fecha_inicio),month(fecha_inicio),year(fecha_inicio),historico_llenado,historico_incidencias)
      
      historico_incidencias_completas <-  rbind(historico_incidencias_completas, informe_por_dia)
      
      fecha_inicio <- fecha_inicio + 1
      
    }
    
    historico_incidencias_completas <- historico_incidencias_completas %>% 
      mutate(Turno_viaje = factor(Turno_viaje, levels = c("MATUTINO", "VESPERTINO", "NOCTURNO")))
    
    historico_incidencias_completas <- historico_incidencias_completas %>% 
      arrange(desc(Dia_incidencia),Nomenclatura,Posicion,desc(Turno_viaje),Fecha_hora_levante) %>% 
      distinct()
    
    
    saveRDS(historico_incidencias_completas, file = archivords_historico_incidencias_completas)
    
    
  }
  
} else {
  
  # Primera vez
  
  fecha_inicio <- as.Date("2024-10-10")
  # Creo un historico vacio (es la primera vez)
  historico_incidencias_completas <- data.frame()

  # Fecha fin, es el último dato que tiene el estado diario.
  fecha_fin <- max(historico_estado_diario$Fecha)
  
  while (fecha_inicio <= fecha_fin) {
    
    ## cargo el informe diario.
    llenado_por_dia <- unir_incidencias_por_gid(day(fecha_inicio),month(fecha_inicio),year(fecha_inicio),historico_llenado,historico_incidencias)
    
    historico_incidencias_completas <-  rbind(historico_incidencias_completas, llenado_por_dia)
    
    fecha_inicio <- fecha_inicio + 1

  }
  
  historico_incidencias_completas <- historico_incidencias_completas %>% 
    mutate(Turno_viaje = factor(Turno_viaje, levels = c("MATUTINO", "VESPERTINO", "NOCTURNO")))
  
  historico_incidencias_completas <- historico_incidencias_completas %>% 
    arrange(desc(Dia_incidencia),Nomenclatura,Posicion,desc(Turno_viaje),Fecha_hora_levante) %>% 
    distinct()
  
  
  saveRDS(historico_incidencias_completas, file = archivords_historico_incidencias_completas)
  
  
}



