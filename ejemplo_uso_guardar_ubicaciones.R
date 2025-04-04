##################################################
# EJEMPLO DE USO DE LA FUNCIÓN PARA GUARDAR UBICACIONES
##################################################

# Cargar librerías necesarias
library(dplyr)

# Cargar la función para guardar ubicaciones
source("guardar_ubicaciones_rds.R")

# En tu script carga_BD.R, después de obtener ubicaciones_existentes:

# 1. Este es el código existente que ya tienes
ubicaciones_existentes <- funcion_listar_ubicaciones_unicas_con_thegeom_y_sin_thegeom()

# 2. Simplemente guarda las ubicaciones en un archivo RDS (sin asignar a ninguna variable)
# La función no devuelve nada, solo guarda el archivo
guardar_ubicaciones_en_rds(ubicaciones_existentes)

# 3. Continuar con el código existente, usando la variable original
estado_diario_global <- funcion_agregar_the_geom_a_faltantes(
  historico_estado_diario,
  ubicaciones_existentes$ubicaciones_con_thegeom
)

##################################################
# PARA IMPLEMENTAR EN carga_BD.R
##################################################

# Para implementar esta funcionalidad en carga_BD.R, debes:
# 1. Añadir esta línea en la sección de carga de dependencias:
#    source("guardar_ubicaciones_rds.R")
#
# 2. Luego, dentro del tryCatch de la sección de procesamiento de datos,
#    justo después de obtener ubicaciones_existentes y antes de usarla:
#    guardar_ubicaciones_en_rds(ubicaciones_existentes)
#
# 3. No necesitas modificar nada más, puedes seguir usando
#    ubicaciones_existentes como lo haces actualmente.
################################################## 