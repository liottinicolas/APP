# Sistema de Análisis de Incidencias

Este sistema permite procesar y analizar datos de incidencias para grúas y plumas. El script principal `carga_BD.R` inicializa la aplicación, carga los datos necesarios y proporciona funciones de utilidad para el análisis.

## Mejoras Implementadas

1. **Estructura Mejorada**
   - Código organizado en secciones claramente definidas
   - Manejo de errores robusto en todas las operaciones críticas

2. **Sistema de Configuración**
   - Configuración centralizada en `config.R`
   - Parametrización de fechas, rutas y opciones

3. **Sistema de Logging**
   - Registro detallado de operaciones
   - Niveles de log configurables (DEBUG, INFO, WARN, ERROR)
   - Archivos de log organizados por fecha y hora

4. **Manejo de Errores**
   - Captura y registro de errores
   - Mensajes de error descriptivos
   - Opciones para continuar o detener la ejecución al encontrar errores

5. **Optimización de Código**
   - Eliminación de código duplicado
   - Mejor organización de funciones
   - Documentación de funciones con roxygen2

## Estructura de Archivos

- `carga_BD.R` - Script principal del sistema
- `config.R` - Configuración centralizada
- `global.R` - Paquetes y funciones básicas
- `funciones_carga_datos.R` - Funciones para actualizar RDS
- `funciones_utiles.R` - Funciones utilitarias generales
- `funciones_para_web.R` - Funciones para la interfaz web
- `carga_datos.R` - Carga de datos principal

## Cómo Utilizar

### Configuración

Para configurar el sistema, edite el archivo `config.R`. Aquí puede ajustar:

- Fechas de análisis
- Directorios de trabajo
- Modo de ejecución (producción/desarrollo)
- Nivel de logging
- Parámetros de formato para exportaciones

### Ejecución

Para ejecutar el sistema:

1. Abra R o RStudio
2. Establezca el directorio de trabajo adecuado
3. Ejecute el script principal:
   ```R
   source("carga_BD.R")
   ```

### Modos de Ejecución

- **Desarrollo**: Muestra más información de depuración y ejecuta ejemplos
- **Producción**: Modo optimizado sin ejemplos y con logging mínimo

### Logging

Los archivos de log se guardan en el directorio `./logs/` con el formato `log_YYYYMMDD_HHMMSS.txt`.

## Funciones Principales

- `funcion_mostrar_responsables_por_incidencias()` - Analiza responsabilidades por incidencias
- `funcion_exportar_incidencias_grua_o_pluma()` - Exporta los resultados de análisis
- `funcion_imprimir_datosporgid()` - Exporta datos de un GID específico a Excel
- `funcion_obtener_datosporgid()` - Obtiene datos de un GID específico

## Notas

- El sistema está configurado con fechas futuras (2025) que deberían ajustarse para uso real
- Para añadir nuevos tipos de análisis, agregue las funciones correspondientes y actualice la configuración 