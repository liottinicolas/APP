# Dashboard de Reportes de Incidencias

## Descripción

Este dashboard proporciona una vista agregada y resumida de la información contenida en el dataframe `web_historico_completo_llenado_incidencias`, permitiendo analizar los datos de llenado e incidencias de contenedores con filtros por rango de fechas.

## Características

### Filtros Disponibles
- **Rango de fechas**: Permite seleccionar un período específico para el análisis
- **Agrupación Principal**: Opciones para agrupar los datos por:
  - Fecha
  - Municipio
  - Circuito corto
  - Turno
  - Incidencia
- **Sub-agrupación**: Permite agregar una segunda categoría para análisis más detallado

### Métricas Generales
El dashboard muestra cuatro métricas principales:
1. **Total Registros**: Número total de registros en el período seleccionado
2. **Contenedores Levantados**: Cantidad de contenedores que fueron levantados (tienen porcentaje de llenado)
3. **Total Incidencias**: Número total de incidencias registradas
4. **Promedio Llenado**: Porcentaje promedio de llenado de los contenedores

### Tabla de Resumen Agregado
La tabla muestra información agregada según la agrupación seleccionada, incluyendo:
- Total de registros por grupo
- Contenedores levantados y no levantados
- Total de incidencias
- Estadísticas de llenado (promedio, mínimo, máximo)

### Funcionalidades Adicionales
- **Descarga en Excel**: Permite exportar los datos a un archivo Excel con dos hojas:
  - Resumen Agregado: Tabla con los datos agregados
  - Métricas Generales: Resumen de las métricas principales

## Cómo Usar

1. **Seleccionar rango de fechas**: Usa el selector de fechas para definir el período de análisis
2. **Elegir agrupación principal**: Selecciona la categoría principal para agrupar los datos
3. **Opcionalmente elegir sub-agrupación**: Agrega una segunda categoría para análisis más detallado
4. **Actualizar reporte**: Haz clic en "Actualizar Reporte" para aplicar los filtros
5. **Analizar datos**: Revisa las métricas generales y la tabla de resumen
6. **Descargar**: Usa el botón de descarga para exportar los datos a Excel

## Campos Incluidos

El dashboard utiliza los siguientes campos del dataframe `web_historico_completo_llenado_incidencias`:

- **Fecha**: Fecha del registro
- **Municipio**: Municipio del contenedor
- **Circuito_corto**: Circuito corto del contenedor
- **Turno**: Turno de trabajo
- **Incidencia**: Descripción de la incidencia
- **Porcentaje_llenado**: Porcentaje de llenado del contenedor

## Cálculos Realizados

### Contenedores Levantados
Se considera que un contenedor fue levantado si tiene un valor en el campo `Porcentaje_llenado` (no es NA).

### Contenedores No Levantados
Se considera que un contenedor no fue levantado si no tiene valor en el campo `Porcentaje_llenado` (es NA).

### Incidencias
Se cuenta como incidencia si el campo `Incidencia` no es NA y no está vacío.

### Estadísticas de Llenado
- **Promedio**: Media aritmética del porcentaje de llenado
- **Mínimo**: Valor mínimo del porcentaje de llenado
- **Máximo**: Valor máximo del porcentaje de llenado

## Ordenamiento Inteligente

El dashboard implementa un sistema de ordenamiento automático según el tipo de agrupación:

### **Agrupación Principal por Fecha**:
- Orden descendente (más reciente primero)

### **Agrupación Principal por Municipio, Circuito o Incidencia**:
- Ordenado por "No Levantados con Incidencia" de mayor a menor
- En caso de empate, orden alfabético por la categoría principal

### **Con Sub-agrupación**:
- **Si la principal es Fecha**: Ordena por fecha descendente, luego por "No Levantados con Incidencia"
- **Si la principal es otra**: Ordena por "No Levantados con Incidencia", luego alfabético por la principal, luego alfabético por la sub-agrupación

## Notas Técnicas

- El dashboard utiliza reactividad de Shiny para actualizar automáticamente los datos cuando cambian los filtros
- La tabla utiliza DataTables para proporcionar funcionalidades de búsqueda, ordenamiento y paginación
- Los datos se exportan en formato Excel usando el paquete `openxlsx`
- El diseño es responsivo y se adapta a diferentes tamaños de pantalla
- Se filtran automáticamente los registros con valores vacíos o NA

## Acceso

El dashboard está disponible en la aplicación principal bajo el menú "Reportes de Incidencias" con el ícono de gráfico de barras. 