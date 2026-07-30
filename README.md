# Modelos AFT Weibull para tiempos de producción de contenido farmacéutico

Trabajo de grado — Universidad Nacional de Colombia, Departamento de Estadística (2026).

## Descripción

Implementación de modelos de tiempo de falla acelerado (AFT) con distribución Weibull
para el análisis de tiempos de producción de contenido farmacéutico, utilizando datos
simulados calibrados a partir de un proceso real del equipo GCS de AstraZeneca en
Latinoamérica.

## Estructura del repositorio

| Carpeta        | Contenido                                              |
|----------------|--------------------------------------------------------|
| `R/`           | Script principal del análisis                          |
| `datos/`       | Datos simulados y modelos exportados (.rds)            |
| `resultados/`  | Tablas de coeficientes, métricas y diagnósticos (.csv) |
| `figuras/`     | Visualizaciones generadas por el análisis              |
| `documento/`   | Código fuente LaTeX del trabajo de grado               |

## Requisitos

- R ≥ 4.3
- Paquetes: `tidyverse`, `survival`, `flexsurv`, `MASS`, `fitdistrplus`,
  `survminer`, `viridis`, `broom`, `knitr`

## Reproducibilidad

```r
# Clonar el repositorio y ejecutar desde la raíz del proyecto:
source("R/analisis_aft_weibull.R")
