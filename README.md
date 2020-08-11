# Taller de Consultoria - 2020


El principal objetivo del Curso es *exponer a los alumnos* a una *experiencia de consultoría real*. 
Por lo tanto, el foco está puesto en resolver un problema mediante las herramientas adquiridas en el 
transcurso del Programa. Para poner esto en practica, cada uno de los 3 módulos 
consistirá en resolver (por parte de Uds.) un problema.


## Módulo 1: El problema de la Recomendación

El escenario que ustedes deberan enfrentar es aquel en que dado un usuario, su historia de preferencias y la información de otros usuarios, se requiere entregarle "buenas" sugerencias. Estas sugerencias deben ser coherentes con los gustos del usuario y para ello deberan ser obtenidas algún tipo de criterio. *Por ejemplo*, este criterio puede considerar aquellos ítems que maximizacen de la valoración esperada por parte del usuario o que tengan información textual (tags por ejemplo) que se aleje minimamente de aquella asociada al usuario.

Ejemplos de este problema pueden observarse en muchos dominios distintos. Por ejemplo, recomendación de productos a usuarios en sitios de ventas en línea (amazon, ebay, falabella on-line ...), recomendación de contenido en plataformas multimedia que consiga mantener a los usuarios "consumiendo". El insumo (*input*) para este tipo de métodos consiste de información relativa a los usuarios, ítems, contextos de consumo y retroalimentación de los usuarios luego de interactuar con los ítems.

## Instalación de este Proyecto

* Descargar o clonar el Proyecto
* Ejecutar R. (Esto descargará automáticamente el [package `renv`](https://rstudio.github.io/renv/) 
* Dentro del intérprete se debe ejecutar: `renv::restore()`

## Conjunto de datos

Se tiene una base de datos con información de películas (título, año, genero ...) y
también de valoraciones de los usuarios (puntuación y etiquetas asignadas).

### Descarga del conjunto de datos

```r
dl <- paste(tempfile(),".zip",sep = "")
download.file("http://files.grouplens.org/datasets/movielens/ml-latest-small.zip", dl)
unzip(dl, list = TRUE) # shows the content
unzip(dl, exdir = getwd()) # extracts into the working dir.
rm(dl)
```
