
<!-- README generado con usethis::use_readme_rmd() y adaptado -->

# readabilityes

<!-- badges: start -->

[![R-CMD-check](https://github.com/JFormoso/readabilityes/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JFormoso/readabilityes/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## 🇪🇸 Introducción (Español)

**readabilityes** ofrece herramientas para **tokenizar**, **segmentar**
y **medir la legibilidad** de textos en español. Incluye:

- Segmentación de **oraciones** respetando abreviaturas (p. ej., *Sr.*,
  *Dra.*), iniciales (*J. R. R.*), elipsis y decimales.
- Tokenización de **palabras** con control de minúsculas, tildes,
  puntuación, guiones, números y símbolos/emoji.
- Conteos básicos: **palabras**, **oraciones**, **sílabas** (aprox. ES).
- Métricas promedio por palabra (**sílabas/letras/caracteres**).
- **Índices de legibilidad** usados en español:
  - **Szigriszt–Pazos** (`szigriszt_pazos()`).
  - **INFLESZ** (`inflesz()`, devuelve puntaje + categoría).
  - **Gutiérrez de Polini** (`gutierrez_de_polini()`).

### Instalación

Versión de desarrollo desde GitHub:

``` r
# install.packages("devtools")
devtools::install_github("JFormoso/readabilityes")
```

### Ejemplo rápido

``` r
txt <- c(
  "Hola. ¿Cómo estás? ¡Todo bien!",
  "La legibilidad facilita la comprensión del texto por parte de las personas lectoras."
)

# Oraciones y palabras
segment_sentences(txt)
#> [[1]]
#> [1] "Hola."        "¿Cómo estás?" "¡Todo bien!" 
#> 
#> [[2]]
#> [1] "La legibilidad facilita la comprensión del texto por parte de las personas lectoras."
count_sentences(txt, drop_empty = TRUE)
#> [1] 3 1
tokenize_words(txt)
#> [[1]]
#> [1] "hola"  "cómo"  "estás" "todo"  "bien" 
#> 
#> [[2]]
#>  [1] "la"          "legibilidad" "facilita"    "la"          "comprensión"
#>  [6] "del"         "texto"       "por"         "parte"       "de"         
#> [11] "las"         "personas"    "lectoras"

# Sílabas y promedios
count_syllables(txt)
#> [1]  9 28
avg_syllables_per_word(txt)
#> [1] 1.800000 2.153846

# Índices de legibilidad
szigriszt_pazos(txt)
#> [1] 93.02833 59.65038
inflesz(txt)                 
#>      score  category
#> 1 93.02833 Muy fácil
#> 2 59.65038    Normal
gutierrez_de_polini(txt)
#> [1] 77.15667 69.75769
```

------------------------------------------------------------------------

## 🇬🇧 Introduction (English)

**readabilityes** provides tools to **tokenize**, **segment**, and
**assess readability** for Spanish-language texts. It includes:

- **Sentence** segmentation that respects abbreviations (e.g., *Sr.*,
  *Dra.*), initials (*J. R. R.*), ellipses, and decimals.
- **Word** tokenization with controls for lowercasing, accents,
  punctuation, hyphens, numbers, and symbols/emoji.
- Basic counts: **words**, **sentences**, **syllables** (approx. ES).
- Per-word averages (**syllables/letters/characters**).
- **Readability indices** commonly used in Spanish:
  - **Szigriszt–Pazos** (`szigriszt_pazos()`).
  - **INFLESZ** (`inflesz()`, returns score + category).
  - **Gutiérrez de Polini** (`gutierrez_de_polini()`).

### Installation

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("JFormoso/readabilityes")
```

### Quick example

``` r
txt <- c(
  "Hola. ¿Cómo estás? ¡Todo bien!",
  "La legibilidad facilita la comprensión del texto por parte de las personas lectoras."
)

# Sentences and words
segment_sentences(txt)
#> [[1]]
#> [1] "Hola."        "¿Cómo estás?" "¡Todo bien!" 
#> 
#> [[2]]
#> [1] "La legibilidad facilita la comprensión del texto por parte de las personas lectoras."
count_sentences(txt, drop_empty = TRUE)
#> [1] 3 1
tokenize_words(txt)
#> [[1]]
#> [1] "hola"  "cómo"  "estás" "todo"  "bien" 
#> 
#> [[2]]
#>  [1] "la"          "legibilidad" "facilita"    "la"          "comprensión"
#>  [6] "del"         "texto"       "por"         "parte"       "de"         
#> [11] "las"         "personas"    "lectoras"

# Syllables and averages
count_syllables(txt)
#> [1]  9 28
avg_syllables_per_word(txt)
#> [1] 1.800000 2.153846

# Readability indices
szigriszt_pazos(txt)
#> [1] 93.02833 59.65038
inflesz(txt)                  
#>      score  category
#> 1 93.02833 Muy fácil
#> 2 59.65038    Normal
gutierrez_de_polini(txt)
#> [1] 77.15667 69.75769
```
