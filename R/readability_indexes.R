#' Índices de legibilidad para textos en español
#'
#' Este archivo implementa funciones para calcular distintos índices de
#' legibilidad usados en español:
#' - \code{szigriszt_pazos()}: Índice de Perspicuidad de Szigriszt-Pazos.
#' - \code{inflesz()}: devuelve el puntaje Szigriszt-Pazos y su categoría según la
#'   escala INFLESZ (ver \code{inflesz_category()}).
#' - \code{gutierrez_de_polini()}: Fórmula de Gutiérrez de Polini.
#' - \code{flesch_es()} (opcional): Flesch Reading Ease aplicado a conteos del texto.
#' - \code{flesch_kincaid_grade_es()} (opcional): Flesch–Kincaid Grade Level aplicado al texto.
#'
#' Todas las funciones aceptan texto sin procesar y delegan los conteos en
#' \code{\link{count_words}}, \code{\link{count_sentences}} y
#' \code{\link{count_syllables}}.
#'
#' @section Notas importantes:
#' - INFLESZ es una escala de interpretación del índice de Szigriszt-Pazos,
#'   con 5 niveles de dificultad. No es una fórmula distinta.
#' - Flesch y Flesch–Kincaid son fórmulas desarrolladas para inglés.
#'   Aquí se ofrecen solo como referencia aplicada a un texto en español; su validez
#'   psicométrica en español no está garantizada.
#'
#' @references
#' - Szigriszt-Pazos: P = 206.835 - 62.3 * (Sy / W) - (W / S).
#'   Donde Sy = total de sílabas, W = total de palabras, S = total de oraciones.
#'   Véase, por ejemplo, el artículo “The Szigriszt-Pazos Perspicuity Index”
#'   en spanishreadability.com.
#' - INFLESZ (categorías): Difícil (P < 40), Algo difícil (40–55),
#'   Normal (55–65), Fácil (65–80), Muy fácil (P > 80).
#' - Gutiérrez de Polini: G = 95.2 - 9.7 * (Sy / W) - 0.35 * (W / S).
#' - Flesch (inglés): FRE = 206.835 - 1.015 * (W / S) - 84.6 * (Sy / W).
#' - Flesch–Kincaid (inglés): FKGL = 0.39 * (W / S) + 11.8 * (Sy / W) - 15.59.
#'
#' @name readability_indexes
NULL

# --- Helper interno ---------------------------------------------------------

# Devuelve una lista con conteos {W,Sy,S} para el texto.
# No exportar.
.readability_counts <- function(text, na_action = c("propagate", "zero"), ...) {
  na_action <- match.arg(na_action)
  na_as_zero <- identical(na_action, "zero")

  W  <- count_words(text, na_as_zero = na_as_zero, ...)
  S  <- count_sentences(text, na_as_zero = na_as_zero)
  Sy <- count_syllables(text, na_as_zero = na_as_zero, ...)

  list(W = W, S = S, Sy = Sy)
}

# --- Szigriszt-Pazos e INFLESZ ---------------------------------------------

#' Índice de Perspicuidad de Szigriszt-Pazos
#'
#' Calcula el índice de perspicuidad (lecturabilidad) propuesto por
#' Szigriszt-Pazos: \deqn{P = 206.835 - 62.3 \cdot (Sy/W) - (W/S)}
#' donde \eqn{Sy} es el número total de sílabas, \eqn{W} el de palabras y
#' \eqn{S} el de oraciones.
#'
#' @param text Vector de caracteres con el/los texto(s) a analizar.
#' @param ... Parámetros que se pasan a las funciones de conteo/tokenización
#'   (p. ej., \code{keep_accents}, \code{strip_punct}, etc.).
#'
#' @return Un vector numérico con el puntaje Szigriszt-Pazos.
#' @examples
#' szigriszt_pazos("Este es un texto de ejemplo sencillo.")
#' @export
szigriszt_pazos <- function(text, ...) {
  counts <- .readability_counts(text, na_action = "propagate", ...)
  W <- counts$W; S <- counts$S; Sy <- counts$Sy

  out <- 206.835 - 62.3 * (Sy / pmax(W, 1)) - (W / pmax(S, 1))
  out[is.na(W) | is.na(S) | is.na(Sy)] <- NA_real_
  out
}

#' Escala INFLESZ (categorías)
#'
#' Asigna la categoría cualitativa de la escala INFLESZ al puntaje de
#' Szigriszt-Pazos. Umbrales:
#' \itemize{
#'   \item "Difícil": P menor que 40.
#'   \item "Algo difícil": P entre 40 y 55 (inclusive).
#'   \item "Normal": P mayor que 55 y hasta 65 (inclusive).
#'   \item "Fácil": P mayor que 65 y hasta 80 (inclusive).
#'   \item "Muy fácil": P mayor que 80.
#' }
#'
#' @param score Vector numérico con puntajes Szigriszt-Pazos.
#' @return Un vector de caracteres con la categoría INFLESZ.
#' @examples
#' p <- szigriszt_pazos("Texto breve y sencillo.")
#' inflesz_category(p)
#' @export
inflesz_category <- function(score) {
  out <- rep(NA_character_, length(score))
  out[!is.na(score) & score < 40]                  <- "Dif\u00edcil"
  out[!is.na(score) & score >= 40 & score <= 55]   <- "Algo dif\u00edcil"
  out[!is.na(score) & score > 55 & score <= 65]    <- "Normal"
  out[!is.na(score) & score > 65 & score <= 80]    <- "F\u00e1cil"
  out[!is.na(score) & score > 80]                  <- "Muy f\u00e1cil"
  out
}

#' Puntaje INFLESZ (puntaje + categoría)
#'
#' Calcula el puntaje de Szigriszt-Pazos y devuelve un \code{data.frame}
#' con el puntaje y su categoría INFLESZ.
#'
#' @param text Vector de caracteres con el/los texto(s) a analizar.
#' @param ... Parámetros para las funciones de conteo/tokenización.
#'
#' @return Un \code{data.frame} con columnas \code{score} (numérico) y
#'   \code{category} (factor con los niveles INFLESZ en orden de dificultad).
#' @examples
#' inflesz("Este es un texto de ejemplo sencillo.")
#' @export
inflesz <- function(text, ...) {
  p <- szigriszt_pazos(text, ...)
  cat <- inflesz_category(p)
  category <- factor(cat,
                     levels = c("Dif\u00edcil", "Algo dif\u00edcil", "Normal", "F\u00e1cil", "Muy f\u00e1cil"),
                     ordered = TRUE)
  data.frame(score = p, category = category)
}

# --- Gutiérrez de Polini ----------------------------------------------------

#' Fórmula de Gutiérrez de Polini
#'
#' Calcula el índice de Gutiérrez de Polini:
#' \deqn{G = 95.2 - 9.7 \cdot (Sy/W) - 0.35 \cdot (W/S)}
#' donde \eqn{Sy} es el número total de sílabas, \eqn{W} el de palabras y
#' \eqn{S} el de oraciones.
#'
#' @param text Vector de caracteres con el/los texto(s) a analizar.
#' @param ... Parámetros que se pasan a las funciones de conteo/tokenización.
#'
#' @return Un vector numérico con el puntaje de Gutiérrez de Polini.
#' @examples
#' gutierrez_de_polini("Este es un texto de ejemplo sencillo.")
#' @export
gutierrez_de_polini <- function(text, ...) {
  counts <- .readability_counts(text, na_action = "propagate", ...)
  W <- counts$W; S <- counts$S; Sy <- counts$Sy

  out <- 95.2 - 9.7 * (Sy / pmax(W, 1)) - 0.35 * (W / pmax(S, 1))
  out[is.na(W) | is.na(S) | is.na(Sy)] <- NA_real_
  out
}

# --- Flesch (opcional, aplicado) --------------------------------------------

#' Flesch Reading Ease (aplicado al texto)
#'
#' Calcula el puntaje Flesch Reading Ease original (inglés) sobre los conteos
#' del texto: \deqn{FRE = 206.835 - 1.015\cdot (W/S) - 84.6\cdot (Sy/W)}.
#' \strong{Advertencia:} esta fórmula fue desarrollada para inglés; su validez en
#' español no está garantizada.
#'
#' @param text Vector de caracteres con el/los texto(s) a analizar.
#' @param ... Parámetros que se pasan a las funciones de conteo/tokenización.
#'
#' @return Un vector numérico con el puntaje Flesch Reading Ease.
#' @examples
#' flesch_es("Ejemplo de texto para estimar Flesch (no validado para ES).")
#' @export
flesch_es <- function(text, ...) {
  counts <- .readability_counts(text, na_action = "propagate", ...)
  W <- counts$W; S <- counts$S; Sy <- counts$Sy

  out <- 206.835 - 1.015 * (W / pmax(S, 1)) - 84.6 * (Sy / pmax(W, 1))
  out[is.na(W) | is.na(S) | is.na(Sy)] <- NA_real_
  out
}

#' Flesch–Kincaid Grade Level (aplicado al texto)
#'
#' Calcula el grado escolar estimado según Flesch–Kincaid (inglés) sobre los
#' conteos del texto: \deqn{FKGL = 0.39\cdot (W/S) + 11.8\cdot (Sy/W) - 15.59}.
#' \strong{Advertencia:} desarrollado para inglés; su validez en español no está
#' garantizada.
#'
#' @param text Vector de caracteres con el/los texto(s) a analizar.
#' @param ... Parámetros que se pasan a las funciones de conteo/tokenización.
#'
#' @return Un vector numérico con el grado estimado (escala de EE. UU.).
#' @examples
#' flesch_kincaid_grade_es("Ejemplo de texto para FKGL (no validado para ES).")
#' @export

flesch_kincaid_grade_es <- function(text, ...) {
  counts <- .readability_counts(text, na_action = "propagate", ...)
  W <- counts$W; S <- counts$S; Sy <- counts$Sy

  out <- 0.39 * (W / pmax(S, 1)) + 11.8 * (Sy / pmax(W, 1)) - 15.59
  out[is.na(W) | is.na(S) | is.na(Sy)] <- NA_real_
  out
}
