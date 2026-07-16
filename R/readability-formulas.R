# ---------- Estadísticas intermedias ----------

# Calcula W, S, Sy, y C (promedio de letras/palabra) para cada texto.
# Reutiliza las funciones de conteo ya existentes, calculadas una sola vez.
.readability_intermediate <- function(text, lang = syll_get_option("lang")) {
  list(
    W  = count_words(text, lang = lang),
    S  = count_sentences(text, lang = lang),
    Sy = count_syllables(text, lang = lang),
    C  = letters_per_word(text, lang = lang)
  )
}

# Mu necesita media y varianza de longitud de palabra POR documento,
# no un total agregado. Se calcula aparte porque ninguna otra fórmula
# la necesita, y no vale la pena cargar ese cómputo para todas.
.mu_stats <- function(text, lang = syll_get_option("lang")) {
  text_safe <- ifelse(is.na(text), "", text)
  toks_list <- tokenize_words(text_safe, lang = lang)

  stats <- lapply(toks_list, function(tks) {
    tks <- trimws(tks)
    tks <- tks[nzchar(tks)]
    n <- length(tks)
    if (n < 2L) return(c(n = n, mean_len = NA_real_, var_len = NA_real_))
    lens <- nchar(tks, type = "chars")
    c(n = n, mean_len = mean(lens), var_len = stats::var(lens))
  })

  n_vec <- vapply(stats, `[[`, numeric(1), "n")

  n_short <- sum(n_vec < 2L, na.rm = TRUE)
  if (n_short > 0L) {
    warning(
      sprintf(
        "Mu no puede calcularse para %d de %d texto(s): se necesitan al menos 2 palabras para estimar la varianza de longitud. Esos textos quedan como NA en 'mu'.",
        n_short, length(n_vec)
      ),
      call. = FALSE
    )
  }

  list(
    n        = n_vec,
    mean_len = vapply(stats, `[[`, numeric(1), "mean_len"),
    var_len  = vapply(stats, `[[`, numeric(1), "var_len")
  )
}

# ---------- Fórmulas puras: f(...) -> numeric ----------
# Cada una recibe exactamente lo que necesita, nada más. No leen texto,
# no tokenizan, no saben de idiomas — son cálculo puro sobre números ya
# agregados por .readability_intermediate()/.mu_stats().

.formula_szigriszt_pazos <- function(W, S, Sy) {
  out <- 206.835 - 62.3 * (Sy / pmax(W, 1)) - (W / pmax(S, 1))
  out[is.na(W) | is.na(S) | is.na(Sy)] <- NA_real_
  out
}

.formula_gutierrez_de_polini <- function(W, S, C) {
  # C = promedio de LETRAS por palabra (no sílabas) - corregido, ver
  # discusión: la fórmula original usa letras, el código previo usaba
  # sílabas por error.
  out <- 95.2 - 9.7 * C - 0.35 * (W / pmax(S, 1))
  out[is.na(W) | is.na(S) | is.na(C)] <- NA_real_
  out
}

.formula_fernandez_huerta <- function(W, S, Sy) {
  out <- 206.84 - 60 * (Sy / pmax(W, 1)) - 102 * (S / pmax(W, 1))
  out[is.na(W) | is.na(S) | is.na(Sy)] <- NA_real_
  out
}

.formula_crawford <- function(W, S, Sy) {
  # P̄ (frases por 100 palabras) redondeado al décimo más cercano,
  # tal como especifica la fórmula original.
  p_bar <- round((S / pmax(W, 1)) * 100, 1)
  s_bar <- (Sy / pmax(W, 1)) * 100
  out <- -0.205 * p_bar + 0.049 * s_bar - 3.407
  out[is.na(W) | is.na(S) | is.na(Sy)] <- NA_real_
  out
}

.formula_mu <- function(n, mean_len, var_len) {
  out <- (n / pmax(n - 1, 1)) * (mean_len / var_len) * 100
  out[is.na(n) | is.na(mean_len) | is.na(var_len) | var_len == 0] <- NA_real_
  out
}

# ---------- Categorizaciones (internas, no exportadas) ----------

.inflesz_category <- function(score) {
  out <- rep(NA_character_, length(score))
  out[!is.na(score) & score < 40] <- "Muy dif\u00edcil"
  out[!is.na(score) & score >= 40 & score <= 55] <- "Algo dif\u00edcil"
  out[!is.na(score) & score > 55 & score <= 65] <- "Normal"
  out[!is.na(score) & score > 65 & score <= 80] <- "Bastante f\u00e1cil"   # corregido
  out[!is.na(score) & score > 80] <- "Muy f\u00e1cil"
  out
}

.mu_category <- function(score) {
  out <- rep(NA_character_, length(score))
  out[!is.na(score) & score >= 91] <- "Muy F\u00e1cil"
  out[!is.na(score) & score >= 81  & score <= 90] <- "F\u00e1cil"
  out[!is.na(score) & score >= 71  & score <= 80] <- "Un poco f\u00e1cil"
  out[!is.na(score) & score >= 61  & score <= 70] <- "Adecuado"
  out[!is.na(score) & score >= 51  & score <= 60] <- "Un poco dif\u00edcil"
  out[!is.na(score) & score >= 31  & score <= 50] <- "Dif\u00edcil"
  out[!is.na(score) & score < 31] <- "Muy dif\u00edcil"
  out
}

# ---------- Registro central de fórmulas ----------
# Cada entrada sabe: cómo calcularse, qué idiomas soporta, si tiene
# categorización opcional, y su referencia bibliográfica.

.readability_formula_registry <- list(
  szigriszt_pazos = list(
    inputs      = c("W", "S", "Sy"),
    fn          = .formula_szigriszt_pazos,
    langs       = "es",
    category_fn = .inflesz_category,
    reference   = "PLACEHOLDER: Szigriszt-Pazos (1993)"
  ),
  gutierrez_de_polini = list(
    inputs      = c("W", "S", "C"),
    fn          = .formula_gutierrez_de_polini,
    langs       = "es",
    category_fn = NULL,
    reference   = "PLACEHOLDER: Guti\u00e9rrez de Polini (1972)"
  ),
  fernandez_huerta = list(
    inputs      = c("W", "S", "Sy"),
    fn          = .formula_fernandez_huerta,
    langs       = "es",
    category_fn = NULL,
    reference   = "PLACEHOLDER: Fern\u00e1ndez-Huerta"
  ),
  crawford = list(
    inputs      = c("W", "S", "Sy"),
    fn          = .formula_crawford,
    langs       = "es",
    category_fn = NULL,
    reference   = "PLACEHOLDER: Crawford (1984)"
  ),
  mu = list(
    inputs      = c("n", "mean_len", "var_len"),   # viene de .mu_stats(), no de .readability_intermediate()
    fn          = .formula_mu,
    langs       = "es",
    category_fn = .mu_category,
    reference   = "PLACEHOLDER: Mu\u00f1oz y Mu\u00f1oz (2006)"
  )
)

#' Referencia bibliográfica de un índice de legibilidad
#'
#' @param index Nombre del índice (p. ej. `"szigriszt_pazos"`). Si es
#'   `NULL`, devuelve las referencias de todos los índices registrados.
#' @return Un character (si `index` no es `NULL`) o un named character
#'   vector con todas las referencias.
#' @examples
#' readability_reference("crawford")
#' readability_reference()
#' @export
readability_reference <- function(index = NULL) {
  if (is.null(index)) {
    return(vapply(.readability_formula_registry, `[[`, character(1), "reference"))
  }
  entry <- .readability_formula_registry[[index]]
  if (is.null(entry)) {
    stop(sprintf("\u00cdndice desconocido: '%s'. Usar uno de: %s.",
                 index, paste(names(.readability_formula_registry), collapse = ", ")),
         call. = FALSE)
  }
  entry$reference
}
