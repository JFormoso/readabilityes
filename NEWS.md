# readabilityes 0.1.0 (2025-08-11)

## What's new

* **Sentence segmentation**

  * `segment_sentences()`: protects abbreviations (e.g., *Dr.*, *etc.*), initials (*J. R. R.*), decimals, and ellipses; supports `extra_abbr`, `keep_delim`, `normalize_spacing`.
  * `count_sentences()`: counts sentences per element, with `na_as_zero` and `drop_empty`.

* **Tokenization and counts**

  * `tokenize_words()`: options `lowercase`, `keep_accents`, `strip_punct`, `keep_hyphens`, `remove_numbers`, **`strip_symbols`**, **`flatten`**.
  * `count_words()`: token count per element (defensive flag normalization with `isTRUE()`).

* **Syllabification (ES)**

  * `syll_split()`, `syll_count()`, `syll_hyphenate()`: deterministic core with diphthongs/triphthongs and C+l/r clusters; **optional cache** via `syll_cache_info()` / `syll_cache_clear()`.

* **Average metrics**

  * `avg_syllables_per_word()`, `letters_per_word()`, `chars_per_word()`.

* **Readability indexes**

  * `szigriszt_pazos()`, `inflesz()` + `inflesz_category()`, `gutierrez_de_polini()`.

## Changes and fixes

* **Segmentation regex**: replaced variable-length lookbehind with a PCRE-compatible pattern to avoid
  `lookbehind assertion is not fixed length`.
* `count_words()` and derived functions: defensive coercion of logical flags (`isTRUE()`).
* Documentation updated: new `strip_symbols` and `flatten` arguments in `tokenize_words()`.
* Expanded testthat suite and README examples in ES/EN.

## Compatibility

* No known breaking changes in this initial release.

