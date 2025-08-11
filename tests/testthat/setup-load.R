# Ejecuta .onLoad() si las regex/caché no están inicializadas aún
local({
  # Si estás corriendo tests con load_all(), normalmente .onLoad() ya corre.
  # Esto es un respaldo por si alguna vez no sucede.
  needs_init <- is.null(.rx_valid_letters) || is.null(.rx_vowel) ||
    is.null(.rx_strong) || is.null(.rx_weak) || is.null(.syll_cache)

  if (needs_init && exists(".onLoad")) {
    # Cambiá "readabilityES" si tu paquete tiene otro nombre
    .onLoad(getNamespace("readabilityES"), "readabilityES")
  }
})
