
tryCatch({
  print("Reading datos_listos.rds...")
  d1 <- readRDS("datos_listos.rds")
  print("datos_listos.rds READ OK")
  print(class(d1))
}, error = function(e) {
  print(paste("Error reading datos_listos.rds:", e$message))
})

tryCatch({
  print("Reading viajespordiayturno.rds...")
  d2 <- readRDS("viajespordiayturno.rds")
  print("viajespordiayturno.rds READ OK")
  print(class(d2))
}, error = function(e) {
  print(paste("Error reading viajespordiayturno.rds:", e$message))
})
