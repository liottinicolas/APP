
d2 <- readRDS("viajespordiayturno.rds")
print("Colnames d2:")
print(names(d2))
print("Unique Turno_levantado d2:")
if("Turno_levantado" %in% names(d2)){
  print(unique(d2$Turno_levantado))
  print(class(d2$Turno_levantado))
}
