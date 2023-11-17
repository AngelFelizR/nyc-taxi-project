
ZoneCodesColTypes <- c(
  "LocationID" = "integer",
  "Borough" = "character",
  "Zone" = "character",
  "service_zone" = "character"
)

custom_theme <-
  theme_light()+
  theme(plot.title = element_text(face = "bold"))

plus1_log2_trans <- trans_new(
  "log2+1",
  \(x) log2(x+1),
  \(x) 2^x - 1
)

