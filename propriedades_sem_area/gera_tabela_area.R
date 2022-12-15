# Script para checar area e preco de propriedades faltantes

library(stringr)
library(sf)

BASE_CRS <- "+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"

# IDS (vindos do email do Rafa/Bocca)

# Propriedades sem área:
# ID Origem: TAR_014, SEN_001, CZS_003, CZS_004, CZS_005, CZS_008, SFX_001, GAN_129
# ID re.green: EXT-AC-0016, EXT-AC-0017, EXT-AC-0019, EXT-AC-0020, EXT-AC-0021, EXT-AC-0024, EXT-PA-0044, EXT-BA-0362

# Propriedade sem preço (e sem preço médio do cluster):
# ID Origem: SFX_001 
# ID re.green: EXT-PA-0044

ids_ac <- c("TAR_014", "SEN_001", "CZS_003", "CZS_004", "CZS_005", "CZS_008")
ids_pa <- "SFX_001"
ids_ba <- "GAN_129"

path_ac <- paste0("/DATA/projetos/bd_terras/shapefile/acre_tmp")

shp_index <- sapply(ids_ac, function(x) which(str_detect(list.files(path_ac, full.names = TRUE), x)))
which(str_detect(list.files(path_ac, full.names = TRUE), ids_pa))
which(str_detect(list.files(path_ac, full.names = TRUE), ids_ba))

df <- data.frame(id_origem = names(shp_index), index = shp_index)
df$area_shp <- NA

for(i in 1:nrow(df)) {
  shp <- st_read(list.files(path_ac, full.names = TRUE)[df$index[i]]) %>%
    st_transform(st_crs(BASE_CRS))
  area_shp <- as.numeric(sum(st_area(shp)) * 0.0001)
  df[i, "area_shp"] <- area_shp
}

df <- df[, c(1, 3)]

write.csv2(df, "propriedades_sem_area/propriedades_sem_area.csv", row.names = FALSE)
