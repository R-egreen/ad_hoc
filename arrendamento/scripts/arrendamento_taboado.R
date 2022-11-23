# Script para analise de arrendamento
# Primeira versao 2022-11-22

devtools::load_all("/DATA/pessoal/sara/pkg/spatial/")
devtools::load_all("/DATA/pessoal/sara/pkg/finance/")


farm_url <- "arrendamento/data/raw/FAZENDA TABOADO - Parte da Gleba 44 - CONCEIÇÃO DO ARAGUAIA-PA.kmz"
rasters_url <- "/DATA/pessoal/sara/elo_workflow/data/rasters/AMZ/land_use/"
car_url <- "/DATA/projetos/bd_terras/CAR_municipios/"

farm_url2 <- "arrendamento/data/raw/FAZENDA ESTRÊLA D'ALVA - Lote 104 - Gleba AlaciLândia - SANTA MARIA DAS BARREIRAS-PA.kmz"
farm2 <- st_read(farm_url2) %>%
  st_transform(st_crs(BASE_CRS))

farm_biome <- "AMZ"
strategy <- "E2"

# Spatial model ----------------------------------------------------------------
spatial_objects <- get_spatial_infos(farm_url = farm_url, rasters_url, car_url)

## Crop spatial data ----------------------------------------------------------
cropped_spatial_object <- crop_spatial_infos(farm_poly = spatial_objects$farm_poly,
                                             rasters = spatial_objects$rasters,
                                             land_use_categories = rest_models$land_use$class,
                                             car = spatial_objects$car_res)


## Land-use allocation --------------------------------------------------------
message("Land-use allocation ... \n")

land_use_res <- land_use_allocation(farm_poly = spatial_objects$farm_poly,
                                    rasters = cropped_spatial_object$cropped_rasters,
                                    land_use_categories = cropped_spatial_object$land_use_categories,
                                    legal_reserve_percentage = rest_models$legal_reserve[[farm_biome]])

## Strategy allocation --------------------------------------------------------
message("Strategy allocation ... \n")


strat_res <-  strategy_allocation(allocation_tbl = land_use_res$allocationTbl,
                                  models_reference = rest_models[[strategy]])
names(strat_res) <- paste0(strategy, "_AREA_HA_", names(strat_res))

# Finance model ----------------------------------------------------------------

## Finance parameters
allocation <- strat_res
names(allocation) <- gsub("E2_AREA_HA_", "", names(allocation))
payments <- 0
carbon_sales_mode <- "standard"
area <- land_use_res$total_area

parcela_vector <- seq(200, 2000, by = 200)

res_df <- data.frame(parcela = parcela_vector)

for (i in 1:nrow(res_df)) {
  parcela_restauravel <- res_df$parcela[i]
  n_anos_parcela <- 30

  plan <- plan_from_allocation(allocation, models, payments, area, template)

  if (carbon_sales_mode == "anticipated") {
    carbon_sales_fun <- anticipated_carbon_sale
  } else {
    carbon_sales_fun <- standard_carbon_sale
  }

  if (plan$total_area < 300) {
    assumpt$carbon$project$valid_cost_us <- 0
    assumpt$carbon$project$verif_cost_us <- 0
  }

  cf <- project_cashflow(
    restoration_plan = plan,
    restoration_models = models,
    assumptions = assumpt,
    growth_curves = curves,
    carbon_sales_function = carbon_sales_fun
  )

  ## Arrendamento
  cf$acquisition <- 0

  area_restauravel <- sum(unlist(allocation))
  valor_arrendamento <- -(parcela_restauravel * area_restauravel)

  acquisition <- rep(valor_arrendamento, times = n_anos_parcela)

  cf_arrendamento <- cf[1:n_anos_parcela, ]
  cf_arrendamento$acquisition <- acquisition

  limits_res <- limits(cf = cf_arrendamento,
                       tir_calc_rate = assumpt$general$tir_calc_rate,
                       total_area = plan$total_area,
                       net_area = sum(unlist(allocation)),
                       thresholds = assumpt$limits[[strategy]])

  cf_sum <- apply(cf_arrendamento, 1, sum)

  res_df$irr[i] <- irr(cf_sum)
  res_df$npv[i] <- npv(assumpt$general$tir_calc_rate, cf_sum)

}

write.csv2(res_df, "arrendamento/outputs/fazenda_taboado.csv", row.names = FALSE)
