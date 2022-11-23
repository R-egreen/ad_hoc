# Finance model ----------------------------------------------------------------
devtools::load_all("/DATA/pessoal/sara/pkg/finance/")

## Finance parameters
area <- 907.1566
area_restauravel <- 0.5 * area
allocation <- list(ARN = 0.2 * area_restauravel,
                   MRNN_C = 0.8 * area_restauravel)
payments <- 0
carbon_sales_mode <- "standard"

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

write.csv2(res_df, "arrendamento/outputs/fazenda_taboado_e_estrela_dalva.csv", row.names = FALSE)
