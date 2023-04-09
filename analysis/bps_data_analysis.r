## -----------------------------------------------------------------------------------------------
## Title: bps_data_analysis.R
## Purpose: Replicates analysis and results
## Paper: "Evaluating the feasibility of achieving building performance standards targets"
## Author: Amanda Webb and Colby McConnell
## Date: March 1, 2023
## -----------------------------------------------------------------------------------------------

## Setup

# Load required packages for data analysis
library(tidyverse)

# Load required packages for plotting: viridis, cowplot
library(viridis)
library(cowplot)

# Load required packages for plotting: ggmacc
#library(devtools)
#devtools::install_github("aj-sykes92/ggmacc")
library(ggmacc)

# Import cleaned benchmarking data
bench <- read_csv("../data/aggregated_data/cleaned_all_cities.csv",
  col_types = cols(city = "f", prop_id = "i", facility_type = "f",  floor_area = "i", site_eui = "d"))

## Data pre-processing

# Create additional analysis variables
bench <- bench %>% 
  mutate(
	city_code = recode_factor(city, "Boston" = "BOS", "Denver" = "DEN",	
	  "Los Angeles" = "LAX",  "New York City" = "NYC", "Orlando" = "ORL", 
	  "Philadelphia" = "PHL", "San Francisco" = "SFO",  "Seattle" = "SEA", 
	  "Washington DC" = "WDC"),
	floor_area_cat = cut(floor_area, 
      breaks=c(min(floor_area),50000,100000,200000,500000,max(floor_area)),
      right=FALSE, 
      include.lowest=TRUE, 
      labels=c("< 50,000","50,000-99,999","100,000-199,999","200,000-499,999","> 500,000"))) %>%	
  relocate(city_code, .after = city) 

### Add site EUI targets 

# Import site EUI targets
eui_targets <- read_csv("../data/eui_targets.csv",
  col_types = cols(city = "f", facility_type = "f",  target = "i"))

# Assign targets to buildings
bench <- bench %>%
	left_join(eui_targets, by=c("city"="city", "facility_type"="facility_type"))

# Create additional analysis variables
bench <- bench %>% 
  mutate(
    distance_target = site_eui - target,
	percentage_target = distance_target/site_eui,
	percentage_target_cat10 = cut(percentage_target, 
      breaks=c(0,.10,.20,0.30,0.40,0.50,0.60,0.70,0.80,.90,1.0),
      right=FALSE, 
      include.lowest=TRUE, # need this to ensure that highest values are included (since right=FALSE)
      labels=c("0-10%","10-20%","20-30%","30-40%","40-50%","50-60%","60-70%","70-80%","80-90%","90-100%")),
	percentage_target_cat5 = cut(percentage_target, 
      breaks=c(0,.05,.15,.25,.35,1.0),
      right=FALSE, 
      include.lowest=TRUE, # need this to ensure that highest values are included (since right=FALSE)
      labels=c("<5%","5-15%","15-25%","25-35%",">35%")),
	percentage_target_cat6 = cut(percentage_target, 
      breaks=c(0,.05,.15,.25,.35,0.50,1.0),
      right=FALSE, 
      include.lowest=TRUE, # need this to ensure that highest values are included (since right=FALSE)
      labels=c("<5%","5-15%","15-25%","25-35%","35-50%",">50%")),
	meets_target = case_when(
	  distance_target > 0 ~ "No",
	  distance_target <= 0 ~ "Yes"),
	energy_use = site_eui * floor_area,
	energy_savings = case_when(
	  meets_target == "Yes" ~ 0,
	  meets_target == "No" ~ distance_target * floor_area))

### Add cost data 

# Import retrofit costs
retrofit_costs <- read_csv("../data/retrofit_costs.csv",
  col_types = cols(percentage_target_cat5 = "f", facility_type = "f",  cost_per_ft2 = "d"))

# Import utility costs
utility_costs <- read_csv("../data/utility_costs.csv",
  col_types = cols(city = "f",  cost_per_kBtu = "d"))

# Assign costs to buildings
bench <- bench %>%
	left_join(retrofit_costs, by=c("percentage_target_cat5"="percentage_target_cat5", 
			  "facility_type"="facility_type")) %>%
	left_join(utility_costs, by=c("city"="city"))
 
# Create additional analysis variables
bench <- bench %>% 
  mutate(
	capital_cost = cost_per_ft2 * floor_area,
	cost_savings = energy_savings * cost_per_kBtu,
	payback = capital_cost/cost_savings)

# Show first rows of new dataset
print(bench, n=6, width=Inf)

## Analysis and Results

### Table 2: Number of properties by city and facility type 

# Number of buildings by city and facility type
count_bldgs <- bench %>% 
  group_by(facility_type, city )%>%
  summarise(n=n()) %>%
  spread(facility_type, n) 

# Display table in markdown format
knitr::kable(count_bldgs)

### Figure 2: Total floor area categorized by building size

# Total floor area by city and facility type: For reference (not plotted)
total_area <- bench %>% 
  group_by(facility_type, city )%>%
  summarise(floor_area = sum(floor_area)) %>%
  spread(facility_type, floor_area)

# Total floor area by city and facility type: Ordered 
total_area_ord <- total_area %>% 
  rowwise() %>% 
  mutate(total = sum(Office, `Multifamily Housing`, `K-12 School`, na.rm=TRUE)) %>%
  arrange(desc(total))  
  
# Display table in markdown format
knitr::kable(total_area_ord)
 
# Total floor area by floor area category: NYC only
total_area_cat1 <- bench %>% 
  filter(city_code == "NYC") %>% 
  mutate(floor_area_cat = recode_factor(floor_area_cat, 
	"< 50,000" = "<50", 
	"50,000-99,999" = "<100", 	
	"100,000-199,999" = "<200", 	
	"200,000-499,999" = "<500", 
	"> 500,000" = ">500")) %>% 
  group_by(facility_type, city_code, floor_area_cat)%>%
  summarise(floor_area = sum(floor_area))

# Total floor area by floor area category: Cities other than NYC
total_area_cat2 <- bench %>% 
  filter(city_code != "NYC") %>% 
  mutate(floor_area_cat = recode_factor(floor_area_cat, 
	"< 50,000" = "<50", 
	"50,000-99,999" = "<100", 	
	"100,000-199,999" = "<200", 	
	"200,000-499,999" = "<500", 
	"> 500,000" = ">500")) %>% 
  group_by(facility_type, city_code, floor_area_cat)%>%
  summarise(floor_area = sum(floor_area))

# Barplot of total floor area by floor area category: NYC only
plot_total_area_cat1 <- ggplot(total_area_cat1, aes(fill=fct_rev(floor_area_cat), y=floor_area/(10^6), 
  x=tidytext::reorder_within(city_code, floor_area, facility_type))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(option="viridis", discrete = T, direction = -1,
    name=bquote(atop("Building area",~(ft^2~"x"~10^3)~"     "))) +
  coord_flip() +
  theme_bw() + 
  xlab("") +
  ylab("") +
  facet_wrap(~facility_type, scales = "free_y") +
  theme(panel.grid.major.y = element_blank(),
		panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse=TRUE))	+
  tidytext::scale_x_reordered() +
  theme(legend.title = element_text(color = "transparent"),
    legend.text = element_text(color = "transparent")) +
  guides(fill = guide_legend(override.aes = list(alpha = 0)))

# Barplot of total floor area by floor area category: Cities other than NYC	
plot_total_area_cat2 <- ggplot(total_area_cat2 , aes(fill=fct_rev(floor_area_cat), y=floor_area/(10^6), 
  x=tidytext::reorder_within(x = city_code, by = floor_area, within = facility_type))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(option="viridis", discrete = T, direction = -1,
    name=bquote(atop("Building area",~(ft^2~"x"~10^3)~"     "))) +
  coord_flip() +
  theme_bw() + 
  ylab(bquote("Total floor area"~(ft^2~"x"~10^6))) +
  xlab("") +
  facet_wrap(~facility_type, scales = "free_y") +
  theme(panel.grid.major.y = element_blank(),
		panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse=TRUE))	+
  tidytext::scale_x_reordered() 

# Side-by-side plots 
figure_2 <- plot_grid(plot_total_area_cat1, plot_total_area_cat2, labels = "", ncol = 1, rel_heights = c(1, 3))

# Save figure to file
ggsave("../results/figure_2.png", figure_2, width = 6.98, height = 4.427, units = "in", dpi = 600)

### Figure 3: Distribution of site EUI relative to target

# Percentiles of site EUI by city and facility type
pct_eui <- bench %>% 
  group_by(facility_type, city_code) %>%
  summarize(pct_10 = quantile(site_eui, prob = 0.10),
			pct_25 = quantile(site_eui, prob = 0.25),
			pct_50 = quantile(site_eui, prob = 0.50),
			pct_75 = quantile(site_eui, prob = 0.75),
			pct_90 = quantile(site_eui, prob = 0.90),
			target = median(target)) %>%
  arrange(facility_type, desc(pct_50))

# Display table in markdown format
knitr::kable(pct_eui)

# Boxplot of site EUI by city and facility type
figure_3 <- ggplot(pct_eui, aes(x = tidytext::reorder_within(city_code, pct_50, facility_type))) +
  geom_errorbar(aes(ymin = pct_10,ymax = pct_90), linetype = 1,width = 0.5) +
  geom_boxplot(aes(ymin = pct_10, lower = pct_25, middle = pct_50, upper = pct_75, ymax = pct_90), 
	stat = "identity") +
  coord_flip() +
  geom_point(aes(x = tidytext::reorder_within(city_code, pct_50, facility_type), y=target), color="red", size=2)+
  theme_bw() + 
  xlab("") +
  theme(text = element_text(size = 11),
		axis.title.x = element_text(size = 10), 
		legend.position="none",
		panel.grid.major.y = element_blank(),
		panel.grid.minor.y = element_blank()) +
  facet_wrap(~facility_type, scales = "free_y") +
  tidytext::scale_x_reordered() +
  scale_y_continuous(bquote("Site EUI"~(kBtu/ft^2)), 
    sec.axis = sec_axis(~ . * 3.15472305134303, name = bquote("Site EUI"~(kWh/m^2))))  

# Save figure to file
ggsave("../results/figure_3.png", figure_3, width = 6.98, height = 4.427, units = "in", dpi = 600)

### Tables 4-6: Summary statistics for properties not meeting target

# Summary statistics for buildings not meeting target: Office buildings 
summary_stats_over_office <- bench %>% 
  filter(facility_type == "Office", meets_target == "No") %>% 
  group_by(facility_type, city) %>% 
  summarise(count=n(),
			      ft2 = sum(floor_area),
            reduce_eui = median(distance_target),
            reduce_pct = median(percentage_target)) %>%
  mutate(count_pct = count/count_bldgs$Office,
			ft2_pct = ft2/total_area$Office) %>%
  relocate(count_pct, .after = count) %>%
  relocate(ft2_pct, .after = ft2) 

# Display table in markdown format
knitr::kable(summary_stats_over_office)

# Summary statistics for buildings not meeting target: Multifamily housing 
summary_stats_over_multifamily <- bench %>% 
  filter(facility_type == "Multifamily Housing", meets_target == "No") %>% 
  group_by(facility_type, city) %>% 
  summarise(count=n(),
			      ft2 = sum(floor_area),
            reduce_eui = median(distance_target),
            reduce_pct = median(percentage_target)) %>%
  mutate(count_pct = count/count_bldgs$`Multifamily Housing`,
			ft2_pct = ft2/total_area$`Multifamily Housing`) %>%
  relocate(count_pct, .after = count) %>%
  relocate(ft2_pct, .after = ft2) 

# Display table in markdown format
knitr::kable(summary_stats_over_multifamily)

# Summary statistics for buildings not meeting target: K-12 schools
summary_stats_over_k12 <- bench %>% 
  filter(facility_type == "K-12 School", meets_target == "No") %>% 
  group_by(facility_type, city) %>% 
  summarise(count=n(),
		      	ft2 = sum(floor_area),
            reduce_eui = median(distance_target),
            reduce_pct = median(percentage_target)) %>%
  mutate(count_pct = count/na.omit(count_bldgs$`K-12 School`),
			ft2_pct = ft2/na.omit(total_area$`K-12 School`)) %>%
  relocate(count_pct, .after = count) %>%
  relocate(ft2_pct, .after = ft2) 

# Display table in markdown format
knitr::kable(summary_stats_over_k12)

### Figure 4: Distribution of reduction in site EUI needed	

# Count and % of buildings in each savings bin 
count_by_pct_savings <- bench %>% 
  filter(meets_target == "No") %>% 
  group_by(facility_type, city_code,  percentage_target_cat10, .drop=FALSE) %>%
  summarise(n=n()) %>%
  mutate(n_pct = n/sum(n),
		 n_pct_cat = cut(n_pct, 
		   breaks=c(0,.05,.10,.15,0.20,0.25,1.0),
		   right=FALSE,  
		   include.lowest=TRUE, # need this to ensure that highest values are included (since right=FALSE)
		   labels=c("0-5%","5-10%","10-15%","15-20%","20-25%",">25%")))

# Display table in markdown format
knitr::kable(count_by_pct_savings)

# Plot discrete heatmap of percentage reduction required
figure_4 <- ggplot(count_by_pct_savings, aes(x = percentage_target_cat10, y=fct_rev(city_code), fill= n_pct_cat)) + 
  geom_tile() +
  scale_fill_viridis(option="viridis", direction=1, na.value="white", discrete = TRUE,
	name = "Percentage of\nproperties", na.translate=FALSE) +
  theme_bw() + 
  xlab("Reduction needed (%)") +
  ylab("") +
  theme(text = element_text(size = 11),
		axis.text.x = element_text(size = 8), 
		legend.key.width= unit(0.25, 'cm'),		
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank()) +
  facet_grid(cols = vars(facility_type))  +
  scale_x_discrete(labels=c("10","20","30","40","50","60","70","80","90","100"))

# Save figure to file
ggsave("../results/figure_4.png", figure_4, width = 6.98, height = 4.427, units = "in", dpi = 600)

### Table 7: Total energy savings resulting from BPS

# Total energy savings by city and facility type: Savings as % of all benchmarked kBtu
total_energy_savings <- bench %>% 
  group_by(facility_type, city) %>% 
  summarise(energy_use = sum(energy_use),
            energy_savings = sum(energy_savings)) %>%
  mutate(pct = energy_savings/energy_use) %>%
  pivot_wider(names_from = facility_type, 
	values_from = c("energy_use", "energy_savings", "pct"), 
	names_sep="_",
	names_vary = 'slowest')

# Display table in markdown format
knitr::kable(total_energy_savings)

### Figure 5: Contribution of each property to total energy savings from BPS

# Subset all office buildings for Denver
den_office_all <- bench %>% 
  filter(facility_type == "Office", city == "Denver") %>% 
  arrange(site_eui) %>%
  mutate(percentage_target_cat6 = recode_factor(percentage_target_cat6, 
		"<5%" = "<5", 
		"5-15%" = "<15", 	
		"15-25%" = "<25", 	
		"25-35%" = "<35", 
		"35-50%" = "<50", 
		">50%" = ">50"))

# Set fill colors
set_fill <- viridis::viridis_pal(option="viridis")(6)
set_fill_with_na <- c(set_fill, "7F7F7F")

# Barplot of site EUI for all office buildings in Denver: Colored by % reduction needed 
plot_den_office_all <- ggplot(den_office_all, 
	aes(x=1:nrow(den_office_all), y=site_eui, fill=percentage_target_cat6)) +
  geom_bar(stat = "identity", width = 1.0) +
  geom_hline(yintercept=46, color = "red") +
  theme_bw() + 
  xlab("Number of buildings") +
  ylab(bquote("Site EUI"~(kBtu/ft^2))) +
  scale_x_continuous(breaks=seq(0,300,50)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 3.15472305134303, 
	name = bquote("Site EUI"~(kWh/m^2)))) +
  scale_fill_manual(values = set_fill_with_na, name = "Reduction\nneeded (%)") +
  theme(panel.grid.minor.x = element_blank(),
		legend.position="top",
		legend.title = element_text(size =8), 
		legend.text  = element_text(size = 8),
		legend.key.size = unit(0.7, "lines"),
		axis.title.y = element_text(size = 10)) + 
  guides(fill=guide_legend(nrow=1, byrow=TRUE)) 

# Subset office buildings not meeting target for Denver
den_office_over <- bench %>% 
  filter(facility_type == "Office", city == "Denver", meets_target == "No") %>%
  mutate(
	floor_area_cat = recode_factor(floor_area_cat, 
		"< 50,000" = "<50", 
		"50,000-99,999" = "<100", 	
		"100,000-199,999" = "<200", 	
		"200,000-499,999" = "<500", 
		"> 500,000" = ">500"),
	percentage_target_cat6 = recode_factor(percentage_target_cat6, 
		"<5%" = "<5", 
		"5-15%" = "<15", 	
		"15-25%" = "<25", 	
		"25-35%" = "<35", 
		"35-50%" = "<50", 
		">50%" = ">50"))  
  
# Subset office buildings not meeting target for Denver: Ordered  
den_office_over_ord <- den_office_over %>%   
  arrange(desc(energy_savings)) %>%
  mutate(pct = energy_savings/sum(energy_savings),
		 pct_cumsum = cumsum(pct))

# Show dataset to view number of properties at 50% and 80% cumulative savings
print(den_office_over_ord, n=nrow(den_office_over_ord), width=Inf)

# Barplot of energy savings for office buildings in Denver not meeting target: Colored by % reduction needed 
plot_den_office_over_ord <- ggplot(den_office_over_ord)  +
  geom_bar(aes(x=1:nrow(den_office_over_ord), y=pct, fill=percentage_target_cat6), stat = "identity", width = 1.0) +
  geom_line(aes(x=1:nrow(den_office_over_ord), y=pct_cumsum*max(den_office_over_ord$pct)), stat = "identity", group=1) +
  scale_fill_viridis(discrete = T, name = "Reduction\nneeded (%)") +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~./max(den_office_over_ord$pct), 
	name = "Cumulative percent")) +
  xlab("Number of buildings") +
  ylab("Percent of total energy savings") +
  theme(legend.position="top") +
  guides(fill=guide_legend(nrow=1, byrow=TRUE)) +
  theme(legend.title = element_text(size = 8), 
		legend.text  = element_text(size = 8),
		legend.key.size = unit(0.7, "lines"),
		axis.title.y = element_text(size = 10)) 

# Side-by-side plots 
figure_5 <- plot_grid(plot_den_office_all, plot_den_office_over_ord, labels = "AUTO")

# Save figure to file
ggsave("../results/figure_5.png", figure_5, width = 8.77, height = 4.427, units = "in", dpi = 600)

### Figure 6: Total energy savings from BPS categorized by reduction in site EUI needed

# Total energy savings by city and facility type: Grouped by % reduction 
total_energy_savings_by_pct_savings <- bench %>% 
  filter(meets_target == "No") %>% 
  group_by(facility_type, city_code,  percentage_target_cat6, .drop=FALSE) %>%
  summarise(energy_savings = sum(energy_savings)) %>%
  mutate(energy_savings_pct = energy_savings/sum(energy_savings))

# Display table in markdown format
knitr::kable(total_energy_savings_by_pct_savings)

# Stacked barplot of total energy savings by % from target category 
figure_6 <- ggplot(total_energy_savings_by_pct_savings, 
  aes(fill=fct_rev(percentage_target_cat6), y=energy_savings, x=fct_rev(city_code))) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip() +
  scale_fill_viridis(discrete = T, direction=-1, name = "Reduction\nneeded (%)") +
  theme_bw() +
  xlab("") +
  ylab("Proportion of total energy savings") +
  theme(text = element_text(size = 11),
		axis.text.x = element_text(size = 8), 
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank()) +
  facet_grid(cols= vars(facility_type)) +
  guides(fill = guide_legend(reverse=TRUE))

# Save figure to file
ggsave("../results/figure_6.png", figure_6, width = 6.98, height = 4.427, units = "in", dpi = 600)

### Table 8: Estimated economic investment required to meet BPS targets

# Summary of investment costs by city and building type
summary_stats_economics <- bench %>% 
  group_by(facility_type, city) %>%
  filter(meets_target == "No") %>% 
  summarise(median_cost = median(capital_cost),
            total_cost = sum(capital_cost),
			payback = median(payback),) %>%  
  pivot_wider(names_from = facility_type, 
	values_from = c("median_cost","total_cost","payback"), 
	names_sep="_",
	names_vary = 'slowest')

# Display table in markdown format
knitr::kable(summary_stats_economics)

### Figure 7: Marginal cost abatement curves

# Show dataset to see highest values
print(arrange(den_office_over, desc(payback)), n=6, width=Inf)

# MACC with percent improvement fill
macc_pct_fill <- den_office_over %>%
  ggmacc(abatement = energy_savings*1000/(10^9), mac = payback, fill = percentage_target_cat6,
         zero_line = TRUE) +
  scale_fill_viridis(discrete = T, direction=1, name = "Reduction needed (%)") +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~./48.3, 
	name = "$/kBtu saved")) +
  labs(fill = "Reduction\nneeded (%)",
       x = bquote("Energy savings (Btu x 10"^{9}*")"),
       y = "Payback (yrs)") +
  theme(legend.position="top") +
  guides(fill=guide_legend(nrow=1, byrow=TRUE)) +
  theme(legend.title = element_text(size = 8), 
		legend.text = element_text(size = 8),
		legend.key.size = unit(0.7, "lines")) 

# MACC with floor area category fill
macc_area_fill <- den_office_over %>%
  ggmacc(abatement = energy_savings*1000/(10^9), mac = payback, fill = floor_area_cat,
         zero_line = TRUE) +
  scale_fill_viridis(discrete = T, direction=1, name = bquote("Building area (ft"^{2}*" x 10"^{3}*")")) +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~./48.3, 
	name = "$/kBtu saved")) +
  labs(x = bquote("Energy savings (Btu x 10"^{9}*")"),
       y = "Payback (yrs)") +
  theme(legend.position="top") +
  theme(legend.title = element_text(size = 8), 
      legend.text  = element_text(size = 8),
      legend.key.size = unit(0.7, "lines")) 
	
# Side-by-side plots 
figure_7 <- plot_grid(macc_pct_fill, macc_area_fill, labels = "AUTO")	

# Save figure to file
ggsave("../results/figure_7.png", figure_7, width = 10, height = 5.04, units = "in", dpi = 600)
