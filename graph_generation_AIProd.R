library(tidyverse)
library(data.table)
library(ggplot2)
library(DaisTheme)
library(readr)
library(scales)

setwd("C:/Users/alockhart/Desktop/FSC AI Productivity 2024/")
graph.data <-  fread("Graphs_spreadsheet.csv")

figure_1_data <- fread("Figure_1.csv")

fig_1 <- ggplot(figure_1_data, aes(x = financial_year, y = mean_tfp, group =C320010)) +
  dais.base.theme()+
  geom_line(linewidth = 0.75, aes(color = C320010))+
  labs(title = graph.data[graph.data$Figure_number=="Figure 1",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 1",Figure_title],
       x = graph.data[graph.data$Figure_number=="Figure 1",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 1",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure 1", Legend_name])+
  ylim(0,7)+
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = ""),
                     limits = c(0,7)) +
  scale_color_manual(values = setNames(c("#eb0072", "#004c9b"),
                                       c(graph.data[graph.data$Figure_number == "Figure 1", "Legend_label_1"],
                                         graph.data[graph.data$Figure_number == "Figure 1", "Legend_label_2"])))+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) 

figure_2_data <- fread("Figure_2.csv")

fig_2 <- ggplot(figure_2_data, aes(x = financial_year, y = mean_tfp, group =C320010)) +
  dais.base.theme()+
  geom_line(linewidth = 0.75, aes(color = C320010))+
  labs(title = graph.data[graph.data$Figure_number=="Figure 2",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 2",Figure_title],
       x = graph.data[graph.data$Figure_number=="Figure 2",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 2",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure 2", Legend_name])+
  # scale_y_continuous(labels = dollar_format(prefix = "$", suffix = ""))+
  scale_color_manual(values = setNames(c("#eb0072", "#004c9b"),
                                       c(graph.data[graph.data$Figure_number == "Figure 2", "Legend_label_1"],
                                         graph.data[graph.data$Figure_number == "Figure 2", "Legend_label_2"])))+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) 

figure_3_data <- fread("Figure_3.csv")

fig_3 <- ggplot(figure_3_data, aes(x = as.numeric(Year), 
                                   fill = Treatment, 
                                   y = `Difference in Productivity Growth`,
                                   color = Treatment)) +
  dais.base.theme()+
  geom_errorbar(width=0.2, size=0.8,
    aes(ymin = Minimum,
        ymax = Maximum))+
  geom_point(size=5)+
  scale_color_manual(values = c("Before adopting AI"="#eb0072", "After adopting AI"="#004c9b"))+
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "black", size = 1) + # Vertical line between 2019 and 2020
  annotate("text", x = 2018, y = 1.4, label = "Period before AI Adoption", hjust = 0.5, color = "black", size = 5, family="Replica-Regular") +
  annotate("text", x = 2021, y = 1.4, label = "AI Adoption Period", hjust = 0.5, color = "black", size = 5, family="Replica-Regular") +
  scale_y_continuous(limits = c(-1.5,1.5))+
  labs(title = graph.data[graph.data$Figure_number=="Figure 3",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 3",Figure_title],
       x = graph.data[graph.data$Figure_number=="Figure 3",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 3",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure 3", Legend_name])+
  geom_hline(yintercept = 0)+
  theme(#legend.text = element_text(size = 12),
        #legend.title = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

export.dais.plot("Exported/Figure_1.pdf",fig_1,p.height = 6, p.width = 9)
export.dais.plot("Exported/Figure_2.pdf",fig_2,p.height = 6, p.width = 9)
export.dais.plot("Exported/Figure_3.pdf",fig_3,p.height = 6, p.width = 9)

export.dais.plot("Exported/Figure_1.svg",fig_1,p.height = 6, p.width = 9, type = "svg")
export.dais.plot("Exported/Figure_2.svg",fig_2,p.height = 6, p.width = 9, type = "svg")
export.dais.plot("Exported/Figure_3.svg",fig_3,p.height = 6, p.width = 9, type = "svg")
