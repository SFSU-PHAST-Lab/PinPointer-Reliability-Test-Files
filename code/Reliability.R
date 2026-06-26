# Packages ---------------------------------------------------------------------
if(!suppressWarnings(require(dplyr))) install.packages("dplyr")
if(!suppressWarnings(require(irr))) install.packages("irr")
if(!suppressWarnings(require(ggplot2))) install.packages("ggplot2")

# Import and clean the data -------------------------------------------------------------------------------------

# Dataset for fidelity analysis:
data_ruler <-  read.csv("data/RulerICC.csv", header = TRUE, 
                        sep = ",", fileEncoding = "UTF-8-BOM")

# Dataset with both trials of 8 raters for intra-rater assessments:
data_intra <-  read.csv("data/IntraraterICC.csv", header = TRUE, 
                        sep = ",", fileEncoding = "UTF-8-BOM")

# Define the type of variable:
data_intra <-  data_intra %>% 
  mutate(R1_t1 = as.numeric(R1_t1),
         R1_t2 = as.numeric(R1_t2),
         R2_t1 = as.numeric(R2_t1),
         R2_t2 = as.numeric(R2_t2),
         R3_t1 = as.numeric(R3_t1),
         R3_t2 = as.numeric(R3_t2),
         R4_t1 = as.numeric(R4_t1),
         R4_t2 = as.numeric(R4_t2),
         R5_t1 = as.numeric(R5_t1),
         R5_t2 = as.numeric(R5_t2),
         R6_t1 = as.numeric(R6_t1),
         R6_t2 = as.numeric(R6_t2),
         R7_t1 = as.numeric(R7_t1),
         R7_t2 = as.numeric(R7_t2),
         R8_t1 = as.numeric(R8_t1),
         R8_t2 = as.numeric(R8_t2))

data_ruler <-  data_ruler %>% 
  mutate(Ruler = as.numeric(Ruler),
         PinPointer = as.numeric(PinPointer))

# Selecting the first trial of each rater for the inter-rater reliability assessments:
data_inter <- data_intra[, c("R1_t1", "R2_t1", "R3_t1", "R4_t1", "R5_t1", "R6_t1", "R7_t1", "R8_t1")] 


# Inspect data
summary(data_intra)
summary(data_inter)
summary(data_ruler)

# Intra-rater Intraclass Correlation  ---------------------------------------------------

# Both images and rater are considered random-effects (thus, twoway), and we expect to use a single measurement
# per image in experiments, rather than the average between two rates (thus, single). We'll calculate the
# absolute agreement (which considers that the systematic differences between trials is relevant)

# Absolute agreement between trials of each one of the eight raters

## Rater 1
data_intra1 <- data_intra[, c("R1_t1", "R1_t2")] 

ICC_intra_agree1 <- icc(data_intra1, model = "twoway", 
                        type = "agreement", unit = "single")
ICC_intra_agree1  # k = 1 (95%CI = [1; 1]), p < .001

## Rater 2
data_intra2 <- data_intra[, c("R2_t1", "R2_t2")]

ICC_intra_agree2 <- icc(data_intra2, model = "twoway", 
                        type = "agreement", unit = "single")
ICC_intra_agree2  # k = 1 (95%CI = [0.998; 1]), p < .001

## Rater 3
data_intra3 <- data_intra[, c("R3_t1", "R3_t2")]

ICC_intra_agree3 <- icc(data_intra3, model = "twoway", 
                        type = "agreement", unit = "single")
ICC_intra_agree3  # k = 0.999 (95%CI = [0.987; 1]), p < .001

## Rater 4
data_intra4 <- data_intra[, c("R4_t1", "R4_t2")]

ICC_intra_agree4 <- icc(data_intra4, model = "twoway", 
                        type = "agreement", unit = "single")
ICC_intra_agree4  # k = 1 (95%CI = [0.996; 1]), p < .001

## Rater 5
data_intra5 <- data_intra[, c("R5_t1", "R5_t2")]

ICC_intra_agree5 <- icc(data_intra5, model = "twoway", 
                        type = "agreement", unit = "single")
ICC_intra_agree5  # k = 1 (95%CI = [0.999; 1]), p < .001

## Rater 6
data_intra6 <- data_intra[, c("R6_t1", "R6_t2")]

ICC_intra_agree6 <- icc(data_intra6, model = "twoway", 
                        type = "agreement", unit = "single")
ICC_intra_agree6  # k = 1 (95%CI = [1; 1]), p < .001

## Rater 7
data_intra7 <- data_intra[, c("R7_t1", "R7_t2")]

ICC_intra_agree7 <- icc(data_intra7, model = "twoway", 
                        type = "agreement", unit = "single")
ICC_intra_agree7  # k = 1 (95%CI = [1; 1]), p < .001

## Rater 8
data_intra8 <- data_intra[, c("R8_t1", "R8_t2")]

ICC_intra_agree8 <- icc(data_intra8, model = "twoway", 
                        type = "agreement", unit = "single")
ICC_intra_agree8  # k = 1 (95%CI = [1; 1]), p < .001

# Intra-rater descriptives---------------------------------------------
# Creating a variable that captures intra-trial variation among trials 
# of the least reliable rater
data_intra3 <- data_intra3 %>%
  mutate (variability_less = abs(R3_t1 - R3_t2))

#min, max, and mean intra-trial variability of the less reliable rater rater
summary(data_intra3$variability_less)
#min = 0.00, max = 1.47, mean = 0.43


Fig2 <- ggplot(data_intra3, aes(x = R3_t1, y = variability_less)) +
  geom_point() +
  scale_x_continuous(name = "Radial Error (cm)", limits = c(0, 75), breaks = seq(0, 75, by = 15)) +
  scale_y_continuous(name = "Inter-Rater Variability (cm)", limits = c(0, 2.1), breaks = seq(0, 2.1, by = 0.3)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.border = element_blank(),      # Remove border
    axis.line = element_line(color = "black")  # Keep axis lines
  )

Fig2

#checking if larger errors tend to have higher intra-rater variabilities (using the least reliable rater)
cor.test(data_intra3$R3_t1, data_intra3$variability_less, method = "pearson") #r = 0.616

# Intraclass correlation among 8 raters with continuous measures ---------------------------------------------------

# Both images and raters are considered random-effects (thus, twoway), and we expect to use a single measurement
# per image in experiments, rather than the average between raters (thus, single). We'll calculate both the
# absolute agreement (which considers that the systematic differences between raters is relevant), and the
# consistency between raters (which considers systematic differences irrelevant).

# Absolute agreement
ICC_inter_agree <- icc(data_inter, model = "twoway", 
  type = "agreement", unit = "single")
ICC_inter_agree  # k = 1 (95%CI = [1; 1]), p < .001

# Consistency
ICC_inter_cons <- icc(data_inter, model = "twoway", 
                         type = "consistency", unit = "single")
ICC_inter_cons # k = 1 (95%CI = [1; 1]), p = .0

# Inter-rater descriptives---------------------------------------------
# Creating a variable that captures intra-trial variation among raters
data_inter <- data_inter %>%
  mutate (variability = pmax(R1_t1, R2_t1, R3_t1, R4_t1, R5_t1, R6_t1, R7_t1, R8_t1) -
                        pmin(R1_t1, R2_t1, R3_t1, R4_t1, R5_t1, R6_t1, R7_t1, R8_t1))

#min, max, and mean intra-trial variability
summary(data_inter$variability)
#min = 0.2, max = 2.08, mean = 0.55


#Plotting this relationship using the least reliable rater
Fig <- ggplot(data_inter, aes(x = R3_t1, y = variability)) +
  geom_point() +
  scale_x_continuous(name = "Radial Error (cm)", limits = c(0, 75), breaks = seq(0, 75, by = 15)) +
  scale_y_continuous(name = "Inter-Rater Variability (cm)", limits = c(0, 2.1), breaks = seq(0, 2.1, by = 0.3)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.border = element_blank(),      # Remove border
    axis.line = element_line(color = "black")  # Keep axis lines
  )

Fig

#checking if larger errors tend to have higher inter-rater variabilities
cor.test(data_inter$R1_t1, data_inter$variability, method = "pearson") #r = 0.723 [0.629 - 0.796], p < .001
cor.test(data_inter$R2_t1, data_inter$variability, method = "pearson") #r = 0.714 [0.618 - 0.789], p < .001
cor.test(data_inter$R3_t1, data_inter$variability, method = "pearson") #r = 0.718 [0.623 - 0.795], p < .001
cor.test(data_inter$R4_t1, data_inter$variability, method = "pearson") #r = 0.725 [0.632 - 0.798], p < .001
cor.test(data_inter$R5_t1, data_inter$variability, method = "pearson") #r = 0.720 [0.625 - 0.793], p < .001
cor.test(data_inter$R6_t1, data_inter$variability, method = "pearson") #r = 0.718 [0.623 - 0.792], p < .001
cor.test(data_inter$R7_t1, data_inter$variability, method = "pearson") #r = 0.721 [0.626 - 0.794], p < .001
cor.test(data_inter$R8_t1, data_inter$variability, method = "pearson") #r = 0.719 [0.624 - 0.793], p < .001

# Intraclass correlation Ruler vs. PinPointer ---------------------------------------------------
# Absolute agreement
ICC_ruler_agree <- icc(data_ruler, model = "twoway", 
                       type = "agreement", unit = "single")
ICC_ruler_agree  # k(125) = 1 (95%CI = [1; 1]), p < .001

Fig3 <- ggplot(data_ruler, aes(x = Ruler, y = PinPointer)) +
  geom_point() +
  scale_x_continuous(name = "Ruler measurement (cm)", limits = c(0, 130), breaks = seq(0, 130, by = 5)) +
  scale_y_continuous(name = "PinPointer (cm)", limits = c(0, 130), breaks = seq(0, 130, by = 5)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.border = element_blank(),      # Remove border
    axis.line = element_line(color = "black")  # Keep axis lines
  )

Fig3

# Creating a variable that captures the difference between ruler measurement and PinPointer measurement
data_ruler <- data_ruler %>%
  mutate (Difference = abs(Ruler - PinPointer))

summary(data_ruler) #mean = 0.17, range = [0; 0.78]

#checking correlation between ruler measurement and difference
cor.test(data_ruler$Ruler, data_ruler$Difference, method = "pearson") #r(123) = .56 (95% CI = [.428; .671]), p <.001

# Plotting this correlation
tiff("product/Figure3.tiff", units = "in", width=10, height=5, res = 600)
Fig4 <- ggplot(data_ruler, aes(x = Ruler, y = Difference)) +
  geom_point() +
  scale_x_continuous(name = "Ruler measurement (cm)", limits = c(0, 130), breaks = seq(0, 130, by = 5)) +
  scale_y_continuous(name = "Absolute Difference between PinPointer and Ruler (cm)", limits = c(0, 2), breaks = seq(0, 2, by = 0.2)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),      
    axis.line = element_line(color = "black")  
  )

Fig4
dev.off()
