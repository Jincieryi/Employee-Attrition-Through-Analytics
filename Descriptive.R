#data1和data2用来对原始数据集进行变化
#merged_data是将两个文件夹用EmployeeID进行合并后的数据集
#data_sum是除去了几个和attrition没有关系的列的数据集（比如id，name，具体的日期等）
#data_num是所有numeric列
#attrited_data是所有离职员工的数据
#===================================================================================
setwd("C:/Users/ASUS/Desktop/Graudate T1/2 Tue Analytics Strategy/Project/archive")
library(data.table)
#===================================================================================
emp_dt<-fread("Employee.csv",stringsAsFactors = TRUE)
per_dt<-fread("PerformanceRating.csv",stringsAsFactors = TRUE)
summary(emp_dt)
summary(per_dt)
data1<-emp_dt
data2<-per_dt
summary(data1)
data1$Education <- factor(data1$Education,
                          levels = c(1,2,3,4,5),
                          labels = c('No Formal Qualifications', 'High School', 'Bachelors', 'Masters', 'Doctorate'))
summary(data1$Education)

#===================================================================================
summary(data2)
data2$EnvironmentSatisfaction<-factor(data2$EnvironmentSatisfaction,
                                      levels = c(1,2,3,4,5),
                                      labels = c('Very Dissatisfied', 'Dissatisfied', 'Neutral', 'Satisfied', 'Very Satisfied'))
data2$JobSatisfaction<-factor(data2$JobSatisfaction,
                              levels = c(1,2,3,4,5),
                              labels = c('Very Dissatisfied', 'Dissatisfied', 'Neutral', 'Satisfied', 'Very Satisfied'))
data2$RelationshipSatisfaction<-factor(data2$RelationshipSatisfaction,
                              levels = c(1,2,3,4,5),
                              labels = c('Very Dissatisfied', 'Dissatisfied', 'Neutral', 'Satisfied', 'Very Satisfied'))
data2$WorkLifeBalance<-factor(data2$WorkLifeBalance,
                                       levels = c(1,2,3,4,5),
                                       labels = c('Very Dissatisfied', 'Dissatisfied', 'Neutral', 'Satisfied', 'Very Satisfied'))
data2$SelfRating<-factor(data2$SelfRating,
                              levels = c(1,2,3,4,5),
                              labels = c('Unacceptable', 'Needs Improvement', 'Meets Expectation', 'Exceeds Expectation', 'Above and Beyond'))
data2$ManagerRating<-factor(data2$ManagerRating,
                              levels = c(1,2,3,4,5),
                              labels = c('Unacceptable', 'Needs Improvement', 'Meets Expectation', 'Exceeds Expectation', 'Above and Beyond'))
summary(data2)
#===================================================================================
summary(data1)
summary(data2)
any(duplicated(data1$EmployeeID))  
any(duplicated(data2$EmployeeID))
any(is.na(data1$EmployeeID))
any(is.na(data2$EmployeeID))
merged_data <- merge(data1, data2, by = "EmployeeID")
summary(merged_data)
summary(merged_data$HireDate)
#Attrition rate/yr -------------------------------------------

library(data.table)
data1[, HireYear := as.numeric(format(as.Date(HireDate, format="%Y/%m/%d"), "%Y"))]
data1[, LeaveYear := ifelse(Attrition == "Yes", HireYear + YearsAtCompany, NA)]

table(data1$LeaveYear)
years <- seq(min(data1$HireYear), max(data1$HireYear + data1$YearsAtCompany, na.rm = TRUE))
yearly_totals <- data.table(Year = numeric(0), TotalEmployees = numeric(0))
for (year in years) {
  total_employees <- data1[, sum(HireYear <= year & (is.na(LeaveYear) | LeaveYear > year))]#在每个年份 `year` 中，计算公司在该年份雇佣的总员工数
  yearly_totals <- rbind(yearly_totals, data.table(Year = year, TotalEmployees = total_employees))
}

yearly_attrition <- data1[!is.na(LeaveYear), .(AttritedEmployees = .N), by = LeaveYear]
yearly_data <- merge(yearly_totals, yearly_attrition, by.x = "Year", by.y = "LeaveYear", all.x = TRUE)
yearly_data[, AttritionRate := (AttritedEmployees / TotalEmployees) * 100]

yearly_data[is.na(AttritedEmployees), AttritedEmployees := 0]


library(ggplot2)
yearly_data_clean <- na.omit(yearly_data)
yearly_data_clean$Year <- factor(yearly_data_clean$Year)

ggplot(yearly_data_clean, aes(x = Year, y = AttritionRate, group = 1)) +
  geom_line(color = "blue") +  
  geom_point(color = "red", size = 3) + 
  geom_text(aes(label = round(AttritionRate, 2)), vjust = -0.5, size = 3) +  # 在每个点上显示数值，保留两位小数
  labs(title = "Attrition Rate per Year", x = "Year", y = "Attrition Rate (%)") +  
  theme_minimal()


#===================================================================================
#Since the cleaned CSV has no time, it is only used when analyzing each feature.
data_sum <- fread("EmployeeCleaned.csv",stringsAsFactors = TRUE)
# Define the correct order for satisfaction levels
satisfaction_levels <- c("Very Dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very Satisfied")

# Ensure all satisfaction-related variables are ordered correctly
data_sum$EnvironmentSatisfaction <- factor(data_sum$EnvironmentSatisfaction, levels = satisfaction_levels)
data_sum$JobSatisfaction <- factor(data_sum$JobSatisfaction, levels = satisfaction_levels)
data_sum$RelationshipSatisfaction <- factor(data_sum$RelationshipSatisfaction, levels = satisfaction_levels)

# If there are any other satisfaction-related columns, repeat for those as well

summary(data_sum)

library(dplyr)
data_sum <- distinct(data_sum)
summary(data_sum)
#====================================================================================
library(ggplot2)
library(patchwork)

generate_plot_dt <- function(factor_var, factor_name, data) {
  # Calculate attrition rate
  attrition_rate <- data[, .(AttritionRate = mean(Attrition == "Yes") * 100), by = factor_var]
  
  # Get the maximum attrition rate for setting y limits
  max_rate <- max(attrition_rate$AttritionRate)
  
  # Create the plot
  plot <- ggplot(attrition_rate, aes_string(x = factor_var, y = "AttritionRate", fill = factor_var)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(AttritionRate, 1)), vjust = -0.5, size = 3) +  # Display the values on the bars
    labs(title = paste("Attrition rates by", factor_name), x = factor_name, y = "Attrition rate (%)") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 20, hjust = 1, size = 8),
          axis.text.y = element_text(size = 12)) +
    ylim(0, max_rate * 1.2)  # Set the y-axis limit with a buffer
  
  return(plot)
}



factor_vars <- list(
  "Gender" = "Gender",
  "Education" = "Education",
  "BusinessTravel" = "BusinessTravel",
  "Department" = "Department",
  "State" = "State",
  "OverTime" = "OverTime",
  "Ethnicity" = "Ethnicity",
  "MaritalStatus" = "MaritalStatus",
  "EducationField" = "EducationField",
  "JobRole" = "JobRole",
  "EnvironmentSatisfaction" = "Environment Satisfaction",
  "JobSatisfaction" = "Job Satisfaction",
  "RelationshipSatisfaction" = "Relationship Satisfaction",
  "WorkLifeBalance" = "Work Life Balance",
  "SelfRating" = "Self Rating",
  "ManagerRating" = "Manager Rating"
)


# Combine 1: Demographics
demographics_plots <- c("Gender", "Education", "MaritalStatus", "Ethnicity")
combined_plot1 <- lapply(demographics_plots, function(var) {
  generate_plot_dt(var, factor_vars[[var]], data_sum)
}) %>%
  patchwork::wrap_plots(ncol = 2)

# Combine 2: Work-Related
work_related_plots <- c("BusinessTravel", "Department", "JobRole", "EducationField")

combined_plot2 <- lapply(work_related_plots, function(var) {
  generate_plot_dt(var, factor_vars[[var]], data_sum)
}) %>%
  patchwork::wrap_plots(ncol = 2)

# Combine 3: Satisfaction & Ratings
satisfaction_plots <- c("EnvironmentSatisfaction", "JobSatisfaction", "RelationshipSatisfaction", 
                        "WorkLifeBalance", "SelfRating", "ManagerRating")
combined_plot3 <- lapply(satisfaction_plots, function(var) {
  generate_plot_dt(var, factor_vars[[var]], data_sum)
}) %>%
  patchwork::wrap_plots(ncol = 2)


combined_plot1
combined_plot2
combined_plot3
#-----------------------------------------------------------------
# Use set() to avoid copying issues in R
data_sum[, YearsAtCompanyGroup := cut(YearsAtCompany,
                                      breaks = c(0, 4, 8, 10), 
                                      labels = c("0-4", "4-8", "8-10"),
                                      include.lowest = TRUE, right = FALSE)]

# Set the levels in the appropriate order
setattr(data_sum$YearsAtCompanyGroup, "levels", c("0-4", "4-8", "8-10"))



attrition_stats <- data_sum[, .(
  TotalEmployees = .N,  
  AttritedEmployees = sum(Attrition == "Yes"),  
  AttritionRate = mean(Attrition == "Yes") * 100  
), by = YearsAtCompanyGroup]


print(attrition_stats)

#-----------------------------------------------------------------
library(ggplot2)
library(data.table)
library(patchwork)

summary(data_sum)
data_sum <- copy(data_sum)  
set(data_sum, j = "YearsAtCompanyGroup", value = cut(data_sum$YearsAtCompany,
                                                     breaks = c(0, 4, 8, 10), 
                                                     labels = c("0-4", "4-8", "8-10"),
                                                     include.lowest = TRUE, right = FALSE))

generate_satisfaction_plot <- function(factor_var, factor_name, data) {
  # Calculate attrition rate by YearsAtCompanyGroup and the factor variable
  attrition_rate <- data[, .(AttritionRate = mean(Attrition == "Yes") * 100), by = .(YearsAtCompanyGroup, get(factor_var))]
  
  # Rename the column to the factor variable name
  setnames(attrition_rate, "get", factor_var)
  
  # Get the maximum attrition rate for setting y limits
  max_rate <- max(attrition_rate$AttritionRate, na.rm = TRUE)
  
  # Create the plot
  plot <- ggplot(attrition_rate, aes_string(x = "YearsAtCompanyGroup", y = "AttritionRate", fill = factor_var)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(AttritionRate, 1)), vjust = -0.3, size = 3, position = position_dodge(0.9)) +  # Adjusted vjust
    labs(title = paste("Attrition rates by", factor_name, "and Years At Company Group"), 
         x = "Years At Company Group", y = "Attrition Rate (%)", fill = factor_name) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 0, hjust = 1, size = 8),
          axis.text.y = element_text(size = 12)) +
    ylim(0, max_rate * 1.3)  # Adjust ylim with a buffer
  
  return(plot)
}




# Satisfaction-related variables
satisfaction_vars <- c("EnvironmentSatisfaction", "JobSatisfaction", "RelationshipSatisfaction", 
                       "WorkLifeBalance", "SelfRating", "ManagerRating")

satisfaction_plots <- lapply(satisfaction_vars, function(var) {
  generate_satisfaction_plot(var, var, data_sum)  
}) %>%
  patchwork::wrap_plots(ncol = 2)

satisfaction_plots

generate_satisfaction_plot <- function(factor_var, factor_name, data) {
  # Calculate attrition statistics by YearsAtCompanyGroup and the factor variable
  attrition_stats <- data[, .(TotalEmployees = .N,
                              AttritedEmployees = sum(Attrition == "Yes"),
                              AttritionRate = mean(Attrition == "Yes") * 100), 
                          by = .(YearsAtCompanyGroup, get(factor_var))]
  
  # Rename the column to the factor variable name
  setnames(attrition_stats, "get", factor_var)
  
  # Get the maximum attrition rate for setting y limits
  max_rate <- max(attrition_stats$AttritionRate, na.rm = TRUE)
  
  # Create the plot
  plot <- ggplot(attrition_stats, aes_string(x = "YearsAtCompanyGroup", y = "AttritionRate", fill = factor_var)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(AttritionRate, 1)), vjust = -0.3, size = 3, position = position_dodge(0.9)) +  # Adjusted vjust
    labs(title = paste("Attrition rates by", factor_name, "and Years At Company Group"), 
         x = "Years At Company Group", y = "Attrition Rate (%)", fill = factor_name) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 12)) +
    ylim(0, max_rate * 1.3)
  
  return(plot)
}


# Satisfaction-related variables
satisfaction_vars <- c("EnvironmentSatisfaction", "JobSatisfaction", "RelationshipSatisfaction", 
                       "WorkLifeBalance", "SelfRating", "ManagerRating")

satisfaction_plots <- lapply(satisfaction_vars, function(var) {
  generate_satisfaction_plot(var, var, data_sum)  
}) %>%
  patchwork::wrap_plots(ncol = 2)

satisfaction_plots


#========================================================================
#Are people who travel frequently sales people?
summary(data_sum)
library(ggplot2)

travel_department_data <- data_sum[, .N, by = .(BusinessTravel, Department)]

ggplot(travel_department_data, aes(x = Department, y = N, fill = BusinessTravel)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Business Travel Frequency by Department",
       x = "Department",
       y = "Proportion of Employees",
       fill = "Business Travel") +
  theme_minimal()

#====================================================================================
library(ggplot2)
summary(data_sum)
data_num <- melt(
  data_sum[, .(Attrition, Age, DistanceFromHome, Salary, YearsAtCompany, 
            YearsInMostRecentRole, YearsSinceLastPromotion, YearsWithCurrManager,
            TrainingOpportunitiesWithinYear, TrainingOpportunitiesTaken)],
  id.vars = "Attrition",  
  variable.name = "Variable",  
  value.name = "Value"  
)

ggplot(data_num, aes(x = Attrition, y = Value, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "The relationship between continuous variables and employee turnover", 
       x = "Attrition", 
       y = "Value") +
  facet_wrap(~ Variable, scales = "free_y") +  
  theme_minimal() +
  scale_fill_manual(values = c("No" = "lightgreen", "Yes" = "salmon"))

data_num <- melt(
  data_sum[, .(Attrition, Age, DistanceFromHome, Salary, YearsAtCompany, 
               YearsInMostRecentRole, YearsSinceLastPromotion, YearsWithCurrManager,
               TrainingOpportunitiesWithinYear, TrainingOpportunitiesTaken)],
  id.vars = c("Attrition", "YearsAtCompany"),  
  variable.name = "Variable",  
  value.name = "Value"
)

years_at_company_list <- unique(data_num$YearsAtCompany)

for (year in years_at_company_list) {
  data_filtered <- data_num[YearsAtCompany == year]
  
  plot <- ggplot(data_filtered, aes(x = Attrition, y = Value, fill = Attrition)) +
    geom_boxplot() +
    labs(title = paste("Continuous variables and employee turnover for", year, "Years At Company"), 
         x = "Attrition", 
         y = "Value") +
    facet_wrap(~ Variable, scales = "free_y") +  
    theme_minimal() +
    scale_fill_manual(values = c("No" = "lightgreen", "Yes" = "salmon"))
  
  print(plot)
}

#--------------------------------------------------------------------------
library(ggplot2)
library(data.table)

data_num <- melt(
  data_sum[, .(Attrition, Age, DistanceFromHome, Salary, YearsAtCompany, 
               YearsInMostRecentRole, YearsSinceLastPromotion, YearsWithCurrManager,
               TrainingOpportunitiesWithinYear, TrainingOpportunitiesTaken)],
  id.vars = c("Attrition", "YearsAtCompany"),  
  variable.name = "Variable",  
  value.name = "Value"
)

data_num$YearsAtCompany <- factor(data_num$YearsAtCompany, levels = sort(unique(data_num$YearsAtCompany)))

variables_list <- unique(data_num$Variable)

for (variable in variables_list) {
  data_filtered <- data_num[Variable == variable]
  
  plot <- ggplot(data_filtered, aes(x = Attrition, y = Value, fill = Attrition)) +
    geom_boxplot() +
    labs(title = paste("Attrition and", variable, "by Years At Company"), 
         x = "Attrition", 
         y = variable) +
    facet_wrap(~ YearsAtCompany, scales = "free_y", labeller = label_both) +  
    theme_minimal() +
    scale_fill_manual(values = c("No" = "lightgreen", "Yes" = "salmon"))
  
  print(plot)
}
#---------------------------------------------------------------
library(ggplot2)
library(data.table)

data_num <- melt(
  data_sum[, .(Attrition, Age, DistanceFromHome, Salary, YearsAtCompany, 
               YearsInMostRecentRole, YearsSinceLastPromotion, YearsWithCurrManager,
               TrainingOpportunitiesWithinYear, TrainingOpportunitiesTaken)],
  id.vars = c("Attrition", "YearsAtCompany"),  
  variable.name = "Variable",  
  value.name = "Value"
)

data_num$YearsAtCompanyGroup <- cut(data_num$YearsAtCompany,
                                    breaks = c(0, 4, 8, 10), 
                                    labels = c("0-4", "4-8", "8-10"),
                                    include.lowest = TRUE, right = FALSE)

variables_list <- unique(data_num$Variable)

for (variable in variables_list) {
  data_filtered <- data_num[Variable == variable]
  
  plot <- ggplot(data_filtered, aes(x = Attrition, y = Value, fill = Attrition)) +
    geom_boxplot() +
    labs(title = paste("Attrition and", variable, "by Years At Company Group"), 
         x = "Attrition", 
         y = variable) +
    facet_wrap(~ YearsAtCompanyGroup, scales = "free_y") +  
    theme_minimal() +
    scale_fill_manual(values = c("No" = "lightgreen", "Yes" = "salmon"))
  
  print(plot)
}
#----------distance--------------------------------------------------------------------
summary(data_num)

data_sum[, DistanceGroup := cut(DistanceFromHome, breaks = 5)]

distance_salary_analysis <- data_sum[, .(
  AvgSalary = mean(Salary, na.rm = TRUE),  
  TotalEmployees = .N  
), by = DistanceGroup]

# 可视化：按距离分组的平均薪资
ggplot(distance_salary_analysis, aes(x = DistanceGroup, y = AvgSalary)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Average Salary by Distance From Home (KM)",
       x = "Distance From Home (Grouped)", y = "Average Salary") +
  theme_minimal()
# 使用箱线图展示不同距离员工的薪资分布
ggplot(data_sum, aes(x = factor(DistanceFromHome), y = Salary)) +
  geom_boxplot(aes(fill = Attrition), outlier.shape = NA) +  # 去掉异常值点
  labs(title = "Salary Distribution by Distance From Home",
       x = "Distance From Home (KM)", y = "Salary") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 按距离分组，展示平均薪资
ggplot(distance_salary_analysis, aes(x = DistanceGroup, y = AvgSalary)) +
  geom_point(color = "blue", size = 3) +  
  geom_line(color = "blue", group = 1) +  
  labs(title = "Average Salary by Distance From Home",
       x = "Distance From Home (Grouped)", y = "Average Salary") +
  theme_minimal()
#---------promote time--------------------------------------------------------------------------
salary_promotion_analysis <- data_sum[, .(
  AvgSalary = mean(Salary, na.rm = TRUE),  
  TotalEmployees = .N,  
  AttritedEmployees = sum(Attrition == "Yes"),  
  AttritionRate = mean(Attrition == "Yes") * 100  
), by = YearsSinceLastPromotion]

salary_promotion_analysis <- salary_promotion_analysis[order(YearsSinceLastPromotion)]
print(salary_promotion_analysis)
library(ggplot2)

ggplot(salary_promotion_analysis, aes(x = YearsSinceLastPromotion, y = AvgSalary)) +
  geom_line(color = "blue") +
  geom_point(aes(size = AttritionRate), color = "red") +  
  labs(title = "Salary and Attrition by Years Since Last Promotion", 
       x = "Years Since Last Promotion", 
       y = "Average Salary") +
  theme_minimal()
#====================================================================================
summary(data_sum)
factor_vars <- names(data_sum)[sapply(data_sum, is.factor)]
factor_data_sum <- data_sum[, ..factor_vars]
summary(factor_data_sum)

factor_vars_count <- length(factor_vars)
factor_vars_count

library(ggplot2)
plots <- lapply(factor_vars, function(factor_var) {
  ggplot(data_sum, aes_string(x = factor_var, y = "Salary", fill = "Attrition")) +
    geom_boxplot() +
    labs(title = paste("Salary Distribution by", factor_var), x = factor_var, y = "Salary") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels to avoid overlap
})

print(plots[[1]])  
print(plots[[2]])  
print(plots[[3]])  
print(plots[[4]])  
print(plots[[5]])  
print(plots[[6]])  
print(plots[[7]])  
print(plots[[8]])  
print(plots[[9]])  

print(plots[[10]])  
#print(plots[[11]])  attrition
print(plots[[12]])  
print(plots[[13]])  
print(plots[[14]])  
print(plots[[15]])
print(plots[[16]])  
print(plots[[17]])  
print(plots[[18]])  



table(data_sum$Gender) 

table(data_sum$Gender)
#====================================================================================
attrited_data <- data_sum[Attrition == "Yes"]

descriptive_stats <- attrited_data[, .(
  min_YearsAtCompany = min(YearsAtCompany),
  max_YearsAtCompany = max(YearsAtCompany),
  mean_YearsAtCompany = mean(YearsAtCompany),
  mode_YearsAtCompany = as.numeric(names(sort(-table(YearsAtCompany))[1])),
  
  min_YearsInMostRecentRole = min(YearsInMostRecentRole),
  max_YearsInMostRecentRole = max(YearsInMostRecentRole),
  mean_YearsInMostRecentRole = mean(YearsInMostRecentRole),
  mode_YearsInMostRecentRole = as.numeric(names(sort(-table(YearsInMostRecentRole))[1])),
  
  min_YearsSinceLastPromotion = min(YearsSinceLastPromotion),
  max_YearsSinceLastPromotion = max(YearsSinceLastPromotion),
  mean_YearsSinceLastPromotion = mean(YearsSinceLastPromotion),
  mode_YearsSinceLastPromotion = as.numeric(names(sort(-table(YearsSinceLastPromotion))[1])),
  
  min_YearsWithCurrManager = min(YearsWithCurrManager),
  max_YearsWithCurrManager = max(YearsWithCurrManager),
  mean_YearsWithCurrManager = mean(YearsWithCurrManager),
  mode_YearsWithCurrManager = as.numeric(names(sort(-table(YearsWithCurrManager))[1]))
)]

descriptive_stats
descriptive_stats_final <- data.table(
  Category = c("YearsAtCompany", "YearsInMostRecentRole", "YearsSinceLastPromotion", "YearsWithCurrManager"),
  min = c(descriptive_stats$min_YearsAtCompany, descriptive_stats$min_YearsInMostRecentRole, descriptive_stats$min_YearsSinceLastPromotion, descriptive_stats$min_YearsWithCurrManager),
  max = c(descriptive_stats$max_YearsAtCompany, descriptive_stats$max_YearsInMostRecentRole, descriptive_stats$max_YearsSinceLastPromotion, descriptive_stats$max_YearsWithCurrManager),
  mean = c(descriptive_stats$mean_YearsAtCompany, descriptive_stats$mean_YearsInMostRecentRole, descriptive_stats$mean_YearsSinceLastPromotion, descriptive_stats$mean_YearsWithCurrManager),
  mode = c(descriptive_stats$mode_YearsAtCompany, descriptive_stats$mode_YearsInMostRecentRole, descriptive_stats$mode_YearsSinceLastPromotion, descriptive_stats$mode_YearsWithCurrManager)
)

descriptive_stats_transposed <- transpose(descriptive_stats_final, make.names = "Category")
descriptive_stats_transposed <- cbind(Statistic = c("min", "max", "mean", "mode"), descriptive_stats_transposed)
descriptive_stats_transposed
#===============================================================
summary(attrited_data)
factor_vars <- names(attrited_data)[sapply(attrited_data, is.factor)]
factor_attrited_data <- attrited_data[, ..factor_vars]
summary(factor_attrited_data)

factor_vars_count <- length(factor_vars)
factor_vars_count

library(ggplot2)
plots <- lapply(factor_vars, function(factor_var) {
  ggplot(attrited_data, aes_string(x = factor_var, y = "Salary", fill = "Attrition")) +
    geom_boxplot() +
    labs(title = paste("Salary Distribution by", factor_var), x = factor_var, y = "Salary") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels to avoid overlap
})





print(plots[[10]])  
#Attrition
print(plots[[11]])  
print(plots[[12]])  
print(plots[[13]])  
#print(plots[[14]])  
print(plots[[15]])
#print(plots[[16]])  
#print(plots[[17]])  
#print(plots[[18]])  



table(attrited_data$Gender) 

table(attrited_data$Gender)
