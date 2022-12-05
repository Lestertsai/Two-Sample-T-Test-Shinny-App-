# Two-Sample-T-Test-Shinny-App-
A R Shiny web app built for conducting a two-sample t-test easily

It might be painful to do a two-sample t-test for people with no statistical background because it involves several assumptions, even this is a common test in statistics. The process of performing such a test in Excel is tedious since we first need to click and drag our data to calculate variance and click on several buttons to acquire a p-value. There is a more elegant way to complete this task utilizing R. This project aims to solve this problem, allowing users to conduct the test swiftly from any data source. It was accomplished by building a web application using the R shiny package. The data planned to compare should be in two variables in the input data, and users can decide which two to select. If the selected columns are characteristic, warning messages will appear. Also, the textbox can prevent users from picking more than two columns. Before the test, the assumptions will be tested beforehand, including the normality of the two variables (Shapiro–Wilk test) and whether their variance is equal (Levene's test). If the sample size is larger than 300, we can assume normality, so only the equivalence of variance will be examined. Otherwise, we need to ensure the assumptions are not violated for small samples. If the normality assumption is not met, the p-value will be calculated based on the two-sample bootstrap t-tests with 1000 bootstrap replicates. Also, both tests will adjust the computation depending on if the variance of the two variables is the same.

You can click [here](https://lestertsai123.shinyapps.io/demo/) to visit my app!

