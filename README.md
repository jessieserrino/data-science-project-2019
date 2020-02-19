How to look at the data
---------------------------------------------------

Go to output > hclust.csv for the data from the hclust analysis. We have 6 clusters.

Some fields will look numerical. Feel free to copy paste the columns that share the same names from input > data.csv for more clarity.

Look at team_cluster_analysis.r for tips on how to analyze the data. We have 6 clusters, the 2nd and the 3rd have the highest and lowest attrition.

Note: If you run `table(data$cluster, data$OTHER_FIELD)`, it will show some interesting insight about what each cluster looks like. I personally like running `table(data$cluster, data$JobLevel)`
