import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

'''
This code identifies seasonality in original datasets to allow for seasonal adjustement
'''

# Plot the independent variables as bar charts
def barchart(df):
  df_barchart = df.melt(id_vars='Year', var_name='Period', value_name= 'Value')

  # Visualize the Bar Chart
  sns.barplot(x='Year', y='Value', hue='Period', data=df_barchart)
  plt.title('Monthly Performance by Year')
  plt.xlabel('Year')
  plt.ylabel('Value')
  plt.show()

  return df_barchart

