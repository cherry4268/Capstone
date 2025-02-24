import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os

def barchart(df, df_name):
    # Bar Chart
    sns.barplot(x='Year', y='Value', hue='Period', data=df)
    plt.title(f'{df_name} Monthly Performance by Year')
    plt.xlabel('Year')
    plt.ylabel('Value')
    plt.xticks(rotation=45)  # Rotate year labels for better readability
    plt.tight_layout()  # Adjust layout to prevent label cutoff
    plt.show()
    
    return df

# Upload data
milk_ne = pd.read_csv('data/Grocery/milk/milk_ne.csv')
milk_south = pd.read_csv('data/Grocery/milk/milk_south.csv')
milk_west = pd.read_csv('data/Grocery/milk/milk_west.csv')


barchart(milk_ne, 'Milk_NE')
barchart(milk_south, 'Milk_South')
barchart(milk_west, 'Milk_West')
