import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os


os.chdir(r"c:\Users\cherr\Downloads\Data Science\Capstone")

# Verify the current working directory
print(os.getcwd())

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
food_bev_midwestA = pd.read_csv('data/Grocery/food_bev/food_bev_midwestA.csv')
food_bev_midwestBC = pd.read_csv('data/Grocery/food_bev/food_bev_midwestBC.csv')
food_bev_westA = pd.read_csv('data/Grocery/food_bev/food_bev_westA.csv')
food_bev_westBC = pd.read_csv('data/Grocery/food_bev/food_bev_westBC.csv')
food_bev_southA = pd.read_csv('data/Grocery/food_bev/food_bev_southA.csv')
food_bev_southBC = pd.read_csv('data/Grocery/food_bev/food_bev_southBC.csv')
food_bev_neA = pd.read_csv('data/Grocery/food_bev/food_bev_neA.csv')
food_bev_neBC = pd.read_csv('data/Grocery/food_bev/food_bev_neBC.csv')

barchart(food_bev_midwestA, 'food_bev_midwestA')
barchart(food_bev_midwestBC, 'food_bev_midwestBC')

import os

# Get the absolute path of the current script file
script_path = os.path.abspath(__file__)

# Get the directory containing the script
script_dir = os.path.dirname(script_path)

# If your project's root directory is the parent directory of the script, 
# you can get it like this:
project_root = os.path.dirname(script_dir)

print(f"Project Root Directory: {project_root}")