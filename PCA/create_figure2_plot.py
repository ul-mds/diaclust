import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Set style for better-looking plots
sns.set_style("whitegrid")
plt.rcParams['figure.figsize'] = (14, 10)
plt.rcParams['font.size'] = 18

# Read the dataset
df = pd.read_csv('Dataset.csv')

# Cluster names mapping
cluster_names = {
    1: 'SIDD',
    2: 'MOD',
    3: 'SIRD',
    4: 'MARD'
}

# Display order and colors by cluster name
cluster_order = ['SIDD', 'SIRD', 'MOD', 'MARD']
colors = {
    'SIDD': '#A6CEE3',
    'SIRD': '#B2DF8A',
    'MOD': '#FBB4AE',
    'MARD': '#CAB2D6'
}

# Map cluster numbers to names
df['cluster_name'] = df['cluster'].map(cluster_names)

# Select features to plot
features = ['age', 'mean_BMI', 'HbA1c', 'Trig/HDL ratio']
feature_labels = ['Age (years)', 'BMI (kg/m²)', 'HbA1c (%)', 'TG/HDL-C ratio']

# Create subplots
fig, axes = plt.subplots(2, 2, figsize=(16, 12))
axes = axes.flatten()

# Plot each feature
for idx, (feature, label) in enumerate(zip(features, feature_labels)):
    ax = axes[idx]
    
    # Create box plot for each cluster in specified order
    data_to_plot = []
    positions = []
    cluster_labels = []
    
    for i, name in enumerate(cluster_order, start=1):
        cluster_num = [k for k, v in cluster_names.items() if v == name][0]
        cluster_data = df[df['cluster'] == cluster_num][feature].dropna()
        data_to_plot.append(cluster_data)
        positions.append(i)
        cluster_labels.append(name)
    
    # Create box plot with custom colors
    bp = ax.boxplot(data_to_plot, positions=positions, patch_artist=True, 
                    widths=0.6, showmeans=True, meanline=True)
    
    # Color the boxes
    for patch, name in zip(bp['boxes'], cluster_labels):
        patch.set_facecolor(colors[name])
        patch.set_alpha(0.7)
        patch.set_edgecolor('black')
        patch.set_linewidth(1.5)
    
    # Style other elements
    for element in ['whiskers', 'fliers', 'means', 'medians', 'caps']:
        plt.setp(bp[element], color='black', linewidth=1.5)
    
    # Set x-axis labels (cluster names, bold)
    ax.set_xticks(range(1, len(cluster_order) + 1))
    ax.set_xticklabels(cluster_order, fontsize=18, fontweight='bold')
    
    # Set y-axis label (mixed number/word expressions, e.g. BMI kg/m², HbA1c)
    ax.set_ylabel(label, fontsize=18, fontweight='bold')
    
    # Y-axis number ticks and x-axis word ticks at 18pt
    ax.tick_params(axis='both', which='major', labelsize=18, width=2, length=6)
    for label_tick in ax.get_yticklabels():
        label_tick.set_fontweight('bold')
    for label_tick in ax.get_xticklabels():
        label_tick.set_fontweight('bold')
    
    # Remove top and right spines
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_linewidth(2)
    ax.spines['bottom'].set_linewidth(2)
    
    # Add grid
    ax.grid(True, alpha=0.3, linestyle='--', axis='y', linewidth=1.5)

# Remove any titles (main title and subgroup titles)
plt.suptitle('', fontsize=0)  # Remove main title

# Tight layout
plt.tight_layout()

# Save the plot
plt.savefig('Figure2_Cluster_Characteristics.png', dpi=300, bbox_inches='tight')
print("Figure2 plot saved as 'Figure2_Cluster_Characteristics.png'")

# plt.show()  # disabled for headless regeneration


