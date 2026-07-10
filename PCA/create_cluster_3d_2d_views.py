"""
Four-panel PCA figure: (a) 3D view; (b–d) 2D projections from different PC planes.
Output: Cluster_3D_and_2D_Views.png
"""

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler

df = pd.read_csv('Dataset.csv')

features = ['age', 'mean_BMI', 'Trig/HDL ratio', 'HbA1c']
X = StandardScaler().fit_transform(df[features].values)

pca_3d = PCA(n_components=3)
X_pca_3d = pca_3d.fit_transform(X)

cluster_names = {1: 'SIDD', 2: 'MOD', 3: 'SIRD', 4: 'MARD'}
colors_new = {1: '#A6CEE3', 2: '#FBB4AE', 3: '#B2DF8A', 4: '#CAB2D6'}
centroid_colors_2d = {
    1: '#3D85AD',  # SIDD — darker blue
    2: '#D96B5C',  # MOD  — darker coral
    3: '#4D9940',  # SIRD — darker green
    4: '#8E6FA0',  # MARD — darker purple
}
cluster_order = [1, 3, 2, 4]
markers = {1: 'o', 2: '+', 3: '^', 4: 'D'}

var = pca_3d.explained_variance_ratio_
pc_labels = [
    f'PC1 ({var[0]:.2%} variance)',
    f'PC2 ({var[1]:.2%} variance)',
    f'PC3 ({var[2]:.2%} variance)',
]

MARKER_SIZE_3D = 75
MARKER_SIZE_2D = 55
CENTROID_MARKERSIZE = 8.8
CENTROID_MARKEREWIDTH = 1.28
LEGEND_FONT_SIZE = 15
LEGEND_MARKER_SCALE = 1.51
AXIS_LABEL_FONT_SIZE = 14
TITLE_FONT_SIZE = 20
TICK_FONT_SIZE = 13
SPREAD_FACTOR_3D = 4.2
AXIS_LIMIT_SPREAD = 3.00  # fixed 3D axis box; only point positions use SPREAD_FACTOR_3D
AXIS_PADDING = 0.14
SPREAD_FACTOR_2D = 3.10  # point spread in panels b–d (fills axis box without clipping)
AXIS_LIMIT_SPREAD_2D = 3.00  # fixed 2D axis box for panels b–d
AXIS_PADDING_2D = 0.035  # minimal margin so points reach panel edges safely

center = X_pca_3d.mean(axis=0)
X_plot_2d = center + (X_pca_3d - center) * SPREAD_FACTOR_2D
X_limits_2d = center + (X_pca_3d - center) * AXIS_LIMIT_SPREAD_2D
X_plot_3d = center + (X_pca_3d - center) * SPREAD_FACTOR_3D
X_limits_3d = center + (X_pca_3d - center) * AXIS_LIMIT_SPREAD

VIEW_3D = (22, 45)
PANEL_LABELS = ['a', 'b', 'c', 'd']

# 2D panels: orthogonal projections onto different principal-component planes
PANELS_2D = [
    (0, 1),
    (0, 2),
    (1, 2),
]


def padded_limits(coords, padding=AXIS_PADDING):
    lo, hi = coords.min(), coords.max()
    pad = (hi - lo) * padding
    return lo - pad, hi + pad


def marker_kwargs(marker, color):
    if marker == '+':
        return dict(edgecolors=color, linewidths=1.8)
    return dict(edgecolors='none', linewidths=0)


def plot_clusters_3d(ax, show_legend=False):
    for cluster in cluster_order:
        mask = df['cluster'] == cluster
        color = colors_new[cluster]
        marker = markers[cluster]
        ax.scatter(
            X_plot_3d[mask, 0],
            X_plot_3d[mask, 1],
            X_plot_3d[mask, 2],
            c=color,
            marker=marker,
            s=MARKER_SIZE_3D,
            alpha=1.0,
            depthshade=False,
            label=cluster_names[cluster] if show_legend else None,
            **marker_kwargs(marker, color),
        )

    # Draw centroids last with high-contrast styling for 3D visibility
    for cluster in cluster_order:
        mask = df['cluster'] == cluster
        centroid_color = centroid_colors_2d[cluster]
        cx = X_plot_3d[mask, 0].mean()
        cy = X_plot_3d[mask, 1].mean()
        cz = X_plot_3d[mask, 2].mean()
        ax.plot(
            [cx], [cy], [cz],
            marker='X',
            markersize=CENTROID_MARKERSIZE,
            markeredgewidth=CENTROID_MARKEREWIDTH,
            markerfacecolor=centroid_color,
            markeredgecolor=centroid_color,
            color=centroid_color,
            linestyle='None',
            zorder=100,
        )

    ax.set_xlabel(pc_labels[0], fontsize=AXIS_LABEL_FONT_SIZE, fontweight='bold', labelpad=8)
    ax.set_ylabel(pc_labels[1], fontsize=AXIS_LABEL_FONT_SIZE, fontweight='bold', labelpad=8)
    ax.set_zlabel(pc_labels[2], fontsize=AXIS_LABEL_FONT_SIZE, fontweight='bold', labelpad=8)
    ax.tick_params(axis='x', labelsize=TICK_FONT_SIZE)
    ax.tick_params(axis='y', labelsize=TICK_FONT_SIZE)
    ax.tick_params(axis='z', labelsize=TICK_FONT_SIZE)
    ax.view_init(elev=VIEW_3D[0], azim=VIEW_3D[1])
    ax.set_xlim(*padded_limits(X_limits_3d[:, 0]))
    ax.set_ylim(*padded_limits(X_limits_3d[:, 1]))
    ax.set_zlim(*padded_limits(X_limits_3d[:, 2]))
    ax.grid(True, alpha=0.3)
    for axis in (ax.xaxis, ax.yaxis, ax.zaxis):
        axis.label.set_fontweight('bold')


def plot_clusters_2d(ax, x_idx, y_idx):
    for cluster in cluster_order:
        mask = df['cluster'] == cluster
        color = colors_new[cluster]
        marker = markers[cluster]
        ax.scatter(
            X_plot_2d[mask, x_idx],
            X_plot_2d[mask, y_idx],
            c=color,
            marker=marker,
            s=MARKER_SIZE_2D,
            alpha=1.0,
            **marker_kwargs(marker, color),
        )
        centroid_color = centroid_colors_2d[cluster]
        ax.plot(
            [X_plot_2d[mask, x_idx].mean()],
            [X_plot_2d[mask, y_idx].mean()],
            marker='X',
            markersize=CENTROID_MARKERSIZE,
            markeredgewidth=CENTROID_MARKEREWIDTH,
            markerfacecolor=centroid_color,
            markeredgecolor=centroid_color,
            color=centroid_color,
            linestyle='None',
            zorder=5,
        )

    ax.set_xlabel(pc_labels[x_idx], fontsize=AXIS_LABEL_FONT_SIZE, fontweight='bold')
    ax.set_ylabel(pc_labels[y_idx], fontsize=AXIS_LABEL_FONT_SIZE, fontweight='bold')
    ax.tick_params(axis='both', labelsize=TICK_FONT_SIZE)
    ax.set_xlim(*padded_limits(X_limits_2d[:, x_idx], AXIS_PADDING_2D))
    ax.set_ylim(*padded_limits(X_limits_2d[:, y_idx], AXIS_PADDING_2D))
    ax.grid(True, alpha=0.3, linestyle='--')
    ax.axhline(0, color='k', linewidth=0.8, alpha=0.35)
    ax.axvline(0, color='k', linewidth=0.8, alpha=0.35)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontweight('bold')


fig = plt.figure(figsize=(18, 16))

ax_a = fig.add_subplot(2, 2, 1, projection='3d')
plot_clusters_3d(ax_a, show_legend=True)
ax_a.set_title(PANEL_LABELS[0], fontsize=TITLE_FONT_SIZE, fontweight='bold', loc='left', pad=10)

ax_a.legend(
    loc='upper left',
    ncol=4,
    fontsize=LEGEND_FONT_SIZE,
    markerscale=LEGEND_MARKER_SCALE,
    handlelength=2.2,
    handletextpad=0.6,
    columnspacing=1.2,
    framealpha=0.95,
    edgecolor='gray',
    prop={'weight': 'bold', 'size': LEGEND_FONT_SIZE},
)

for panel_idx, (x_idx, y_idx) in enumerate(PANELS_2D, start=1):
    ax = fig.add_subplot(2, 2, panel_idx + 1)
    plot_clusters_2d(ax, x_idx, y_idx)
    ax.set_title(
        PANEL_LABELS[panel_idx],
        fontsize=TITLE_FONT_SIZE,
        fontweight='bold',
        loc='left',
        pad=10,
    )

plt.tight_layout()
output_file = 'Cluster_3D_and_2D_Views.png'
plt.savefig(output_file, dpi=300, bbox_inches='tight', facecolor='white')
plt.close(fig)
print(f"Figure saved as '{output_file}'")
