"""
Interactive 3D PCA cluster plot.
Drag to rotate, scroll to zoom. Click custom legend to show/hide clusters.
Output: Cluster_Separation_PCA_3D_interactive.html
"""

import json
import pandas as pd
import numpy as np
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import plotly.graph_objects as go

df = pd.read_csv('Dataset.csv')

features = ['age', 'mean_BMI', 'Trig/HDL ratio', 'HbA1c']
X = df[features].values

scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

pca_3d = PCA(n_components=3)
X_pca_3d = pca_3d.fit_transform(X_scaled)

cluster_names = {1: 'SIDD', 2: 'MOD', 3: 'SIRD', 4: 'MARD'}
colors_new = {1: '#A6CEE3', 2: '#FBB4AE', 3: '#B2DF8A', 4: '#CAB2D6'}
cluster_order = [1, 3, 2, 4]

LEGEND_FONT_SIZE = 18
LEGEND_SYMBOL_SIZE = 22
plotly_symbols = {1: 'circle', 2: 'cross', 4: 'diamond'}
# Per-symbol sizes tuned so all clusters appear the same visual size in 3D.
marker_sizes = {1: 6.5, 2: 4.8, 4: 5.8}
legend_symbols = {1: '●', 2: '✚', 3: '▲', 4: '◆'}
TEXT_MARKER_CLUSTERS = {3}
TEXT_MARKER_CHAR = {3: '▲'}
TEXT_MARKER_SIZE = 9

fig = go.Figure()
trace_map = {}

for cluster in cluster_order:
    mask = df['cluster'] == cluster
    group = cluster_names[cluster]
    trace_indices = []

    cluster_color = colors_new[cluster]
    if cluster in TEXT_MARKER_CLUSTERS:
        fig.add_trace(
            go.Scatter3d(
                x=X_pca_3d[mask, 0],
                y=X_pca_3d[mask, 1],
                z=X_pca_3d[mask, 2],
                mode='text',
                text=[TEXT_MARKER_CHAR[cluster]] * int(mask.sum()),
                textfont=dict(
                    color=cluster_color,
                    size=TEXT_MARKER_SIZE,
                    family='Arial Black, Arial, sans-serif',
                ),
                name=group,
                showlegend=False,
                hovertemplate=(
                    f'<b>{group}</b><br>'
                    'PC1: %{x:.2f}<br>'
                    'PC2: %{y:.2f}<br>'
                    'PC3: %{z:.2f}<extra></extra>'
                ),
            )
        )
    else:
        fig.add_trace(
            go.Scatter3d(
                x=X_pca_3d[mask, 0],
                y=X_pca_3d[mask, 1],
                z=X_pca_3d[mask, 2],
                mode='markers',
                name=group,
                showlegend=False,
                marker=dict(
                    size=marker_sizes[cluster],
                    color=cluster_color,
                    symbol=plotly_symbols[cluster],
                    opacity=1.0,
                    line=dict(width=0, color=cluster_color),
                ),
                hovertemplate=(
                    f'<b>{group}</b><br>'
                    'PC1: %{x:.2f}<br>'
                    'PC2: %{y:.2f}<br>'
                    'PC3: %{z:.2f}<extra></extra>'
                ),
            )
        )
    trace_indices.append(len(fig.data) - 1)

    fig.add_trace(
        go.Scatter3d(
            x=[X_pca_3d[mask, 0].mean()],
            y=[X_pca_3d[mask, 1].mean()],
            z=[X_pca_3d[mask, 2].mean()],
            mode='markers',
            name=f'{group} centroid',
            showlegend=False,
            marker=dict(size=6, color='#FFEB3B', symbol='x', line=dict(width=2)),
            hoverinfo='skip',
        )
    )
    trace_indices.append(len(fig.data) - 1)
    trace_map[group] = trace_indices

# Enforce zero-width borders on cluster marker traces
for i, cluster in enumerate(cluster_order):
    data_idx = i * 2
    cluster_color = colors_new[cluster]
    fig.data[data_idx].marker.line = dict(width=0, color=cluster_color)
    fig.data[data_idx].marker.opacity = 1

var = pca_3d.explained_variance_ratio_
fig.update_layout(
    title=None,
    scene=dict(
        domain=dict(x=[0.0, 1.0], y=[0.0, 1.0]),
        xaxis=dict(
            title=dict(
                text=f'PC1 ({var[0]:.2%} variance)',
                font=dict(size=14, family='Arial Black, Arial, sans-serif'),
            ),
            backgroundcolor='white',
            gridcolor='lightgray',
        ),
        yaxis=dict(
            title=dict(
                text=f'PC2 ({var[1]:.2%} variance)',
                font=dict(size=14, family='Arial Black, Arial, sans-serif'),
            ),
            backgroundcolor='white',
            gridcolor='lightgray',
        ),
        zaxis=dict(
            title=dict(
                text=f'PC3 ({var[2]:.2%} variance)',
                font=dict(size=14, family='Arial Black, Arial, sans-serif'),
            ),
            backgroundcolor='white',
            gridcolor='lightgray',
        ),
        bgcolor='white',
    ),
    showlegend=False,
    margin=dict(l=0, r=0, t=0, b=0),
    paper_bgcolor='white',
    width=920,
    height=760,
)

plot_div_id = 'pca-plot-3d'
plot_html = fig.to_html(
    include_plotlyjs=False,
    full_html=False,
    div_id=plot_div_id,
    config={'scrollZoom': True, 'displayModeBar': True, 'displaylogo': False},
)

legend_items_html = []
for cluster in cluster_order:
    group = cluster_names[cluster]
    color = colors_new[cluster]
    sym = legend_symbols[cluster]
    legend_items_html.append(
        f'<button type="button" class="legend-item active" data-cluster="{group}" '
        f'aria-pressed="true" title="Click: show/hide · Double-click: isolate">'
        f'<span class="legend-symbol" style="color:{color};">{sym}</span>'
        f'<span class="legend-label" style="color:{color};">{group}</span>'
        f'</button>'
    )

trace_map_json = json.dumps(trace_map)

full_html = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>Cluster Separation - PCA Analysis (3D)</title>
<script src="https://cdn.plot.ly/plotly-3.6.0.min.js"></script>
<style>
  * {{ box-sizing: border-box; }}
  body {{
    margin: 0;
    background: #fff;
    font-family: Arial, sans-serif;
  }}
  #page {{
    width: 1100px;
    margin: 0 auto;
    padding: 6px 8px 10px;
  }}
  #plot-title {{
    margin: 0 0 4px 0;
    text-align: center;
    font-size: 18px;
    font-weight: 800;
    font-family: 'Arial Black', Arial, sans-serif;
    line-height: 1.15;
  }}
  #main-row {{
    display: flex;
    align-items: stretch;
    gap: 0;
    height: 760px;
  }}
  #custom-legend {{
    width: 148px;
    flex: 0 0 148px;
    padding: 14px 10px 10px 8px;
    background: rgba(255, 255, 255, 0.98);
    border: 1px solid #888;
    border-radius: 4px;
    box-shadow: 0 1px 4px rgba(0,0,0,0.1);
    z-index: 10;
  }}
  .legend-item {{
    display: flex;
    align-items: center;
    gap: 8px;
    width: 100%;
    margin: 0 0 10px 0;
    padding: 4px 2px;
    border: none;
    background: transparent;
    cursor: pointer;
    font-weight: bold;
    text-align: left;
  }}
  .legend-item:last-child {{ margin-bottom: 0; }}
  .legend-item:hover {{ background: rgba(0,0,0,0.04); border-radius: 3px; }}
  .legend-item.inactive {{ opacity: 0.35; }}
  .legend-item.inactive .legend-label,
  .legend-item.inactive .legend-symbol {{
    text-decoration: line-through;
  }}
  .legend-symbol {{
    width: 28px;
    text-align: center;
    font-size: {LEGEND_SYMBOL_SIZE}px;
    line-height: 1;
    flex-shrink: 0;
  }}
  .legend-label {{
    font-size: {LEGEND_FONT_SIZE}px;
    line-height: 1.1;
  }}
  #plot-area {{
    flex: 1 1 auto;
    min-width: 0;
    height: 760px;
  }}
  #{plot_div_id} {{
    width: 100%;
    height: 100%;
  }}
</style>
</head>
<body>
<div id="page">
  <h1 id="plot-title">Cluster Separation - PCA Analysis (3D)</h1>
  <div id="main-row">
    <div id="custom-legend" aria-label="Cluster legend">
      {''.join(legend_items_html)}
    </div>
    <div id="plot-area">
      {plot_html}
    </div>
  </div>
</div>
<script>
(function() {{
  const PLOT_ID = '{plot_div_id}';
  const TRACE_MAP = {trace_map_json};
  const visibility = {{}};
  Object.keys(TRACE_MAP).forEach(function(name) {{ visibility[name] = true; }});

  function setClusterVisible(name, visible) {{
    const indices = TRACE_MAP[name];
    Plotly.restyle(PLOT_ID, {{'visible': visible}}, indices);
    visibility[name] = visible;
    const el = document.querySelector('.legend-item[data-cluster="' + name + '"]');
    if (el) {{
      el.classList.toggle('active', visible);
      el.classList.toggle('inactive', !visible);
      el.setAttribute('aria-pressed', visible ? 'true' : 'false');
    }}
  }}

  function bindLegend() {{
    document.querySelectorAll('.legend-item').forEach(function(item) {{
      item.addEventListener('click', function(e) {{
        e.preventDefault();
        e.stopPropagation();
        const name = item.getAttribute('data-cluster');
        setClusterVisible(name, !visibility[name]);
      }});
      item.addEventListener('dblclick', function(e) {{
        e.preventDefault();
        e.stopPropagation();
        const name = item.getAttribute('data-cluster');
        Object.keys(TRACE_MAP).forEach(function(other) {{
          setClusterVisible(other, other === name);
        }});
      }});
    }});
  }}

  function removeMarkerOutlines(gd) {{
    for (var i = 0; i < gd.data.length; i++) {{
      var tr = gd.data[i];
      if (!tr.marker || (tr.name && tr.name.indexOf('centroid') >= 0)) continue;
      var fill = tr.marker.color;
      tr.marker.opacity = 1;
      tr.marker.line = {{width: 0, color: fill}};
    }}
    return Plotly.react(PLOT_ID, gd.data, gd.layout, {{
      scrollZoom: true,
      displayModeBar: true,
      displaylogo: false,
      responsive: true
    }});
  }}

  function waitForPlot(cb) {{
    const gd = document.getElementById(PLOT_ID);
    if (gd && gd.data && gd.data.length) {{
      cb(gd);
      return;
    }}
    setTimeout(function() {{ waitForPlot(cb); }}, 80);
  }}

  waitForPlot(function(gd) {{
    removeMarkerOutlines(gd).then(function() {{
      bindLegend();
    }});
  }});
}})();
</script>
</body>
</html>
"""

output_file = 'Cluster_Separation_PCA_3D_interactive.html'
with open(output_file, 'w', encoding='utf-8') as f:
    f.write(full_html)

print(f"Interactive 3D PCA plot saved as '{output_file}'")
print("Layout: legend left column · click to toggle clusters")
