"""Run all figure-generation scripts in this folder."""

import subprocess
import sys
from pathlib import Path

SCRIPTS = [
    'create_figure2_plot.py',
    'create_cluster_3d_2d_views.py',
    'create_pca_plot_3d_interactive.py',
]

def main():
    root = Path(__file__).resolve().parent
    for name in SCRIPTS:
        script = root / name
        print(f'Running {name}...')
        subprocess.run([sys.executable, str(script)], cwd=root, check=True)
    print('All figures generated.')

if __name__ == '__main__':
    main()
