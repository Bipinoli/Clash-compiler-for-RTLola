import matplotlib.pyplot as plt
import numpy as np
import yaml

with open('results.yml', 'r') as file:
    results = yaml.safe_load(file)

def plot_total_cells(results):
    num_specs = len(results['specs'])
    x = np.arange(num_specs)

    cells_old = []
    cells_new = []

    for result in results['specs']:
        cells_old.append(result['result']['old_architecture']['number_cells'])
        cells_new.append(result['result']['new_architecture']['number_cells'])

    width = 0.35
    fig, ax = plt.subplots(layout='constrained')

    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')

    ax.bar_label(rects1, padding=3, fontsize=6)
    ax.bar_label(rects2, padding=3, fontsize=6)

    ax.set_ylabel('Total number of cells')
    ax.set_title("RTLola Spec: Total Cells")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 120000)

    plt.show()



plot_total_cells(results)
