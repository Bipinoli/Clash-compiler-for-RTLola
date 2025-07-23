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
        cells_old.append(result['result']['old_architecture']['number_of_cells'])
        cells_new.append(result['result']['new_architecture']['number_of_cells'])
    width = 0.35
    fig, ax = plt.subplots(layout='constrained')
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.bar_label(rects1, padding=3, fontsize=6)
    ax.bar_label(rects2, padding=3, fontsize=6)
    ax.set_ylabel('Total number of cells')
    ax.set_title("Total Cells")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 120000)
    plt.savefig("plots/total_cells.svg", format="svg")
    # plt.show()


def plot_sequential_cells(results):
    num_specs = len(results['specs'])
    x = np.arange(num_specs)
    cells_old = []
    cells_new = []
    for result in results['specs']:
        cells_old.append(result['result']['old_architecture']['number_of_sequential_cells'])
        cells_new.append(result['result']['new_architecture']['number_of_sequential_cells'])
    width = 0.35
    fig, ax = plt.subplots(layout='constrained')
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.bar_label(rects1, padding=3, fontsize=6)
    ax.bar_label(rects2, padding=3, fontsize=6)
    ax.set_ylabel('Total number of sequential cells')
    ax.set_title("Sequential cells")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 20000)
    plt.savefig("plots/sequential_cells.svg", format="svg")
    # plt.show()


def plot_combinational_cells(results):
    num_specs = len(results['specs'])
    x = np.arange(num_specs)
    cells_old = []
    cells_new = []
    for result in results['specs']:
        cells_old.append(result['result']['old_architecture']['number_of_combinational_cells'])
        cells_new.append(result['result']['new_architecture']['number_of_combinational_cells'])
    width = 0.35
    fig, ax = plt.subplots(layout='constrained')
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.bar_label(rects1, padding=3, fontsize=6)
    ax.bar_label(rects2, padding=3, fontsize=6)
    ax.set_ylabel('Total number of combinational cells')
    ax.set_title("Combinational cells")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 120000)
    plt.savefig("plots/combinational_cells.svg", format="svg")
    # plt.show()


def plot_wire_bits(results):
    num_specs = len(results['specs'])
    x = np.arange(num_specs)
    cells_old = []
    cells_new = []
    for result in results['specs']:
        cells_old.append(result['result']['old_architecture']['number_of_wire_bits'])
        cells_new.append(result['result']['new_architecture']['number_of_wire_bits'])
    width = 0.35
    fig, ax = plt.subplots(layout='constrained')
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.bar_label(rects1, padding=3, fontsize=6)
    ax.bar_label(rects2, padding=3, fontsize=6)
    ax.set_ylabel('Total number of wire bits')
    ax.set_title("Wire bits")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 340000)
    plt.savefig("plots/wire_bits.svg", format="svg")
    # plt.show()


def plot_wires(results):
    num_specs = len(results['specs'])
    x = np.arange(num_specs)
    cells_old = []
    cells_new = []
    for result in results['specs']:
        cells_old.append(result['result']['old_architecture']['number_of_wires'])
        cells_new.append(result['result']['new_architecture']['number_of_wires'])
    width = 0.35
    fig, ax = plt.subplots(layout='constrained')
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.bar_label(rects1, padding=3, fontsize=6)
    ax.bar_label(rects2, padding=3, fontsize=6)
    ax.set_ylabel('Total number of wires')
    ax.set_title("Wires")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 100000)
    plt.savefig("plots/wires.svg", format="svg")
    # plt.show()


def plot_public_wires(results):
    num_specs = len(results['specs'])
    x = np.arange(num_specs)
    cells_old = []
    cells_new = []
    for result in results['specs']:
        cells_old.append(result['result']['old_architecture']['number_of_public_wires'])
        cells_new.append(result['result']['new_architecture']['number_of_public_wires'])
    width = 0.35
    fig, ax = plt.subplots(layout='constrained')
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.bar_label(rects1, padding=3, fontsize=6)
    ax.bar_label(rects2, padding=3, fontsize=6)
    ax.set_ylabel('Total number of public wires')
    ax.set_title("Public wires")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 2000)
    plt.savefig("plots/public_wires.svg", format="svg")
    # plt.show()


plot_total_cells(results)
plot_sequential_cells(results)
plot_combinational_cells(results)
plot_wire_bits(results)
plot_wires(results)
plot_public_wires(results)
