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
    fig, ax = plt.subplots(layout='constrained', figsize=(10, 5))
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.bar_label(rects1, padding=3, fontsize=8)
    ax.bar_label(rects2, padding=3, fontsize=8)
    ax.set_ylabel('Total number of primitives')
    ax.set_title("Total number of primitives")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 120000)
    plt.savefig("plots/total_cells.svg", format="svg")
    # plt.show()

def plot_total_cells_relative(results):
    num_specs = len(results['specs'])
    x = np.arange(num_specs)
    cells_old = []
    cells_new = []
    for result in results['specs']:
        in_old = result['result']['old_architecture']['number_of_cells'] 
        in_new = result['result']['new_architecture']['number_of_cells']
        total = in_old + in_new
        cells_old.append(in_old / total)
        cells_new.append(in_new / total)
    width = 0.35
    fig, ax = plt.subplots(layout='constrained', figsize=(10, 5))
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.set_ylabel('Ratio')
    ax.set_title("Relative number of primitives")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 0.8)
    plt.savefig("plots/total_cells_relative.svg", format="svg")
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
    fig, ax = plt.subplots(layout='constrained', figsize=(10, 5))
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.bar_label(rects1, padding=3, fontsize=8)
    ax.bar_label(rects2, padding=3, fontsize=8)
    ax.set_ylabel('Total number of sequential primitives')
    ax.set_title("Sequential primitives")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 20000)
    plt.savefig("plots/sequential_cells.svg", format="svg")
    # plt.show()


def plot_sequential_ratio(results):
    num_specs = len(results['specs'])
    x = np.arange(num_specs)
    cells_old = []
    cells_new = []
    for result in results['specs']:
        cells_old.append(result['result']['old_architecture']['number_of_sequential_cells'] / result['result']['old_architecture']['number_of_cells'])
        cells_new.append(result['result']['new_architecture']['number_of_sequential_cells'] / result['result']['new_architecture']['number_of_cells'])
    width = 0.35
    fig, ax = plt.subplots(layout='constrained', figsize=(10, 5))
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    # ax.bar_label(rects1, padding=3, fontsize=8)
    # ax.bar_label(rects2, padding=3, fontsize=8)
    ax.set_ylabel('Ratio')
    ax.set_title("Relative need of sequential primitives in synthesized design (sequential primitives / all primitives)")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 0.7)
    plt.savefig("plots/sequential_ratio.svg", format="svg")
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
    fig, ax = plt.subplots(layout='constrained', figsize=(10, 5))
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.bar_label(rects1, padding=3, fontsize=8)
    ax.bar_label(rects2, padding=3, fontsize=8)
    ax.set_ylabel('Total number of combinational primitives')
    ax.set_title("Combinational primitives")
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
    fig, ax = plt.subplots(layout='constrained', figsize=(10, 5))
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.bar_label(rects1, padding=3, fontsize=8)
    ax.bar_label(rects2, padding=3, fontsize=8)
    ax.set_ylabel('Total number of wire bits')
    ax.set_title("Wire bits")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 340000)
    plt.savefig("plots/wire_bits.svg", format="svg")
    # plt.show()

def plot_wire_bits_relative(results):
    num_specs = len(results['specs'])
    x = np.arange(num_specs)
    cells_old = []
    cells_new = []
    for result in results['specs']:
        in_old = result['result']['old_architecture']['number_of_wire_bits'] 
        in_new = result['result']['new_architecture']['number_of_wire_bits']
        total = in_old + in_new
        cells_old.append(in_old / total)
        cells_new.append(in_new / total)
    width = 0.35
    fig, ax = plt.subplots(layout='constrained', figsize=(10, 5))
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.set_ylabel('Ratio')
    ax.set_title("Relative number of bits in wires")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 0.8)
    plt.savefig("plots/wire_bits_relative.svg", format="svg")
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
    fig, ax = plt.subplots(layout='constrained', figsize=(10, 5))
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.bar_label(rects1, padding=3, fontsize=8)
    ax.bar_label(rects2, padding=3, fontsize=8)
    ax.set_ylabel('Total number of wires')
    ax.set_title("Wires")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 100000)
    plt.savefig("plots/wires.svg", format="svg")
    # plt.show()


def plot_wires_relative(results):
    num_specs = len(results['specs'])
    x = np.arange(num_specs)
    cells_old = []
    cells_new = []
    for result in results['specs']:
        in_old = result['result']['old_architecture']['number_of_wires'] 
        in_new = result['result']['new_architecture']['number_of_wires']
        total = in_old + in_new
        cells_old.append(in_old / total)
        cells_new.append(in_new / total)
    width = 0.35
    fig, ax = plt.subplots(layout='constrained', figsize=(10, 5))
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.set_ylabel('Ratio')
    ax.set_title("Relative number of wires")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 0.8)
    plt.savefig("plots/wires_relative.svg", format="svg")
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
    fig, ax = plt.subplots(layout='constrained', figsize=(10, 5))
    rects1 = ax.bar(x - width/2, cells_old, width, label='Existing compiler')
    rects2 = ax.bar(x + width/2, cells_new, width, label='Our compiler')
    ax.bar_label(rects1, padding=3, fontsize=8)
    ax.bar_label(rects2, padding=3, fontsize=8)
    ax.set_ylabel('Total number of public wires')
    ax.set_title("Public wires")
    ax.set_xticks(x)
    ax.set_xticklabels([f"spec{i}" for i in range(1, num_specs + 1)])
    ax.legend()
    ax.set_ylim(0, 2000)
    plt.savefig("plots/public_wires.svg", format="svg")
    # plt.show()

def print_table(results):
    for i, item in enumerate(results['specs']): 
        result = item['result']
        print(f"[Spec{i+1}], [{result['number_of_eval_nodes']}], [{result['number_of_sliding_window_buckets']}], [{result['new_architecture']['pipeline_wait']}],")


def plot_throughput():
    specs = ['Spec1', 'Spec2', 'Spec3', 'Spec4', 'Spec5', 'Spec6', 'Spec7', 'Spec8', 'Spec9']
    x = np.arange(len(specs))
    throughput_old = [1/9, 1/7, 1/5, 1/6, 1/6, 1/5, 1/4, 1/5, 1/8]
    throughput_new = [1, 1, 1, 1/3, 1/3, 1, 1, 1/2, 1/3]
    width = 0.35
    fig, ax = plt.subplots(layout='constrained', figsize=(10, 5))
    rects1 = ax.bar(x - width/2, throughput_old, width, label='Existing architecture')
    rects2 = ax.bar(x + width/2, throughput_new, width, label='Our architecture')
    ax.set_ylabel('Evaluation throughput')
    ax.set_title("Throughput")
    ax.set_xticks(x)
    ax.set_xticklabels(specs)
    ax.legend()
    ax.set_ylim(0, 1)
    plt.savefig("plots/throughput.svg", format="svg")
    plt.show()


# plot_total_cells(results)
# plot_sequential_cells(results)
# plot_combinational_cells(results)
# plot_wire_bits(results)
# plot_wires(results)
# plot_public_wires(results)
# plot_sequential_ratio(results)
# plot_total_cells_relative(results)
# plot_wire_bits_relative(results)
# plot_wires_relative(results)
plot_throughput()
# print_table(results)


