import json, sys, os, subprocess

def read_layer(layer):
    # NOTE:
    # Deducing the actual eval layer in old compiler when there is a sliding window
    # I am using `shift + evaluation` as a layer number 
    # However, when there are aggregates of aggregates the shift doesn't seem to increase
    # so the output with `shift_layer` feature doesn't make complete sense to me 
    shift = layer['shift'] if 'shift' in layer else 0
    # UPDATE: After talking to Federick I found that my assumption of shift is wrong
    # It is not made for this purpose
    shift = 0
    return layer['evaluation'] 

def has_shift(mir):
    if 'inputs' in mir:
        return 'shift' in mir['inputs'][0]['layer']
    if 'outputs' in mir:
        return 'shift' in mir['outputs'][0]['layer']
    return False


def extract_order_of_outputs(content):
    
    max_layer = 1 + max([read_layer(otpt['layer']) for otpt in content['outputs']])
    event_based = [[] for i in range(max_layer)]
    periodic = [[] for i in range(max_layer)]
    for i, inpt in enumerate(content['inputs']):
        name = f"in{i}"
        layer = read_layer(inpt['layer'])
        event_based[layer].append(name)
    for i, otpt in enumerate(content['outputs']):
        is_periodic = 'GlobalPeriodic' in otpt['eval']['eval_pacing'] 
        name = f"out{i}"
        layer = read_layer(otpt['layer'])
        if is_periodic:
            periodic[layer].append(name)
        else:
            event_based[layer].append(name)
    retval = "\nEvent-based:\n"
    for i, lst in enumerate(event_based):
        retval += f"{i}: {', '.join(lst)}\n"
    retval += "\nPeriodic:\n"
    for i, lst in enumerate(periodic):
        retval += f"{i}: {', '.join(lst)}\n"
    return retval


def extract_layers(content):
    retval = extract_order_of_outputs(content)
    retval += "\ntype, evaluation_layer, name\n"
    retval += "------------------------------\n"
    for i, inpt in enumerate(content['inputs']):
        retval += f"in{i}, {inpt['layer']['evaluation']}, {inpt['name']}\n"

    
    retval += f"\ntype, periodic/event-based, evalution_layer, name\n"
    retval += "------------------------------\n"
    for i, otpt in enumerate(content['outputs']):
        is_periodic = 'GlobalPeriodic' in otpt['eval']['eval_pacing']
        stream_type = "periodic" if is_periodic else "event-based"
        retval += f"out{i}, {stream_type}, {read_layer(otpt['layer'])}, {otpt['name']}\n"

    retval += f"\nSliding windows\n"
    total_sliding_window_buckets = 0
    for i, sw in enumerate(content['sliding_windows']):
        target = f"out{sw['target']['Out']}" if 'Out' in sw['target'] else f"in{sw['target']['In']}"
        caller = f"out{sw['caller']['Out']}"
        retval += f"sw{i} = sw({target}, {caller})\n"
        total_sliding_window_buckets += sw['num_buckets']['Bounded']

    retval += f"\nToal number of sliding window buckets: {total_sliding_window_buckets}\n"
    return retval


def simplify_spec(spec, mir):
    for i, otpt in enumerate(mir['outputs']):
        spec = spec.replace(f"{otpt['name']} ", f'out{i} ')
        spec = spec.replace(f"{otpt['name']}.", f'out{i}.')
    for i, inpt in enumerate(mir['inputs']):
        spec = spec.replace(f"{inpt['name']} ", f'in{i} ')
        spec = spec.replace(f"{inpt['name']}.", f'in{i}.')
    return spec


def extract(spec, mir):
    retval = simplify_spec(spec, mir)
    retval += '-' * 50 + '\n'
    retval += extract_layers(mir)
    return retval


def find_project_root(cur_path="."):
    if "Cargo.toml" in os.listdir(cur_path):
        return cur_path
    return find_project_root(cur_path + "/..")


def path_to_cwd_from_root():
    relative_to_prj_root = find_project_root()
    cwd = os.getcwd().split("/")
    steps = relative_to_prj_root.count("..")
    return "/".join(cwd[-steps:])


def generate_mirs(specs_path):
    rel_path_to_prj_root = find_project_root()
    path_from_prj_root = path_to_cwd_from_root()
    specs = os.listdir(specs_path)
    for folder in specs:
        spec_folder = f"{path_from_prj_root}/{specs_path}/{folder}" 
        spec_path = f"{spec_folder}/spec.lola"
        subprocess.run(['cargo', 'run', '--', '--spec', spec_path, '--output', spec_folder, '--mir'], cwd=rel_path_to_prj_root)


def extract_eval_orders(specs_path):
    for folder in os.listdir(specs_path):
        print(f"Analysing {specs_path}/{folder}\n")
        with open(f"{specs_path}/{folder}/spec.lola", "r") as spec_file:
            spec = spec_file.read()
        with open(f"{specs_path}/{folder}/spec.json", "r") as file:
            mir = json.load(file)
            extracted = extract(spec, mir)
            file_name = 'eval_order.txt' if not has_shift(mir) else 'eval_order_from_shifted_mir.txt'
            with open(f"{specs_path}/{folder}/{file_name}", "w") as out_file:
                out_file.write(extracted)

specs_path = "./specs"

if __name__ == "__main__":
    generate_mirs(specs_path)
    extract_eval_orders(specs_path)
    print(f"Done! view the results in the {specs_path}")