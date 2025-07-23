import json, sys, os, subprocess


def extract(content):
    retval = "\ntype, evaluation_layer, name\n"
    retval += "------------------------------\n"
    for i, inpt in enumerate(content['inputs']):
        retval += f"input{i}, {inpt['layer']['evaluation']}, {inpt['name']}\n"

    retval += f"\ntype, periodic/event-based, evalution_layer, name\n"
    retval += "------------------------------\n"
    for i, otpt in enumerate(content['outputs']):
        is_periodic = 'GlobalPeriodic' in otpt['eval']['eval_pacing']
        stream_type = "periodic" if is_periodic else "event-based"
        retval += f"output{i}, {stream_type}, {otpt['layer']['evaluation']}, {otpt['name']}\n"
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
        with open(f"{specs_path}/{folder}/spec.json", "r") as file:
            content = json.load(file)
            extracted = extract(content)
            with open(f"{specs_path}/{folder}/eval_order.txt", "w") as out_file:
                out_file.write(extracted)

specs_path = "./specs"

if __name__ == "__main__":
    generate_mirs(specs_path)
    extract_eval_orders(specs_path)