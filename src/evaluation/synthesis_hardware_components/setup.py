import os, subprocess

specs_path = "specs"

if __name__ == "__main__":
    for folder in os.listdir(specs_path):
        # subprocess.run(["rm", "-rf", "generated"], cwd=f"{specs_path}/{folder}")
        # subprocess.run(["chmod", "+x", "run.sh"], cwd=f"{specs_path}/{folder}")
        # subprocess.run(["./run.sh"], cwd=f"{specs_path}/{folder}")
        subprocess.run(["rm", "-rf", "generated_vhdl_comp_related"], cwd=f"{specs_path}/{folder}")
        subprocess.run(["cargo", "run", "--", f"../{specs_path}/{folder}/spec.lola", f"../{specs_path}/{folder}/generated_vhdl_comp_related", "templates", "--online"], cwd="rtlola-compilation-vhdl")
        subprocess.run(["python3", "old_compiler_synth_script_gen.py", f"{specs_path}/{folder}/generated_vhdl_comp_related"])


