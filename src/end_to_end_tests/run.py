#!/usr/local/bin/python3
import os
import re
import subprocess

def read_expected_output(test_dir):
    def parse(line):
            pattern = r'\[([^\]]+)\]\[([^\]]+)\]\[([^\]]+)\]\[([^\]]+)\] = ([^\s]+)'
            match = re.match(pattern, line)
            if match:
                groups = match.groups()
                return groups
            else:
                raise f"couldn't parse the line {line}"
    with open(f"{test_dir}/expected_output.txt", 'r') as f:
        content = f.read().split("\n")
        expected = list(map(parse, content))
        expected = list(filter(lambda data: data[1] != "Input", expected))
        grouped = {}
        for (time, data_type, output_name, _, value) in expected:
            if time not in grouped:
                 grouped[time] = []
            grouped[time].append(value)
        grouped = list(grouped.values())
        return grouped


def read_actual_output(test_dir):
    def parse(line):
            pattern = r'Time (\d+): Active outputs: \((\d+), (\d+), (\d+)\), Outputs: \((\d+), (\d+), (\d+)\)'
            match = re.match(pattern, line)
            if match:
                groups = match.groups()
                return groups
            else:
                return ()
    with open(f"{test_dir}/generated/output.txt", 'r') as f:
        content = f.read().split("\n")
        actual = list(map(parse, content))
        actual = list(filter(lambda data: len(data) > 0, actual))
        grouped = []
        for data in actual:
            num_outputs = (len(data) - 1) // 2
            group = []
            for i in range(num_outputs):
                has_output = data[i + 1]
                value = data[i + 1 + num_outputs]
                if has_output == "1":
                    group.append(value)
            grouped.append(group)
        return grouped



if __name__ == "__main__":
    all_tests_passed = True
    results = []
    for test_dir in os.listdir('tests'):
        if "ignore" in test_dir:
            continue
        subprocess.run(['bash', 'run.sh'], cwd=f'tests/{test_dir}')
        expected = read_expected_output(f"tests/{test_dir}")
        actual = read_actual_output(f"tests/{test_dir}")
        if actual == expected:
            results.append(f"Passed: {test_dir}")
        else:
            results.append(f"Failed: {test_dir}")
            all_tests_passed = False

    print("\n\n\nTest results:")
    for result in results:
        print(result)

    if all_tests_passed:
        print("\nAll tests passed!\n")


