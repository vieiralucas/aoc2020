#!/usr/bin/env python3

def solve_part1(report):
    entries = set(report)
    for entry in report:
        complement = 2020 - entry
        if complement in entries:
            return complement * entry
    

def solve_part2(report):
    entries = set(report)
    for i, i_value in enumerate(report):
        for j_value in report[i:]:
            complement = 2020 - i_value - j_value
            if complement > 0 and complement in entries:
                return complement * i_value * j_value

if __name__ == "__main__":
    with open("input.txt") as f:
        report = list(map(int, f.readlines()))
        # print(solve_part_1(report))
        print(solve_part2(report))

