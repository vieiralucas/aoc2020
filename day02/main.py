#!/usr/bin/env python3

def parse(line):
    rules, password = line.split(":")
    min_max, char = rules.split(" ")
    minimum, maximum = min_max.split("-")

    return int(minimum), int(maximum), char.strip(), password.strip()

def is_valid1(minimum, maximum, char, password):
    count = 0
    for c in password:
        if c == char:
            count += 1

        if count > maximum:
            return False

    return count >= minimum

def part1():
    with open("input.txt") as f:
        result = 0
        for line in f.readlines():
            minimum, maximum, char, password = parse(line)
            if is_valid1(minimum, maximum, char, password):
                result += 1

        print(result)

def xor(b1, b2):
    return b1 and not b2 or not b1 and b2

def is_valid2(pos1, pos2, char, password):
    return xor(password[pos1 - 1] == char, password[pos2 - 1] == char)

def part2():
    with open("input.txt") as f:
        result = 0
        for line in f.readlines():
            pos1, pos2, char, password = parse(line)
            if is_valid2(pos1, pos2, char, password):
                result += 1

        print(result)

if __name__ == "__main__":
    # part1()
    part2()

