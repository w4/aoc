#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3Packages.pillow python3Packages.networkx

import sys
import networkx as nx

[unparsed_states, operations] = "".join(sys.stdin.readlines()).strip().split("\n\n")

states = dict([(state.split(": ")[0], state.split(": ")[1] != "0") for state in unparsed_states.strip().split("\n")])
parsed_operations = []

G = nx.DiGraph()

for operation in operations.strip().split("\n"):
    [deps, out] = operation.split(' -> ')
    [left, oper, right] = deps.split(' ')
    G.add_node(left)
    G.add_node(right)
    G.add_node(out, oper = oper)
    G.add_edge(left, out)
    G.add_edge(right, out)

    parsed_operations.append((left, oper, right, out))

top_level_nodes = [node for node in G.nodes() if G.in_degree(node) == 0]
topo = [v for v in (nx.topological_sort(G)) if v not in top_level_nodes]
visited = set()

for node in topo:
    if node in visited:
        continue

    visited.add(node)
    
    [a, b] = [a for [a, _] in G.in_edges(node)]
    operation = G.nodes[node]["oper"]

    if operation == "XOR":
        states[node] = states[a] ^ states[b]
    elif operation == "OR":
        states[node] = states[a] or states[b]
    elif operation == "AND":
        states[node] = states[a] and states[b]

part1 = 0

for i, key in enumerate([key for key in sorted(states.keys()) if key.startswith('z')]):
    part1 |= states[key] << i

print(part1)

part2 = []

lowest_bit = [key for key in sorted(states.keys()) if key.startswith('z')][-1]
output_rails = ["x", "y", "z"]

for left, oper, right, out in parsed_operations:
    is_non_xor_z_output = out[0] == "z" and oper != "XOR" and out != lowest_bit
    is_invalid_and = oper == "AND" and "x00" not in (left, right) and any(
        out in (left_, right_) and oper_ != "OR"
        for left_, oper_, right_, out_ in parsed_operations
    )
    is_invalid_xor = oper == "XOR" and (
        all(v[0] not in output_rails for v in (left, right, out)) or 
        any(out in (left_, right_) and oper_ == "OR" 
            for left_, oper_, right_, out_ in parsed_operations)
    )

    if is_non_xor_z_output or is_invalid_and or is_invalid_xor:
        part2.append(out)
        

print(','.join(sorted(part2)))

