#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3Packages.pillow python3Packages.networkx

import sys
import networkx as nx

input = [line.strip().split("-") for line in sys.stdin]

nodes = set([node for pair in input for node in pair])
k_nodes = [node for node in nodes if node.startswith("t")]

G = nx.Graph()
G.add_nodes_from(nodes)
G.add_edges_from(input)

cliques = list(nx.enumerate_all_cliques(G))

part1 = 0

for clique in cliques:
    if len(clique) < 3:
        continue
    elif len(clique) > 3:
        break
    elif any(node in k_nodes for node in clique):
        part1 += 1

print(part1)

part2 = ",".join(sorted(cliques[-1]))
print(part2)
