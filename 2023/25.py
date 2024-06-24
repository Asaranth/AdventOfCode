from networkx import Graph, minimum_edge_cut, connected_components

g = Graph()
for line in open('data/25.txt'):
    left, right = line.split(':')
    for node in right.split():
        g.add_edge(left, node)
        g.add_edge(node, left)
g.remove_edges_from(minimum_edge_cut(g))
a, b = connected_components(g)
print('Solution: %d' % (len(a) * len(b)))
