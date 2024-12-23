use std::collections::{HashMap, HashSet};
use crate::utils::get_input_data;

fn bron_kerbosch(graph: &HashMap<String, HashSet<String>>, r: HashSet<String>, p: HashSet<String>, x: HashSet<String>, max_clique: &mut Vec<String>) {
    if p.is_empty() && x.is_empty() {
        if r.len() > max_clique.len() {
            *max_clique = r.into_iter().collect();
        }
        return;
    }
    let mut p_clone = p.clone();
    for v in &p {
        let neighbors = graph.get(v).unwrap();
        let mut r_new = r.clone();
        r_new.insert(v.clone());
        let p_new: HashSet<String> = p_clone.intersection(neighbors).cloned().collect();
        let x_new: HashSet<String> = x.intersection(neighbors).cloned().collect();
        bron_kerbosch(graph, r_new, p_new, x_new, max_clique);
        p_clone.remove(v);
    }
}

fn solve_part_one(graph: HashMap<String, HashSet<String>>) -> usize {
    let mut groups = Vec::new();
    let mut visited = HashSet::new();
    for (node, neighbors) in &graph {
        let neighbors_vec: Vec<&String> = neighbors.iter().collect();
        for i in 0..neighbors_vec.len() {
            for j in (i + 1)..neighbors_vec.len() {
                let a = neighbors_vec[i];
                let b = neighbors_vec[j];
                if graph.get(a).unwrap().contains(b) {
                    let mut clique = vec![node.clone(), a.clone(), b.clone()];
                    clique.sort();
                    if visited.insert(clique.clone()) {
                        groups.push(clique);
                    }
                }
            }
        }
    }
    groups.iter().filter(|g| g.iter().any(|c| c.starts_with('t'))).count()
}

fn solve_part_two(graph: HashMap<String, HashSet<String>>) -> String {
    let all_nodes: HashSet<String> = graph.keys().cloned().collect();
    let mut max_clique = Vec::new();
    bron_kerbosch(&graph, HashSet::new(), all_nodes, HashSet::new(), &mut max_clique);
    max_clique.sort();
    max_clique.join(",")
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<String> = get_input_data(23).await?.lines().map(|s| s.to_string()).collect();
    let mut graph: HashMap<String, HashSet<String>> = HashMap::new();
    for line in data {
        let parts: Vec<&str> = line.split('-').collect();
        let (a, b) = (parts[0].to_string(), parts[1].to_string());
        graph.entry(a.clone()).or_insert_with(HashSet::new).insert(b.clone());
        graph.entry(b.clone()).or_insert_with(HashSet::new).insert(a.clone());
    }
    println!("Part One: {}", solve_part_one(graph.clone()));
    println!("Part Two: {}", solve_part_two(graph));
    Ok(())
}