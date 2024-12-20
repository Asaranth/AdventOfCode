use crate::utils::get_input_data;
use std::collections::{BinaryHeap, HashMap, HashSet};

fn dijkstra(start: (i32, i32), free_spaces: &HashSet<(i32, i32)>) -> HashMap<(i32, i32), i32> {
    let mut to_visit = BinaryHeap::new();
    let mut visited = HashMap::new();
    visited.insert(start, 0);
    to_visit.push((0, start));
    while let Some((score, (cx, cy))) = to_visit.pop() {
        let score = -score;
        if visited.get(&(cx, cy)).map_or(false, |&v| v < score) {
            continue;
        }
        for (dx, dy) in [(0, 1), (0, -1), (1, 0), (-1, 0)] {
            let np = (cx + dx, cy + dy);
            if free_spaces.contains(&np) && visited.get(&np).map_or(true, |&v| v > score + 1) {
                visited.insert(np, score + 1);
                to_visit.push((-(score + 1), np));
            }
        }
    }
    visited
}

fn solve(distances: &HashMap<(i32, i32), i32>, jump_size: i32) -> usize {
    let mut ret = 0;
    for p in distances.keys() {
        for dx in -jump_size..=jump_size {
            for dy in -jump_size..=jump_size {
                let np = (p.0 + dx, p.1 + dy);
                if dx.abs() + dy.abs() > jump_size {
                    continue;
                }
                if let Some(&initial_cost) = distances.get(p) {
                    if let Some(&np_cost) = distances.get(&np) {
                        let cheat_cost = dx.abs() + dy.abs();
                        if (initial_cost - np_cost - cheat_cost) >= 100 {
                            ret += 1;
                        }
                    }
                }
            }
        }
    }
    ret
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<String> = get_input_data(20).await?.lines().map(|s| s.to_string()).collect();
    let mut free_spaces = HashSet::new();
    let mut start = None;
    for (y, line) in data.iter().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c != '#' {
                free_spaces.insert((x as i32, y as i32));
            }
            if c == 'S' {
                start = Some((x as i32, y as i32));
            }
        }
    }
    let start = start.expect("No starting position found");
    let distances = dijkstra(start, &free_spaces).iter().map(|(&d, &v)| (d, v)).collect::<HashMap<(i32, i32), i32>>();
    println!("Part One: {}", solve(&distances, 2));
    println!("Part Two: {}", solve(&distances, 20));
    Ok(())
}