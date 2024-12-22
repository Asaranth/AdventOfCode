use std::collections::HashMap;
use crate::utils::get_input_data;

fn evolve(secret: usize) -> (usize, Vec<(i8, i8)>) {
    let mut evolution = secret;
    let mut differences: Vec<(i8, i8)> = Vec::new();
    let mut prev_unit: i8 = (secret % 10) as i8;
    for _ in 0..2000 {
        evolution = (evolution ^ (evolution * 64)) % 16777216;
        evolution = (evolution ^ (evolution / 32)) % 16777216;
        evolution = (evolution ^ (evolution * 2048)) % 16777216;
        let unit: i8 = (evolution % 10) as i8;
        differences.push((unit, unit - prev_unit));
        prev_unit = unit;
    }
    (evolution, differences)
}

fn solve_part_two(differences: Vec<Vec<(i8, i8)>>) -> usize {
    let mut scores: HashMap<Vec<i8>, usize> = HashMap::new();
    let mut result: usize = 0;
    for difference in &differences {
        if difference.len() < 4 {
            continue;
        }
        let mut seen: HashMap<Vec<i8>, bool> = HashMap::new();
        for i in 0..(difference.len() - 3) {
            let sequence: Vec<i8> = difference[i..i + 4].iter().map(|(_, diff)| *diff).collect();
            let value = difference[i + 3].0 as usize;
            if seen.contains_key(&sequence) {
                continue;
            }
            seen.insert(sequence.clone(), true);
            let score = scores.entry(sequence.clone()).or_insert(0);
            *score += value;
            result = result.max(*score);
        }
    }
    result
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<usize> = get_input_data(22).await?.lines().filter_map(|l| l.parse().ok()).collect();
    let mut evolutions: Vec<usize> = Vec::new();
    let mut differences: Vec<Vec<(i8, i8)>> = Vec::new();
    for secret in data {
        let result = evolve(secret);
        evolutions.push(result.0);
        differences.push(result.1);
    }
    println!("Part One: {}", evolutions.iter().sum::<usize>());
    println!("Part Two: {}", solve_part_two(differences));
    Ok(())
}