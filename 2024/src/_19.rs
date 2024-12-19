use std::collections::HashMap;
use crate::utils::get_input_data;

fn possible_config(config: String, towels: &[String]) -> bool {
    if config.is_empty() {
        return true;
    }
    for towel in towels {
        if config.starts_with(towel) {
            let remaining_target = &config[towel.len()..];
            if possible_config(remaining_target.parse().unwrap(), towels) {
                return true
            }
        }
    }
    false
}

fn count_configurations(config: &str, towels: &[String], memo: &mut HashMap<String, i64>) -> i64 {
    if config.is_empty() {
        return 1;
    }
    if let Some(&count) = memo.get(config) {
        return count;
    }
    let mut total_count = 0;
    for towel in towels {
        if config.starts_with(towel) {
            let remaining_target = &config[towel.len()..];
            total_count += count_configurations(remaining_target, towels, memo);
        }
    }
    memo.insert(config.to_string(), total_count);
    total_count
}

fn solve_part_one(towels: &[String], configs: &[String]) -> i32 {
    let mut possible_configs = 0;
    for config in configs {
        if possible_config(config.clone(), towels) {
            possible_configs += 1;
        }
    }
    possible_configs
}

fn solve_part_two(towels: &[String], configs: &[String]) -> i64 {
    let mut total_configs = 0i64;
    let mut memo = HashMap::new();
    for config in configs {
        total_configs += count_configurations(config, towels, &mut memo);
    }
    total_configs
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<String> = get_input_data(19).await?.lines().map(|line| line.to_string()).collect();
    let towels: Vec<String> = data.get(0).unwrap().split(',').map(|s| s.trim().to_string()).collect();
    let configs: Vec<String> = data.iter().skip(2).map(|s| s.trim().to_string()).collect();
    println!("Part One: {}", solve_part_one(&towels, &configs));
    println!("Part Two: {}", solve_part_two(&towels, &configs));
    Ok(())
}