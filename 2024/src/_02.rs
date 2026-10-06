//! Day 02: Red-Nosed Reports
//!
//! Validates nuclear plant safety reports by evaluating monotonic ordering and adjacent level differences.

use crate::utils::get_input_data;

/// Checks whether adjacent differences between consecutive levels are between 1 and 3 inclusive.
fn valid_increments(vec: &[i32]) -> bool {
    vec.windows(2).all(|pair| {
        let diff = (pair[0] - pair[1]).abs();
        diff >= 1 && diff <= 3
    })
}

/// Solves Part One: counts reports that are strictly monotonic with valid step differences.
fn solve_part_one(data: &[String]) -> i32 {
    let mut safe_reports = 0;
    for line in data {
        let levels: Vec<i32> = line.split_whitespace().filter_map(|s| s.parse::<i32>().ok()).collect();
        if (levels.is_sorted() || levels.iter().cloned().rev().collect::<Vec<_>>().is_sorted()) && valid_increments(&levels) {
            safe_reports += 1;
        }
    }
    safe_reports
}

/// Solves Part Two: counts reports that can be made safe by removing at most one level (Problem Dampener).
fn solve_part_two(data: Vec<String>) -> i32 {
    let mut safe_reports = 0;
    for line in data {
        let levels: Vec<i32> = line.split_whitespace().filter_map(|s| s.parse::<i32>().ok()).collect();
        if (levels.is_sorted() || levels.iter().cloned().rev().collect::<Vec<_>>().is_sorted()) && valid_increments(&levels) {
            safe_reports += 1;
            continue;
        }
        for i in 0..levels.len() {
            let mut temp_levels = levels.clone();
            temp_levels.remove(i);
            if valid_increments(&temp_levels) &&
                (temp_levels.is_sorted() || temp_levels.iter().rev().cloned().collect::<Vec<_>>().is_sorted()) {
                safe_reports += 1;
                break;
            }
        }
    }
    safe_reports
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<String> = get_input_data(2).await?.lines().map(|s| s.to_string()).collect();
    println!("Part One: {}", solve_part_one(&data));
    println!("Part Two: {}", solve_part_two(data));
    Ok(())
}