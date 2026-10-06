//! Day 01: Historian Hysteria
//!
//! Pairs and calculates differences between sorted location IDs, and calculates similarity scores using frequency counts.

use std::collections::HashMap;
use crate::utils::get_input_data;

/// Solves Part One: sorts both lists and sums the absolute differences between corresponding elements.
fn solve_part_one(left_list: &[i32], right_list: &[i32]) -> i32 {
    let mut left_sorted = left_list.to_vec();
    let mut right_sorted = right_list.to_vec();
    left_sorted.sort();
    right_sorted.sort();
    left_sorted.iter().zip(right_sorted.iter()).map(|(left, right)| (left - right).abs()).sum()
}

/// Solves Part Two: calculates similarity score by multiplying each left list number by its occurrence count in the right list.
fn solve_part_two(left_list: Vec<i32>, right_list: Vec<i32>) -> i32 {
    let mut right_count = HashMap::new();
    for &number in &right_list {
        *right_count.entry(number).or_insert(0) += 1;
    }
    left_list.iter().map(|&number| number * right_count.get(&number).unwrap_or(&0)).sum()
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<String> = get_input_data(1).await?.lines().map(|s| s.to_string()).collect();
    let mut left_list = Vec::new();
    let mut right_list = Vec::new();
    for line in data {
        if let Some((left_str, right_str)) = line.split_once("   ") {
            if let (Ok(left), Ok(right)) = (left_str.parse::<i32>(), right_str.parse::<i32>()) {
                left_list.push(left);
                right_list.push(right);
            }
        }
    }
    println!("Part One: {}", solve_part_one(&left_list, &right_list));
    println!("Part Two: {}", solve_part_two(left_list, right_list));
    Ok(())
}