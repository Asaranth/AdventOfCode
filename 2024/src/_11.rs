use crate::utils::get_input_data;
use std::collections::HashMap;

fn num_digits(n: i64) -> usize {
    if n == 0 {
        1
    } else {
        (n as f64).log10().floor() as usize + 1
    }
}

fn split_in_middle(num: i64) -> (i64, i64) {
    let digits = num_digits(num);
    let divisor = 10_i64.pow((digits / 2) as u32);
    (num / divisor, num % divisor)
}

fn simulate_blinks(data: Vec<i64>, blinks: usize) -> usize {
    let mut stones: HashMap<i64, usize> = HashMap::new();
    for stone in data {
        *stones.entry(stone).or_insert(0) += 1;
    }
    for _ in 0..blinks {
        let mut after_blink: HashMap<i64, usize> = HashMap::new();
        for (stone, count) in stones {
            if stone == 0 {
                *after_blink.entry(1).or_insert(0) += count;
            } else if num_digits(stone) % 2 == 0 {
                let (left, right) = split_in_middle(stone);
                *after_blink.entry(left).or_insert(0) += count;
                *after_blink.entry(right).or_insert(0) += count;
            } else {
                let new_value = stone.wrapping_mul(2024);
                *after_blink.entry(new_value).or_insert(0) += count;
            }
        }
        stones = after_blink;
    }
    stones.values().sum()
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<i64> = get_input_data(11).await?.split_whitespace().filter_map(|s| s.parse::<i64>().ok()).collect();
    println!("Part One: {}", simulate_blinks(data.clone(), 25));
    println!("Part Two: {}", simulate_blinks(data, 75));
    Ok(())
}