use regex::Regex;
use crate::utils::get_input_data;

fn solve_part_one(data: &[String]) -> i32 {
    let mut total = 0;
    let re_mul = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").unwrap();
    for line in data {
        for cap in re_mul.captures_iter(line) {
            let x: i32 = cap[1].parse().unwrap();
            let y: i32 = cap[2].parse().unwrap();
            total += x * y;
        }
    }
    total
}

fn solve_part_two(data: Vec<String>) -> i32 {
    let mut total = 0;
    let mut enabled = true;
    let re = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)").unwrap();
    for line in data {
        for cap in re.captures_iter(&line) {
            if let Some(_) = cap.get(1) {
                if enabled {
                    let x: i32 = cap[1].parse().unwrap();
                    let y: i32 = cap[2].parse().unwrap();
                    total += x * y;
                }
            } else if let Some(do_cap) = cap.get(0) {
                if do_cap.as_str() == "do()" {
                    enabled = true;
                } else if do_cap.as_str() == "don't()" {
                    enabled = false;
                }
            }
        }
    }
    total
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<String> = get_input_data(3).await?.lines().map(|s| s.to_string()).collect();
    println!("Part One: {}", solve_part_one(&data));
    println!("Part Two: {}", solve_part_two(data));
    Ok(())
}