use std::collections::{HashMap, HashSet};
use crate::utils::get_input_data;

fn is_inbound(matrix: &Vec<Vec<char>>, pos: (i32, i32)) -> bool {
    let (x, y) = pos;
    x >= 0 && x < matrix.len() as i32 && y >= 0 && y < matrix[0].len() as i32
}

fn process(matrix: &Vec<Vec<char>>, vec: &Vec<(i32, i32)>, part_two: bool) -> HashSet<(i32, i32)> {
    let mut set: HashSet<(i32, i32)> = HashSet::new();
    for (i, &item1) in vec.iter().enumerate() {
        let (x1, y1) = item1;
        if part_two {
            set.insert(item1);
        }
        for &item2 in &vec[i + 1..] {
            let (x2, y2) = item2;
            let height = x2 - x1;
            let width = y2 - y1;
            let mut factor = 1;
            if part_two {
                set.insert(item2);
            }
            loop {
                let mut result: u32 = 0;
                let f_antenna = (x2 + height * factor, y2 + width * factor);
                if is_inbound(matrix, f_antenna) {
                    set.insert(f_antenna);
                    result += 1;
                }
                let s_antenna = (x1 - height * factor, y1 - width * factor);
                if is_inbound(matrix, s_antenna) {
                    set.insert(s_antenna);
                    result += 1;
                }
                if !part_two || result == 0 {
                    break;
                }
                factor += 1;
            }
        }
    }
    set
}

fn solve_part_one(matrix: &Vec<Vec<char>>, map: &HashMap<char, Vec<(i32, i32)>>) -> i32 {
    let mut antinodes: HashSet<(i32, i32)> = HashSet::new();
    map.values().for_each(|vec| { antinodes.extend(process(&matrix, &vec, false)) });
    antinodes.len() as i32
}

fn solve_part_two(matrix: Vec<Vec<char>>, map: HashMap<char, Vec<(i32, i32)>>) -> i32 {
    let mut antinodes: HashSet<(i32, i32)> = HashSet::new();
    map.into_values().for_each(|vec| { antinodes.extend(process(&matrix, &vec, true)) });
    antinodes.len() as i32
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let mut map: HashMap<char, Vec<(i32, i32)>> = HashMap::new();
    let matrix = get_input_data(8).await?.lines().enumerate().map(|(i, line)| {
        let chars: Vec<char> = line.chars().collect();
        for (j, &c) in chars.iter().enumerate() {
            if c != '.' && c != '#' {
                map.entry(c).or_insert_with(Vec::new).push((i as i32, j as i32));
            }
        }
        chars
    }).collect();
    println!("Part One: {}", solve_part_one(&matrix, &map));
    println!("Part Two: {}", solve_part_two(matrix, map));
    Ok(())
}