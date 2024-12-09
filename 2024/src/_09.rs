use crate::utils::get_input_data;

fn parse_disk(data: &str) -> Vec<Option<i32>> {
    let chars: Vec<char> = data.chars().collect();
    let mut disk: Vec<Option<i32>> = Vec::new();
    let mut idx = 0;
    let mut file_id = 0;

    while idx < chars.len() {
        if let Some(file_length) = chars[idx].to_digit(10) {
            disk.extend(vec![Some(file_id); file_length as usize]);
            idx += 1;
        }
        if idx < chars.len() {
            if let Some(space_length) = chars[idx].to_digit(10) {
                disk.extend(vec![None; space_length as usize]);
                idx += 1;
            }
        }
        file_id += 1;
    }

    disk
}

fn calculate_checksum(disk: &[Option<i32>]) -> i64 {
    disk.iter().enumerate().filter_map(|(position, &file_option)| { file_option.map(|file_id| position as i64 * file_id as i64) }).sum()
}

fn solve_part_one(data: &str) -> i64 {
    let mut disk = parse_disk(data);
    let mut first_empty = 0;
    let mut last_filled = disk.len() - 1;

    while first_empty < last_filled {
        while first_empty < disk.len() && disk[first_empty].is_some() {
            first_empty += 1;
        }
        while last_filled > 0 && disk[last_filled].is_none() {
            last_filled -= 1;
        }
        if first_empty < last_filled {
            disk[first_empty] = disk[last_filled];
            disk[last_filled] = None;
        }
    }

    calculate_checksum(&disk)
}

fn solve_part_two(data: &str) -> i64 {
    let mut disk = parse_disk(data);
    let max_file_id = disk.iter().filter_map(|&x| x).max().unwrap_or(0);

    for current_file_id in (0..=max_file_id).rev() {
        let mut start_index = None;
        let mut file_length = 0;

        for (index, &cell) in disk.iter().enumerate() {
            if cell == Some(current_file_id) {
                if start_index.is_none() {
                    start_index = Some(index);
                }
                file_length += 1;
            } else if start_index.is_some() {
                break;
            }
        }

        if let Some(start) = start_index {
            let mut empty_start = None;
            let mut empty_length = 0;

            for (index, &cell) in disk.iter().enumerate() {
                if cell.is_none() {
                    if empty_start.is_none() {
                        empty_start = Some(index);
                    }
                    empty_length += 1;
                    if empty_length >= file_length {
                        break;
                    }
                } else {
                    if empty_start.is_some() {
                        empty_start = None;
                        empty_length = 0;
                    }
                }
            }

            if let Some(empty_start_index) = empty_start {
                if empty_length >= file_length && empty_start_index < start {
                    for offset in 0..file_length {
                        disk[empty_start_index + offset] = Some(current_file_id);
                        disk[start + offset] = None;
                    }
                }
            }
        }
    }

    calculate_checksum(&disk)
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let input_data = get_input_data(9).await?;
    let data = input_data.trim();

    println!("Part One: {}", solve_part_one(data));
    println!("Part Two: {}", solve_part_two(data));

    Ok(())
}