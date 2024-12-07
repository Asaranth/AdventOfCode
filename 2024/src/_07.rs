use crate::utils::get_input_data;

fn can_form_target_y(target_y: i64, xs: &[i64], allow_concatenation: bool) -> bool {
    let n_ops = xs.len() - 1;
    let max_combinations = if allow_concatenation {
        3usize.pow(n_ops as u32)
        // 1 << (2 * n_ops)
    } else {
        2usize.pow(n_ops as u32)
        //1 << n_ops
    };

    for i in 0..max_combinations {
        let mut current_value = xs[0];
        for j in 0..n_ops {
            let op_bits = if allow_concatenation {
                (i / 3usize.pow(j as u32)) % 3
            } else {
                (i / 2usize.pow(j as u32)) % 2
            };

            current_value = match op_bits {
                0 => current_value + xs[j + 1],
                1 => current_value * xs[j + 1],
                2 => concatenate(current_value, xs[j + 1]),
                _ => current_value
            };

            if current_value > target_y {
                break;
            }
        }

        if current_value == target_y {
            return true;
        }
    }

    false
}

fn concatenate(a: i64, b: i64) -> i64 {
    let concatenated = format!("{}{}", a, b);
    concatenated.parse().unwrap_or(0)
}

fn solve_part_one(data: &[String]) -> i64 {
    let mut total_calibration = 0;

    for line in data {
        let parts: Vec<&str> = line.split(':').collect();
        if parts.len() != 2 {
            continue;
        }

        let target_y: i64 = parts[0].trim().parse().unwrap_or(0);
        let xs: Vec<i64> = parts[1].trim().split_whitespace().map(|x| x.parse().unwrap_or(0)).collect();

        if can_form_target_y(target_y, &xs, false) {
            total_calibration += target_y;
        }
    }

    total_calibration
}

fn solve_part_two(data: Vec<String>) -> i64 {
    let mut total_calibration = 0;

    for line in data {
        let parts: Vec<&str> = line.split(':').collect();
        if parts.len() != 2 {
            continue;
        }

        let target_y: i64 = parts[0].trim().parse().unwrap_or(0);
        let xs: Vec<i64> = parts[1].trim().split_whitespace().map(|x| x.parse().unwrap_or(0)).collect();

        if can_form_target_y(target_y, &xs, true) {
            total_calibration += target_y;
        }
    }

    total_calibration
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let input_data = get_input_data(7).await?;
    let data: Vec<String> = input_data.lines().map(|s| s.to_string()).collect();

    println!("Part One: {}", solve_part_one(&data));
    println!("Part Two: {}", solve_part_two(data));

    Ok(())
}