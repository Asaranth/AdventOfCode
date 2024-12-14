use crate::utils::get_input_data;

fn count_xmas(line: &str) -> i32 {
    line.matches("XMAS").count() as i32
}

fn solve_part_one(data: &[String]) -> i32 {
    let mut count = 0;
    let x = data.len();
    let y = data[0].len();
    for line in data {
        count += count_xmas(line);
        let reversed: String = line.chars().rev().collect();
        count += count_xmas(&reversed);
    }
    for col in 0..y {
        let column: String = data.iter().map(|line| line.chars().nth(col).unwrap()).collect();
        count += count_xmas(&column);
        let reversed: String = column.chars().rev().collect();
        count += count_xmas(&reversed);
    }
    for d in 0..(x + y - 1) {
        let mut top_left_diagonal = String::new();
        let mut top_right_diagonal = String::new();
        for i in 0..x {
            if d >= i && d - i < y {
                top_left_diagonal.push(data[i].chars().nth(d - i).unwrap());
                top_right_diagonal.push(data[i].chars().nth(y - 1 - (d - i)).unwrap());
            }
        }
        count += count_xmas(&top_left_diagonal);
        count += count_xmas(&top_left_diagonal.chars().rev().collect::<String>());
        count += count_xmas(&top_right_diagonal);
        count += count_xmas(&top_right_diagonal.chars().rev().collect::<String>());
    }
    count
}

fn solve_part_two(data: Vec<String>) -> i32 {
    let mut count = 0;
    let x = data.len();
    let y = data[0].len();
    for i in 1..x - 1 {
        for j in 1..y - 1 {
            if data[i].chars().nth(j).unwrap() == 'A' {
                if ((data[i - 1].chars().nth(j - 1).unwrap() == 'M' && data[i + 1].chars().nth(j + 1).unwrap() == 'S') ||
                    (data[i - 1].chars().nth(j - 1).unwrap() == 'S' && data[i + 1].chars().nth(j + 1).unwrap() == 'M')) &&
                   ((data[i + 1].chars().nth(j - 1).unwrap() == 'M' && data[i - 1].chars().nth(j + 1).unwrap() == 'S') ||
                    (data[i + 1].chars().nth(j - 1).unwrap() == 'S' && data[i - 1].chars().nth(j + 1).unwrap() == 'M')) {
                    count += 1;
                }
            }
        }
    }
    count
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<String> = get_input_data(4).await?.lines().map(|s| s.to_string()).collect();
    println!("Part One: {}", solve_part_one(&data));
    println!("Part Two: {}", solve_part_two(data));
    Ok(())
}