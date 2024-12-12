use std::collections::HashSet;
use crate::utils::get_input_data;

fn calculate_price(grid: &[Vec<char>], visited: &mut HashSet<(usize, usize)>, x: usize, y: usize, plant: char) -> i32 {
    let rows = grid.len();
    let cols = grid[0].len();
    let directions = [(0, 1), (1, 0), (0, -1), (-1, 0)];
    let mut stack = vec![(x, y)];
    let mut area = 0;
    let mut perimeter = 0;

    while let Some((cx, cy)) = stack.pop() {
        if visited.contains(&(cx, cy)) {
            continue;
        }
        visited.insert((cx, cy));
        area += 1;
        for (dx, dy) in &directions {
            let nx = cx as isize + dx;
            let ny = cy as isize + dy;
            if nx >= 0 && ny >= 0 && (nx as usize) < rows && (ny as usize) < cols {
                let nx = nx as usize;
                let ny = ny as usize;
                if grid[nx][ny] == plant {
                    if !visited.contains(&(nx, ny)) {
                        stack.push((nx, ny));
                    }
                } else {
                    perimeter += 1;
                }
            } else {
                perimeter += 1;
            }
        }
    }
    area * perimeter
}

fn solve_part_one(data: &[Vec<char>]) -> i32 {
    let rows = data.len();
    let cols = data[0].len();
    let mut visited = HashSet::new();
    let mut total_price = 0;

    for x in 0..rows {
        for y in 0..cols {
            if !visited.contains(&(x, y)) {
                let plant = data[x][y];
                total_price += calculate_price(data, &mut visited, x, y, plant);
            }
        }
    }

    total_price
}

fn solve_part_two(data: &[Vec<char>]) -> i32 {
    0
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let input_data = get_input_data(12).await?;
    let data: Vec<String> = input_data.lines().map(|s| s.to_string()).collect();
    let grid: Vec<Vec<char>> = data.iter().map(|row| row.chars().collect()).collect();

    println!("Part One: {}", solve_part_one(&grid));
    println!("Part Two: {}", solve_part_two(&grid));

    Ok(())
}