use std::collections::{HashSet, VecDeque};
use crate::utils::get_input_data;

fn get_area(region: &HashSet<(usize, usize)>) -> i32 {
    region.len() as i32
}

fn get_perimeter(grid: &[Vec<char>], region: &HashSet<(usize, usize)>) -> i32 {
    let directions = [(-1, 0), (1, 0), (0, -1), (0, 1)];
    region.iter().fold(0, |perimeter, &(x, y)| {
        perimeter + directions
            .iter()
            .filter(|&&(dx, dy)| {
                let nx = x as isize + dx;
                let ny = y as isize + dy;
                nx < 0 || ny < 0 || nx >= grid.len() as isize || ny >= grid[0].len() as isize || grid[nx as usize][ny as usize] != grid[x][y]
            }).count() as i32
    })
}

fn get_value(grid: &[Vec<char>], x: isize, y: isize) -> char {
    if x >= 0 && y >= 0 && x < grid.len() as isize && y < grid[0].len() as isize {
        grid[x as usize][y as usize]
    } else {
        '_'
    }
}

fn get_sides(grid: &[Vec<char>], region: &HashSet<(usize, usize)>) -> i32 {
    let directions = [((-1, -1), (-1, 0), (0, -1)), ((-1, 1), (-1, 0), (0, 1)), ((1, -1), (1, 0), (0, -1)), ((1, 1), (1, 0), (0, 1))];
    region.iter().fold(0, |total_sides, &(x, y)| {
        total_sides + directions
            .iter()
            .filter(|&&(opposite, adj1, adj2)| {
                let (ox, oy) = (x as isize + opposite.0, y as isize + opposite.1);
                let (a1x, a1y) = (x as isize + adj1.0, y as isize + adj1.1);
                let (a2x, a2y) = (x as isize + adj2.0, y as isize + adj2.1);
                let plot_val = grid[x][y];
                let opposite_val = get_value(grid, ox, oy);
                let adj1_val = get_value(grid, a1x, a1y);
                let adj2_val = get_value(grid, a2x, a2y);
                (plot_val != adj1_val && plot_val != adj2_val) || (plot_val == adj1_val && plot_val == adj2_val && plot_val != opposite_val)
            }).count() as i32
    })
}

fn find_region(grid: &[Vec<char>], start: (usize, usize), plant_type: char, visited: &mut HashSet<(usize, usize)>) -> HashSet<(usize, usize)> {
    let mut region = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back(start);
    visited.insert(start);
    while let Some((x, y)) = queue.pop_front() {
        region.insert((x, y));
        for &(dx, dy) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let nx = x as isize + dx;
            let ny = y as isize + dy;
            if nx >= 0 && ny >= 0 && nx < grid.len() as isize && ny < grid[0].len() as isize
            {
                let nx = nx as usize;
                let ny = ny as usize;
                if grid[nx][ny] == plant_type && !visited.contains(&(nx, ny)) {
                    visited.insert((nx, ny));
                    queue.push_back((nx, ny));
                }
            }
        }
    }
    region
}

fn solve_part_one(grid: &[Vec<char>]) -> i32 {
    let mut visited = HashSet::new();
    let mut total_cost = 0;
    for x in 0..grid.len() {
        for y in 0..grid[0].len() {
            if visited.contains(&(x, y)) {
                continue;
            }
            let region = find_region(grid, (x, y), grid[x][y], &mut visited);
            let area = get_area(&region);
            let perimeter = get_perimeter(grid, &region);
            total_cost += area * perimeter;
        }
    }
    total_cost
}

fn solve_part_two(grid: &[Vec<char>]) -> i32 {
    let mut visited = HashSet::new();
    let mut total_cost = 0;
    for x in 0..grid.len() {
        for y in 0..grid[0].len() {
            if visited.contains(&(x, y)) {
                continue;
            }
            let region = find_region(grid, (x, y), grid[x][y], &mut visited);
            let area = get_area(&region);
            let sides = get_sides(grid, &region);
            total_cost += area * sides;
        }
    }
    total_cost
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let grid: Vec<Vec<char>> = get_input_data(12).await?.lines().map(|s| s.chars().collect()).collect();
    println!("Part One: {}", solve_part_one(&grid));
    println!("Part Two: {}", solve_part_two(&grid));
    Ok(())
}