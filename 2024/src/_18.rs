//! Day 18: RAM Run
//!
//! Simulates falling memory byte corruptions on a 71x71 grid and computes exit path reachability.

use std::collections::VecDeque;
use crate::utils::get_input_data;

/// Orthogonal directional coordinate offsets.
const DIRECTIONS: [(isize, isize); 4] = [(0, 1), (1, 0), (0, -1), (-1, 0)];

/// Performs a breadth-first search on the grid from (0, 0) to find the shortest step distance satisfying the end condition.
fn bfs<F>(grid: &Vec<Vec<char>>, mut end_condition: F) -> Option<i32> where F: FnMut((isize, isize)) -> bool
{
    let mut queue = VecDeque::new();
    queue.push_back(((0, 0), 0));
    let mut visited = vec![vec![false; 71]; 71];
    visited[0usize][0usize] = true;
    while let Some(((x, y), steps)) = queue.pop_front() {
        if end_condition((x, y)) {
            return Some(steps);
        }
        for (dx, dy) in &DIRECTIONS {
            let new_x = (x + dx) as usize;
            let new_y = (y + dy) as usize;
            if new_x < 71 && new_y < 71 && !visited[new_y][new_x] && grid[new_y][new_x] != '#' {
                visited[new_y][new_x] = true;
                queue.push_back(((new_x as isize, new_y as isize), steps + 1));
            }
        }
    }
    None
}

/// Initialises a 71x71 memory space with the first `limit` corrupted byte obstacles marked.
fn initialize_grid(data: &[(usize, usize)], limit: usize) -> Vec<Vec<char>> {
    let mut grid = vec![vec!['.'; 71]; 71];
    for &(x, y) in &data[..limit] {
        grid[y][x] = '#';
    }
    grid
}

/// Solves Part One: computes the minimum steps to reach (70, 70) after 1024 bytes have fallen.
fn solve_part_one(data: &[(usize, usize)]) -> i32 {
    let grid = initialize_grid(data, data.len().min(1024));
    bfs(&grid, |pos| pos == (70, 70)).unwrap_or(0)
}

/// Solves Part Two: determines the coordinates of the first falling byte that completely cuts off all paths to the exit.
fn solve_part_two(data: &[(usize, usize)]) -> String {
    let mut grid = initialize_grid(data, data.len().min(1024));
    fn simulate(grid: &mut Vec<Vec<char>>, data: &[(usize, usize)]) -> Option<(usize, usize)> {
        for &(x, y) in data {
            grid[y][x] = '#';
            if !bfs(grid, |pos| pos == (70, 70)).is_some() {
                return Some((x, y));
            }
        }
        None
    }
    if let Some((x, y)) = simulate(&mut grid, &data[data.len().min(1024)..]) {
        return format!("{},{}", x, y);
    }
    "No blockage detected".to_string()
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<(usize, usize)> = get_input_data(18).await?.lines().filter_map(|line| {
        let parts: Vec<&str> = line.split(',').collect();
        if parts.len() == 2 {
            if let (Ok(x), Ok(y)) = (parts[0].parse::<usize>(), parts[1].parse::<usize>()) {
                return Some((x, y));
            }
        }
        None
    }).collect();
    println!("Part One: {}", solve_part_one(&data));
    println!("Part Two: {}", solve_part_two(&data));
    Ok(())
}