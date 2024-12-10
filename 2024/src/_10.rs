use std::collections::{HashSet, VecDeque};
use crate::utils::get_input_data;

const DIRECTIONS: [(i32, i32); 4] = [(0, 1), (1, 0), (0, -1), (-1, 0)];

fn is_valid_move(x: i32, y: i32, from_height: i32, grid: &[Vec<i32>]) -> bool {
    if x < 0 || x >= grid.len() as i32 || y < 0 || y >= grid[0].len() as i32 {
        return false;
    }
    let current_height = grid[x as usize][y as usize];
    current_height == from_height + 1
}

fn solve_part_one(grid: &[Vec<i32>]) -> i32 {
    let mut total_score = 0;

    fn bfs(start_x: usize, start_y: usize, grid: &[Vec<i32>]) -> i32 {
        let mut queue = VecDeque::new();
        let mut visited = vec![vec![false; grid[0].len()]; grid.len()];
        queue.push_back((start_x, start_y, grid[start_x][start_y]));
        visited[start_x][start_y] = true;

        let mut reachable_nines = 0;

        while let Some((x, y, h)) = queue.pop_front() {
            if h == 9 {
                reachable_nines += 1;
                continue;
            }
            for &(dx, dy) in &DIRECTIONS {
                let nx = x as i32 + dx;
                let ny = y as i32 + dy;
                if is_valid_move(nx, ny, h, grid) && !visited[nx as usize][ny as usize] {
                    visited[nx as usize][ny as usize] = true;
                    queue.push_back((nx as usize, ny as usize, grid[nx as usize][ny as usize]));
                }
            }
        }

        reachable_nines
    }

    for i in 0..grid.len() {
        for j in 0..grid[0].len() {
            if grid[i][j] == 0 {
                total_score += bfs(i, j, grid);
            }
        }
    }

    total_score
}

fn solve_part_two(grid: &[Vec<i32>]) -> i32 {
    let mut total_paths = 0;

    fn dfs(x: usize, y: usize, current: i32, grid: &[Vec<i32>], path_set: &mut HashSet<(usize, usize)>) -> i32 {
        if current == 9 {
            return 1;
        }

        let mut count = 0;
        for &(dx, dy) in &DIRECTIONS {
            let nx = x as isize + dx as isize;
            let ny = y as isize + dy as isize;
            if nx >= 0 && nx < grid.len() as isize && ny >= 0 && ny < grid[0].len() as isize {
                let nx = nx as usize;
                let ny = ny as usize;
                if !path_set.contains(&(nx, ny)) && grid[nx][ny] == current + 1 {
                    path_set.insert((nx, ny));
                    count += dfs(nx, ny, current + 1, grid, path_set);
                    path_set.remove(&(nx, ny));
                }
            }
        }
        count
    }

    for x in 0..grid.len() {
        for y in 0..grid[0].len() {
            if grid[x][y] == 0 {
                let mut path_set = HashSet::new();
                path_set.insert((x, y));
                total_paths += dfs(x, y, 0, grid, &mut path_set);
            }
        }
    }

    total_paths
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let input_data = get_input_data(10).await?;
    let data: Vec<Vec<i32>> = input_data.lines().map(|row| row.chars().map(|c| c.to_digit(10).unwrap() as i32).collect()).collect();

    println!("Part One: {}", solve_part_one(&data));
    println!("Part Two: {}", solve_part_two(&data));

    Ok(())
}