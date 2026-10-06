//! Day 06: Guard Gallivant
//!
//! Simulates guard patrol routes on a mapped grid to track visited positions and detect obstruction loops.

use crate::utils::get_input_data;
use std::collections::HashSet;

/// Represents the guard's cardinal facing direction.
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
enum Direction { Up, Down, Left, Right }

/// Returns the new direction after turning 90 degrees clockwise.
fn turn_right(current_direction: &Direction) -> Direction {
    match current_direction {
        Direction::Up => Direction::Right,
        Direction::Right => Direction::Down,
        Direction::Down => Direction::Left,
        Direction::Left => Direction::Up,
    }
}

/// Locates the initial position and facing direction of the guard in the grid.
fn find_initial_guard(data: &[String]) -> ((usize, usize), Direction) {
    data.iter()
        .enumerate()
        .find_map(|(i, line)| {
            line.chars().enumerate().find_map(|(j, c)| {
                let direction = match c {
                    '^' => Some(Direction::Up),
                    'v' => Some(Direction::Down),
                    '>' => Some(Direction::Right),
                    '<' => Some(Direction::Left),
                    _ => None,
                };
                direction.map(|d| ((i, j), d))
            })
        }).unwrap_or(((0, 0), Direction::Up))
}

/// Advances the guard one step forward or turns right if blocked by an obstacle.
fn move_guard(data: &[String], position: (usize, usize), direction: &Direction) -> ((usize, usize), Direction, bool) {
    let (new_x, new_y) = match direction {
        Direction::Up => (position.0.wrapping_sub(1), position.1),
        Direction::Down => (position.0 + 1, position.1),
        Direction::Left => (position.0, position.1.wrapping_sub(1)),
        Direction::Right => (position.0, position.1 + 1),
    };
    if new_x >= data.len() || new_y >= data[0].len() {
        return (position, *direction, false);
    }
    if data[new_x].chars().nth(new_y) == Some('#') {
        (position, turn_right(direction), true)
    } else {
        ((new_x, new_y), *direction, true)
    }
}

/// Solves Part One: counts distinct positions visited by the guard before exiting the map.
fn solve_part_one(data: &[String]) -> i32 {
    let (mut position, mut direction) = find_initial_guard(data);
    let mut visited = vec![vec![false; data[0].len()]; data.len()];
    loop {
        let (new_position, new_direction, valid_move) = move_guard(data, position, &direction);
        if !valid_move {
            break;
        }
        position = new_position;
        direction = new_direction;
        visited[position.0][position.1] = true;
    }
    visited.iter().flat_map(|row| row.iter()).filter(|&&v| v).count() as i32
}

/// Solves Part Two: counts grid positions where adding a single obstacle causes the guard to enter an infinite loop.
fn solve_part_two(data: Vec<String>) -> i32 {
    let (initial_position, initial_direction) = find_initial_guard(&data);
    let mut loop_causing_obstructions = 0;
    for i in 0..data.len() {
        for j in 0..data[0].len() {
            if (i, j) == initial_position || data[i].chars().nth(j) != Some('.') {
                continue;
            }
            let mut visited_states = HashSet::new();
            let mut position = initial_position;
            let mut direction = initial_direction;
            let is_looping;
            loop {
                let new_state = (position, direction);
                if visited_states.contains(&new_state) {
                    is_looping = true;
                    break;
                }
                visited_states.insert(new_state);
                let (mut new_position, mut new_direction, valid_move) = move_guard(&data, position, &direction);
                if !valid_move {
                    is_looping = false;
                    break;
                }
                if (new_position.0, new_position.1) == (i, j)
                    || data[new_position.0].chars().nth(new_position.1) == Some('#')
                {
                    new_direction = turn_right(&direction);
                    new_position = position;
                }
                position = new_position;
                direction = new_direction;
            }
            if is_looping {
                loop_causing_obstructions += 1;
            }
        }
    }
    loop_causing_obstructions
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<String> = get_input_data(6).await?.lines().map(|s| s.to_string()).collect();
    println!("Part One: {}", solve_part_one(&data));
    println!("Part Two: {}", solve_part_two(data));
    Ok(())
}
