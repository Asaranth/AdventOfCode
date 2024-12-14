use std::collections::HashSet;
use crate::utils::get_input_data;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Plot { x: i32, y: i32 }
impl Plot {
    fn new(x: i32, y: i32) -> Self { Self { x, y } }

    fn add_wrapped(&self, other: Plot, bounds: Plot) -> Self {
        Plot {
            x: (self.x + other.x).rem_euclid(bounds.x),
            y: (self.y + other.y).rem_euclid(bounds.y)
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Robot { pos: Plot, vel: Plot }

fn solve_part_one(data: &[Robot]) -> i32 {
    let bounds = Plot::new(101, 103);
    let cx = bounds.x / 2;
    let cy = bounds.y / 2;
    let mut quadrant_counts = [0; 4];
    for robot in data {
        let final_pos = robot.pos.add_wrapped(Plot::new(100 * robot.vel.x, 100 * robot.vel.y), bounds);
        if final_pos.x == cx || final_pos.y == cy {
            continue;
        } else if final_pos.x < cx && final_pos.y < cy {
            quadrant_counts[0] += 1;
        } else if final_pos.x >= cx && final_pos.y < cy {
            quadrant_counts[1] += 1;
        } else if final_pos.x < cx && final_pos.y >= cy {
            quadrant_counts[2] += 1;
        } else if final_pos.x >= cx && final_pos.y >= cy {
            quadrant_counts[3] += 1;
        }
    }
    quadrant_counts.iter().product()
}

fn solve_part_two(mut data: Vec<Robot>) -> i32 {
    let bounds = Plot::new(101, 103);
    let max_frames = 10000; // Arbitrary number
    let mut compressed = vec![0; max_frames];
    for i in 0..max_frames {
        let x_positions: HashSet<i32> = data.iter().map(|robot| robot.pos.x).collect();
        let y_positions: HashSet<i32> = data.iter().map(|robot| robot.pos.y).collect();
        compressed[i] = x_positions.len() + y_positions.len();
        for robot in &mut data {
            robot.pos = robot.pos.add_wrapped(robot.vel, bounds);
        }
    }
    let (seconds, _) = compressed.iter().enumerate().min_by_key(|&(_, &length)| length).unwrap();
    seconds as i32
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<Robot> = get_input_data(14).await?.lines().map(|line| {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let pos: Vec<i32> = parts[0].trim_start_matches("p=").split(',').map(|x| x.parse().unwrap()).collect();
        let vel: Vec<i32> = parts[1].trim_start_matches("v=").split(',').map(|x| x.parse().unwrap()).collect();
        Robot { pos: Plot::new(pos[0], pos[1]), vel: Plot::new(vel[0], vel[1]) }
    }).collect();
    println!("Part One: {}", solve_part_one(&data));
    println!("Part Two: {}", solve_part_two(data));
    Ok(())
}