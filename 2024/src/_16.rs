use std::collections::{BinaryHeap, HashMap, HashSet};
use crate::utils::get_input_data;

const DIRECTIONS: [(i32, i32); 4] = [(0, 1), (1, 0), (0, -1), (-1, 0)];

#[derive(Eq, PartialEq)]
struct State { position: (usize, usize), direction: usize, cost: i32 }

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn try_add_state(visited: &HashSet<((usize, usize), usize)>, parents: &mut HashMap<((usize, usize), usize), Vec<((usize, usize), usize)>>, heap: &mut BinaryHeap<State>, new_state: State, current_position: ((usize, usize), usize)) {
    if !visited.contains(&(new_state.position, new_state.direction)) {
        parents.entry((new_state.position, new_state.direction)).or_default().push(current_position);
        heap.push(new_state);
    }
}

fn solve_part_one(data: &[Vec<char>], start: (usize, usize), end: (usize, usize)) -> i32 {
    let mut heap = BinaryHeap::new();
    let mut visited = HashSet::new();
    heap.push(State { position: start, direction: 0, cost: 0 });
    while let Some(State { position, direction, cost }) = heap.pop() {
        if !visited.insert((position, direction)) {
            continue;
        }
        if position == end {
            return cost;
        }
        let new_row = position.0 as i32 + DIRECTIONS[direction].0;
        let new_col = position.1 as i32 + DIRECTIONS[direction].1;
        if new_row >= 0 && new_row < data.len() as i32 && new_col >= 0 && new_col < data[0].len() as i32 && data[new_row as usize][new_col as usize] != '#'
        {
            heap.push(State { position: (new_row as usize, new_col as usize), direction, cost: cost + 1 });
        }
        let next_direction = (direction + 1) % 4;
        heap.push(State { position, direction: next_direction, cost: cost + 1000 });
        let prev_direction = (direction + 3) % 4;
        heap.push(State { position, direction: prev_direction, cost: cost + 1000 });
    }
    0
}

fn solve_part_two(data: Vec<Vec<char>>, start: (usize, usize), end: (usize, usize)) -> i32 {
    let directions = DIRECTIONS;
    let mut heap = BinaryHeap::new();
    let mut visited = HashSet::new();
    let mut parents: HashMap<((usize, usize), usize), Vec<((usize, usize), usize)>> = HashMap::new();
    let mut costs: HashMap<((usize, usize), usize), i32> = HashMap::new();
    heap.push(State { position: start, direction: 0, cost: 0 });
    let mut best_cost = i32::MAX;
    let mut best_paths = HashSet::new();
    while let Some(State { position, direction, cost }) = heap.pop() {
        if !visited.insert((position, direction)) {
            continue;
        }
        if cost > best_cost {
            break;
        }
        if position == end {
            best_cost = cost;
        }
        if let Some(&current_cost) = costs.get(&(position, direction)) {
            if cost > current_cost {
                continue;
            }
        }
        costs.insert((position, direction), cost);
        let new_row = position.0 as i32 + directions[direction].0;
        let new_col = position.1 as i32 + directions[direction].1;
        if new_row >= 0 && new_row < data.len() as i32 && new_col >= 0 && new_col < data[0].len() as i32 && data[new_row as usize][new_col as usize] != '#'
        {
            let new_position = (new_row as usize, new_col as usize);
            let new_state = State { position: new_position, direction, cost: cost + 1 };
            try_add_state(&visited, &mut parents, &mut heap, new_state, (position, direction));
        }
        let next_direction = (direction + 1) % 4;
        let new_state = State { position, direction: next_direction, cost: cost + 1000 };
        try_add_state(&visited, &mut parents, &mut heap, new_state, (position, direction));
        let prev_direction = (direction + 3) % 4;
        let new_state = State { position, direction: prev_direction, cost: cost + 1000 };
        try_add_state(&visited, &mut parents, &mut heap, new_state, (position, direction));
    }
    let mut stack = Vec::new();
    for direction in 0..4 {
        if let Some(&cost) = costs.get(&(end, direction)) {
            if cost == best_cost {
                stack.push((end, direction));
            }
        }
    }
    while let Some(pos_dir) = stack.pop() {
        best_paths.insert(pos_dir.0);
        if let Some(parents_list) = parents.get(&pos_dir) {
            for &parent in parents_list {
                if let (Some(&parent_cost), Some(&child_cost)) = (costs.get(&parent), costs.get(&pos_dir)) {
                    if parent_cost + 1 == child_cost || parent_cost + 1000 == child_cost {
                        stack.push(parent);
                    }
                }
            }
        }
    }
    best_paths.len() as i32
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<Vec<char>> = get_input_data(16).await?.lines().map(|s| s.chars().collect()).collect();
    let mut start = (0, 0);
    let mut end = (0, 0);
    for (r, row) in data.iter().enumerate() {
        for (c, &cell) in row.iter().enumerate() {
            if cell == 'S' {
                start = (r, c);
            } else if cell == 'E' {
                end = (r, c);
            }
        }
    }
    println!("Part One: {}", solve_part_one(&data, start, end));
    println!("Part Two: {}", solve_part_two(data, start, end));
    Ok(())
}