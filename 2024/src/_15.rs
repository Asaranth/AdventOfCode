use crate::utils::get_input_data;

fn directions(ch: char) -> (i32, i32) {
    match ch {
        '^' => (-1, 0),
        'v' => (1, 0),
        '<' => (0, -1),
        '>' => (0, 1),
        _ => (0, 0)
    }
}

fn expression(ch: char) -> Vec<char> {
    match ch {
        '#' => vec!['#', '#'],
        'O' => vec!['[', ']'],
        '.' => vec!['.', '.'],
        '@' => vec!['@', '.'],
        _ => vec!['.', '.']
    }
}

fn get_robot_pos(grid: &[Vec<char>]) -> (usize, usize) {
    for r in 0..grid.len() {
        for c in 0..grid[r].len() {
            if grid[r][c] == '@' {
                return (r, c);
            }
        }
    }
    (0, 0)
}

fn calculate_sum(grid: &[Vec<char>], rows: i32, cols: i32, target: char) -> i32 {
    (0..rows)
        .flat_map(|r| (0..cols).map(move |c| (r as usize, c as usize)))
        .filter(|&(r, c)| grid[r][c] == target)
        .map(|(r, c)| 100 * r as i32 + c as i32)
        .sum()
}

fn solve_part_one(map: &[String], movements: &str) -> i32 {
    let mut grid: Vec<Vec<char>> = map.iter().map(|line| line.chars().collect()).collect();
    let rows = grid.len() as i32;
    let cols = grid[0].len() as i32;
    let mut robot_pos = get_robot_pos(&grid);
    for movement in movements.chars() {
        let (dr, dc) = directions(movement);
        let mut r = robot_pos.0 as i32;
        let mut c = robot_pos.1 as i32;
        let mut targets = vec![(r as usize, c as usize)];
        let mut valid_move = true;
        while valid_move {
            r += dr;
            c += dc;
            if r < 0 || r >= rows || c < 0 || c >= cols {
                valid_move = false;
                break;
            }
            match grid[r as usize][c as usize] {
                '#' => valid_move = false,
                'O' => targets.push((r as usize, c as usize)),
                '.' => break,
                _ => {}
            }
        }
        if valid_move {
            grid[robot_pos.0][robot_pos.1] = '.';
            robot_pos = ((robot_pos.0 as i32 + dr) as usize, (robot_pos.1 as i32 + dc) as usize);
            grid[robot_pos.0][robot_pos.1] = '@';
            for &(br, bc) in targets.iter().skip(1).rev() {
                grid[br][bc] = '.';
                grid[(br as i32 + dr) as usize][(bc as i32 + dc) as usize] = 'O';
            }
        }
    }
    calculate_sum(&grid, rows, cols, 'O')
}

fn solve_part_two(map: Vec<String>, movements: &str) -> i32 {
    let mut grid: Vec<Vec<char>> = map.iter().map(|line| line.chars().flat_map(|ch| expression(ch)).collect()).collect();
    let rows = grid.len() as i32;
    let cols = grid[0].len() as i32;
    let mut robot_pos = get_robot_pos(&grid);
    for movement in movements.chars() {
        let (dr, dc) = directions(movement);
        let mut targets = vec![robot_pos];
        let mut valid_move = true;
        let mut queue = vec![robot_pos];
        while let Some((cr, cc)) = queue.pop() {
            let nr = cr as i32 + dr;
            let nc = cc as i32 + dc;
            if nr < 0 || nr >= rows || nc < 0 || nc >= cols {
                valid_move = false;
                break;
            }
            let (nr, nc) = (nr as usize, nc as usize);
            if targets.contains(&(nr, nc)) {
                continue;
            }
            match grid[nr][nc] {
                '#' => {
                    valid_move = false;
                    break;
                }
                '[' => {
                    targets.push((nr, nc));
                    queue.push((nr, nc));
                    if nc + 1 < cols as usize {
                        targets.push((nr, nc + 1));
                        queue.push((nr, nc + 1));
                    }
                }
                ']' => {
                    targets.push((nr, nc));
                    queue.push((nr, nc));
                    if nc > 0 {
                        targets.push((nr, nc - 1));
                        queue.push((nr, nc - 1));
                    }
                }
                _ => {}
            }
        }
        if !valid_move {
            continue;
        }
        let prev_grid = grid.clone();
        grid[robot_pos.0][robot_pos.1] = '.';
        robot_pos = ((robot_pos.0 as i32 + dr) as usize, (robot_pos.1 as i32 + dc) as usize);
        grid[robot_pos.0][robot_pos.1] = '@';
        for &(br, bc) in targets.iter().skip(1) {
            grid[br][bc] = '.';
        }
        for &(br, bc) in targets.iter().skip(1) {
            let new_r = (br as i32 + dr) as usize;
            let new_c = (bc as i32 + dc) as usize;
            if new_r < rows as usize && new_c < cols as usize {
                grid[new_r][new_c] = prev_grid[br][bc];
            }
        }
    }
    calculate_sum(&grid, rows, cols, '[')
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<String> = get_input_data(15).await?.lines().map(|s| s.to_string()).collect();
    let map_end = data.iter().position(|line| line.trim().is_empty()).unwrap_or(data.len());
    let map: Vec<String> = data[..map_end].to_vec();
    let movements: String = data[map_end + 1..].join("");
    println!("Part One: {}", solve_part_one(&map, &movements));
    println!("Part Two: {}", solve_part_two(map, &movements));
    Ok(())
}