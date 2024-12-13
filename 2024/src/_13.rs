use crate::utils::get_input_data;

#[derive(Debug)]
struct Machine { ax: i64, ay: i64, bx: i64, by: i64, px: i64, py: i64 }

fn parse_input(input: &str) -> Vec<Machine> {
    let mut machines = Vec::new();
    for group in input.split("\n\n") {
        let mut ax = 0;
        let mut ay = 0;
        let mut bx = 0;
        let mut by = 0;
        let mut px = 0;
        let mut py = 0;
        for line in group.lines() {
            if line.starts_with("Button A:") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                ax = parts[2].trim_start_matches("X+").replace(",", "").parse().unwrap();
                ay = parts[3].trim_start_matches("Y+").replace(",", "").parse().unwrap();
            } else if line.starts_with("Button B:") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                bx = parts[2].trim_start_matches("X+").replace(",", "").parse().unwrap();
                by = parts[3].trim_start_matches("Y+").replace(",", "").parse().unwrap();
            } else if line.starts_with("Prize:") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                px = parts[1].trim_start_matches("X=").replace(",", "").parse().unwrap();
                py = parts[2].trim_start_matches("Y=").replace(",", "").parse().unwrap();
            }
        }
        machines.push(Machine { ax, ay, bx, by, px, py });
    }
    machines
}

fn solve_machine(machine: &Machine, offset: i64) -> i64 {
    let px = machine.px + offset;
    let py = machine.py + offset;
    let det = machine.ax * machine.by - machine.ay * machine.bx;
    let a = (px * machine.by - py * machine.bx) / det;
    let b = (machine.ax * py - machine.ay * px) / det;
    if (machine.ax * a + machine.bx * b, machine.ay * a + machine.by * b) == (px, py) {
        a * 3 + b
    } else {
        0
    }
}

fn solve_part_one(data: &[Machine]) -> i64 {
    data.iter().map(|machine| solve_machine(machine, 0)).sum()
}

fn solve_part_two(data: &[Machine]) -> i64 {
    data.iter().map(|machine| solve_machine(machine, 1e13 as i64)).sum()
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let input_data = get_input_data(13).await?;
    let data = parse_input(&input_data);

    println!("Part One: {}", solve_part_one(&data));
    println!("Part Two: {}", solve_part_two(&data));

    Ok(())
}