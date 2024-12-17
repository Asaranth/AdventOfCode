use crate::utils::get_input_data;

struct Computer<'a> { program: &'a [i32], ip: usize, a: i32, b: i32, c: i32 }
impl<'a> Computer<'a> {
    fn run(&mut self) -> Option<i32> {
        while self.ip < self.program.len() {
            let combo = |index: usize| match self.program[index] {
                0..=3 => self.program[index],
                4 => self.a,
                5 => self.b,
                6 => self.c,
                _ => panic!("Invalid instruction"),
            };
            match self.program[self.ip] {
                0 => self.a >>= combo(self.ip + 1),
                1 => self.b ^= self.program[self.ip + 1],
                2 => self.b = combo(self.ip + 1) % 8,
                3 => {
                    if self.a != 0 {
                        self.ip = self.program[self.ip + 1] as usize;
                        continue;
                    }
                }
                4 => self.b ^= self.c,
                5 => {
                    let out = combo(self.ip + 1) % 8;
                    self.ip += 2;
                    return Some(out);
                }
                6 => self.b = self.a >> combo(self.ip + 1),
                7 => self.c = self.a >> combo(self.ip + 1),
                _ => panic!("Invalid instruction"),
            }
            self.ip += 2;
        }
        None
    }
}

fn solve_part_one(a: i32, program: &[i32]) -> String {
    let mut computer = Computer { program, ip: 0, a, b: 0, c: 0 };
    let mut out = Vec::new();
    while let Some(n) = computer.run() {
        let digit = (n as u8 + b'0') as char;
        out.push(digit);
        out.push(',');
    }
    if !out.is_empty() {
        out.pop();
    }
    out.iter().collect()
}

fn solve_part_two(program: Vec<i32>) -> i32 {
    0
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data: Vec<String> = get_input_data(17).await?.lines().map(|s| s.to_string()).collect();
    let mut program = Vec::new();
    let mut register_a = 0;
    for line in data {
        if line.starts_with("Register A") {
            if let Some((_, value)) = line.split_once(": ") {
                register_a = value.trim().parse::<i32>().unwrap();
            }
        } else if line.starts_with("Program:") {
            let program_str = line.split_once(": ").unwrap().1;
            program = program_str
                .split(',')
                .map(|num| num.trim().parse::<i32>().unwrap())
                .collect();
        }
    }

    println!("Part One: {}", solve_part_one(register_a, &program));
    println!("Part Two: {}", solve_part_two(program));
    Ok(())
}