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

fn solve_part_two(program: Vec<i32>) -> i64 {
    fn find(target: &[i64], ans: i64, program: &[i32]) -> Option<i64> {
        if target.is_empty() {
            return Some(ans);
        }
        for t in 0..8 {
            let a = (ans << 3) | t;
            let mut b = 0;
            let mut c = 0;
            let mut output = None;
            let combo = |operand: i32, a: i64, b: i64, c: i64| -> i64 {
                match operand {
                    0..=3 => operand as i64,
                    4 => a,
                    5 => b,
                    6 => c,
                    _ => panic!("Unrecognized combo operand {}", operand),
                }
            };
            let mut pointer = 0;
            while pointer < program.len() - 2 {
                let instruction = program[pointer];
                let operand = program[pointer + 1];
                match instruction {
                    0 => {}
                    1 => {
                        b ^= operand as i64;
                    }
                    2 => {
                        b = combo(operand, a, b, c) % 8;
                    }
                    3 => {}
                    4 => {
                        b ^= c;
                    }
                    5 => {
                        output = Some(combo(operand, a, b, c) % 8);
                    }
                    6 => {
                        let shift_amount = combo(operand, a, b, c) % 64;
                        b = a >> shift_amount;
                    }
                    7 => {
                        let shift_amount = combo(operand, a, b, c) % 64;
                        c = a >> shift_amount;
                    }
                    _ => panic!("Unrecognized instruction {}", instruction),
                }
                pointer += 2;
            }
            if output == Some(target[target.len() - 1]) {
                if let Some(sub_ans) = find(&target[..target.len() - 1], a, program) {
                    return Some(sub_ans);
                }
            }
        }
        None
    }
    find(&program.iter().map(|&x| x as i64).collect::<Vec<_>>(), 0, &program).unwrap_or(0)
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