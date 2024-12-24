use std::collections::{HashMap};
use itertools::Itertools;
use crate::utils::get_input_data;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum LogicGate { And, Or, Xor }
impl LogicGate {
    fn evaluate(&self, left: bool, right: bool) -> bool {
        match self {
            LogicGate::And => left && right,
            LogicGate::Or => left || right,
            LogicGate::Xor => left ^ right
        }
    }

    fn from_str(s: &str) -> Self {
        match s {
            "AND" => LogicGate::And,
            "OR" => LogicGate::Or,
            "XOR" => LogicGate::Xor,
            _ => panic!("Invalid logic gate")
        }
    }
}

struct GateVerifier<'a> {
    gates: &'a HashMap<&'a str, (&'a str, LogicGate, &'a str)>,
}
impl<'a> GateVerifier<'a> {
    fn are_sorted_equal(vec1: &[&str], vec2: &[&str]) -> bool {
        let mut sorted1 = vec1.to_vec();
        let mut sorted2 = vec2.to_vec();
        sorted1.sort();
        sorted2.sort();
        sorted1 == sorted2
    }

    fn make_sorted_wire_pair(c1: char, c2: char, n: usize) -> Vec<String> {
        vec![Self::make_wire(c1, n), Self::make_wire(c2, n)]
    }

    fn make_wire(c: char, n: usize) -> String {
        format!("{}{:02}", c, n)
    }

    fn verify_z(&self, wire: &str, n: usize) -> bool {
        if let Some((x, gate, y)) = self.gates.get(wire) {
            if *gate == LogicGate::Xor {
                if n == 0 {
                    return Self::are_sorted_equal(&[*x, *y], &["x00", "y00"]);
                }
                return (self.verify_intermediate_xor(x, n) && self.verify_carry_bit(y, n))
                    || (self.verify_intermediate_xor(y, n) && self.verify_carry_bit(x, n));
            }
        }
        false
    }

    fn verify_intermediate_xor(&self, wire: &str, n: usize) -> bool {
        if let Some((x, gate, y)) = self.gates.get(wire) {
            if *gate == LogicGate::Xor {
                return Self::are_sorted_equal(
                    &[*x, *y],
                    &Self::make_sorted_wire_pair('x', 'y', n)
                        .iter()
                        .map(String::as_str)
                        .collect::<Vec<_>>(),
                );
            }
        }
        false
    }

    fn verify_carry_bit(&self, wire: &str, n: usize) -> bool {
        if let Some((x, gate, y)) = self.gates.get(wire) {
            if n == 1 && *gate == LogicGate::And {
                return Self::are_sorted_equal(&[*x, *y], &["x00", "y00"]);
            }
            if *gate == LogicGate::Or {
                return (self.verify_direct_carry(x, n - 1) && self.verify_recarry(y, n - 1))
                    || (self.verify_direct_carry(y, n - 1) && self.verify_recarry(x, n - 1));
            }
        }
        false
    }

    fn verify_direct_carry(&self, wire: &str, n: usize) -> bool {
        if let Some((x, gate, y)) = self.gates.get(wire) {
            if *gate == LogicGate::And {
                return Self::are_sorted_equal(
                    &[*x, *y],
                    &Self::make_sorted_wire_pair('x', 'y', n)
                        .iter()
                        .map(String::as_str)
                        .collect::<Vec<_>>(),
                );
            }
        }
        false
    }

    fn verify_recarry(&self, wire: &str, n: usize) -> bool {
        if let Some((x, gate, y)) = self.gates.get(wire) {
            if *gate == LogicGate::And {
                return (self.verify_intermediate_xor(x, n) && self.verify_carry_bit(y, n))
                    || (self.verify_intermediate_xor(y, n) && self.verify_carry_bit(x, n));
            }
        }
        false
    }

    fn verify(&self, n: usize) -> bool {
        self.verify_z(Self::make_wire('z', n).as_str(), n)
    }

    fn progress(&self) -> usize {
        let mut i = 0;
        while self.verify(i) {
            i += 1;
        }
        i
    }
}

fn solve_part_one<'a>(wires: &mut HashMap<&'a str, bool>, gates: &HashMap<&'a str, (&'a str, LogicGate, &'a str)>) -> u64 {
    let mut to_evaluate: Vec<_> = gates.keys().cloned().collect();
    while !to_evaluate.is_empty() {
        let mut next_to_evaluate = Vec::new();
        for output in to_evaluate.iter() {
            if let Some((a, gate, b)) = gates.get(output) {
                if let (Some(&val_a), Some(&val_b)) = (wires.get(*a), wires.get(*b)) {
                    wires.insert(*output, gate.evaluate(val_a, val_b));
                } else {
                    next_to_evaluate.push(*output);
                }
            }
        }
        if next_to_evaluate == to_evaluate {
            break;
        }
        to_evaluate = next_to_evaluate;
    }
    wires.iter().filter(|(k, _)| k.starts_with('z')).sorted_by(|(k1, _), (k2, _)| k2.cmp(k1)).fold(0u64, |acc, (_, &val)| acc * 2 + if val { 1 } else { 0 })
}

fn solve_part_two(mut gates: HashMap<&str, (&str, LogicGate, &str)>) -> String {
    let mut swaps = Vec::new();
    for _ in 0..4 {
        let baseline = {
            let verifier = GateVerifier { gates: &gates };
            verifier.progress()
        };
        let mut found_swap = false;

        for x in gates.keys().cloned().collect_vec() {
            for y in gates.keys().cloned().collect_vec() {
                if x == y {
                    continue;
                }
                let temp = gates.get(x).cloned();
                if let Some(value) = gates.get(y) {
                    gates.insert(x, *value);
                }
                if let Some(value) = temp {
                    gates.insert(y, value);
                }
                let new_progress = {
                    let verifier = GateVerifier { gates: &gates };
                    verifier.progress()
                };
                if new_progress > baseline {
                    found_swap = true;
                    swaps.push(x);
                    swaps.push(y);
                    break;
                }
                let temp = gates.get(x).cloned();
                if let Some(value) = gates.get(y) {
                    gates.insert(x, *value);
                }
                if let Some(value) = temp {
                    gates.insert(y, value);
                }
            }
            if found_swap {
                break;
            }
        }
    }
    swaps.sort();
    swaps.dedup();
    swaps.join(",")
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data = get_input_data(24).await?;
    let (p1, p2) = data.split_once("\n\n").unwrap();
    let mut wires = HashMap::new();
    let mut gates = HashMap::new();
    for line in p1.lines() {
        let (name, value) = line.split_once(": ").unwrap();
        wires.insert(name, value == "1");
    }
    for line in p2.lines() {
        let (a, gate, b, _, output) = line.split(' ').collect_tuple().unwrap();
        gates.insert(output, (a, LogicGate::from_str(gate), b));
    }
    println!("Part One: {}", solve_part_one(&mut wires, &gates));
    println!("Part Two: {}", solve_part_two(gates));
    Ok(())
}