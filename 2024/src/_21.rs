use std::cmp::min;
use std::collections::HashMap;
use crate::utils::get_input_data;

fn numpad(key: char) -> (i32, i32) {
    match key {
        '7' => (0, 0),
        '8' => (0, 1),
        '9' => (0, 2),
        '4' => (1, 0),
        '5' => (1, 1),
        '6' => (1, 2),
        '1' => (2, 0),
        '2' => (2, 1),
        '3' => (2, 2),
        '0' => (3, 1),
        'A' => (3, 2),
        _ => panic!()
    }
}

fn dirpad(key: char) -> (i32, i32) {
    match key {
        '^' => (0, 1),
        'A' => (0, 2),
        '<' => (1, 0),
        'v' => (1, 1),
        '>' => (1, 2),
        _ => panic!()
    }
}

fn arrows(y: i32, x: i32, steps: usize, x_first: bool, memo: &mut HashMap<(i32, i32, usize, bool), usize>) -> usize {
    if let Some(&result) = memo.get(&(y, x, steps, x_first)) {
        return result;
    }
    let (abs_y, abs_x) = (y.unsigned_abs() as usize, x.unsigned_abs() as usize);
    let mut path = vec![if y > 0 { '^' } else { 'v' }; abs_y];
    path.extend(vec![if x > 0 { '<' } else { '>' }; abs_x]);
    if x_first {
        path.reverse();
    }
    path.push('A');
    let result = if steps == 0 {
        path.len()
    } else {
        let mut cur = dirpad('A');
        path.into_iter().map(|c| {
            let next = dirpad(c);
            let prev = cur;
            cur = next;
            let delta = (prev.0 - next.0, prev.1 - next.1);
            if delta.0 == 0 || delta.1 == 0 {
                arrows(delta.0, delta.1, steps - 1, false, memo)
            } else if next == (1, 0) && prev.0 == 0 {
                arrows(delta.0, delta.1, steps - 1, false, memo)
            } else if prev == (1, 0) && next.0 == 0 {
                arrows(delta.0, delta.1, steps - 1, true, memo)
            } else {
                min(arrows(delta.0, delta.1, steps - 1, false, memo), arrows(delta.0, delta.1, steps - 1, true, memo))
            }
        }).sum()
    };
    memo.insert((y, x, steps, x_first), result);
    result
}

fn enter_code(sequence: &str, steps: usize) -> usize {
    let mut cur = numpad('A');
    let mut memo = HashMap::new();
    sequence[0..3].parse::<usize>().unwrap() * sequence.chars().map(|c| {
        let next = numpad(c);
        let prev = cur;
        let delta = (cur.0 - next.0, cur.1 - next.1);
        cur = next;
        if prev.0 == 3 && next.1 == 0 {
            arrows(delta.0, delta.1, steps, false, &mut memo)
        } else if prev.1 == 0 && next.0 == 3 {
            arrows(delta.0, delta.1, steps, true, &mut memo)
        } else {
            min(arrows(delta.0, delta.1, steps, true, &mut memo), arrows(delta.0, delta.1, steps, false, &mut memo))
        }
    }).sum::<usize>()
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data = get_input_data(21).await?.lines().map(|s| s.to_string()).collect::<Vec<_>>();
    println!("Part One: {}", &data.iter().map(|l| enter_code(l, 2)).sum::<usize>());
    println!("Part Two: {}", &data.iter().map(|l| enter_code(l, 25)).sum::<usize>());
    Ok(())
}