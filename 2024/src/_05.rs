use crate::utils::get_input_data;

fn extract_sections(data: &[String]) -> (Vec<(i32, i32)>, Vec<Vec<i32>>) {
    let mut rules = Vec::new();
    let mut updates = Vec::new();
    let mut is_update_section = false;

    for line in data {
        if line.trim().is_empty() {
            is_update_section = true;
            continue;
        }
        if is_update_section {
            let update_numbers: Vec<i32> = line.split(',').filter_map(|s| s.trim().parse::<i32>().ok()).collect();
            updates.push(update_numbers);
        } else {
            let rule_parts: Vec<&str> = line.split('|').collect();
            if let (Some(part_x), Some(part_y)) = (rule_parts.get(0), rule_parts.get(1)) {
                if let (Ok(x), Ok(y)) = (part_x.trim().parse::<i32>(), part_y.trim().parse::<i32>()) {
                    rules.push((x, y));
                }
            }
        }
    }

    (rules, updates)
}

fn is_rule_valid(rule: &(i32, i32), page: &[i32]) -> bool {
    let (x, y) = *rule;
    let index_x = page.iter().position(|&n| n == x);
    let index_y = page.iter().position(|&n| n == y);
    if let (Some(ix), Some(iy)) = (index_x, index_y) {
        return ix < iy;
    }
    true
}

fn solve_part_one(rules: &[(i32, i32)], updates: &[Vec<i32>]) -> i32 {
    let correct_updates: Vec<&Vec<i32>> = updates.iter().filter(|page| {
        rules.iter().all(|rule| is_rule_valid(rule, page))
    }).collect();
    correct_updates.iter().map(|page| page[page.len() / 2]).sum()
}

fn solve_part_two(rules: &[(i32, i32)], updates: &[Vec<i32>]) -> i32 {
    let incorrect_updates: Vec<&Vec<i32>> = updates.iter().filter(|page| {
        rules.iter().any(|rule| !is_rule_valid(rule, page))
    }).collect();
    let mut fixed_updates = Vec::new();

    for update in incorrect_updates {
        let mut numbers = update.clone();
        let mut swaps_made = true;
        while swaps_made {
            swaps_made = false;
            for rule in rules {
                let (x, y) = *rule;
                if let (Some(ix), Some(iy)) = (numbers.iter().position(|&n| n == x), numbers.iter().position(|&n| n == y)) {
                    if ix >= iy {
                        numbers.swap(ix, iy);
                        swaps_made = true;
                    }
                }
            }
        }
        fixed_updates.push(numbers);
    }

    fixed_updates.iter().map(|numbers| numbers[numbers.len() / 2]).sum()
}

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let input_data = get_input_data(5).await?;
    let data: Vec<String> = input_data.lines().map(|s| s.to_string()).collect();
    let (rules, updates) = extract_sections(&data);

    println!("Part One: {}", solve_part_one(&rules, &updates));
    println!("Part Two: {}", solve_part_two(&rules, &updates));

    Ok(())
}