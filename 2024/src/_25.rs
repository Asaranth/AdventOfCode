use crate::utils::get_input_data;

pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
    let data = get_input_data(25).await?;
    let grids = data.split("\n\n").collect::<Vec<_>>();
    let mut locks = Vec::new();
    let mut keys = Vec::new();
    let mut pin_height = 0;
    for grid in grids.iter() {
        let grid = grid.lines().map(|l| l.chars().collect::<Vec<_>>()).collect::<Vec<_>>();
        let is_key = grid[0][0] != '#';
        pin_height = grid.len();
        let mut heights = vec![0usize; grid[0].len()];
        for row in grid {
            for (x, c) in row.iter().enumerate() {
                if *c == '#' {
                    heights[x] += 1;
                }
            }
        }
        if is_key {
            keys.push(heights);
        } else {
            locks.push(heights);
        }
    }
    let mut total = 0;
    for k in &keys {
        for l in &locks{
            if k.iter().zip(l).all(|(a, b)| a + b <= pin_height) {
                total += 1;
            }
        }
    }
    println!("Solution: {}", total);
    Ok(())
}