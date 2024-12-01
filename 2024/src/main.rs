mod utils;
mod _01;

use std::io;
use std::io::Write;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    print!("Enter the day number you want to run (1-25): ");
    io::stdout().flush()?;
    let mut day_str = String::new();
    io::stdin().read_line(&mut day_str)?;
    let day: i32 = day_str.trim().parse()?;

    if (1..=25).contains(&day) {
        match day {
            1 => _01::run().await,
            _ => {
                println!("Solution for the given day is not implemented yet.");
                Ok(())
            }
        }
    } else {
        println!("Invalid input. Please enter a number between 1 and 25.");
        Ok(())
    }
}
