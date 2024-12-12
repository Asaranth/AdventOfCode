mod utils;
mod _01;
mod _02;
mod _03;
mod _04;
mod _05;
mod _06;
mod _07;
mod _08;
mod _09;
mod _10;
mod _11;
mod _12;

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
            2 => _02::run().await,
            3 => _03::run().await,
            4 => _04::run().await,
            5 => _05::run().await,
            6 => _06::run().await,
            7 => _07::run().await,
            8 => _08::run().await,
            9 => _09::run().await,
            10 => _10::run().await,
            11 => _11::run().await,
            12 => _12::run().await,
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
