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
mod _13;
mod _14;
mod _15;
mod _16;
mod _17;
mod _18;
mod _19;
mod _20;
mod _21;
mod _22;
mod _23;

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
            13 => _13::run().await,
            14 => _14::run().await,
            15 => _15::run().await,
            16 => _16::run().await,
            17 => _17::run().await,
            18 => _18::run().await,
            19 => _19::run().await,
            20 => _20::run().await,
            21 => _21::run().await,
            22 => _22::run().await,
            23 => _23::run().await,
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
