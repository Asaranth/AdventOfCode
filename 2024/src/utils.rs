use reqwest::Client;
use std::env;
use std::fs;
use std::path::Path;
use std::io::{self, Write};
use dotenv::from_filename;

pub async fn get_input_data(day: i32) -> Result<String, Box<dyn std::error::Error>> {
    from_filename("../.env")?;
    let cache_file = format!("data/{:02}.txt", day);
    if Path::new(&cache_file).exists() {
        return Ok(fs::read_to_string(cache_file)?);
    }

    let session_cookie = env::var("AOC_SESSION_COOKIE")?;
    if session_cookie.is_empty() {
        return Err(Box::new(io::Error::new(io::ErrorKind::InvalidData, "AOC_SESSION_COOKIE is not configured.")));
    }

    let url = format!("https://adventofcode.com/2024/day/{}/input", day);
    let client = Client::new();
    let res = client.get(url).header("Cookie", format!("session={}", session_cookie)).send().await?;

    if !res.status().is_success() {
        return Err(Box::new(io::Error::new(io::ErrorKind::Other, format!("Failed to fetch data: {}", res.status()))));
    }

    let data = res.text().await?;
    fs::create_dir_all("data")?;
    let mut file = fs::File::create(&cache_file)?;
    file.write_all(data.as_bytes())?;

    Ok(data)
}