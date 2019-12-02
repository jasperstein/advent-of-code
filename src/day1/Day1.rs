use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::cmp::max;

fn main() {

    let mut fuel_weight: i32 = 0;
    let mut total_fuel: i32 = 0;

    // File hosts must exist in current path before this produces output
    if let Ok(lines) = read_lines("src/day1/Input.txt") {
        // Consumes the iterator, returns an (Optional) String
        for line_result in lines {
            if let Ok(line) = line_result {
                let module_mass = line.parse::<i32>().unwrap();
                let module_fuel = fuel(module_mass);
                fuel_weight += module_fuel;
                let delta = tot_fuel(module_fuel);
                println!("Module of mass {} needs total fuel {}", module_mass, delta);
                total_fuel += delta;
            }
        }
    }
    println!("{}", fuel_weight);
    println!("{}", total_fuel);
}

fn fuel(mass: i32) -> i32 {
    max(0, (mass / 3) - 2)
}

fn tot_fuel(delta: i32) -> i32 {
    if delta <= 0 {
        0
    } else {
        let result = delta + tot_fuel(fuel(delta));
        println!("delta {} requires fuel {}", delta, result);
        result
    }
}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}