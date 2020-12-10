pub mod crate20201210;

fn main() {    
	match std::env::args().nth(1).unwrap().as_str() {
        "20201210" => crate20201210::solve(),
		_ => println!("unknown puzzle"),
	}
}
