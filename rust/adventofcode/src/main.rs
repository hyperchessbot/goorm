use cached::cached_key;
use cached::SizedCache;
use std::collections::HashSet;
use std::{env, fs};

cached_key! {
    VALID_ARRS: SizedCache<usize, usize> = SizedCache::with_size(200);
    Key = start;
    fn valid_arrs(jolts: &HashSet<usize>, start: usize, end: &usize) -> usize = {
        if start == *end {
            return 1;
        }
        let mut output = 0usize;
        for jolt in start + 1..=start + 3 {
            if jolts.contains(&jolt) {
                output += valid_arrs(jolts, jolt, end);
            }
        }
        output
    }
}

fn main() {
    let mut records: Vec<usize> = fs::read_to_string("20201210/example2.txt")
        .unwrap()
        .split('\n')
        .map(|num| num.parse().unwrap())
        .collect();
    records.sort_unstable();
    let jolts: HashSet<usize> = records.iter().cloned().collect();

    match env::args().nth(1).unwrap().as_str() {
        "1" => {
            let mut diff1 = (records[0] == 1) as usize;
            let mut diff3 = 1usize;
            for i in 1..records.len() {
                if records[i] - records[i - 1] == 1 {
                    diff1 += 1
                } else if records[i] - records[i - 1] == 3 {
                    diff3 += 1
                }
            }
            println!("{}", diff1 * diff3);
        }
        "2" => println!("{}", valid_arrs(&jolts, 0, records.last().unwrap())),
        _ => println!("Only two puzzles daily!"),
    }
}
