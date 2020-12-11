use std::collections::HashMap;
use std::{env, fs};

fn generate_lines(nb_rows: usize, nb_cols: usize) -> Vec<Vec<(usize, usize)>> {
    let mut output: Vec<Vec<(usize, usize)>> = vec![];

    for r in 0..nb_rows {
        let mut tmp: Vec<(usize, usize)> = vec![];
        for c in 0..nb_cols {
            tmp.push((r, c))
        }
        output.push(tmp);
    }

    for c in 0..nb_cols {
        let mut tmp: Vec<(usize, usize)> = vec![];
        for r in 0..nb_rows {
            tmp.push((r, c))
        }
        output.push(tmp)
    }

    for r in 0..nb_rows {
        let mut c = 0usize;
        let mut tmp: Vec<(usize, usize)> = vec![];
        while (r + c < nb_rows) & (c < nb_cols) {
            tmp.push((r + c, c));
            c += 1
        }
        output.push(tmp)
    }

    for c in 1..nb_cols {
        let mut r = 0usize;
        let mut tmp: Vec<(usize, usize)> = vec![];
        while (r < nb_rows) & (r + c < nb_cols) {
            tmp.push((r, r + c));
            r += 1
        }
        output.push(tmp)
    }

    for c in 1..nb_cols {
        let mut r = nb_rows - 1;
        let mut tmp: Vec<(usize, usize)> = vec![];
        while (r > 0) & (c + nb_rows - r - 1 < nb_cols) {
            tmp.push((r, c + nb_rows - r - 1));
            r -= 1
        }
        output.push(tmp)
    }

    for r in (1..nb_rows).rev() {
        let mut c = 0usize;
        let mut tmp: Vec<(usize, usize)> = vec![];
        while c < nb_cols {
            tmp.push((r - c, c));
            if r - c == 0 {
                break;
            }
            c += 1
        }
        output.push(tmp)
    }

    output
}

fn update_view(
    records: &[Vec<char>],
    line: &[(usize, usize)],
    adjacent: bool,
    seen: &mut HashMap<(usize, usize), usize>,
) {
    let data: Vec<(&(usize, usize), char)> = line
        .iter()
        .map(|p| (p, records[p.0][p.1]))
        .filter(|d| adjacent || d.1 != '.')
        .collect();

    if data.len() < 2usize {
        return;
    }

    let current = seen.entry(*data[0].0).or_default();
    *current += (data[1].1 == '#') as usize;
    let current = seen.entry(*data[data.len() - 1].0).or_default();
    *current += (data[data.len() - 2].1 == '#') as usize;

    for (idx, val) in data[1..data.len() - 1].iter().enumerate() {
        let current = seen.entry(*val.0).or_default();
        *current += (data[idx].1 == '#') as usize + (data[idx + 2].1 == '#') as usize
    }
}

fn main() {
    let mut records: Vec<Vec<char>> = fs::read_to_string("./src/bin/day11/input")
        .unwrap()
        .split('\n')
        .map(|x| x.chars().collect())
        .collect();
    let nb_rows: usize = records.len();
    let nb_cols: usize = records[0].len();
    let lines = generate_lines(nb_rows, nb_cols);

    let adjacent: bool;
    let tolerance: usize;

    match env::args().nth(1).unwrap().as_str() {
        "1" => {
            adjacent = true;
            tolerance = 4;
        }
        "2" => {
            adjacent = false;
            tolerance = 5;
        }

        _ => {
            println!("Only two puzzles daily!");
            return;
        }
    }

    let mut flag = true;
    let mut output = 0usize;
    while flag {
        let mut to_occ: Vec<(usize, usize)> = vec![];
        let mut to_emp: Vec<(usize, usize)> = vec![];
        let mut seen: HashMap<(usize, usize), usize> = HashMap::new();

        for line in &lines {
            update_view(&records, line, adjacent, &mut seen)
        }

        for r in 0..nb_rows {
            for c in 0..nb_cols {
                if records[r][c] == 'L' && seen[&(r, c)] == 0 {
                    to_occ.push((r, c))
                }
                if records[r][c] == '#' && seen[&(r, c)] >= tolerance {
                    to_emp.push((r, c))
                }
            }
        }

        output += to_occ.len() - to_emp.len();
        flag = (to_occ.len() + to_emp.len()) != 0;

        for p in to_occ {
            records[p.0][p.1] = '#'
        }
        for p in to_emp {
            records[p.0][p.1] = 'L'
        }
    }
    println!("{}", output);
}
