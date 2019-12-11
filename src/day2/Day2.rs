use std::fs::File;
use std::io::{BufReader, BufRead};
use std::path::Path;

fn main() {
    let input = File::open(Path::new("src/day2/Input.txt")).unwrap();
    let mut reader = BufReader::new(input);
    let buf: &mut String = &mut String::new();
    reader.read_line(buf).unwrap();
    let orig_mem: Vec<i32> = buf.split(",").map(|it| it.parse::<i32>().unwrap()).collect();

    let mem = &mut (orig_mem.clone());
    mem.splice(1..3, [12, 2].iter().cloned());
    run_prog(mem);
    println!("{:?}", mem);

    for x in 0..10000 {
        let mem = &mut (orig_mem.clone());
        mem.splice(1..3, [x/100, x%100].iter().cloned());
        run_prog(mem);
        if mem[0 as usize] == 19690720 {
            println!("{}", x);
        }
    }

}

fn add_cmd(mem: &mut Vec<i32>, prg_ctr: i32) {
    let idx1 = mem[(prg_ctr + 1) as usize];
    let idx2 = mem[(prg_ctr + 2) as usize];
    let idx3 = mem[(prg_ctr + 3) as usize] as usize;
    let val1 = mem[idx1 as usize];
    let val2 = mem[idx2 as usize];
    mem.splice(idx3 .. idx3+1, [val1+val2].iter().cloned());
}

fn mult_cmd(mem: &mut Vec<i32>, prg_ctr: i32) {
    let idx1 = mem[(prg_ctr + 1) as usize];
    let idx2 = mem[(prg_ctr + 2) as usize];
    let idx3 = mem[(prg_ctr + 3) as usize] as usize;
    let val1 = mem[idx1 as usize];
    let val2 = mem[idx2 as usize];
    mem.splice(idx3 .. idx3+1, [val1*val2].iter().cloned());
}

fn run_prog(mem: &mut Vec<i32>) {
    let mut prg_ctr = 0;
    let mut next_cmd = mem[prg_ctr as usize];
    while next_cmd != 99 {
        if next_cmd == 1 { add_cmd(mem, prg_ctr); }
        else if next_cmd == 2 { mult_cmd(mem, prg_ctr); }
        prg_ctr += 4;
        next_cmd = mem[prg_ctr as usize];
    }
}
