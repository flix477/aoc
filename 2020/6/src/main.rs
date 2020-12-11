use std::fs::File;
use std::io::{BufRead, BufReader, Lines};
use std::ops::{BitAnd, BitOr};

fn main() {
    let lines = BufReader::new(File::open("input.txt").unwrap()).lines();
    let groups = Groups::new(lines);
    let result = all_sum(groups);

    println!("{:?}", result);
}

fn any_sum<B: BufRead>(groups: Groups<B>) -> u32 {
    group_to_sum(u32::bitor, 0, groups)
}

fn all_sum<B: BufRead>(groups: Groups<B>) -> u32 {
    group_to_sum(u32::bitand, 0xFFFFFFFF, groups)
}

fn group_to_sum<B: BufRead>(combine: fn(u32, u32) -> u32, initial: u32, groups: Groups<B>) -> u32 {
    groups.map(|group|
        group.iter()
            .map(line_to_bits)
            .fold(initial, combine)
            .count_ones()
    ).sum()
}

fn line_to_bits(line: &String) -> u32 {
    line.as_bytes().iter()
        .map(|x| x - 97)
        .fold(0u32, |acc, x| acc | (1 << x))
}

struct Groups<B> {
    lines: Lines<B>
}

impl<B> Groups<B> {
   pub fn new(lines: Lines<B>) -> Self {
       Self { lines }
   }
}

impl<B: BufRead> Iterator for Groups<B> {
    type Item = Vec<String>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut group: Vec<String> = Vec::new();

        loop {
            let line = self.lines.next();
            if line.is_none() {
                break;
            }
            let line = line.unwrap().unwrap();

            if line.is_empty() {
                return Some(group);
            } else {
                group.push(line);
            }
        }

        if group.is_empty() {
            None
        } else {
            Some(group)
        }
    }
}
