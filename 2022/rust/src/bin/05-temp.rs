use std::collections::HashMap;

use itertools::Itertools;

#[derive(Debug)]
#[derive(PartialEq)]
struct Data<'a> {
    stacks: Vec<&'a str>,
    stack_headers: Vec<u8>,
    moves: Vec<Move>
}

impl<'a> Data<'a> {
    fn new(stacks_vec: Vec<&'a str>, column_headers_vec: Vec<u8>, moves_vec: Vec<Move>) -> Data<'a> {
        Data { stacks: stacks_vec, stack_headers: column_headers_vec, moves: moves_vec }
    }

   //https://users.rust-lang.org/t/idiomatic-naming-when-a-name-clashes-with-a-keyword/32472 
    fn execute_move(&self, mve: Move) -> &Data<'a> {
        let mut from = self.stacks[mve.from];
        let mut to = self.stacks[mve.to];
        
        //mutate the from and to

        //copy the stack to a new stack with the new from and to values


        &Data::new(stacks_vec, column_headers_vec, moves_vec)
    }
}

#[derive(Debug)]
#[derive(PartialEq)]
struct Move {
    from: usize,
    to: usize,
    quantity: usize
}
impl Move {
    fn new (f: u8, t: u8, q: u8) -> Move {
        Move{ from: f, to: t, quantity: q}
    }
    fn new_from_line(line: &str) -> Move {
        //move 1 from 2 to 1
        let moves = line
            .replace("move ", ":")
            .replace(" from ", ":")
            .replace(" to ", ":")
            .split(":")
            .filter(|s| *s != "")
            .map(|s| s.trim().parse::<u8>().unwrap())
            .collect::<Vec<u8>>();

        Move { from: moves[0], to: moves[1], quantity: moves[2] }
    }
}

// copied from https://github.com/beny23/advent-of-code/
fn parse_stacks(crate_strs: &Vec<String>, size: usize) -> Vec<Vec<char>> {
    let mut stacks = vec![Vec::<char>::new(); size];
    for l in crate_strs.iter().rev() {
        for i in 0..size {
            if let Some(c) = l.chars().nth(1 + i*4) {
                if c != ' ' {
                    stacks[i].push(c);
                }
            }
        }
    }
    stacks
}


fn parse_headers(line: &str) -> Vec<u8> {
    line
        .trim()
        .split(" ")
        .filter(|s| *s != "")
        .map(|s| s.parse::<u8>().unwrap())
        .collect()
}

fn parse_input(input: &str) -> Data {
    let mut rows: Vec<&str> = vec![];
    let mut column_headers: Vec<u8> = vec![];
    let mut moves: Vec<Move> = vec![];

    input
        .lines()
        .into_iter()
        .for_each(|l| {
            if l.contains("[") {
               rows.push(l);
            } else if l.starts_with(" 1") {
                column_headers = parse_headers(l);
            } else if l.starts_with("move") {
                moves.push(Move::new_from_line(l));
            } else {
                println!("Ignoring [{}]", l);
            }
        });

    dbg!(&rows);
    dbg!(&column_headers);
    dbg!(&moves);


    Data::new(rows, column_headers, moves)
}


pub fn part_one(input: &str) -> Option<u32> {
    None
}

pub fn part_two(input: &str) -> Option<u32> {
    None
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 5);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ROWS() -> Vec<String> {
        vec![ String::from("    [D]    "), String::from("[N] [C]    "), String::from("[Z] [M] [P]")]
    }

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 5);
        assert_eq!(part_one(&input), None);
    }
    
    #[test]
    fn test_execute_move() {
        let expected: Vec<Vec<char>> = vec![vec!['Z', 'N'], vec!['M', 'C', 'D'], vec!['P']];
        let mve = Move::new(1, 2, 1);
        assert_eq!(Data::execute_move(data, mve), expected);
    }


    #[test]
    fn test_parse_rows() {
        let expected: Vec<Vec<char>> = vec![vec!['Z', 'N'], vec!['M', 'C', 'D'], vec!['P']];
        assert_eq!(parse_stacks(&ROWS(), 3), expected);
    }


    #[test]
    fn test_parse_headers() {
        let header_line = " 1   2   3 ";
        assert_eq!(parse_headers(header_line), vec![1,2,3]);
    }

    #[test]
    fn test_parse_move() {
        let move_line = "move 1 from 2 to 1";
        assert_eq!(Move::new_from_line(move_line), Move::new(1, 2, 1));
    }
    #[test]
    fn test_parse_data() {
        
        let columns: Vec<&str> = vec!["    [D]    ", "[N] [C]    ", "[Z] [M] [P]"];
        let column_headers = vec![1,2,3];
        let moves = vec![Move::new(1, 2, 1), Move::new(3,1, 3), Move::new(2,2,1), Move::new(1, 1, 2)];

        let input = advent_of_code::read_file("examples", 5);

        let data = Data::new(columns, column_headers, moves);
        assert_eq!(parse_input(&input), data);
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 5);
        assert_eq!(part_two(&input), None);
    }
}
