use std::collections::HashMap;

use itertools::Itertools;

#[derive(Debug)]
#[derive(PartialEq)]
struct Data {
    stacks: Vec<Vec<char>>,
    stack_headers: Vec<usize>,
    moves: Vec<Move>
}

impl Data {
    fn new(stacks_vec: Vec<Vec<char>>, column_headers_vec: Vec<usize>, moves_vec: Vec<Move>) -> Data {
        Data { stacks: stacks_vec, stack_headers: column_headers_vec, moves: moves_vec }
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
    fn new (from_: usize, to_: usize, q: usize) -> Move {
        Move{ from: from_, to: to_, quantity: q}
    }
    fn new_from_line(line: &str) -> Move {
        //move 1 from 2 to 1
        let moves = line
            .replace("move ", ":")
            .replace(" from ", ":")
            .replace(" to ", ":")
            .split(":")
            .filter(|s| *s != "")
            .map(|s| s.trim().parse::<usize>().unwrap())
            .collect::<Vec<usize>>();

        Move { from: moves[1], to: moves[2], quantity: moves[0] }
    }
}
    //https://users.rust-lang.org/t/idiomatic-naming-when-a-name-clashes-with-a-keyword/32472 
fn execute_move(mut data: Data, mve: Move) -> Vec<Vec<char>> {
    let mut from = data.stacks[(mve.from - 1)].clone();
    let mut to = data.stacks[(mve.to - 1)].clone();
   //mutate the from and to
    for _i in 1..=mve.quantity {
        to.push(from.pop().unwrap());
    }
 
    //copy the stack to a new stack with the new from and to values
    let _x = std::mem::replace(&mut data.stacks[(mve.from - 1)], from);
    let _y = std::mem::replace(&mut data.stacks[(mve.to - 1)], to);

    
    data.stacks
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


fn parse_headers(line: &str) -> Vec<usize> {
    line
        .trim()
        .split(" ")
        .filter(|s| *s != "")
        .map(|s| s.parse::<usize>().unwrap())
        .collect()
}

fn parse_input(input: &str) -> Data {
    let mut rows: Vec<String> = vec![];
    let mut column_headers: Vec<usize> = vec![];
    let mut moves: Vec<Move> = vec![];

    input
        .lines()
        .into_iter()
        .for_each(|l| {
            if l.contains("[") {
               rows.push(l.to_string());
            } else if l.starts_with(" 1") {
                column_headers = parse_headers(l);
            } else if l.starts_with("move") {
                moves.push(Move::new_from_line(l));
            } else {
                println!("Ignoring [{}]", l);
            }
        });

    //dbg!(&rows);
    //dbg!(&column_headers);
    //dbg!(&moves);


    Data::new(parse_stacks(&rows, *column_headers.last().unwrap()), column_headers, moves)
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
    fn test_execute_one_move() {
        let input = advent_of_code::read_file("examples", 5);
        let expected: Vec<Vec<char>> = vec![vec!['Z', 'N', 'D'], vec!['M', 'C'], vec!['P']];
        let mve = Move { from: 2, to: 1, quantity: 1 };

        assert_eq!(execute_move(parse_input(&input), mve), expected);
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
        assert_eq!(Move::new_from_line(move_line), Move::new(2, 1, 1));
    }
//    #[test]
    fn test_parse_data() {
        
        let columns: Vec<Vec<char>> = vec![vec!['Z', 'N'], vec!['M', 'C', 'D'], vec!['P']];
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