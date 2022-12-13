use std::collections::HashMap;

use itertools::Itertools;

#[derive(Debug)]
#[derive(PartialEq)]
struct Data<'a> {
    columns: Vec<&'a str>,
    column_headers: Vec<u8>,
    moves: Vec<Move>
}

impl<'a> Data<'a> {
    fn new(columns_vec: Vec<&'a str>, column_headers_vec: Vec<u8>, moves_vec: Vec<Move>) -> Data<'a> {
        Data { columns: columns_vec, column_headers: column_headers_vec, moves: moves_vec }
    }
}

#[derive(Debug)]
#[derive(PartialEq)]
struct Move {
    from: u8,
    to: u8,
    quantity: u8
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

fn parse_column<'a>(mut rows: &Vec<&str>, max_colum: u8) -> HashMap<u8, Vec<&'a[char]>> {
    let mut columm_map: HashMap<u8, Vec<&[char]>> = HashMap::new();
    let mut count: usize = 0;

    let chars = rows.iter().map(|s| {
                                      let mut f = s.chars().collect::<Vec<char>>();
                                      f.append(&mut vec![' ']);
                                      f
                                    }).collect::<Vec<Vec<char>>>();

    for row in chars {

        let row_vec: Vec<&[char]> = row
            .chunks(max_colum as usize).into_iter()
            .collect::<Vec<_>>();

        for col in (1..max_colum) {
           if let Some(v) = columm_map.get_mut(&col)  {
                v.append(&mut vec![row_vec[col as usize].clone()]); 
           } else {
                columm_map.insert(col, vec![row_vec[col as usize].clone()]);
           }
      }
    }
    columm_map
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
    let mut columns: Vec<&str> = vec![];
    let mut column_headers: Vec<u8> = vec![];
    let mut moves: Vec<Move> = vec![];

    input
        .lines()
        .into_iter()
        .for_each(|l| {
            if l.contains("[") {
               columns.push(l);
            } else if l.starts_with(" 1") {
                column_headers = parse_headers(l);
            } else if l.starts_with("move") {
                moves.push(Move::new_from_line(l));
            } else {
                ()
            }
        });

    dbg!(&columns);
    dbg!(&column_headers);
    dbg!(&moves);


    Data::new(columns, column_headers, moves)
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

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 5);
        assert_eq!(part_one(&input), None);
    }
    
    #[test]
    fn test_parse_rows() {
        let columns: Vec<&str> = vec!["    [D]    ", "[N] [C]    ", "[Z] [M] [P]"];
        let result = parse_column(&columns, 3);
        dbg!(result);
        //assert_eq!(parse_headers(header_line), vec![1,2,3]);
        assert!(false);
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
