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

fn execute_moves(mut data: Data, f: fn(&mut Vec<Vec<char>>, Move)) -> Vec<Vec<char>>  {
    
    for m in data.moves {
        f(&mut data.stacks, m);
    }
    data.stacks

}

//https://users.rust-lang.org/t/idiomatic-naming-when-a-name-clashes-with-a-keyword/32472 
fn move_crates_one_at_a_time(stacks: &mut Vec<Vec<char>>, mve: Move)  {
    let mut from = stacks[(mve.from - 1)].clone();
    let mut to = stacks[(mve.to - 1)].clone();

    for _i in 1..=mve.quantity {
        to.push(from.pop().unwrap());
    }
 
    let _x = std::mem::replace(&mut stacks[(mve.from - 1)], from);
    let _y = std::mem::replace(&mut stacks[(mve.to - 1)], to);
    
}

fn move_crates_in_group(stacks: &mut Vec<Vec<char>>, mve: Move)  {
    let mut from = stacks[(mve.from - 1)].clone();
    let mut to = stacks[(mve.to - 1)].clone();
    let mut group: Vec<char> = vec![];


    for _i in 1..=mve.quantity {
        group.push(from.pop().unwrap());
    }
    group.reverse();
    to.append(&mut group);
 
    let _x = std::mem::replace(&mut stacks[(mve.from - 1)], from);
    let _y = std::mem::replace(&mut stacks[(mve.to - 1)], to);
    
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

    Data::new(parse_stacks(&rows, *column_headers.last().unwrap()), column_headers, moves)
}

fn stack_tops_as_string(stacks: &Vec<Vec<char>>) -> Option<String> {
   let mut result: String = "".to_string();

   for v in stacks {
    result.push(v.last().unwrap().to_owned()); 
   }
   Some(result)
    
}
pub fn part_one(input: &str) -> Option<String> {
   let data: Data =  parse_input(input);
   let stacks = execute_moves(data, move_crates_one_at_a_time);

   stack_tops_as_string(&stacks)
}

pub fn part_two(input: &str) -> Option<String> {

   let data: Data =  parse_input(input);
   let stacks = execute_moves(data, move_crates_in_group);

    stack_tops_as_string(&stacks)
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
        assert_eq!(part_one(&input), Some("CMZ".to_string()));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 5);
        assert_eq!(part_two(&input), Some("MCD".to_string()));
    }
    
    #[test]
    fn test_example_part_one() {
        let input = advent_of_code::read_file("examples", 5);
        let expected: Vec<Vec<char>> = vec![vec!['C'], vec!['M'], vec!['P', 'D', 'N', 'Z']];

        assert_eq!(execute_moves(parse_input(&input), move_crates_one_at_a_time), expected);
    }

    #[test]
    fn test_execute_one_move() {
        let input = advent_of_code::read_file("examples", 5);
        let expected: Vec<Vec<char>> = vec![vec!['Z', 'N', 'D'], vec!['M', 'C'], vec!['P']];
        let mve = Move { from: 2, to: 1, quantity: 1 };
        let mut data: Data = parse_input(&input);

        move_crates_one_at_a_time(&mut data.stacks, mve);
        assert_eq!(data.stacks, expected);
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
    #[test]
    fn test_parse_data() {
        
        let columns: Vec<Vec<char>> = vec![vec!['Z', 'N'], vec!['M', 'C', 'D'], vec!['P']];
        let column_headers = vec![1,2,3];
        let moves = vec![Move::new(2, 1, 1), Move::new(1,3, 3), Move::new(2,1,2), Move::new(1, 2, 1)];

        let input = advent_of_code::read_file("examples", 5);

        let data = Data::new(columns, column_headers, moves);
        assert_eq!(parse_input(&input), data);
    }

    
}
