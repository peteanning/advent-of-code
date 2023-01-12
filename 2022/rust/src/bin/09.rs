use std::collections::HashSet;

use itertools::Itertools;

type Point = (i32, i32);

pub fn parse_input(input: &str) -> Vec<(&str, &str)> {
        input.lines()
        .into_iter()
        .map(|l| l.split(' ')
                   .into_iter()
                   .next_tuple()
                   .unwrap()).collect()
}

pub fn is_diagonal(h: Point, t: Point) -> bool{
    is_up_right(h,t)|| is_down_right(h, t) || is_up_left(h, t) || is_down_left(h, t) 
}

pub fn is_up_right(h: Point, t: Point) -> bool {
    h.0 - 1 == t.0 - 1 && h.1 + 1 == t.1 + 1
}

pub fn is_down_right(h: Point, t: Point) -> bool {
    h.0 + 1 == t.0 + 1 && h.1 + 1 == t.1 + 1
}

pub fn is_up_left(h: Point, t: Point) -> bool {
    h.0 - 1 == t.0 - 1 && h.1 - 1  == t.1 - 1
}

pub fn is_down_left(h: Point, t: Point) -> bool{
    h.0 + 1 == t.0 + 1 && h.1 - 1 == t.1 - 1 
}
    
pub fn is_touching(h: Point, t: Point) -> bool {
   // covers
    h == t || 
   // left
    (h.0 == t.0 && h.1 == t.1 - 1) || 
  // right
    (h.0 == t.0 && h.1 == t.1 + 1) ||
  // up
    (h.0 == t.0 + 1 && h.1 == t.1) ||
  //down
    (h.0 == t.0 - 1 && h.1 == t.1) ||

    is_diagonal(h, t)

}

pub fn part_one(input: &str) -> Option<usize> {
    let mut head: Point = (0,0);
    let mut global_tail: Point = (0,0);
    let mut moves: HashSet<Point> = HashSet::new();
    let commands = parse_input(&input);
    
    for c in commands {
       let m = c.1.parse::<usize>().unwrap();
       let cmd = c.0.chars().nth(0).unwrap();
       let mut local_tail = global_tail;
       
       for i in 1..=m {
            match cmd {
                'D' => {
                    head = (head.0 + 1, head.1);
                    local_tail = (global_tail.0 + 1, global_tail.1);
                },
                'U' => {
                    head = (head.0 - 1, head.1);
                    local_tail = (global_tail.0 - 1, global_tail.1);
                },
                'R' => {
                    head = (head.0, head.1 + 1);
                    local_tail = (global_tail.0, global_tail.1 + 1);
                },
                'L' => {
                    head = (head.0, head.1 - 1);
                    local_tail = (global_tail.0, global_tail.1 - 1);
                },
                _ => {panic!("Unknown command {:?}", c);}
            }
             if !is_touching(head, global_tail) {
                   global_tail = local_tail; // move the global tail
                   moves.insert(global_tail);
             }
       }

    }
    Some(moves.len())
}

pub fn part_two(input: &str) -> Option<u32> {
    None
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 9);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 9);
        assert_eq!(part_one(&input), Some(13));
    }
    #[test]
    fn test_is_touching() {
        let h: Point = (1,1);
        let t: Point = (1,1);
        assert!(is_touching(h, t));
        
        assert!(is_touching(h, (t.0 + 1, t.1))); //down
        assert!(is_touching(h, (t.0 - 1, t.1))); //up
        assert!(is_touching(h, (t.0, t.1 + 1))); //right
        assert!(is_touching(h, (t.0, t.1 - 1))); //left

        assert!(is_touching((h.0 + 1, h.1 + 1), (t.0 + 1, t.1 + 1))); //down-rght
        assert!(is_touching((h.0 - 1, h.1 + 1), (t.0 - 1, t.1 + 1))); //up-right

        assert!(is_touching((h.0 + 1, h.1 - 1), (t.0 + 1, t.1 - 1))); //down-left
        assert!(is_touching((h.0 - 1, h.1 - 1), (t.0 - 1, t.1 - 1))); //up-left
    }
    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 9);
        assert_eq!(part_two(&input), None);
    }
}
