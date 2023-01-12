use std::collections::HashSet;

use itertools::Itertools;

type Point = (i32, i32);

pub fn parse_input(input: &str) {
    let mut h: Point = (0,0);
    let mut t: Point = (0,0);
    let mut moves: HashSet<(usize,usize)> = HashSet::new();

    input.lines()
        .into_iter()
        .map(|l| l.split(' ')
                   .into_iter()
                   .next_tuple()
                   .unwrap())
                   .for_each(|c: (&str,&str)|{
                        println!("{}, {}", c.0, c.1);
                        let command = c.0;
                        let value = c.1;

                   });
    

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

  //up-right
     (h.0 - 1 == t.0 - 1 && h.1 + 1 == t.1 + 1) ||
  //down-right
     (h.0 + 1 == t.0 + 1 && h.1 + 1 == t.1 + 1) ||

  //up-left
     (h.0 - 1 == t.0 - 1 && h.1 - 1  == t.1 - 1) ||

  //down-right
     (h.0 + 1 == t.0 + 1 && h.1 - 1 == t.1 - 1) 

}
pub fn part_one(input: &str) -> Option<u32> {
    parse_input(&input);
    None
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
        assert_eq!(part_one(&input), None);
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
