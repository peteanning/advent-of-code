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

pub fn not_touching_not_same_row_and_col(h: Point, t: Point) -> bool {
   //up 2 left || right
    (h.0 == t.0 - 2 && (h.1 == t.1 - 1 || h.1 == t.1 + 1)) ||
   //down 2 left || right
    (h.0 == t.0 + 2 && (h.1 == t.1 - 1 || h.1 == t.1 + 1)) ||
    //upper_mid left || right
    (h.0 == t.0 - 1 && (h.1 == t.1 - 2 || h.1 == t.1 + 2)) || 
    //lower_mid left || right
    (h.0 == t.0 + 1 && (h.1 == t.1 - 2 || h.1 == t.1 + 2))  

}

pub fn move_diagonal(h: Point, t: Point) -> Point {
   let up_left: Point = (t.0 - 2, t.1 - 1);
   let upper_mid_left: Point = (t.0 - 1, t.1 - 2);

   let up_right: Point = (t.0 - 2, t.1 + 1);
   let upper_mid_right: Point = (t.0 - 1, t.1 + 2);

   let down_left: Point = (t.0 + 2, t.1 - 1);
   let lower_mid_left: Point = (t.0 + 1, t.1 - 2);

   let down_right: Point = (t.0 + 2, t.1 + 1);
   let lower_mid_right: Point = (t.0 + 1, t.1 + 2);

   if h == up_left || h == upper_mid_left {
       (t.0 - 1, t.1 - 1) 
   } else if h ==  up_right || h == upper_mid_right {
       (t.0 - 1, t.1 + 1) 
   } else if h == down_left || h == lower_mid_left  {
       (t.0 + 1, t.1 - 1) 
   } else if h == down_right || h == lower_mid_right{
       (t.0 + 1, t.1 + 1) 
   } else {
       t
   }
}
    
pub fn is_touching_same_row_and_column(h: Point, t: Point) -> bool {
   // covers
    h == t || 
   // left
    (h.0 == t.0 && h.1 == t.1 - 1) || 
  // right
    (h.0 == t.0 && h.1 == t.1 + 1) ||
  // up
    (h.0 == t.0 + 1 && h.1 == t.1) ||
  //down
    (h.0 == t.0 - 1 && h.1 == t.1) 

}

pub fn one_away_same_row_col(h: Point, t: Point) -> bool {
   // left
    (h.0 == t.0 && h.1 == t.1 - 2) || 
  // right
    (h.0 == t.0 && h.1 == t.1 + 2) ||
  // up
    (h.0 == t.0 + 2 && h.1 == t.1) ||
  //down
    (h.0 == t.0 - 2 && h.1 == t.1) 

}

pub fn move_one(p: Point, direction: char) -> Point {
    match direction {
        'D' => (p.0, p.1 + 1),
        'U' => (p.0, p.1 - 1),
        'L' => (p.0 - 1, p.1),
        'R' => (p.0 + 1, p.1),
        _ => panic!("Unknown direction {:?}", direction)
    }
}

pub fn part_one(input: &str) -> Option<usize> {
    let mut head: Point = (0,0);
    let mut tail: Point = (0,0);
    let mut moves: HashSet<Point> = HashSet::new();
    moves.insert(tail);

    let commands = parse_input(&input);
     
    for c in commands {
       let m = c.1.parse::<usize>().unwrap();
       let direction = c.0.chars().nth(0).unwrap();
       for _ in 1..=m {
             head = move_one(head, direction);
             if one_away_same_row_col(head, tail) {
                 tail = move_one(tail, direction); // move the global tail to the locally moved tail
                 moves.insert(tail);
             }

             if not_touching_not_same_row_and_col(head, tail) {
                    tail = move_diagonal(head, tail);
                    moves.insert(tail);
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
    fn test_move_diagonal() {
        let t: Point = (1,1);
        assert_eq!(move_diagonal((-1, 2), t), (0,2)); //up_right
        assert_eq!(move_diagonal((-1, 0), t), (0,0)); //up_left
       
        assert_eq!(move_diagonal((0, 3), t), (0,2)); //up_mid_right
        assert_eq!(move_diagonal((0, -1), t), (0,0)); //up_mid_left

        assert_eq!(move_diagonal((3, 0), t), (2,0)); //down_left
        assert_eq!(move_diagonal((3, 2), t), (2,2)); //down_right

        assert_eq!(move_diagonal((2, 3), t), (2,2)); //down_mid_right
        assert_eq!(move_diagonal((2, -1), t), (2,0)); //down_mid_left
        
    }


    #[test]
    fn test_not_touch_not_same_row_col() {
        let t: Point = (1,1);
        assert!(not_touching_not_same_row_and_col((-1, 0), t)); //up_left
        assert!(not_touching_not_same_row_and_col((-1, 2), t)); //up_right
        assert!(not_touching_not_same_row_and_col((3, 2), t)); //down_right
        assert!(not_touching_not_same_row_and_col((3, 0), t)); //down_right
        
        assert!(not_touching_not_same_row_and_col((0, 3), t)); //upper_mid_right
        assert!(not_touching_not_same_row_and_col((0, -1), t)); //upper_mid_left
        
        assert!(not_touching_not_same_row_and_col((2, 3), t)); //lower_mid_right
        assert!(not_touching_not_same_row_and_col((2, -1), t)); //lower_mid_left
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 9);
        assert_eq!(part_two(&input), None);
    }
}
