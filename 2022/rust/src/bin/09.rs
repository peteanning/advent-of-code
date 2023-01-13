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

pub fn part_one(input: &str) -> Option<usize> {
    let mut global_head: Point = (0,0);
    let mut global_tail: Point = (0,0);
    let mut moves: HashSet<Point> = HashSet::new();
    moves.insert(global_tail);

    let commands = parse_input(&input);
     
    for c in commands {
       let m = c.1.parse::<usize>().unwrap();
       let cmd = c.0.chars().nth(0).unwrap();
       let mut local_tail = global_tail;
       println!("== {:?} ==",c );
       for i in 1..=m {
           println!("moving {}", i);
           let mut local_head = global_head; 
            match cmd {
                'D' => {
                    global_head = (global_head.0 + 1, global_head.1);
                    local_tail = (global_tail.0 + 1, global_tail.1);
                },
                'U' => {
                    global_head = (global_head.0 - 1, global_head.1);
                    local_tail = (global_tail.0 - 1, global_tail.1);
                },
                'R' => {
                    global_head = (global_head.0, global_head.1 + 1);
                    local_tail = (global_tail.0, global_tail.1 + 1);
                },
                'L' => {
                    global_head = (global_head.0, global_head.1 - 1);
                    local_tail = (global_tail.0, global_tail.1 - 1);
                },
                _ => {panic!("Unknown command {:?}", c);}
            }
             if one_away_same_row_col(global_head, global_tail) {
                 global_tail = local_tail; // move the global tail to the locally moved tail
                 moves.insert(global_tail);
             }

             if not_touching_not_same_row_and_col(global_head, global_tail) {
                    println!("Not Touching same row and column");
                    global_tail = move_diagonal(global_head, global_tail);
                    moves.insert(global_tail);
             }

             println!("Head at {:?} Tail at {:?}", global_head, global_tail);
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
    fn test_is_touching() {
        let h: Point = (1,1);
        let t: Point = (1,1);
        assert!(is_touching_same_row_and_column(h, t));
        
        assert!(is_touching_same_row_and_column(h, (t.0 + 1, t.1))); //down
        assert!(is_touching_same_row_and_column(h, (t.0 - 1, t.1))); //up
        assert!(is_touching_same_row_and_column(h, (t.0, t.1 + 1))); //right
        assert!(is_touching_same_row_and_column(h, (t.0, t.1 - 1))); //left

        assert!(is_touching_same_row_and_column((h.0 + 1, h.1 + 1), (t.0 + 1, t.1 + 1))); //down-rght
        assert!(is_touching_same_row_and_column((h.0 - 1, h.1 + 1), (t.0 - 1, t.1 + 1))); //up-right

        assert!(is_touching_same_row_and_column((h.0 + 1, h.1 - 1), (t.0 + 1, t.1 - 1))); //down-left
        assert!(is_touching_same_row_and_column((h.0 - 1, h.1 - 1), (t.0 - 1, t.1 - 1))); //up-left
    }
    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 9);
        assert_eq!(part_two(&input), None);
    }
}
