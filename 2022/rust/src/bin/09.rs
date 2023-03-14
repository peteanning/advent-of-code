use std::collections::HashSet;

use itertools::Itertools;


#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Hash)]
#[derive(Clone)]
#[derive(Copy)]
pub struct Point {
    col: i32,
    row: i32
}

impl Point {
   fn new(column: i32, row: i32) -> Point {
       Point { col: column, row }
   } 

   fn move_one(&self, direction: char) -> Point {
       match direction {
           'D' => Point::new(self.col, self.row - 1),
           'U' => Point::new(self.col, self.row + 1),
           'L' => Point::new(self.col - 1, self.row),
           'R' => Point::new(self.col + 1, self.row),
           _ => panic!("Unknown direction {}", direction) 
       }
   }

   fn not_touching_not_same_row_and_col(&self, tail: Point) -> bool {
       let up_mid_left = Point::new(tail.col - 2, tail.row - 1);
       let up_left = Point::new(tail.col - 1, tail.row -2);
       let top_left = Point::new(tail.col -2, tail.row -2);

       let up_mid_right = Point::new(tail.col + 2, tail.row - 1);
       let up_right = Point::new(tail.col + 1, tail.row - 2);
       let top_right = Point::new(tail.col + 2, tail.row - 2);

       let lower_mid_left = Point::new(tail.col - 2, tail.row + 1);
       let lower_left = Point::new(tail.col - 1, tail.row + 2);
       let bottom_left = Point::new(tail.col - 2, tail.row + 2);

       let lower_mid_right = Point::new(tail.col + 2, tail.row + 1);
       let lower_right = Point::new(tail.col + 1, tail.row + 2);
       let bottom_right = Point::new(tail.col + 2, tail.row + 2);

       let cs = [up_mid_left, up_left, top_left, up_mid_right, up_right,
                    top_right, lower_mid_left, lower_left, bottom_left,
                    lower_mid_right, lower_right, bottom_right];

       cs.contains(self)
   }


    fn one_away_same_row_col(&self,  tail: Point) -> bool {
       self.col == tail.col && (self.row - 2 == tail.row || self.row + 2 == tail.row ) ||
       self.row == tail.row && (self.col - 2 == tail.col || self.col + 2 == tail.col )
    }

    fn get_direction_to_move(&self,  tail: Point) -> Option<char> {
       let up = Point::new(tail.col, tail.row + 2);
       let down = Point::new(tail.col, tail.row - 2);
       
       let left = Point::new(tail.col - 2, tail.row);
       let right = Point::new(tail.col + 2, tail.row);

       if &up == self {
           Some('U')
       } else if &down == self {
           Some('D')
       } else if &left == self {
           Some('L')
       } else if &right == self {
           Some('R')
       } else {
           None
       }
       
    }

    fn move_diagonal(&self,  tail: &Point) -> Point {
       let up_mid_left = Point::new(tail.col - 2, tail.row - 1);
       let up_left = Point::new(tail.col - 1, tail.row -2);
       let top_left = Point::new(tail.col -2, tail.row -2);
       let left_diagonal = Point::new(tail.col -1, tail.row -1);

       let up_mid_right = Point::new(tail.col + 2, tail.row - 1);
       let up_right = Point::new(tail.col + 1, tail.row - 2);
       let top_right = Point::new(tail.col + 2, tail.row - 2);
       let right_diagonal = Point::new(tail.col + 1, tail.row - 1);


       let lower_mid_left = Point::new(tail.col - 2, tail.row + 1);
       let lower_left = Point::new(tail.col - 1, tail.row + 2);
       let bottom_left = Point::new(tail.col - 2, tail.row + 2);
       let lower_left_diagonal = Point::new(tail.col - 1, tail.row + 1);

       let lower_mid_right = Point::new(tail.col + 2, tail.row + 1);
       let lower_right = Point::new(tail.col + 1, tail.row + 2);
       let bottom_right = Point::new(tail.col + 2, tail.row + 2);
       let lower_right_diagonal = Point::new(tail.col + 1, tail.row + 1);


           if &up_mid_left == self || &up_left == self || self == &top_left {
                left_diagonal
           } else if &up_mid_right == self || &up_right == self || &top_right == self {
                right_diagonal 
           } else if &lower_mid_left == self || &lower_left == self || &bottom_left == self {
               lower_left_diagonal
           } else if &lower_mid_right == self || &lower_right == self || &bottom_right == self {
               lower_right_diagonal
           } else {
               panic!("Unknown diagonal mapping for head {:?}, tail {:?}", self, tail); 
           }
           
    }

}

pub fn parse_input(input: &str) -> Vec<(&str, &str)> {
        input.lines()
        .into_iter()
        .map(|l| l.split(' ')
                   .into_iter()
                   .next_tuple()
                   .unwrap()).collect()
}

pub fn execute_puzzle(input: &str, len: usize, two: bool) -> Option<usize>{
    let head: Point = Point::new(0,0);

    let mut points: Vec<Point>  = vec![Point::new(0,0); len + 1];
    points[0] = head; // head at points[0]
    let mut moves: HashSet<(usize, Point)> = HashSet::new();

    for i in 1..points.len() {
        moves.insert((i, points[i]));
    }

    let commands = parse_input(&input);
     
    for c in commands {
       let m = c.1.parse::<usize>().unwrap();
       let direction = c.0.chars().nth(0).unwrap();
       //println!("== {} {} ==", direction, m);
       for j in 1..=m {
           //println!("move {}", j);
           points[0] = points[0].move_one(direction);
           //println!("H {:?}", points[0]);
           for i in 1..=len {
                 let _direction = points[i - 1].get_direction_to_move(points[i]); 
                 

                 if points[i - 1].one_away_same_row_col(points[i]) {
                     //println!("Direction is {} calculated direct is {:?}", direction, _direction);
                     points[i] = points[i].move_one(_direction.unwrap()); // move the global tail to the locally moved tail
                     moves.insert((i, points[i]));
                     //println!("T{} {:?} m1", i, points[i]);
                 } else if  points[i-1].not_touching_not_same_row_and_col(points[i]) {
                        points[i] = points[i-1].move_diagonal(&points[i]);
                        moves.insert((i, points[i]));
                        //println!("T{} {:?} md", i, points[i]);
                 } else {
         //           print!("T{} {:?}", i, points[i]);
          //          println!("Not Moving T{}", i);
                 }
           }
       }
    }
   Some(moves.into_iter().filter(|t| t.0 == len ).count())
}


pub fn part_one(input: &str) -> Option<usize> {
    execute_puzzle(input, 1, false)
}

pub fn part_two(input: &str) -> Option<usize> {
    execute_puzzle(input, 9, true)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 9);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    const TAIL: Point = Point { col: 1, row: 1};
    const UP_MID_LEFT: Point = Point { col: -1, row: 0};
    const UP_LEFT: Point = Point { col: 0, row: -1};
    const TOP_LEFT: Point = Point{col: -1, row: -1};
    const LEFT_DIAGONAL: Point = Point { col: 0, row: 0};

    const UP_MID_RIGHT: Point = Point { col: 3, row: 0}; 
    const UP_RIGHT: Point = Point { col: 2, row: -1};
    const TOP_RIGHT: Point = Point { col: 3,  row: -1};
    const RIGHT_DIAGONAL: Point = Point { col: 2, row: 0};

    const LOWER_MID_LEFT: Point = Point { col: -1, row: 2}; 
    const LOWER_LEFT: Point = Point{ col: 0, row: 3};
    const BOTTOM_LEFT: Point = Point { col: -1, row: 3};
    const LOWER_LEFT_DIAGONAL: Point = Point { col: 0, row: 2};

    const LOWER_MID_RIGHT: Point = Point { col: 3, row: 2}; 
    const LOWER_RIGHT: Point = Point { col: 2, row: 3};
    const BOTTOM_RIGHT: Point = Point { col: 3, row: 3};
    const LOWER_RIGHT_DIAGONAL: Point = Point { col: 2, row: 2};

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 9);
        assert_eq!(part_one(&input), Some(13));
    }



    #[test]
    fn test_not_touch_same_row_col() {
        let tail = Point::new(1, 1);
        assert!(tail.one_away_same_row_col(Point::new(1, -1))); //up
        assert!(tail.one_away_same_row_col(Point::new(1, 3))); //down
        
        assert!(tail.one_away_same_row_col(Point::new(-1, 1))); //left
        assert!(tail.one_away_same_row_col(Point::new(3, 1))); //right
    }

    #[test]
    fn test_move_diagonal_cord() {

       let lefts = [UP_MID_LEFT, UP_LEFT, TOP_LEFT];
       test_quadrants(&lefts, &TAIL, &LEFT_DIAGONAL);

       let rights = [UP_MID_RIGHT, UP_RIGHT,TOP_RIGHT];

       test_quadrants(&rights, &TAIL, &RIGHT_DIAGONAL);
        
       let lower_lefts = [LOWER_MID_LEFT, LOWER_LEFT, BOTTOM_LEFT];
        
       test_quadrants(&lower_lefts, &TAIL, &LOWER_LEFT_DIAGONAL);

       let lower_rights = [LOWER_MID_RIGHT, LOWER_RIGHT, BOTTOM_RIGHT];

       test_quadrants(&lower_rights, &TAIL, &LOWER_RIGHT_DIAGONAL);

       fn test_quadrants(q: &[Point; 3], tail: &Point, diagonal: &Point) {
           for c in q {
               assert_eq!(&c.move_diagonal(tail), diagonal);
           }
       }
    }

    #[test]
    fn test_get_direction() {
        let head = Point::new(1, 1);
        assert_eq!(head.get_direction_to_move(Point::new(1, -1)).unwrap(), 'U');
        assert_eq!(head.get_direction_to_move(Point::new(1, 3)).unwrap(), 'D');
        
        assert_eq!(head.get_direction_to_move(Point::new(3, 1)).unwrap(), 'L');
        assert_eq!(head.get_direction_to_move(Point::new(-1, 1)).unwrap(), 'R');
    }
    #[test]
    fn test_not_touch_not_same_row_col_cord() {
        let all = [UP_MID_LEFT, UP_LEFT, TOP_LEFT,
                   UP_MID_RIGHT, UP_RIGHT, TOP_RIGHT,
                   LOWER_MID_LEFT, LOWER_LEFT, BOTTOM_LEFT,
                   LOWER_MID_RIGHT, LOWER_RIGHT, BOTTOM_RIGHT];

        for c in all {
            assert!(c.not_touching_not_same_row_and_col(TAIL));
        }
       
        assert!(!Point::new(1, -1).not_touching_not_same_row_and_col(TAIL));

    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 9);
        assert_eq!(part_two(&input), Some(1));
    }
    #[test]
    fn test_part_two_two() {
        let input = advent_of_code::read_file("examples", 91);
        assert_eq!(part_two(&input), Some(36));
    }
}
