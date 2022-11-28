#![allow(unused)]

enum cmd {
  UP, DOWN, LEFT, RIGHT
}

struct command {
  cmd: cmd,
  value: u32
}


pub fn part_one(input: &str) -> Option<u32> {
    let cmdStr = input.lines().map(|s| 
                      let v: Vec<String> = s.split(" ").collect();
                      (v(0), v(1));

  
}

pub fn part_two(input: &str) -> Option<u32> {
    None
}

fn main() {
    let input = &aoc::read_file("inputs", 2);
    aoc::solve!(1, part_one, input);
    aoc::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = aoc::read_file("examples", 2);
        assert_eq!(part_one(&input), Some(150));
    }

    #[test]
    fn test_part_two() {
        let input = aoc::read_file("examples", 2);
        assert_eq!(part_two(&input), None);
    }
}
