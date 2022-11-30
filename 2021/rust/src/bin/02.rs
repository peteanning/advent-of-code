
enum Cmd {
  UP, DOWN, FORWARD
}

struct Command {
  cmd: Cmd,
  value: u32
}

impl Command {
    fn new(cmd: &str, n: &str) -> Command { //todo make this a Result<Command, Err>
         match cmd {
             "up" => Command { cmd: Cmd::UP, value: n.parse().unwrap()  },
             "down" => Command { cmd: Cmd::DOWN, value: n.parse().unwrap()  },
             "forward" => Command { cmd: Cmd::FORWARD, value: n.parse().unwrap()  },
             _ => Command { cmd: Cmd::UP, value: 0 } // this is a cludge should be an error
        }
    }
    
}

pub fn part_one(input: &str) -> Option<u32> {


    let result = input.lines()
                      .map(|s| s.split(' ').collect::<Vec<&str>>())
                      .map(|v| Command::new(v[0], v[1]))
                      .fold((0,0), |acc, cmd| {
                        match cmd {
                            Command {cmd: Cmd::UP, value: n} => (acc.0 - n, acc.1),
                            Command {cmd: Cmd::DOWN, value: n} => (acc.0 + n, acc.1),
                            Command {cmd: Cmd::FORWARD, value: n} => (acc.0, acc.1 + n)
                        }
                      });

    Some(result.0 * result.1)

  
}

pub fn part_two(input: &str) -> Option<u32> {
        // (Depth, Horizontal_Pos, Aim)
        let result = input.lines()
                      .map(|s| s.split(' ').collect::<Vec<&str>>())
                      .map(|v| Command::new(v[0], v[1]))
                      .fold((0,0,0), |acc, cmd| {
                        match cmd {
                            Command {cmd: Cmd::UP, value: n} => (acc.0, acc.1, acc.2 - n),
                            Command {cmd: Cmd::DOWN, value: n} => (acc.0, acc.1, acc.2 + n),
                            Command {cmd: Cmd::FORWARD, value: n} => (acc.0 + (acc.2 * n), acc.1 + n, acc.2)
                        }
                      });

    Some(result.0 * result.1)

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
        assert_eq!(part_two(&input), Some(900));
    }
}
