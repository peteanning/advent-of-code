use anyhow::Result;

pub fn part_one_wrapper(input: &str) -> Option<i32> {
    part_one(input).unwrap_or(None)
}
pub fn part_one(input: &str)  -> Result<Option<i32>>{
    
    let mut x_register = 1;
    let mut signal_strength = 0;
    let mut cycles = 0;
    let mut pointer = 20;
    for line in input.lines() {
        let (cycle, inc) = match line.trim().split_once(' ').unwrap_or((line, "")) {
            ("noop", "") => (1, 0),
            ("addx", n) => (2, n.parse()?),
            _ => anyhow::bail!("unknown instruction"),
        };
        //println!("{}", line);
        //println!("(cycle, inc) =  ({},{})", cycle, inc );

        for _ in 0..cycle {
            cycles += 1;
          //  println!("cycles {}", cycles);

            if cycles % pointer == 0 {
            //    println!("Signal Strength is {} * {} = {}", cycles, x_register, cycles * x_register);
                signal_strength += cycles * x_register;
                pointer += 40;
              //  println!("Pointer {} signal_strength {}", pointer, signal_strength);
            }
        }
        //println!("line {} x_register {} increment {}", line, x_register, inc);
        x_register += inc;
    }
    Ok(Some(signal_strength))

}

pub fn part_two(input: &str) -> Option<u32> {
    None
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 10);
    advent_of_code::solve!(1, part_one_wrapper, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load() {
        let input = advent_of_code::read_file("examples", 10);

    }

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 10);
        assert_eq!(part_one(&input).unwrap(), Some(13140));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 10);
        assert_eq!(part_two(&input), None);
    }
}
