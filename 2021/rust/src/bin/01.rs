use std::convert::TryFrom;

pub fn part_one(input: &str) -> Option<usize> {

    let result =
            input
            .lines()
            .map(|n| n.parse().unwrap())
            .collect::<Vec<u32>>()
            .windows(2)
            .flat_map(<&[u32; 2]>::try_from)
            .filter(|&&[a,b]| a < b)
            .count();
    
    Some(result)

}

pub fn part_two(input: &str) -> Option<u32> {
    None
}


fn main() {
    let input = &aoc::read_file("inputs", 1);
    aoc::solve!(1, part_one, input);
    aoc::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = aoc::read_file("examples", 1);
        assert_eq!(part_one(&input), Some(7));
    }

    #[test]
    fn test_part_two() {
        let input = aoc::read_file("examples", 1);
        assert_eq!(part_two(&input), None);
    }
}
