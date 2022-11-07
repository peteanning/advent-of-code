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

pub fn part_two(input: &str) -> Option<usize> {
    let count =
            input
            .lines()
            .map(|n| n.parse().unwrap())
            .collect::<Vec<u32>>()
            .windows(3)
            .flat_map(<&[u32; 3]>::try_from)
            .map(|s| s.into_iter().sum())
            .collect::<Vec<u32>>()
            .windows(2)
            .flat_map(<&[u32; 2]>::try_from)
            .filter(|&&[a,b]| a < b)
            .count();
    
    Some(count)
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
        let mut input = aoc::read_file("examples", 1);
        assert_eq!(part_one(&input), Some(7));

        input = aoc::read_file("inputs", 1);
        assert_eq!(part_one(&input), Some(1228))
    }

    #[test]
    fn test_part_two() {
        let mut input = aoc::read_file("examples", 1);
        assert_eq!(part_two(&input), Some(5));

        input = aoc::read_file("inputs", 1);
        assert_eq!(part_two(&input), Some(1257))

    }
}
