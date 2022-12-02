pub fn part_one(input: &str) -> Option<u32> {
    let result: Vec<Option<u32>> = input.lines().map(|s| { 
        if s != "" {
            Some(s.parse().unwrap())
        } else {
            None
        }
    }).collect();

    let mut total: u32 = 0;
    let mut calories: Vec<u32> = vec![];

    for c in result {
        match c {
            Some(n) => total += n,
            None => { calories.push(total); total = 0; }
        }
    }

    if total > 0 {
        calories.push(total);
    }

    let max = calories.into_iter().fold((0), |acc, n| if n >= acc { n } else { acc});
    Some(max)
}

pub fn part_two(input: &str) -> Option<u32> {
    let result: Vec<Option<u32>> = input.lines().map(|s| { 
        if s != "" {
            Some(s.parse().unwrap())
        } else {
            None
        }
    }).collect();

    let mut total: u32 = 0;
    let mut calories: Vec<u32> = vec![];

    for c in result {
        match c {
            Some(n) => total += n,
            None => { calories.push(total); total = 0; }
        }
    }

    if total > 0 {
        calories.push(total);
    }

    calories.sort_by(|a, b| b.cmp(a));
    let t = calories[0] + calories[1] + calories[2];
    Some(t)
}


fn main() {
    let input = &advent_of_code::read_file("inputs", 1);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 1);
        assert_eq!(part_one(&input), Some(24000));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 1);
        assert_eq!(part_two(&input), Some(45000));
    }
}
