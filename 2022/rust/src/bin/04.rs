use itertools::Itertools;

#[derive(Debug)]
struct ElfRange {
    start: u32,
    end: u32
}


impl ElfRange {
    fn new(str_range: &str) -> ElfRange {
        let  (s, e) = str_range.split("-").next_tuple().unwrap(); 
        ElfRange { start: s.parse().unwrap(), end: e.parse().unwrap() }    
    }

    fn contains(this: &ElfRange, that: &ElfRange) -> bool {
        this.start <= that.start && this.end >= that.end || 
        this.start >= that.start && this.end <= that.end 
    }

    fn overlaps(this: &ElfRange, that: &ElfRange) -> bool {
        this.end >= that.start && this.start <= that.end 
            //|| ElfRange::contains(this, that)
    }

}

fn parse_lines(input: &str) -> Vec<(ElfRange, ElfRange)>{
    let parsed = input
        .lines()
        .into_iter()
        .map(|l| l.split(",").collect::<Vec<&str>>())
        .collect::<Vec<Vec<&str>>>();

     parsed.into_iter()
        .map(|pair| (ElfRange::new(pair[0]), ElfRange::new(pair[1]))).collect::<Vec<(ElfRange, ElfRange)>>() 
    
}

pub fn part_one(input: &str) -> Option<u32> {
        let count: u32 = parse_lines(input)
            .iter()
            .map(|e| ElfRange::contains(&e.0, &e.1))
                    .fold(0, |acc, b| if b { acc + 1 } else {acc});

    Some(count)

}

pub fn part_two(input: &str) -> Option<u32> {
         let count: u32 = parse_lines(input)
            .iter()
            .map(|e| ElfRange::overlaps(&e.0, &e.1))
                    .fold(0, |acc, b| if b { acc + 1 } else {acc});

    Some(count)

}

fn main() {
    let input = &advent_of_code::read_file("inputs", 4);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 4);
        assert_eq!(part_one(&input), Some(2));
    }

    #[test]
    fn test_contains() {
        let range_1 = ElfRange::new("1-4");
        let range_2 = ElfRange::new("2-3");
        assert!(ElfRange::contains(&range_1, &range_2));
        assert!(ElfRange::contains(&range_2, &range_1));

        let range_1 = ElfRange::new("4-6");
        let range_2 = ElfRange::new("6-6");
        assert!(ElfRange::contains(&range_1, &range_2));
        assert!(ElfRange::contains(&range_2, &range_1));

        let range_1 = ElfRange::new("2-4");
        let range_2 = ElfRange::new("2-3");

        assert!(ElfRange::contains(&range_1, &range_2));
        assert!(ElfRange::contains(&range_2, &range_1));

        let range_1 = ElfRange::new("6-10");
        let range_2 = ElfRange::new("2-3");

        assert!(!ElfRange::contains(&range_1, &range_2));
        assert!(!ElfRange::contains(&range_2, &range_1));

    }

    #[test]
    fn test_overlap_end_start() {
        let range_1 = ElfRange::new("5-7");
        let range_2 = ElfRange::new("7-9");

        assert!(ElfRange::overlaps(&range_1, &range_2));
        assert!(ElfRange::overlaps(&range_2, &range_1));
    }

    #[test]
    fn test_overlap_and_contains() {
        let range_1 = ElfRange::new("1-4");
        let range_2 = ElfRange::new("2-3");

        assert!(ElfRange::overlaps(&range_1, &range_2));
    }
    
    #[test]
    fn test_no_overlap() {
        let range_1 = ElfRange::new("1-4");
        let range_2 = ElfRange::new("5-6");

        assert!(!ElfRange::overlaps(&range_1, &range_2));
    }





    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 4);
        assert_eq!(part_two(&input), Some(4));
    }
}
