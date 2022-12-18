

fn solve(input: &str, window: usize) -> Option<usize> {
    let mut pointer: usize = 0;
    let mut accumulator: Vec<char> = vec![];
    let mut duplicates: usize = 0;

    let parsed_input: Vec<char> = 
        input
        .chars()
        .into_iter()
        .collect();



   while pointer < parsed_input.len() {
    
     //println!("Pointer {} accumulator {:?} curent value {}", pointer, accumulator, &parsed_input[pointer]);
    
     if accumulator.len() < window {
        if !accumulator.contains(&parsed_input[pointer]) {
            accumulator.push(parsed_input[pointer]);
            pointer += 1;
        } else {
       //   println!("found a duplicate");
          duplicates += 1;
          accumulator.clear();
          pointer = duplicates;
          continue;
        }
     }
     
     if accumulator.len() == window {
        //println!("Result was {}", pointer);
        //println!("******************");
        break;
     }  
     
    }
    Some(pointer)


}



pub fn part_one(input: &str) -> Option<usize> {
     solve(&input, 4)
}

pub fn part_two(input: &str) -> Option<usize> {
   solve(&input, 14)
}


fn main() {
    let input = &advent_of_code::read_file("inputs", 6);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn test_part_one() {
        let test_data: Vec<(&str, usize)> = vec![("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7), 
                                                 ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5),
                                                 ("nppdvjthqldpwncqszvftbrmjlhg", 6),
                                                 ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10),
                                                 ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",11)];
        for test_case in test_data {
            assert_eq!(part_one(&test_case.0), Some(test_case.1));
        }
    }

    #[test]
    fn test_part_two() {
        let test_data: Vec<(&str, usize)> = vec![("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19), 
                                                 ("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
                                                 ("nppdvjthqldpwncqszvftbrmjlhg", 23),
                                                 ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
                                                 ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",26)];
        for test_case in test_data {
            assert_eq!(part_two(&test_case.0), Some(test_case.1));
        }
    }


}
