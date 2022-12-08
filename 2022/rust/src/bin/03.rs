use std::collections::HashMap;

use itertools::Itertools;


fn build_score_maps() -> HashMap<char, u8> {

 let lowers: HashMap<char, u8>  = (b'a'..=b'z').map(char::from).zip((1..=26).map(u8::from)).collect::<HashMap<_,_>>();
 let uppers: HashMap<char, u8> = (b'A'..=b'Z').map(char::from).zip((27..=52).map(u8::from)).collect::<HashMap<_,_>>();

 lowers.into_iter().chain(uppers).collect()
}

pub fn part_one(input: &str) -> Option<u32> {

    let scores: HashMap<char, u8> = build_score_maps();

    let chars: Vec<Vec<char>> = input
        .lines()
        .into_iter()
        .map(|s| s.chars().collect::<Vec<char>>()).collect();

    let halves: Vec<Vec<Vec<char>>> = chars.into_iter().map(|l| l.chunks(l.len()/2).map(|x| x.to_vec()).collect()).collect();
    let mut filtered: Vec<Vec<char>> = halves
          .into_iter()
          .map(|line: Vec<Vec<char>>| {
               line[0]
                   .clone()
                   .into_iter()
                   .filter(|c| line[1].contains(c)) 
                   .unique() // only need oneinstance of the duplicate values filtering gives all of them!
                   .collect()

          }
              )
          .collect(); 

    let total: u32 = Iterator::flatten( filtered
           .iter_mut()
           .map(|v| v.iter_mut()
                .map(|c| *scores.get(c).unwrap() as u32)
                .collect::<Vec<u32>>()))
           .fold(0, |acc, n| acc + n); // using fold as sum

    Some(total)

}

pub fn part_two(input: &str) -> Option<u32> {
    let scores: HashMap<char, u8> = build_score_maps();

    let chars: Vec<Vec<char>> = input
        .lines()
        .into_iter()
        .map(|s| s.chars().collect::<Vec<char>>()).collect();

    let groups_of_three: Vec<Vec<Vec<char>>> = chars.chunks(3).map(|x| x.to_vec()).collect();


    let  mut filtered: Vec<Vec<char>> = groups_of_three.into_iter()
                                       .map(|three| three[0]
                                                    .clone()
                                                    .into_iter()
                                                    .filter(|c| three[1].contains(c) && three[2].contains(c))
                                                    .unique()
                                                    .collect::<Vec<char>>())
                                       .collect();


    let total: u32 = Iterator::flatten( filtered
           .iter_mut()
           .map(|v| v.iter_mut()
                .map(|c| *scores.get(c).unwrap() as u32)
                .collect::<Vec<u32>>()))
           .fold(0, |acc, n| acc + n); // using fold as sum



    Some(total)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 3);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 3);
        assert_eq!(part_one(&input), Some(157));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 3);
        assert_eq!(part_two(&input), Some(70));
    }
}
