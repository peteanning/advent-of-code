use std::collections::HashMap;


fn init() -> HashMap<char, u8> {

 let lowers: HashMap<char, u8>  = (b'a'..=b'z').map(char::from).zip((1..=26).map(u8::from)).collect::<HashMap<_,_>>();
 let uppers: HashMap<char, u8> = (b'A'..=b'Z').map(char::from).zip((27..=52).map(u8::from)).collect::<HashMap<_,_>>();

  lowers.into_iter().chain(uppers).collect()


}
pub fn part_one(input: &str) -> Option<u32> {
    use itertools::Itertools;

    let scores = init();

    let chars: Vec<Vec<char>> = input
        .lines()
        .into_iter()
        .map(|s| s.chars().collect::<Vec<char>>()).collect();
        //dbg!(&chars);
//    let halves: Vec<Vec<&[char]>> = 
//        chars.iter().map(|v| v.chunks(2).collect()).collect();
      let halves: Vec<Vec<Vec<char>>> = chars.into_iter().map(|l| l.chunks(l.len()/2).map(|x| x.to_vec()).collect()).collect();
      dbg!(&halves);
      let mut filtered: Vec<Vec<char>> = halves
          .into_iter()
          .map(|line: Vec<Vec<char>>| {
               line[0]
                   .clone()
                   .into_iter()
                   .filter(|c| line[1].contains(c)) 
                   .unique()
                   .collect()

          }
              )
          .collect(); 
       // only need oneinstance of the duplicate values filtering gives all of them!
       dbg!(&filtered);

       let numbers: u32 = Iterator::flatten( filtered
           .iter_mut()
           .map(|v| v.iter_mut()
                .map(|c| *scores.get(c).unwrap() as u32).collect::<Vec<u32>>())).fold(0, |acc, n| acc + n);

    Some(numbers)

}

pub fn part_two(_input: &str) -> Option<u32> {
    None
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
        assert_eq!(part_two(&input), None);
    }
}
