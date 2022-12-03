
enum Choice {
    Rock, Paper, Scissors
}


struct Round {
    their_choice: Choice,
    your_choice: Choice
}


const ROCK: u32 = 1;
const PAPER: u32 = 2;
const SCISSORS: u32 = 3;
const WIN: u32 = 6;
const LOOSE: u32 = 0;
const DRAW: u32 = 3;
    

impl Round {
    
    pub fn new(their_choice: &str, your_choice: &str) -> Result<Round, String> {
        match (Round::str_to_choice(their_choice), Round::str_to_choice(your_choice))  {
            (Ok(theirs), Ok(yours)) => Ok(Round { their_choice: theirs, your_choice: yours }),
            _ => Err(format!("Unkown choice their's {}, yours {}", their_choice, your_choice))
        }
    }

    fn str_to_choice(choice: &str) -> Result<Choice, String> {
        match choice {
            "A" => Ok(Choice::Rock),
            "B" => Ok(Choice::Paper),
            "C" => Ok(Choice::Scissors),
            "X" => Ok(Choice::Rock),
            "Y" => Ok(Choice::Paper),
            "Z" => Ok(Choice::Scissors),
                _ => Err("Unknown Choice".to_string())
        }
    }

    
    fn score(round: Round) -> u32 {
        match round {
            Round { their_choice: Choice::Rock, your_choice: Choice::Rock } => DRAW + ROCK,
            Round { their_choice: Choice::Rock, your_choice: Choice::Paper } => WIN + PAPER,
            Round { their_choice: Choice::Rock, your_choice: Choice::Scissors } => LOOSE + SCISSORS,
           
            Round { their_choice: Choice::Paper, your_choice: Choice::Rock } => LOOSE + ROCK,
            Round { their_choice: Choice::Paper, your_choice: Choice::Paper } => DRAW + PAPER,
            Round { their_choice: Choice::Paper, your_choice: Choice::Scissors } => WIN + SCISSORS,

            Round { their_choice: Choice::Scissors, your_choice: Choice::Rock } => WIN + ROCK,
            Round { their_choice: Choice::Scissors, your_choice: Choice::Paper } => LOOSE + PAPER,
            Round { their_choice: Choice::Scissors, your_choice: Choice::Scissors } => DRAW + SCISSORS


        }
    }
    
}


pub fn part_one(input: &str) -> Option<u32> {
    let result: Vec<Vec<&str>>  = 
        input
        .lines()
        .into_iter()
        .map(|l| l.split(' ').collect::<Vec<&str>>())
        .collect();

    let rounds =
        result.into_iter()
        .map(|v| {
            match Round::new(v[0],v[1]) {
                Ok(round) => round,
                _ => panic!("unable to parse input")
            }}).collect::<Vec<Round>>();

    let scores: Vec<u32> = 
        rounds.into_iter()
        .map(|r| Round::score(r)).collect();

    Some(scores.into_iter().sum())

}

pub fn part_two(input: &str) -> Option<u32> {
    None
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 2);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 2);
        assert_eq!(part_one(&input), Some(15));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 2);
        assert_eq!(part_two(&input), None);
    }
}
