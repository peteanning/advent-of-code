
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
    
    pub fn new(their_choice: &str, your_choice: &str, eval: fn(&str, Option<&str>) -> Result<Choice, String>) -> Result<Round, String> {
        match (Round::str_to_choice(their_choice, None), eval(their_choice, Some(your_choice)))  {
            (Ok(theirs), Ok(yours)) => Ok(Round { their_choice: theirs, your_choice: yours }),
            _ => Err(format!("Unkown choice their's {}, yours {}", their_choice, your_choice))
        }
    }


    fn str_to_choice(choice: &str, _your_choice: Option<&str>) -> Result<Choice, String> {
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

    fn str_to_choice_with_outcome(choice: &str, your_choice: Option<&str>) -> Result<Choice, String> {
        match (choice, your_choice) {
            ("A", Some("X")) => Ok(Choice::Scissors), // loose against Rock
            ("A", Some("Y")) => Ok(Choice::Rock), // draw
            ("A", Some("Z")) => Ok(Choice::Paper), // Win

            ("B", Some("X")) => Ok(Choice::Rock), //loose against Paper
            ("B", Some("Y")) => Ok(Choice::Paper), //draw
            ("B", Some("Z")) => Ok(Choice::Scissors), //win


           ("C", Some("X")) => Ok(Choice::Paper), //loose aainst Scissors
           ("C", Some("Y")) => Ok(Choice::Scissors), //draw
           ("C", Some("Z")) => Ok(Choice::Rock), // win
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

fn evalualte_rounds(input: Vec<Vec<&str>>, eval: fn(&str, Option<&str>) -> Result<Choice, String>) -> Vec<Round> {
    input.into_iter()
        .map(|v| {
            match Round::new(v[0], v[1], eval) {
                Ok(round) => round,
                _ => panic!("unable to parse input")
            }}).collect::<Vec<Round>>()
}

fn parse(input: &str) -> Vec<Vec<&str>> {
        input
        .lines()
        .into_iter()
        .map(|l| l.split(' ').collect::<Vec<&str>>())
        .collect()
}

fn scores(rounds: Vec<Round>) -> Vec<u32> {
  rounds.into_iter()
        .map(|r| Round::score(r)).collect()
}

pub fn part_one(input: &str) -> Option<u32> {
    let parsed: Vec<Vec<&str>>  = parse(input);

    let rounds: Vec<Round> = evalualte_rounds(parsed, Round::str_to_choice);

    let scores: Vec<u32> = scores(rounds);

    Some(scores.into_iter().sum())

}

pub fn part_two(input: &str) -> Option<u32> {
    let parsed: Vec<Vec<&str>>  = parse(input);

    let rounds: Vec<Round> = evalualte_rounds(parsed, Round::str_to_choice_with_outcome);

    let scores: Vec<u32> = scores(rounds);

    Some(scores.into_iter().sum())

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
        assert_eq!(part_two(&input), Some(12));
    }
}
