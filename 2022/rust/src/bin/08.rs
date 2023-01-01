mod arbor {
#[derive(PartialEq)]
#[derive(Debug)]
pub struct Forest {
    trees: Vec<Vec<Tree>>
}

impl Forest {

    pub fn new(trees: Vec<Vec<Tree>>) -> Forest {
        Forest { trees }
    }
    
    pub fn tree_at(&self, row: usize, col: usize) -> Option<&Tree> {
       self.trees.get(row).map(|r| r.get(col)).flatten()
    }

    pub fn is_border_tree(&self, row: usize, col: usize) -> bool {
        row == 0 || col == 0 || row == self.trees.len() - 1 || col == self.trees.get(row).map(|r| r.len() - 1).unwrap_or_else(|| 0)
    }

    pub fn visible_direction(&self, row: usize, col: usize) -> Option<char> {
        let d: usize = self.trees.len() - 1; 
        let r: usize = self.trees[d].len() - 1;

        if row == 0 {
            Some('u')
        } else if col == 0 {
            Some('l')
        } else if row == d {
            Some('d') 
        } else if col == r {
            Some('r')
        } else { // not yet processed
            None 
        }
    }

    pub fn set_borders_as_visible(&mut self) {
        for i in 0..self.trees.len() {
           for j in 0..self.trees[i].len() {
                self.trees[i][j].is_visible = self.visible_direction(i, j);
            }
        }
    }

   pub fn ask_neighbour(&mut self, height: usize, direction: char, row: usize, col: usize) -> Option<char> {
        let col_ = col;
        let row_ = row;

        match  direction {
            'r' => {
                    if !self.is_border_tree(row, col + 1) {
                         if self.trees[row][col + 1].height >= height {
                            None 
                         } else if self.trees[row][col + 1].height < height && self.trees[row][col + 1].is_visible == Some(direction) {
                            Some(direction)
                        } else {
                            self.ask_neighbour(height, direction, row, col + 1)
                        }
                    } else { // at the border
                        if self.trees[row][col + 1].height < height {
                            Some(direction)
                        } else {
                            None
                        }
                    }

            },

            'l' => {
                    if !self.is_border_tree(row, col - 1) {
                         if self.trees[row][col - 1].height >= height {
                            None 
                         } else if self.trees[row][col - 1].height < height && self.trees[row][col - 1].is_visible == Some(direction) {
                            Some(direction)
                        } else {
                            self.ask_neighbour(height, direction, row, col - 1)
                        }
                    } else { // at the border
                        if self.trees[row][col - 1].height < height {
                            Some(direction)
                        } else {
                            None
                        }
                    }

            },

             'u' => {
                    if !self.is_border_tree(row - 1, col) {
                         if self.trees[row - 1][col].height >= height {
                            None 
                         } else if self.trees[row - 1][col].height < height && self.trees[row - 1][col].is_visible == Some(direction) {
                            Some(direction)
                        } else {
                            self.ask_neighbour(height, direction, row - 1, col)
                        }
                    } else { // at the border
                        if self.trees[row - 1][col].height < height {
                            Some(direction)
                        } else {
                            None
                        }
                    }

            },

             'd' => {
                    if !self.is_border_tree(row + 1, col) {
                         if self.trees[row + 1][col].height >= height {
                            None 
                         } else if self.trees[row + 1][col].height < height && self.trees[row + 1][col].is_visible == Some(direction) {
                            Some(direction)
                        } else {
                            self.ask_neighbour(height, direction, row + 1, col)
                        }
                    } else { // at the border
                        if self.trees[row + 1][col].height < height {
                            Some(direction)
                        } else {
                            None
                        }
                    }

            },
            s => panic!("Unknow processing direction {}", s)
            
        }

   }

   pub fn set_visibilities(&mut self) {
       let directions = vec!['r', 'l', 'u', 'd'];

        for i in 1..self.trees.len() - 1 {
           for j in 1..self.trees[i].len() - 1 {
                for direction in &directions {
                    if self.trees[i][j].is_visible == None {
                        self.trees[i][j].is_visible = self.ask_neighbour(self.trees[i][j].height, *direction, i, j);
                    }
                }
            }
        }

   }
        
}

#[derive(PartialEq)]
#[derive(Debug)]
pub struct Tree {
    pub height: usize,
    pub is_visible: Option<char>
}

impl Tree {
    pub fn new(h: usize) -> Tree {
       Tree { height: h, is_visible: None } 
    }
}

}

use arbor::*;

fn parse_input(input: &str) -> Vec<Vec<usize>> {
    input
        .lines()
        .into_iter()
        .map(|s| s.chars()
             .flat_map(|c| c.to_digit(10))
             .map(|n| n as usize).collect())
        .collect()
}

fn make_trees(raw_heights: Vec<Vec<usize>>) -> Forest {
    Forest::new(
       raw_heights.iter()
           .map(|hs| hs.iter()
                       .map(|h| Tree::new(*h))
                       .collect())
           .collect()
     )
}

pub fn part_one(input: &str) -> Option<u32> {
    None
}

pub fn part_two(input: &str) -> Option<u32> {
    None
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 8);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_border_tree() {
        let input = advent_of_code::read_file("examples", 8);
        let forest = make_trees(parse_input(&input));
        
        assert_eq!(forest.is_border_tree(0,0), true);
        assert_eq!(forest.is_border_tree(0,1), true);
        assert_eq!(forest.is_border_tree(0,2), true);
        assert_eq!(forest.is_border_tree(0,3), true);
        assert_eq!(forest.is_border_tree(0,4), true);
        assert_eq!(forest.is_border_tree(1,4), true);
        assert_eq!(forest.is_border_tree(2,4), true);
        assert_eq!(forest.is_border_tree(3,4), true);
        assert_eq!(forest.is_border_tree(1,0), true);
        assert_eq!(forest.is_border_tree(2,0), true);
        assert_eq!(forest.is_border_tree(3,0), true);
        assert_eq!(forest.is_border_tree(4,0), true);
        assert_eq!(forest.is_border_tree(4,1), true);
        assert_eq!(forest.is_border_tree(4,2), true);
        assert_eq!(forest.is_border_tree(4,3), true);
        assert_eq!(forest.is_border_tree(4,4), true);
       

        assert_eq!(forest.is_border_tree(1,1), false);
        assert_eq!(forest.is_border_tree(1,2), false);
        assert_eq!(forest.is_border_tree(1,3), false);


        assert_eq!(forest.is_border_tree(2,1), false);
        assert_eq!(forest.is_border_tree(2,2), false);
        assert_eq!(forest.is_border_tree(2,3), false);


        assert_eq!(forest.is_border_tree(3,1), false);
        assert_eq!(forest.is_border_tree(3,2), false);
        assert_eq!(forest.is_border_tree(3,3), false);
    }


    #[test]
    fn test_is_visible_direction() {
        let input = advent_of_code::read_file("examples", 8);
        let forest = make_trees(parse_input(&input));
        let up =  Some('u');
        let down = Some('d');
        let left = Some('l');
        let right = Some('r');

        assert_eq!(forest.visible_direction(0,0), up);
        assert_eq!(forest.visible_direction(0,1), up);
        assert_eq!(forest.visible_direction(0,2), up);
        assert_eq!(forest.visible_direction(0,3), up);
        assert_eq!(forest.visible_direction(0,4), up);
        assert_eq!(forest.visible_direction(1,4), right);
        assert_eq!(forest.visible_direction(2,4), right);
        assert_eq!(forest.visible_direction(3,4), right);
        assert_eq!(forest.visible_direction(1,0), left);
        assert_eq!(forest.visible_direction(2,0), left);
        assert_eq!(forest.visible_direction(3,0), left);
        assert_eq!(forest.visible_direction(4,0), left);
        assert_eq!(forest.visible_direction(4,1), down);
        assert_eq!(forest.visible_direction(4,2), down);
        assert_eq!(forest.visible_direction(4,3), down);
        assert_eq!(forest.visible_direction(4,4), down);
       

        assert_eq!(forest.visible_direction(1,1), None);
        assert_eq!(forest.visible_direction(1,2), None);
        assert_eq!(forest.visible_direction(1,3), None);


        assert_eq!(forest.visible_direction(2,1), None);
        assert_eq!(forest.visible_direction(2,2), None);
        assert_eq!(forest.visible_direction(2,3), None);


        assert_eq!(forest.visible_direction(3,1), None);
        assert_eq!(forest.visible_direction(3,2), None);
        assert_eq!(forest.visible_direction(3,3), None);
    }

    #[test]
    fn test_make_trees() {
        let input = advent_of_code::read_file("examples", 8);
        let forest = make_trees(parse_input(&input));
        
        assert_eq!(forest.tree_at(0,0).map(|t| t.height), Some(3));
        assert_eq!(forest.tree_at(0,0).map(|t| t.is_visible).flatten(), None);
        assert_eq!(forest.tree_at(4,4).map(|t| t.height), Some(0));
        assert_eq!(forest.tree_at(4,4).map(|t| t.is_visible).flatten(), None);
    }

    #[test]
    fn test_set_borders_as_visible() {
        let input = advent_of_code::read_file("examples", 8);
        let mut forest = make_trees(parse_input(&input));
        forest.set_borders_as_visible();
        
        assert_eq!(forest.tree_at(0,0).map(|t| t.height), Some(3));
        assert_eq!(forest.tree_at(0,0).map(|t| t.is_visible).flatten(), Some('u'));
        assert_eq!(forest.tree_at(4,4).map(|t| t.height), Some(0));
        assert_eq!(forest.tree_at(4,4).map(|t| t.is_visible).flatten(), Some('d'));
    }


    #[test]
    fn test_set_visibilities() {
        let input = advent_of_code::read_file("examples", 8);
        let mut forest = make_trees(parse_input(&input));
        forest.set_borders_as_visible();
        forest.set_visibilities();
        
        assert_eq!(forest.tree_at(1,1).map(|t| t.is_visible).flatten(), Some('l'));
        assert_eq!(forest.tree_at(1,2).map(|t| t.is_visible).flatten(), Some('r'));
        assert_eq!(forest.tree_at(1,3).map(|t| t.is_visible).flatten(), None);
        
        assert_eq!(forest.tree_at(2,1).map(|t| t.is_visible).flatten(), Some('r'));
        assert_eq!(forest.tree_at(2,2).map(|t| t.is_visible).flatten(), None);
        assert_eq!(forest.tree_at(2,3).map(|t| t.is_visible).flatten(), Some('r'));

        assert_eq!(forest.tree_at(3,1).map(|t| t.is_visible).flatten(), None);
        assert_eq!(forest.tree_at(3,2).map(|t| t.is_visible).flatten(), Some('l'));
        assert_eq!(forest.tree_at(3,3).map(|t| t.is_visible).flatten(), None);
    }

    #[test]
    fn test_new_tree() {
        assert_eq!(Tree::new(1), Tree { height: 1, is_visible: None });
    }

    #[test]
    fn test_parse_input() {
        let input = advent_of_code::read_file("examples", 8);
        let v2d = vec![vec![3,0,3,7,3], vec![2,5,5,1,2], vec![6,5,3,3,2], vec![3,3,5,4,9], vec![3,5,3,9,0]];
        assert_eq!(parse_input(&input), v2d);
    }
    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 8);
        assert_eq!(part_one(&input), None);
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 8);
        assert_eq!(part_two(&input), None);
    }
}
