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
        let mut col_ = col;
        let mut row_ = row;

        match  direction { 
            'r' => col_ = col + 1, 
            'l' => col_ = col - 1, 
            'u' => row_ = row - 1, 
            'd' => row_ = row + 1, 
            unknown => panic!("Unknow processing direction {}", unknown)
        }
        if !self.is_border_tree(row_, col_) {
           if self.trees[row_][col_].height >= height {
              None 
           } else {
              self.ask_neighbour(height, direction, row_, col_)
           }
         } else { // at the border
            if self.trees[row_][col_].height >= height {
              None
            } else {
              Some(direction)
           }
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

    pub fn count_visible_trees(&mut self) -> usize {
        let mut count: usize = 0;

        for i in 0..self.trees.len() {
           for j in 0..self.trees[i].len() {
                if self.trees[i][j].is_visible != None {
                    count = count + 1;
                }
            }
        }
        count
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

pub fn part_one(input: &str) -> Option<usize> {
    let mut forest = make_trees(parse_input(&input));
    
    forest.set_borders_as_visible();
    forest.set_visibilities();
    Some(forest.count_visible_trees())
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
    fn test_set_visibilities2() {
        let input = advent_of_code::read_file("examples", 82);
        let mut forest = make_trees(parse_input(&input));
        forest.set_borders_as_visible();
        forest.set_visibilities();
        let count = forest.count_visible_trees();
        assert_eq!(count, 60); // got this figure running from fspoettel's code
        
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
    fn test_bug_hunt() {
        let input2 = advent_of_code::read_file("inputs", 8);
        let raw_heights = parse_input(&input2);
        assert_eq!(raw_heights.len(), 99);
        assert_eq!(raw_heights[98].len(), 99);

        let mut forest: Forest = make_trees(raw_heights);
        forest.set_borders_as_visible();
        //forest.set_visibilities();
        let count = forest.count_visible_trees();
        assert_eq!(count, (99 *2) + (97 *2));

        let tree = forest.tree_at(56, 69);
        assert_eq!(tree.unwrap().height, 9);
        assert_eq!(tree.unwrap().is_visible, None);

        




    }
    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 8);
        assert_eq!(part_one(&input), Some(21));

        let input2 = advent_of_code::read_file("examples", 81);
        assert_eq!(part_one(&input2), Some(23));
 
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 8);
        assert_eq!(part_two(&input), None);
    }
}
