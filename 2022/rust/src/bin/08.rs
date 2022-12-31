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

    pub fn set_borders_as_visible(&mut self) {
        for i in (0..self.trees.len()) {
           for j in (0..self.trees[i].len()) {
                if self.is_border_tree(i, j) {
                    self.trees[i][j].is_visible = Some(true);
                 }
            }
        }
    }
    
        
}

#[derive(PartialEq)]
#[derive(Debug)]
pub struct Tree {
    pub height: usize,
    pub is_visible: Option<bool>
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
        assert_eq!(forest.tree_at(0,0).map(|t| t.is_visible).flatten(), Some(true));
        assert_eq!(forest.tree_at(4,4).map(|t| t.height), Some(0));
        assert_eq!(forest.tree_at(4,4).map(|t| t.is_visible).flatten(), Some(true));
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
