use itertools::Itertools;

type NodeId = usize;

#[derive(PartialEq)]
#[derive(Debug)]
pub enum FileSystemType {
   d,f 
}
pub struct FileSystemNode {
    name: String,
    size: Option<usize>,
    file_system_type: FileSystemType
}

impl FileSystemNode {
    fn new(name: String, size: Option<usize>, file_system_type: FileSystemType) -> FileSystemNode {
       match (size, &file_system_type) {
           (Some(_), FileSystemType::d) => panic!("bad size"),
           (None, FileSystemType::f ) => panic!("bad size"),
           _ => FileSystemNode { name, size, file_system_type } 
       }
    }

    fn new_dir(name: String) -> FileSystemNode {
        FileSystemNode::new(name, None, FileSystemType::d)
    }
    
    fn new_from_line(line: &str) -> FileSystemNode {
       let (first_part, name_) = line.split(" ").next_tuple().unwrap();
       if first_part.starts_with("dir") {
            FileSystemNode::new_dir(first_part.to_string())
       } else {
           FileSystemNode::new(name_.to_string(), first_part.parse::<usize>().ok(), FileSystemType::f)
       }
    }
}

pub struct FileSystem {
    nodes: Vec<Node<FileSystemNode>>
}

pub struct Node<T> {

    parent: Option<NodeId>,
    previous_sibling: Option<NodeId>,
    next_sibling: Option<NodeId>,
    first_child: Option<NodeId>,
    last_child: Option<NodeId>,
    data: T,
}

impl FileSystem {
    fn new_node(&mut self, data: FileSystemNode) -> NodeId {
        let next_index = self.nodes.len();

        self.nodes.push(Node { 
            parent: None, 
            previous_sibling: None, 
            next_sibling: None, 
            first_child: None, 
            last_child: None, 
            data });

        next_index
    }

    fn add_child(&mut self, parent: NodeId, data: FileSystemNode) -> NodeId {
        let next_index = self.nodes.len();
     
        self.nodes.push(Node { 
            parent: Some(parent), 
            previous_sibling: None, 
            next_sibling: None, 
            first_child: None, 
            last_child: None, 
            data });

        next_index   
    }

    fn get_parent_for_id(&self, id: NodeId) -> Option<NodeId> {
        if self.nodes.len() > id {
            self.nodes[id].parent
        } else {
            None
        }
    }

    fn new() -> FileSystem {
        FileSystem { nodes: Vec::new() }
    }
}

fn directory_name_from_line(line: &str) -> &str { 
    
    if line.starts_with("$ ") {
        if line.len() >= 6 { &line[5..].trim() } else { "" }
    } else if line.starts_with("dir") {
        if line.len() >= 5 { &line[4..].trim() } else { "" }
    } else { "" }

}

pub fn parse_input(input: &str) -> FileSystem {
    let mut file_system = FileSystem::new();

    let lines = 
        input
        .lines()
        .into_iter();
    
    let mut cwd_id: usize = 0;


    for line in lines {
        if line.starts_with("$") { // in cmd mode
            if line.starts_with("$ cd ..") { // change dir up
                // get the parent id of the cwd and set it as the cwd
                cwd_id = file_system.get_parent_for_id(cwd_id).unwrap();
            } else if line.starts_with("$ cd") { // new dir
                cwd_id = file_system.new_node(FileSystemNode::new_dir(directory_name_from_line(line).to_string()));
            } else { // ls for cwd
                continue;
            }
        } else { // in a listing
            // parse the line as a FileSystemNode and add it to the cwd
            file_system.new_node(FileSystemNode::new_from_line(line));
        }
    } //for line in lines

    file_system
}


pub fn part_one(input: &str) -> Option<u32> {

    None
}

pub fn part_two(input: &str) -> Option<u32> {
    None
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 7);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;
    
    fn root_dir() -> FileSystemNode {
        FileSystemNode::new_dir("/".to_string())
    }

    #[test]
    fn test_directory_name_from_line() {
        let dir_line = "dir bla";
        let dir_single_char = "dir b";
        let cmd_dir_line = "$ cd bla";
        let bad_dir = "dir ";

        assert_eq!(directory_name_from_line(dir_line), "bla");
        assert_eq!(directory_name_from_line(dir_single_char), "b");
        assert_eq!(directory_name_from_line(cmd_dir_line), "bla");
        assert_eq!(directory_name_from_line(bad_dir), "");
        assert_eq!(directory_name_from_line(""), "");

    }


    #[test]
    fn test_new_file_system_node() {
        let dir = root_dir();

        assert_eq!(dir.name, "/".to_string());
        assert_eq!(dir.size, None);
        assert_eq!(dir.file_system_type, FileSystemType::d);
    }

    #[test]
    #[should_panic(expected = "bad size")]
    fn test_new_valid_file_system_node() {
       FileSystemNode::new("test".to_string(), None, FileSystemType::f); 
       FileSystemNode::new("test".to_string(), Some(1234), FileSystemType::d); 
       FileSystemNode::new_from_line("bad-size a.txt"); 
    }


    #[test]
    fn test_new_file_system() {
        let file_system = FileSystem::new(); 
        assert_eq!(file_system.nodes.len(), 0);
    }
   
    #[test]
    fn test_new_node() {
        let mut file_system = FileSystem::new(); 
        let id = file_system.new_node(root_dir());

        assert_eq!(file_system.nodes.len(), 1);
    }


    #[test]
    fn test_add_child() {
        let mut file_system = FileSystem::new(); 
        let id = file_system.new_node(root_dir());
        let child_id_1 = file_system.add_child(id, FileSystemNode::new("a.txt".to_string(), Some(1000), FileSystemType::f));
        let child_id_2 = file_system.add_child(id, FileSystemNode { name: "b".to_string(), size: None, file_system_type: FileSystemType::d });

        assert_eq!(file_system.nodes.len(), 3);

        assert_eq!(Some(id), file_system.get_parent_for_id(child_id_1));
        assert_eq!(Some(id), file_system.get_parent_for_id(child_id_2));
    }



    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 7);
        assert_eq!(part_one(&input), None);
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 7);
        assert_eq!(part_two(&input), None);
    }
}
