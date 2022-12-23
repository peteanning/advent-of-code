use std::collections::HashMap;

use itertools::Itertools;

type NodeId = usize;

#[derive(PartialEq)]
#[derive(Debug)]
pub enum FileSystemType {
   d,f 
}

#[derive(Debug)]
pub struct FileSystemNode {
    name: String,
    size: usize,
    file_system_type: FileSystemType
}

impl FileSystemNode {
    fn new(name: String, size: usize, file_system_type: FileSystemType) -> FileSystemNode {
       let maybe_size: Option<usize> = if size == 0 { None } else { Some(size) };
       match (maybe_size, &file_system_type) {
           (Some(_), FileSystemType::d) => panic!("bad size"),
           (None, FileSystemType::f ) => panic!("bad size"),
           _ => FileSystemNode { name, size, file_system_type } 
       }
    }

    fn new_dir(name: String) -> FileSystemNode {
        FileSystemNode::new(name, 0, FileSystemType::d)
    }
    
    fn new_from_line(line: &str) -> FileSystemNode {
       let (first_part, name_) = line.split(" ").next_tuple().unwrap();
       if first_part.starts_with("dir") {
            FileSystemNode::new_dir(name_.to_string())
       } else {
           FileSystemNode::new(name_.to_string(), first_part.parse::<usize>().unwrap(), FileSystemType::f)
       }
    }
}

#[derive(Debug)]
pub struct FileSystem {
    nodes: Vec<Node<FileSystemNode>>
}

#[derive(Debug)]
pub struct Node<T> {
    id: NodeId,
    parent: Option<NodeId>,
    data: T,
}

impl FileSystem {
    fn new_node(&mut self, data: FileSystemNode) -> NodeId {
        let next_index = self.nodes.len();
         
        self.nodes.push(Node { 
            id: next_index,
            parent: None, 
            data });

        next_index
    }

    fn update_parents(&mut self, p_id: NodeId, size: usize) {
        self.nodes[p_id].data.size += size;
        match (size, self.nodes[p_id].parent) {
            (size, Some(p)) => FileSystem::update_parents(self, p, size),
            _ => ()
        }
    }


    fn add_child(&mut self, parent: NodeId, data: FileSystemNode) -> NodeId {
        let next_index = self.nodes.len();
        
        FileSystem::update_parents(self, parent, data.size);

        self.nodes.push(Node { 
            id: next_index,
            parent: Some(parent), 
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

    fn iter(&self) -> impl Iterator<Item = &Node<FileSystemNode>> {
        self.nodes.iter()
    }


    fn get_node_id_for(&self, name: String, file_system_type: FileSystemType, parent: Option<NodeId>) -> Option<NodeId> {
       self.nodes.iter()
           .enumerate()
           .find(|(_, n)| n.data.name == name && n.data.file_system_type == file_system_type && n.parent == parent)
           .map(|(i,_)| i)
    }

    fn get_children_for(&self, id: NodeId) -> Vec<NodeId> {
       self.nodes.iter()
           .filter(|n| n.parent == Some(id))
           .enumerate()
           .map(|(i,_)| i)
           .collect()
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
    let root_dir = file_system.new_node(FileSystemNode::new("/".to_string(), 0, FileSystemType::d)); 
    let mut cwd_id: usize = root_dir;
    let mut counter: usize = 0;
    let mut temp_node_id = 0;


    for line in lines {
        counter += 1;
        println!("Line no. {} {}", counter, line);
        if line.starts_with("$") { // in cmd mode
            if line.starts_with("$ cd ..") { // change dir up
                // get the parent id of the cwd and set it as the cwd
                cwd_id = file_system.get_parent_for_id(cwd_id).unwrap();
                println!("Changing up a directorty to {}", file_system.nodes[cwd_id].data.name);
            } else if line.starts_with("$ cd /") { 
                println!("Changing to root");
                cwd_id = root_dir;
            } else if line.starts_with("$ cd") { // new root dir
                cwd_id = file_system.get_node_id_for(directory_name_from_line(line).to_string(), FileSystemType::d, Some(cwd_id)).unwrap();
                println!("Set cwd to directorty to {}", file_system.nodes[cwd_id].data.name);
            } else { // ls for cwd
                println!("Ignoring ls");
                continue;
            }
        } else { // in a listing
            // parse the line as a FileSystemNode and add it to the cwd
            let temp_node_id =  file_system.add_child(cwd_id, FileSystemNode::new_from_line(line));
            println!("Added {} {:?}", file_system.nodes[temp_node_id].data.name, file_system.nodes[temp_node_id].data.file_system_type );
        }
    } //for line in lines
    dbg!(&file_system);
    file_system
}


pub fn part_one(input: &str) -> Option<usize> {
    use FileSystemType::*;

    let sum: usize = 
        parse_input(&input)
        .iter()
        .filter(|n| n.data.file_system_type == d)
        .filter(|n| n.data.size <= 100000)
        .map(|n| n.data.size)
        .fold(0, |acc, s| acc + s);
    Some(sum)
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
        assert_eq!(dir.size, 0);
        assert_eq!(dir.file_system_type, FileSystemType::d);
    }

    #[test]
    #[should_panic(expected = "bad size")]
    fn test_new_valid_file_system_node() {
       FileSystemNode::new("test".to_string(), 0, FileSystemType::f); 
       FileSystemNode::new("test".to_string(), 1234, FileSystemType::d); 
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
    fn test_new_node_from_line() {
        let dir = FileSystemNode::new_from_line("dir d");

        assert_eq!(dir.name, "d".to_string());
        assert_eq!(dir.file_system_type, FileSystemType::d);
        assert_eq!(dir.size, 0);
    }



    #[test]
    fn test_add_child() {
        let mut file_system = FileSystem::new(); 
        let id = file_system.new_node(root_dir());
        let child_id_1 = file_system.add_child(id, FileSystemNode::new("a.txt".to_string(), 1000, FileSystemType::f));
        let child_id_2 = file_system.add_child(id, FileSystemNode { name: "b".to_string(), size: 0, file_system_type: FileSystemType::d });

        assert_eq!(file_system.nodes.len(), 3);

        assert_eq!(Some(id), file_system.get_parent_for_id(child_id_1));
        assert_eq!(Some(id), file_system.get_parent_for_id(child_id_2));
    }

    #[test]
    fn test_parse_input() {
        let input = advent_of_code::read_file("examples", 7);
        let file_system: FileSystem = parse_input(&input);

        assert_eq!(file_system.iter().count(), 14);

        let e_id = file_system.get_node_id_for("e".to_string(), FileSystemType::d, Some(1)).unwrap();
        assert_eq!(file_system.nodes[e_id].data.size, 584);

        let a_id = file_system.get_node_id_for("a".to_string(), FileSystemType::d, Some(0)).unwrap();
        assert_eq!(file_system.nodes[a_id].data.size, 94853);

        let d_id = file_system.get_node_id_for("d".to_string(), FileSystemType::d, Some(0)).unwrap();
        assert_eq!(file_system.nodes[d_id].data.size, 24933642);


        let root_id = file_system.get_node_id_for("/".to_string(), FileSystemType::d, None).unwrap();
        assert_eq!(file_system.nodes[root_id].data.size, 48381165);


    }



    #[test]
    fn test_get_node_by_name() {
        let input = advent_of_code::read_file("examples", 71);
        let file_system: FileSystem = parse_input(&input);
        assert_eq!(file_system.iter().count(), 15);
        let id = file_system.get_node_id_for("a".to_string(), FileSystemType::d, Some(4)).unwrap();
        assert_eq!(id, 10); 


    }

    #[test]
    fn test_get_children() {
        let input = advent_of_code::read_file("examples", 7);
        let file_system: FileSystem = parse_input(&input);
        let root_id = file_system.get_node_id_for("/".to_string(), FileSystemType::d, None);

        assert_eq!(root_id.unwrap(), 0);
        assert_eq!(file_system.iter().count(), 14);
        assert_eq!(file_system.get_children_for(0).iter().count(), 4);
    }

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 7);
        assert_eq!(part_one(&input), Some(95437));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 7);
        assert_eq!(part_two(&input), None);
    }
}
