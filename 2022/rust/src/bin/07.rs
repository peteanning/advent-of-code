use itertools::Itertools;

#[derive(PartialEq)]
#[derive(Debug)]
struct File<'a> {
    name: &'a str,
    size: usize
}
impl<'a> File<'a>  {
    fn new_from_cmdline(line: &str) -> File{
       let (size_, name_) = line.split(" ").next_tuple().unwrap();
       File { name: name_, size: size_.parse::<usize>().unwrap() }
    }    
}

#[derive(PartialEq)]
#[derive(Debug)]
struct NewFile {
    name: String,
    size: usize
}

impl NewFile  {
    fn new_from_cmdline(line: &str) -> NewFile{
       let (size_, name_) = line.split(" ").next_tuple().unwrap();
       NewFile { name: name_.to_string(), size: size_.parse::<usize>().unwrap() }
    }    
}


#[derive(PartialEq)]
#[derive(Debug)]
struct NewDir {
    directories: Vec<NewDir>,
    files: Vec<NewFile>,
    name: String
}

impl NewDir {
    fn new(line: &str) -> NewDir {
        NewDir { directories: vec![], files: vec![], name: directory_name_from_line(line).to_string() }
    }

    fn cd_up(&mut self) -> NewDir {
       if self.directories.len() >= 1 {
            self.directories.pop().unwrap()
       } else {
           panic!("There are no directories in the tree!");
       }
    }
}


#[derive(PartialEq)]
#[derive(Debug)]
struct Dir<'a> {
    directories: Vec<&'a Dir<'a>>,
    files: Vec<File<'a>>,
    name: &'a str
}

impl<'a> Dir<'a> {
    fn new(line: &str) -> Dir {
        Dir { directories: vec![], files: vec![], name: directory_name_from_line(line) }
    }

    fn cd_up(&mut self) -> &Dir {
       if self.directories.len() >= 1 {
            self.directories.pop().unwrap()
       } else {
           panic!("There are no directories in the tree!");
       }
    }
}

#[derive(PartialEq)]
#[derive(Debug)]
enum Cmd {
    Ls, Cd, CdUp
}
#[derive(PartialEq)]
#[derive(Debug)]
struct TerminalOuput<'a> {
    cmd: Cmd,
    name: Option<&'a str>
}

fn directory_name_from_line(line: &str) -> &str { 
    
    if line.starts_with("$ ") {
        if line.len() >= 6 { &line[5..].trim() } else { "" }
    } else if line.starts_with("dir") {
        if line.len() >= 5 { &line[4..].trim() } else { "" }
    } else { "" }

}

impl<'a> TerminalOuput<'a> {
    fn new_from_cmdline(line: &str) -> TerminalOuput {
         let start = &line[0..=0];
         let cmd = &line[2..=3];
         let name = directory_name_from_line(line);
         dbg!(line);
         dbg!(&start);
         dbg!(&cmd);
         dbg!(&name);

            match (start, cmd, name) {
                ("$", "cd", "..") => TerminalOuput { cmd: Cmd::CdUp, name: None},
                ("$", "ls", _) => TerminalOuput { cmd: Cmd::Ls, name: None},
                ("$", "cd", _) => TerminalOuput { cmd: Cmd::Cd, name: Some(name)},
                _ => panic!("Unknown Command {}", line)
            }
    }
}



    

fn new_parse_input(input: &str) -> NewDir {
    let mut cwd: NewDir = NewDir::new("Unitialised");
    let mut state: &str = "Unitialised";

    let lines = 
        input
        .lines()
        .into_iter();

    for line in lines {
        if line.starts_with("$") { // cmd state
           let terminal_output: TerminalOuput = TerminalOuput::new_from_cmdline(line);
           match terminal_output {
               TerminalOuput { cmd: Cmd::Ls, name: None } => state = "WaitingLs",
               TerminalOuput { cmd: Cmd::Cd, name: Some(dir) } => {
                   state = "InDir"; 
                   cwd = NewDir { directories: vec![], files: vec![], name: dir.to_string() };
               },
               TerminalOuput { cmd: Cmd::CdUp, name: None } => state = "InDir",
               _ => panic!("Unknown cmd line {}", line)
               
           }
        } else if state == "WaitingLs" {
            //parse the line as a listing
            if line.starts_with("dir") {
                // create a dir and push it into the cwd
                cwd.directories.push(NewDir::new(line));
            } else { // file
                // create a file and push it into the cwd
                cwd.files.push(NewFile::new_from_cmdline(line));
            }
        }
    }

    cwd
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

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 7);
        assert_eq!(part_one(&input), None);
    }


    #[test]
    fn test_new_file() {
        let line = "12345 a.txt";
        let expected_file = File { name: "a.txt", size: 12345};

        assert_eq!(File::new_from_cmdline(line), expected_file);
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
    fn test_new_from_cmdline() {
        let change_dir_line = "$ cd someDir";
        let change_dir_up_line = "$ cd ..";
        let ls_line = "$ ls";

        assert_eq!(TerminalOuput::new_from_cmdline(change_dir_line), TerminalOuput { cmd: Cmd::Cd, name: Some("someDir")})  ;
        assert_eq!(TerminalOuput::new_from_cmdline(change_dir_up_line), TerminalOuput { cmd: Cmd::CdUp, name: None})  ;
        assert_eq!(TerminalOuput::new_from_cmdline(ls_line), TerminalOuput { cmd: Cmd::Ls, name: None})  ;
        
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 7);
        assert_eq!(part_two(&input), None);
    }
}
