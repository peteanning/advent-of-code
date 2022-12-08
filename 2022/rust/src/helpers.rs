#[allow(dead_code)]
/*
 * Use this file if you want to extract helpers from your solutions.
 * Example import from this file: `use aoc::helpers::example_fn;`.
 */

fn an_experiment() -> usize  {
    let bar: Vec<u64> = vec![2, 4, 3, 7]; // For example

    let res = bar
        .windows(2)
        .flat_map(<&[u64; 2]>::try_from)
        .filter(|&&[a,b]| a < b)
        .count(); 


    println!("The result was {}", res);

    res
}
