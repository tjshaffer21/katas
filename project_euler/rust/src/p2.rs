// Project Euler - Problem 2 - Even Fibonacci numbers
// URL: https://projecteuler.net/problem=1

fn main() {
    let n = 4000000;

    let mut eone = 0;
    let mut etwo = 2;
    let mut sum = 2;    // eone+etwo

    while etwo < n {
        let ethr = (4*etwo) + eone;

        if ethr > n {
            break;
        }

        eone = etwo;
        etwo = ethr;
        sum += etwo;
    }

    assert_eq!(4613732, sum);
    println!("The sum of even fiboacci terms under {} is {} ", n, sum);
}