// Project Euler - Problem 1 - Multiples of 3 and 5.
// URL: https://projecteuler.net/problem=1

fn main() {
    let multiples_below = |n: i32, m: i32| -> i32 { (n-1) / m };
    let ident = |n: i32, k: i32| -> i32 { k * (n * (n+1) / 2) };

    let n = 10;
    let r = ident(multiples_below(n, 3), 3) + ident(multiples_below(n, 5), 5) -
            ident(multiples_below(n, 15), 15);
    assert_eq!(23, r);
    println!("Multiples of 3 and 5 for n={} is {}", n, r);

    let n = 1000;
    let r = ident(multiples_below(n, 3), 3) + ident(multiples_below(n, 5), 5) -
            ident(multiples_below(n, 15), 15);

    assert_eq!(233168, r);
    println!("Multiples of 3 and 5 for n={} is {}", n, r);
}