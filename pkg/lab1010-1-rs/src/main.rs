#[macro_use] extern crate text_io;
use std::io::Write;
use std::io::stdout;

fn main() {
  print!("Enter a year: ");

  stdout().flush().unwrap();

  let year: i32 = read!();

  let c = year / 100;
  let n = year - 19 * ( year / 19 );
  let k = ( c - 17 ) / 25;

  let mut i = c - c / 4 - ( c - k ) / 3 + 19 * n + 15;
          i = i - 30 * ( i / 30 );
          i = i - ( i / 28 ) * ( 1 - ( i / 28 ) * ( 29 / ( i + 1 ) ) * ( ( 21 - n ) / 11 ) );

  let mut j = year + year / 4 + i + 2 - c + c / 4;
          j = j - 7 * ( j / 7 );

  let l = i - j;

  let month = 3 + ( l + 40 ) / 44;
  let day = l + 28 - 31 * ( month / 4 );

  println!("{:02}/{:02}/{:02}", month, day, year);
}
