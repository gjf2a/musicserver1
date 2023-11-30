use std::collections::BinaryHeap;

fn main() {
    let mut heap = BinaryHeap::new();
    for i in 0..10 {
        heap.push(i);
    }

    while let Some(popped) = heap.pop() {
        println!("Popped {}", popped);
    }
}