# Difficient
Efficient, type-safe, (almost) zero-allocation structural diffing.

* Annotate your `struct` or `enum` with `#[derive(Diffable)]`
* Diff your value with another value of the same type
* Apply the diff! 
* Serialize the diff!
* `Visit` the diff, generating a custom stream of change events

The possibilities are endless!


## Example Usage

```rust
use difficient::{Diffable, DeepDiff, AtomicDiff, PatchOnlyDiff, Id, Replace};

// here is the type we would like to diff - note `#[derive(Diffable)]`
#[derive(Diffable, PartialEq, Debug, Clone)]
enum SimpleEnum {
    First,
    Second { x: &'static str, y: SimpleStruct },  // SimpleStruct defined below
}

// create an initial value
let mut source = SimpleEnum::First;

// diffing a value against itself results in no change
let diff1 = source.diff(&source);
assert!(diff1.is_unchanged());

// create a second value and diff
let mut target1 = SimpleEnum::Second {
    x: "hello",
    y: SimpleStruct { a: "aaa".into(), b: 123, c: vec![1.23] }
};
let diff2 = source.diff(&target1);
// the value is fully replaced by a different enum variant
let expect_diff = DeepDiff::Replaced(&target1);
assert_eq!(diff2, expect_diff);

// 'applying' a diff to source value results in the target value
source.apply(&diff2).unwrap();
assert_eq!(source, target1);

// create a third value and diff
let target2 = SimpleEnum::Second {
    x: "goodbye",
    y: SimpleStruct { a: "aaa".into(), b: 234, c: vec![1.23] }
};
let diff3 = target1.diff(&target2);
// here the variant is patched but not totally replaced
let expect_diff = DeepDiff::Patched(SimpleEnumDiff::Second {
    x: AtomicDiff::Replaced(&"goodbye"),
    y: PatchOnlyDiff::Patched(
        SimpleStructDiff { a: AtomicDiff::Unchanged, b: AtomicDiff::Replaced(&234) }
    )
});
assert_eq!(diff3, expect_diff);
target1.apply(&diff3).unwrap();
assert_eq!(target1, target2);

// It works for structs too!

#[derive(Diffable, PartialEq, Debug, Clone)]
struct SimpleStruct {
    a: String,
    b: i32,
    #[diffable(skip)]  // do not take `c` field into account when diffing
    c: Vec<f64>,
}

let mut source = SimpleStruct {
    a: "Hello".into(),
    b: 123,
    c: vec![3.3, 2.2, 1.1],
};
let target = SimpleStruct {
    a: "Hello".into(),
    b: 123,
    c: vec![5.5, 4.4, 3.3],
};
let diff = source.diff(&target);
assert!(diff.is_unchanged());  // unchanged because `c` is ignored
```

## Visitor

With the `visitor` feature activated, we can generate a series of discrete changes from the `Diff` type.

See `examples/visitor.rs` for a worked example.


## Efficient?

The diff function does not allocate, in most cases. This makes it extremely fast.

The exceptions are

* when dealing with boxed types
* when dealing collections (`Vec`, `HashMap` or `BTreeMap`)

Then it _may_ have to allocate, which makes it merely very fast.
