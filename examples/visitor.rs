use difficient::{AcceptVisitor, Diffable, Enter};

// Define our visitor
#[derive(Default)]
struct ChangeEmitter {
    location: Vec<Enter>,
    changes: Vec<(String, serde_json::Value)>,
}

// impl Visitor
impl difficient::Visitor for ChangeEmitter {
    fn enter(&mut self, val: Enter) {
        self.location.push(val);
    }
    fn exit(&mut self) {
        self.location.pop().unwrap();
    }
    fn replaced<T: serde::Serialize>(&mut self, val: T) {
        use Enter::*;
        let mut path = String::new();
        for (ix, loc) in self.location.iter().enumerate() {
            match loc {
                NamedField { name, .. } => path.push_str(name),
                PositionalField(p) => path.push_str(&p.to_string()),
                Variant { name, .. } => path.push_str(name),
                MapKey(key) => path.push_str(&key),
                Index(key) => path.push_str(&key.to_string()),
            }
            if ix != self.location.len() - 1 {
                path.push('.');
            }
        }

        self.changes
            .push((path, serde_json::to_value(val).unwrap()));
    }
    fn splice<T: serde::Serialize>(&mut self, _from_index: usize, _replace: usize, _values: &[T]) {
        todo!()
    }
}

#[derive(Diffable, PartialEq, Debug, Clone, serde::Serialize, serde::Deserialize)]
struct Struct1 {
    a: Struct2,
    b: Struct2,
}

#[derive(Diffable, PartialEq, Debug, Clone, serde::Serialize, serde::Deserialize)]
struct Struct2 {
    c: Struct3,
}

#[derive(Diffable, PartialEq, Debug, Clone, serde::Serialize, serde::Deserialize)]
struct Struct3 {
    d: i32,
    e: i32,
}

fn main() {
    let mut emitter = ChangeEmitter::default();

    let v1 = Struct1 {
        a: Struct2 {
            c: Struct3 { d: 1, e: 2 },
        },
        b: Struct2 {
            c: Struct3 { d: 3, e: 4 },
        },
    };

    let v2 = Struct1 {
        a: Struct2 {
            c: Struct3 { d: 1, e: 22 },
        },
        b: Struct2 {
            c: Struct3 { d: 33, e: 4 },
        },
    };

    let diff = v1.diff(&v2);

    diff.accept(&mut emitter);

    println!("Changed paths:");
    for (path, val) in emitter.changes {
        println!("{path}: {val}")
    }
}
