use std::collections::btree_map::BTreeMap;
use std::ops::RangeInclusive;
use read_input::InputBuild;
use read_input::prelude::input;

pub struct ChooserTable<T: Clone> {
    name2choice: BTreeMap<String,T>,
    names: Vec<String>,
    current_name: String
}

impl <T:Clone> ChooserTable<T> {
    pub fn from(choices: &Vec<(&str,T)>) -> Self {
        let current_name = choices.iter().next().unwrap().0.to_string();
        let mut name2choice = BTreeMap::new();
        for (name, choice) in choices.iter() {
            name2choice.insert(name.to_string(), choice.clone());
        }
        let names: Vec<String> = choices.iter().map(|c| c.0.to_string()).collect();
        ChooserTable {name2choice, names, current_name}
    }

    pub fn choose(&mut self, choice: &str) {
        assert!(self.name2choice.contains_key(choice));
        self.current_name = choice.to_owned();
    }

    pub fn current_name(&self) -> &str {
        self.current_name.as_str()
    }

    pub fn current_choice(&self) -> T {
        self.name2choice.get(self.current_name.as_str()).unwrap().clone()
    }

    pub fn name_vec(&self) -> Vec<String> {
        self.names.clone()
    }

    pub fn console_pick(&mut self) {
        self.current_name = user_pick_element(self.names.iter().cloned(), |s| s.clone())
    }
}

#[macro_export]
macro_rules! arc_vec {
    ($( ($s:expr, $f:expr)),* ) => {vec![$(($s, Arc::new($f)),)*]}
}

#[macro_export]
macro_rules! func_vec {
    ($t:ident, $( ($s:expr, $f:expr)),+ ) => {vec![$($t::new($s, Arc::new($f)),)+]}
}

#[macro_export]
macro_rules! make_chooser_table {
    ($tabletype:ident, $functype:ident, $funcsig:ident) => {
        #[derive(Clone)]
        pub struct $functype {
            name: String,
            func: Arc<$funcsig>
        }

        impl $functype {
            pub fn new(name: &str, func: Arc<$funcsig>) -> Self {
                $functype {name: name.to_owned(), func}
            }
            pub fn name(&self) -> &str {self.name.as_str()}
            pub fn func(&self) -> Arc<$funcsig> {
                self.func.clone()
            }
        }

        #[derive(Clone)]
        pub struct $tabletype {
            name2choice: BTreeMap<String,$functype>,
            names: Vec<String>,
            choices: Vec<$functype>,
            current_name: String
        }
        impl $tabletype {
            pub fn from(choices: &Vec<$functype>) -> Self {
                let current_name = choices.iter().next().unwrap().name().to_string();
                let mut name2choice = BTreeMap::new();
                for choice in choices.iter() {
                    name2choice.insert(choice.name().to_string(), choice.clone());
                }
                let names: Vec<String> = choices.iter().map(|c| c.name().to_string()).collect();
                $tabletype {name2choice, names, current_name, choices: choices.clone()}
            }

            pub fn choose(&mut self, choice: &str) {
                assert!(self.name2choice.contains_key(choice));
                self.current_name = choice.to_owned();
            }

            pub fn current_name(&self) -> &str {
                self.current_name.as_str()
            }

            pub fn current_func(&self) -> $functype {
                self.name2choice.get(self.current_name.as_str()).unwrap().clone()
            }

            pub fn name_vec(&self) -> Vec<String> {
                self.names.clone()
            }

            pub fn console_pick(&mut self) {
                self.current_name = user_pick_element(self.choices.iter().cloned(),
                    |f| f.name().to_string().clone()).name().to_string();
            }
        }
    }
}

#[derive(Copy, Clone)]
pub struct SliderValue<T: Copy + Clone> {
    current: T,
    lo: T,
    hi: T
}

impl <T: Copy + Clone + std::str::FromStr + PartialOrd + 'static> SliderValue<T> {
    pub fn new(current: T, min: T, max: T) -> Self {
        SliderValue {current, lo: min, hi: max }
    }

    pub fn make_range(&self) -> RangeInclusive<T> {
        self.lo..=self.hi
    }

    pub fn set_current(&mut self, new_current: T) {
        self.current = new_current;
    }

    pub fn get_current(&self) -> T {
        self.current
    }

    pub fn console_pick(&mut self, prompt: &str) {
        self.current = input().msg(prompt).inside(self.make_range()).get();
    }
}

pub fn replay_slider() -> SliderValue<f64> {
    SliderValue::new(1.5, 1.0, 5.0)
}

pub fn prob_slider() -> SliderValue<f64> {
    SliderValue::new(1.0, 0.0, 1.0)
}

pub fn user_pick_element<T: Clone, S: Fn(&T) -> String>(choices: impl Iterator<Item=T>, show: S) -> T {
    let choices = choices.collect::<Vec<_>>();
    for (i, item) in choices.iter().enumerate() {
        println!("{}) {}", i+1, show(item));
    }
    let choice: usize = input()
        .msg("Enter choice: ")
        .inside(1..=choices.len())
        .get();
    choices[choice - 1].clone()
}