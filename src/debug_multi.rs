pub trait DebugMulti: std::fmt::Debug {
    fn lines(&self) -> Vec<String> {
        format!("{self:?}").split("\r\n").map(|s|s.to_string()).collect()
    }
}

impl<T: std::fmt::Debug> DebugMulti for T {}