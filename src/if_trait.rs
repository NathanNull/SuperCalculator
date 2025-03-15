pub struct If<const B: bool>;
pub trait True {}
impl True for If<true> {}