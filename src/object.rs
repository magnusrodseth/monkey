#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}
