#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Null,
}

impl Object {
    pub fn to_string(&self) -> String {
        match self {
            Object::Integer(integer) => integer.to_string(),
            Object::Boolean(boolean) => boolean.to_string(),
            Object::Return(return_object) => return_object.to_string(),
            Object::Null => "null".to_string(),
        }
    }
}
