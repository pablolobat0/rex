#[cfg(test)]
mod test {
    use crate::vm::chunk::{Chunk, OpCode};

    #[test]
    fn test_constant() {
        let mut chunk = Chunk::new();
        let constant = chunk.add_constant(1.2);
        chunk.write(OpCode::Constant(constant as u8), 1);

        let code = vec![OpCode::Constant(constant as u8)];

        assert_eq!(chunk.get(constant), code.get(constant));
        assert_eq!(chunk.get_line(constant), Option::Some(1));
    }
}
