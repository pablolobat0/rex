#[cfg(test)]
mod test {
    use crate::vm::{
        chunk::{Chunk, OpCode},
        vm::{InterpretResult, VirtualMachine},
    };

    #[test]
    fn test_constant() {
        let mut chunk = Chunk::new();
        let constant = chunk.add_constant(1.2);
        chunk.write(OpCode::Constant(constant), 1);

        let code = vec![OpCode::Constant(constant)];

        assert_eq!(chunk.get(constant), code.get(constant));
        assert_eq!(chunk.get_line(constant), Option::Some(1));
    }

    #[test]
    fn test_negate() {
        let mut chunk = Chunk::new();
        let constant = chunk.add_constant(1.2);
        chunk.write(OpCode::Constant(constant), 1);
        chunk.write(OpCode::Negate, 1);
        let mut vm = VirtualMachine::new(&chunk);

        assert_eq!(vm.interpret(), InterpretResult::Ok);
        assert_eq!(vm.stack.get(0), Some(-1.2).as_ref());
    }

    #[test]
    fn test_add() {
        let mut chunk = Chunk::new();
        let constant = chunk.add_constant(5.0);
        chunk.write(OpCode::Constant(constant), 1);
        let constant = chunk.add_constant(10.0);
        chunk.write(OpCode::Constant(constant), 1);
        chunk.write(OpCode::Add, 1);
        let mut vm = VirtualMachine::new(&chunk);

        assert_eq!(vm.interpret(), InterpretResult::Ok);
        assert_eq!(vm.stack.get(0), Some(15.0).as_ref());
    }
    #[test]
    fn test_subtract() {
        let mut chunk = Chunk::new();
        let constant = chunk.add_constant(5.0);
        chunk.write(OpCode::Constant(constant), 1);
        let constant = chunk.add_constant(10.0);
        chunk.write(OpCode::Constant(constant), 1);
        chunk.write(OpCode::Subtract, 1);
        let mut vm = VirtualMachine::new(&chunk);

        assert_eq!(vm.interpret(), InterpretResult::Ok);
        assert_eq!(vm.stack.get(0), Some(5.0).as_ref());
    }
    #[test]
    fn test_multiply() {
        let mut chunk = Chunk::new();
        let constant = chunk.add_constant(5.0);
        chunk.write(OpCode::Constant(constant), 1);
        let constant = chunk.add_constant(10.0);
        chunk.write(OpCode::Constant(constant), 1);
        chunk.write(OpCode::Multiply, 1);
        let mut vm = VirtualMachine::new(&chunk);

        assert_eq!(vm.interpret(), InterpretResult::Ok);
        assert_eq!(vm.stack.get(0), Some(50.0).as_ref());
    }
    #[test]
    fn test_divide() {
        let mut chunk = Chunk::new();
        let constant = chunk.add_constant(5.0);
        chunk.write(OpCode::Constant(constant), 1);
        let constant = chunk.add_constant(10.0);
        chunk.write(OpCode::Constant(constant), 1);
        chunk.write(OpCode::Divide, 1);
        let mut vm = VirtualMachine::new(&chunk);

        assert_eq!(vm.interpret(), InterpretResult::Ok);
        assert_eq!(vm.stack.get(0), Some(2.0).as_ref());
    }
    #[test]
    fn test_divide_by_0() {
        let mut chunk = Chunk::new();
        let constant = chunk.add_constant(0.0);
        chunk.write(OpCode::Constant(constant), 1);
        let constant = chunk.add_constant(10.0);
        chunk.write(OpCode::Constant(constant), 1);
        chunk.write(OpCode::Divide, 1);
        let mut vm = VirtualMachine::new(&chunk);

        assert_eq!(vm.interpret(), InterpretResult::RuntimeError);
    }
}
