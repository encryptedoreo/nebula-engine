pub const UCIError = error{
    UnknownCommand,
    BufferOverflow,
    ReadFailed,
    WriteFailed,
    ExitOK,
    InvalidPosition,
};
