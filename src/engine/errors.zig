pub const UCIError = error{
    UnknownCommand,
    BufferOverflow,
    ReadFailed,
    ExitOK,
    InvalidPosition,
};
