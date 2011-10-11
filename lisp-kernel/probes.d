provider ccl {
    probe gc__start(unsigned long);
    probe gc__finish(unsigned long);
    probe egc__start(unsigned long, unsigned);
    probe egc__finish(unsigned long, unsigned);
    probe create__thread(unsigned long);
};

/*
gc-start(bytes_allocated)
gc-finish(bytes-freed)
egc-start(bytes-allocated, generation)
egc-finish(bytes-freed, generation)
create-thread(thread-id)
*/
