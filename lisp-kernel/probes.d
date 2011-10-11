provider ccl {
    probe gc__start(uint64_t);
    probe gc__finish(uint64_t);
    probe egc__start(uint64_t, unsigned);
    probe egc__finish(uint64_t, unsigned);
    probe create__thread(unsigned long);
};

/*

gc-start(uint64_t bytes_allocated)
gc-finish(uint64_t bytes-freed)
egc-start(uint64_t bytes-allocated, unsigned generation)
egc-finish(uint64_t bytes-freed, unsigned generation)
create-thread(unsigned-long thread-id)

*/
