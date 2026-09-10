#ifndef SBCL_SPROF_H
#define SBCL_SPROF_H

#define ELEMENT_SIZE 8
#define TRACE_BUFFER_LEN 300
#define MAX_RECORDED_TRACE_LEN 62
#define TRACE_PREFIX_ELEMENTS 2

/* The trace layout is shared with contrib/sb-sprof/record.lisp. */
struct trace {
    uint32_t next;
    uint32_t multiplicity;
#ifdef LISP_FEATURE_64_BIT
    uword_t header;
    uword_t locs[TRACE_BUFFER_LEN];
#define trace_len(trace) ((int32_t)((trace)->header))
#else
    sword_t len;
    uword_t hash;
    struct loc { uint32_t word0, word1; } locs[TRACE_BUFFER_LEN];
#define trace_len(trace) ((trace)->len)
#endif
};

/* Addresses in a copied stack retain their original values. Only the reader
 * translates them into offsets in the snapshot. */
struct sprof_snapshot {
    uword_t start;
    uword_t size;
    char *data;
    uword_t frame_pointer;
    int foreignp;
};

int sprof_prepare_trace(struct thread*, os_context_t*,
                        struct sprof_snapshot*, struct trace*);
int sprof_store_trace(struct thread*, struct trace*, int stable);
#if defined LISP_FEATURE_WIN32 && defined LISP_FEATURE_X86_64
int win32_sprof_unwind(struct thread*, os_context_t*, struct sprof_snapshot*,
                       uword_t*, int, int);
#endif

#endif
