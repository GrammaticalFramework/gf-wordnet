#ifndef EM_DATA_STREAM_H
#define EM_DATA_STREAM_H

#include <gu/mem.h>
#include <gu/exn.h>

typedef struct EMDataStream EMDataStream;

EMDataStream*
em_new_data_stream(size_t region_size, size_t max_elem_size, size_t n_threads,
                   GuPool* pool, GuExn* err);

void
em_data_stream_start_element(EMDataStream* stream, GuExn* err);

void
em_data_stream_add_element(EMDataStream* stream, void* elem);

void*
em_data_stream_malloc(EMDataStream* stream, size_t size);

void
em_data_stream_restart(EMDataStream* stream, size_t thread_idx, GuExn* err);

void*
em_data_stream_fetch_element(EMDataStream* stream, size_t thread_idx);

void
em_data_stream_close(EMDataStream* stream, GuExn* err);

#endif
