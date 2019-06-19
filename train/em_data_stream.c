#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <inttypes.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <pthread.h>
#include "em_data_stream.h"


struct EMDataStream {
	int fd;
	size_t n_regions;
	void* region;

	size_t region_size;
	size_t max_elem_size;
	uint8_t* start;
	uint8_t* end;

	size_t i_elem;
	size_t i_region;

	pthread_barrier_t barrier1, barrier2;
};

EMDataStream*
em_new_data_stream(size_t region_size, size_t max_elem_size, size_t n_threads,
                   GuPool* pool, GuExn* err)
{
	EMDataStream* stream = gu_new(EMDataStream, pool);

	stream->fd = open("/tmp", O_RDWR | O_TMPFILE, S_IRWXU);
	if (stream->fd < 0) {
		gu_raise_errno(err);
		return NULL;
	}

	size_t pagesize = getpagesize();
	
	stream->n_regions    = 0;
	stream->region       = NULL;
	stream->region_size  = ((region_size + pagesize-1) / pagesize)*pagesize;
	stream->max_elem_size= max_elem_size;
	stream->start = NULL;
	stream->end   = NULL;
	stream->i_elem   = 0;
	stream->i_region = 0;

	if ((errno = pthread_barrier_init(&stream->barrier1, NULL, n_threads)) != 0) {
		gu_raise_errno(err);
		return NULL;
	}
	if ((errno = pthread_barrier_init(&stream->barrier2, NULL, n_threads)) != 0) {
		pthread_barrier_destroy(&stream->barrier1);
		gu_raise_errno(err);
		return NULL;
	}

	return stream;
}

void
em_data_stream_start_element(EMDataStream* stream, GuExn* err)
{
	if (stream->start+stream->max_elem_size > stream->end) {
		off_t offset = stream->n_regions*stream->region_size;
		if (ftruncate(stream->fd, offset+stream->region_size) != 0) {
			gu_raise_errno(err);
			return;
		}

		if (stream->region == NULL) {
			void* region = mmap(NULL, stream->region_size,
								PROT_READ|PROT_WRITE,MAP_SHARED,
								stream->fd, 0);
			if (region == MAP_FAILED) {
				gu_raise_errno(err);
				return;
			}

			stream->region = region;
		} else {
			if (msync(stream->region, stream->region_size, MS_SYNC) != 0) {
				gu_raise_errno(err);
				return;
			}

			void* region = mmap(stream->region, stream->region_size,
								PROT_READ|PROT_WRITE,MAP_SHARED|MAP_FIXED,
								stream->fd, offset);
			if (region != stream->region) {
				gu_raise_errno(err);
				return;
			}
		}

		stream->n_regions++;
		stream->start = stream->region;
		stream->end   = stream->start + stream->region_size;

		*((size_t*) stream->start) = 0;
		stream->start += sizeof(size_t);
	}
}

void
em_data_stream_add_element(EMDataStream* stream, void* elem)
{
	(*((size_t*) stream->region))++;
	*((void**) stream->start) = elem;
	stream->start += sizeof(void*);
}

void*
em_data_stream_malloc(EMDataStream* stream, size_t size)
{
	if (stream->start+size > stream->end) {
		printf("em_data_stream_malloc failed (requested %ld, available %ld): increase max_elem_size\n", size, stream->end-stream->start);
		exit(1);
	}

	stream->end -= size;
	return stream->end;
}

void
em_data_stream_restart(EMDataStream* stream, size_t thread_idx, GuExn* err)
{
	if (thread_idx == 0) {
		void* region = mmap(stream->region, stream->region_size,
							PROT_READ,MAP_SHARED|MAP_FIXED,
							stream->fd, 0);
		if (region != stream->region) {
			gu_raise_errno(err);
			return;
		}

		stream->i_elem   = 0;
		stream->i_region = 0;
	}
}

void*
em_data_stream_fetch_element(EMDataStream* stream, size_t thread_idx)
{
	if (stream->region == NULL)
		return NULL;

	size_t i;
	for (;;) {
		i = __sync_fetch_and_add(&stream->i_elem, 1);
		if (i < *((size_t*) stream->region))
			break;

		pthread_barrier_wait(&stream->barrier1);

		if (thread_idx == 0) {
			stream->i_region++;
			stream->i_elem = 0;

			if (stream->i_region < stream->n_regions) {
				off_t offset = stream->i_region*stream->region_size;
				void* region = mmap(stream->region, stream->region_size,
									PROT_READ,MAP_SHARED|MAP_FIXED,
									stream->fd, offset);
				if (region != stream->region) {
					stream->region = NULL;
				}
			}
		}

		pthread_barrier_wait(&stream->barrier2);

		if (stream->i_region >= stream->n_regions)
			return NULL;
	}

	return *((void**) (stream->region+sizeof(size_t)+i*sizeof(void*)));
}

void
em_data_stream_close(EMDataStream* stream, GuExn* err)
{
	int res;

	if (stream->region != NULL) {
		if (munmap(stream->region, stream->region_size) != 0) {
			gu_raise_errno(err);
			return;
		}
	}

	pthread_barrier_destroy(&stream->barrier2);
	pthread_barrier_destroy(&stream->barrier1);

	if (close(stream->fd) != 0) {
		gu_raise_errno(err);
		return;
	}
}

