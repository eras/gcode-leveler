/* pretty much fully based on http://v4l2spec.bytesex.org/spec/capture-example.html */

#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <stdio.h>
#include <malloc.h>
#include <sys/mman.h>
#include <assert.h>
#include <string.h>
#include <setjmp.h>

#include <jpeglib.h>
#include <linux/videodev2.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

struct buffer {
  void*  start;
  size_t length;
};

struct t {
  int			fd;
  int			image_size;
  int			n_buffers;
  int			buffer_size;
  struct buffer*	buffers;
  
  char*			(*start)(struct t*);
  char*			(*stop)(struct t*);
  char*			(*done)(struct t*);
  value			(*get_frame)(struct t*, char** error);
};

static JHUFF_TBL dc_huff_tables[2] = {
  { bits : { 0, 0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0 },
    huffval: { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 } },
  { bits : { 0, 0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0 },
    huffval : { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 } }
};

static JHUFF_TBL ac_huff_tables[2] = {
  { bits : { 0, 0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, 0x7d },
    huffval : { 0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05, 0x12,
		0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61, 0x07,
		0x22, 0x71, 0x14, 0x32, 0x81, 0x91, 0xa1, 0x08,
		0x23, 0x42, 0xb1, 0xc1, 0x15, 0x52, 0xd1, 0xf0,
		0x24, 0x33, 0x62, 0x72, 0x82, 0x09, 0x0a, 0x16,
		0x17, 0x18, 0x19, 0x1a, 0x25, 0x26, 0x27, 0x28,
		0x29, 0x2a, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
		0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
		0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
		0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
		0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
		0x7a, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
		0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98,
		0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
		0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6,
		0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3, 0xc4, 0xc5,
		0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2, 0xd3, 0xd4,
		0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xe1, 0xe2,
		0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea,
		0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
		0xf9, 0xfa } },
  { bits : { 0, 0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, 0x77 },
    huffval : { 0x00, 0x01, 0x02, 0x03, 0x11, 0x04, 0x05, 0x21,
		0x31, 0x06, 0x12, 0x41, 0x51, 0x07, 0x61, 0x71,
		0x13, 0x22, 0x32, 0x81, 0x08, 0x14, 0x42, 0x91,
		0xa1, 0xb1, 0xc1, 0x09, 0x23, 0x33, 0x52, 0xf0,
		0x15, 0x62, 0x72, 0xd1, 0x0a, 0x16, 0x24, 0x34,
		0xe1, 0x25, 0xf1, 0x17, 0x18, 0x19, 0x1a, 0x26,
		0x27, 0x28, 0x29, 0x2a, 0x35, 0x36, 0x37, 0x38,
		0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
		0x49, 0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
		0x59, 0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68,
		0x69, 0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
		0x79, 0x7a, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
		0x88, 0x89, 0x8a, 0x92, 0x93, 0x94, 0x95, 0x96,
		0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5,
		0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4,
		0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3,
		0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2,
		0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda,
		0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9,
		0xea, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
		0xf9, 0xfa } },
};

static int
xioctl(int fd,
       int request,
       void *arg)
{
  int r;

  do r = ioctl (fd, request, arg);
  while (-1 == r && EINTR == errno);

  return r;
}

static char*
start_mmap(struct t* t)
{
  int i;
  for (i = 0; i < t->n_buffers; ++i) {
    struct v4l2_buffer buf = { 0 };

    buf.type        = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory      = V4L2_MEMORY_MMAP;
    buf.index       = i;

    if (xioctl(t->fd, VIDIOC_QBUF, &buf) == -1)
      return "start_mmap: cannot query video buffers\n";
  }
                
  enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

  if (xioctl(t->fd, VIDIOC_STREAMON, &type) == -1)
    return "start_mmap: cannot start stream monitor";

  return NULL;
}

static char*
stop_mmap(struct t* t)
{
  enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

  if (xioctl(t->fd, VIDIOC_STREAMOFF, &type) == -1)
    return "stop_mmap: cannot stop stream";

  return NULL;
}

static char*
done_mmap(struct t* t)
{
  int i;
  for (i = 0; i < t->n_buffers; ++i) {
    if (munmap(t->buffers[i].start, t->buffers[i].length) == -1)
      return "done_mmap: cannot munmap";
  }

  return NULL;
}

static value
read_mmap(struct t* t, char** error)
{
  CAMLparam0();
  CAMLlocal1(result);
  struct v4l2_buffer buf = { 0 };

  buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  buf.memory = V4L2_MEMORY_MMAP;

  if (xioctl(t->fd, VIDIOC_DQBUF, &buf) == -1) {
    switch (errno) {
    case EAGAIN:
      return 0;

    case EIO:
      /* Could ignore EIO, see spec. */

      /* fall through */

    default:
      *error = "get_frame: cannot dequeue buffer";
    }
  }

  assert (buf.index < t->n_buffers);

  result = alloc_bigarray_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, 
			       NULL, buf.bytesused);
  memcpy((void*) Data_bigarray_val(result), t->buffers[buf.index].start, buf.bytesused);

  if (xioctl(t->fd, VIDIOC_QBUF, &buf) == -1)
    *error = "get_frame: cannot enqueue buffer";

  CAMLreturn(result);
}

static char*
init_mmap(struct t* t)
{
  struct v4l2_requestbuffers req = { 0 };

  req.count  = 4;
  req.type   = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  req.memory = V4L2_MEMORY_MMAP;

  if (xioctl (t->fd, VIDIOC_REQBUFS, &req) == -1) {
    if (EINVAL == errno) {
      return "v4l2_open/init_mmap: device does not support memory mapping";
    } else {
      return "v4l2_open/init_mmap: cannot request buffers";
    }
  }

  if (req.count < 2) {
    return "v4l2_open/init_mmap: insufficient buffer memory on device";
  }

  t->buffers = calloc (req.count, sizeof (*t->buffers));

  if (!t->buffers) {
    return "v4l2_open/init_mmap: out of memory";
  }

  for (t->n_buffers = 0; t->n_buffers < req.count; ++t->n_buffers) {
    struct v4l2_buffer buf = { 0 };

    buf.type        = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory      = V4L2_MEMORY_MMAP;
    buf.index       = t->n_buffers;

    if (xioctl (t->fd, VIDIOC_QUERYBUF, &buf) == -1)
      return "v4l2_open/init_mmap: cannot query buffers";

    t->buffers[t->n_buffers].length = buf.length;
    t->buffers[t->n_buffers].start =
      mmap (NULL /* start anywhere */,
	    buf.length,
	    PROT_READ | PROT_WRITE /* required */,
	    MAP_SHARED /* recommended */,
	    t->fd, buf.m.offset);

    if (MAP_FAILED == t->buffers[t->n_buffers].start)
      return "v4l2_open/init_mmap; cannot mmap";
  }

  return NULL;
}

#if 0
static char*
init_userp(struct t* t)
{
  struct v4l2_requestbuffers req = { 0 };
  unsigned int page_size;

  page_size = getpagesize ();
  t->buffer_size = (t->image_size + page_size - 1) & ~(page_size - 1);

  req.count               = 4;
  req.type                = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  req.memory              = V4L2_MEMORY_USERPTR;

  if (-1 == xioctl (t->fd, VIDIOC_REQBUFS, &req)) {
    if (EINVAL == errno) {
      return "caml_open/init_userp: device does not support user pointer i/o";
    } else {
      return "caml_open/init_userp: failed to request buffers";
    }
  }

  t->buffers = calloc (4, sizeof (*t->buffers));

  if (!t->buffers) {
    return "caml_open/init_userp: out of memory";
  }

  for (t->n_buffers = 0; t->n_buffers < 4; ++t->n_buffers) {
    t->buffers[t->n_buffers].length = t->buffer_size;
    t->buffers[t->n_buffers].start = memalign (/* boundary */ page_size,
					    t->buffer_size);
    
    if (!t->buffers[t->n_buffers].start) {
      return "caml_open/init_userp: out of memory";
    }
  }

  return NULL;
}
#endif

value
v4l2_open(value name, value width, value height)
{
  CAMLparam3(name, width, height);
  const char* msg = NULL;

  int fd = open(String_val(name), O_RDWR);
  struct t* t = NULL;
  if (fd < 0) {
    msg = "v4l2_open: cannot open device";
    goto cleanup;
  }

  t = malloc(sizeof(*t));
  if (!t) {
    msg = "v4l2_open: cannot allocate memory";
    goto cleanup;
  }
  t->fd = fd;

  struct v4l2_capability cap;
  if (xioctl(fd, VIDIOC_QUERYCAP, &cap) == -1) {
    if (errno == EINVAL) {
      msg = "v4l2_open: cannot allocate memory";
    } else {
      msg = "v4l2_open: cannot query capabilities";
    }
    goto cleanup;
  }

  if (!(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE)) {
    msg = "v4l2_open: device is not a video capture device";
    goto cleanup;
  }

  if (!(cap.capabilities & V4L2_CAP_STREAMING)) {
    msg = "v4l2_open: device does not support streaming IO";
    goto cleanup;
  }

  struct v4l2_cropcap cropcap;
  struct v4l2_crop crop;

  cropcap.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

  if (xioctl(fd, VIDIOC_CROPCAP, &cropcap) == 0) {
    crop.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    crop.c = cropcap.defrect; /* reset to default */

    if (xioctl(fd, VIDIOC_S_CROP, &crop) == -1) {
      switch (errno) {
      case EINVAL:
	/* Cropping not supported. */
	break;
      default:
	/* Errors ignored. */
	break;
      }
    }
  } else {        
    /* Errors ignored. */
  }

  struct v4l2_format fmt = { 0 };
  fmt.type                = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  fmt.fmt.pix.width       = width; 
  fmt.fmt.pix.height      = height;
  fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_MJPEG;
  fmt.fmt.pix.field       = V4L2_FIELD_ANY;

  if (xioctl(fd, VIDIOC_S_FMT, &fmt) == -1) {
    msg = "v4l2_open: cannot setup video format";
    goto cleanup;
  }

  t->image_size = fmt.fmt.pix.sizeimage;

  msg = init_mmap(t);
  if (msg) {
    goto cleanup;
  }

  t->start = start_mmap;
  t->stop = stop_mmap;
  t->done = done_mmap;
  t->get_frame = read_mmap;

  CAMLreturn((size_t) t);

 cleanup:
  if (fd >= 0) {
    close(fd);
  }
  free(t);
  caml_failwith(msg);
  return 0;
}

static void
v4l2_jpeg_init_source(j_decompress_ptr dec)
{
  (void) dec;
}

static boolean
v4l2_jpeg_fill_input_buffer(j_decompress_ptr dec)
{
  jpeg_abort_decompress(dec);
  return 0;
}

static void
v4l2_jpeg_skip_input_data(j_decompress_ptr dec, long ofs)
{
  dec->src->next_input_byte += ofs;
}

static boolean
v4l2_jpeg_resync_to_restart(j_decompress_ptr dec, int ofs)
{
  (void) dec;
  (void) ofs;
  return 0;
}

static void
v4l2_jpeg_term_source(j_decompress_ptr dec)
{
  (void) dec;
}

struct custom_jpeg_decompress_struct {
  struct jpeg_decompress_struct jpeg;
  jmp_buf decode_env;
};

static void
v4l2_jpeg_error_exit(j_common_ptr cinfo)
{
  struct custom_jpeg_decompress_struct* custom_dec = (void*) cinfo;
  longjmp(custom_dec->decode_env, 1);
}

value
v4l2_decode_frame(value frame)
{
  CAMLparam1(frame);
  CAMLlocal1(result);

  struct jpeg_source_mgr src;
  void* orig = Data_bigarray_val(frame);
  src.next_input_byte	= orig;
  src.bytes_in_buffer	= Caml_ba_array_val(frame)->dim[0];
  src.init_source	= v4l2_jpeg_init_source;
  src.fill_input_buffer = &v4l2_jpeg_fill_input_buffer;
  src.skip_input_data	= &v4l2_jpeg_skip_input_data;
  src.resync_to_restart	= &v4l2_jpeg_resync_to_restart;
  src.term_source	= &v4l2_jpeg_term_source;

  struct custom_jpeg_decompress_struct custom_dec = {
    jpeg : { 0 }
  };
  struct jpeg_decompress_struct* dec = &custom_dec.jpeg;
  jpeg_create_decompress(dec);
  struct jpeg_error_mgr error;
  dec->err = jpeg_std_error(&error);
  dec->err->error_exit = &v4l2_jpeg_error_exit;
  dec->src = &src;

  if (setjmp(custom_dec.decode_env) == 0) {
    jpeg_read_header(dec, TRUE);

    dec->dc_huff_tbl_ptrs[0] = &dc_huff_tables[0];
    dec->dc_huff_tbl_ptrs[1] = &dc_huff_tables[1];
    dec->ac_huff_tbl_ptrs[0] = &ac_huff_tables[0];
    dec->ac_huff_tbl_ptrs[1] = &ac_huff_tables[1];

    jpeg_start_decompress(dec);

    int size = dec->output_width * dec->output_height * 3;
    int pitch = dec->output_width * 3;
    result = alloc_bigarray_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1,
				 NULL, size);
    JSAMPLE* begin = (void*) Data_bigarray_val(result);
    JSAMPLE* buffer = begin;
    const JSAMPLE* end = (void*) (((char*) Data_bigarray_val(result)) + size);

    if (setjmp(custom_dec.decode_env) == 0) {
      while (dec->output_scanline < dec->output_height) {
	assert(buffer + pitch <= end);
	jpeg_read_scanlines(dec, &buffer, 1);
	buffer += pitch;
      }
      jpeg_finish_decompress(dec);
    } else {
      // uh oh. well, just keep on going and zero the rest.
      printf("decoding error\n");
      while (buffer < end) {
	*buffer = 0;
	++buffer;
      }
    }
  } else {
    // uh oh 2
    printf("header decoding error\n");

    result = alloc_bigarray_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1,
				 NULL, 0);
  }

  jpeg_destroy_decompress(dec);

  CAMLreturn(result);
}

value
v4l2_done(value t)
{
  CAMLparam1(t);
  struct t* t_ = (struct t*) t;
  char* msg = t_->done(t_);
  close(t_->fd);
  if (msg) {
    caml_failwith(msg);
  }
  CAMLreturn(0);
}

value
v4l2_start(value t)
{
  CAMLparam1(t);
  struct t* t_ = (struct t*) t;
  char* msg = t_->start(t_);
  if (msg) {
    caml_failwith(msg);
  }
  CAMLreturn(0);
}

value
v4l2_stop(value t)
{
  CAMLparam1(t);
  struct t* t_ = (struct t*) t;
  char* msg = t_->stop(t_);
  if (msg) {
    caml_failwith(msg);
  }
  CAMLreturn(0);
}

value
v4l2_get_frame(value t)
{
  CAMLparam1(t);
  CAMLlocal1(result);
  struct t* t_ = (struct t*) t;
  char* msg = NULL;
  result = t_->get_frame(t_, &msg);
  if (msg) {
    caml_failwith(msg);
  }
  CAMLreturn(result);
}
