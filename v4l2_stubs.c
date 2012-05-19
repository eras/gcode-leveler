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

#include <linux/videodev2.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

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
  value			(*get_frame)(struct t*, char** error);
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

  result = caml_alloc_string(buf.bytesused);
  memcpy(String_val(result), t->buffers[buf.index].start, buf.bytesused);

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

  t = malloc(sizeof(t));
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
