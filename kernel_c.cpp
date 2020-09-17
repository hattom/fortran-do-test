extern "C" {
  void c_inner_sub(float *elem, const int *id) {
    *elem = 0.5 * (*elem + (float)*id / *elem);
  }
}
