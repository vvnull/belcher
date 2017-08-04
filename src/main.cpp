/*
  Based on:
  https://chriskohlhepp.wordpress.com/advanced-c-lisp/embedding-lisp-in-cplusplus-a-recipe/
*/

#include <iostream>
#include <cstdlib>
#include <ecl/ecl.h>

// A macro to create a DEFUN abstraction in C++
// Credit: https://gist.github.com/vwood/662109
#define DEFUN(name, fun, args) \
  cl_def_c_function(c_string_to_object(name), \
  (cl_objectfn_fixed)fun, \
  args)
 
cl_object lisp(const std::string & call) {
   return cl_safe_eval(c_string_to_object(call.c_str()), Cnil, Cnil);
}

void initialize(int argc, char *argv[]) {
 
  // Bootstrap
  cl_boot(argc, argv);
  atexit(cl_shutdown);
 
  lisp("(load \"src/lisp/belcher.lisp\")");
}
 
int main(int argc, char *argv[]) {
 
  // Bootstrap Lisp
  initialize(argc,argv);
 
  lisp("(init)");
  lisp("(meta-play)");
 
  return EXIT_SUCCESS;
}
