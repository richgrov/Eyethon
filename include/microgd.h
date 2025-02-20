#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>

using CFunctionCallback = int32_t(*)(const double*, uintptr_t, double*, uintptr_t*);

extern "C" {

void *gd_vm_create();

void gd_vm_destroy(void *vm);

int32_t gd_register_function(void *vm, const char *name, CFunctionCallback callback);

int32_t vm_run(void *vm, const char *code);

}  // extern "C"
