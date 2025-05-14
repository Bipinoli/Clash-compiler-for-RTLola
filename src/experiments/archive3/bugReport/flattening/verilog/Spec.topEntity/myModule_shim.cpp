#include <cstdlib>

#include <verilated.h>

#include "VmyModule.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VmyModule *top = new VmyModule;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

