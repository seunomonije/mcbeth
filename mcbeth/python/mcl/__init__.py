import os
import sys
import shutil

# Rough way to add to the Python path.
# TODO: find a cleaner way to implement this. You have to switch
# directories currently to find it.
for src in ["ocaml.so", "../_build/default/python/ocaml.so"]:
  if os.path.exists(src):
    shutil.copyfile(src, "mcl/ocaml.so")
sys.path.append(".")

## Enables the shared object file to be understood by Python
from ctypes import PyDLL, RTLD_GLOBAL, c_char_p

curdir = dir_path = os.path.dirname(os.path.realpath(__file__))
dll = PyDLL(f"{curdir}/ocaml.so", RTLD_GLOBAL)
argv_t = c_char_p * 2
argv = argv_t("ocaml.so".encode('utf-8'), None)
dll.caml_startup(argv)

# Import relevant files
from ZXBuilder import ZXBuilder
from CirqBuilder import StrictCirqBuilder, ValidCirqBuilder