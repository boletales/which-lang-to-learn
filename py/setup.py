from distutils.core import setup
from Cython.Build import cythonize
import numpy

setup(
    name = 'cylib',
    ext_modules = cythonize('cylib.pyx'),
    include_dirs = [numpy.get_include()]
    )
