# fsparse

## Usage

Simply type `make` to build the library and test tools.
By default, AMD and AMDBAR (32bit and 64bit) will be avtivated.

If you want to use more libraries including METIS and MT-METIS, first build the external libraries and put the files into `libs`.
Then run `make` with a specific variable.

```bash
# METIS 32bit
$ USE_METIS_32=1 make

# METIS 64bit
$ USE_METIS_64=1 make

# MT-METIS 32bit
$ USE_MTMETIS_32=1 make

# MT-METIS 64bit
$ USE_MTMETIS_64=1 make

# Intel MKL PARDISO 32bit (Intel Fortran Only so far)
$ USE_MKL_PARDISO_32=1 make
```

To use Intel MKL PARDISO, you must copy `$MKLROOT/include/mkl_pardiso.fi` to `src/`.

# License

See `LICENSE` for this library and `AMD/License.txt` for the AMD library.
