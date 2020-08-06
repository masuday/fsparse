# fsparse

## Usage

Simply type `make` to build the library and test tools.
By default, AMD and AMDBAR (32bit and 64bit) will be avtivated.

If you want to use more libraries including METIS and MT-METIS, first build the external libraries and put the files into `libs`.
Then run `make` with a specific variable.

```Bash
# METIS 32bit
$ USE_METIS_32 make

# METIS 64bit
$ USE_METIS_64 make

# MT-METIS 32bit
$ USE_MTMETIS_32 make

# MT-METIS 64bit
$ USE_MTMETIS_64 make

# Intel MKL PARDISO 32bit (Intel Fortran Only so far)
$ USE_MKL_PARDISO_32 make
```

# License

See `LICENSE` for this library and `AMD/License.txt` for the AMD library.
