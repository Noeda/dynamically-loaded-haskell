#include <stdio.h>

const char* get_tempdir( void )
{
    if ( P_tmpdir )
        return P_tmpdir;
    return "/tmp";
}

