/*
 * Copyright (C) 2003 David Roundy
 * Most of the UTF code is Copyright (C) 1999-2001 Free Software Foundation, Inc.
 * This file is part of darcs.
 *
 * Darcs is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the GNU LIBICONV Library; see the file COPYING.LIB.
 * If not, write to the Free Software Foundation, Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "fpstring.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>



#ifdef _WIN32
#include <windows.h>
#else
#include <sys/mman.h>
#endif

int has_funky_char(const char *s, int len)
{
  // We check first for the more likely \0 so we can break out of
  // memchr that much sooner.
  return !!(memchr(s, 0, len) || memchr(s, 26, len));

}
