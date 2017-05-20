/* multiface.c: Multiface 1/128/3 handling routines
   Copyright (c) 2005,2007 Gergely Szasz

   $Id: $

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

   Author contact information:

   Gergely: szaszg@hu.inter.net

*/

#ifndef FUSE_MULTIFACE_H
#define FUSE_MULTIFACE_H

#include <libspectrum.h>

extern int multiface_activated;		/* RED BUTTON PUSHED */
extern int multiface_active;
extern int multiface_available;

int multiface_init( void );
void multiface_status_update( void );

void multiface_red_button( void );
void multiface_setic8( void );
int multiface_unittest( void );

#endif				/* #ifndef FUSE_MULTIFACE_H */
