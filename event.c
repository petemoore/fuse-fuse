/* event.c: Routines needed for dealing with the event list
   Copyright (c) 2000-2003 Philip Kendall

   $Id$

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

   E-mail: pak21-fuse@srcf.ucam.org
   Postal address: 15 Crescent Road, Wokingham, Berks, RG40 2DB, England

*/

#include <config.h>

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_LIB_GLIB
#include <glib.h>
#endif				/* #ifdef HAVE_LIB_GLIB */

#include <libspectrum.h>

#include "display.h"
#include "event.h"
#include "fuse.h"
#include "machine.h"
#include "psg.h"
#include "rzx.h"
#include "tape.h"
#include "trdos.h"
#include "ui/ui.h"
#include "spectrum.h"
#include "z80/z80.h"

/* A large value to mean `no events due' */
const libspectrum_dword event_no_events = 0xffffffff;

/* When will the next event happen? */
libspectrum_dword event_next_event;

/* The actual list of events */
static GSList* event_list;

/* Comparision function so events stay in t-state order */
static gint event_add_cmp( gconstpointer a, gconstpointer b );

/* User function for event_interrupt(...) */
void event_reduce_tstates(gpointer data,gpointer user_data);

/* Make the event have no effect if it matches the given type */
static void set_event_null( gpointer data, gpointer user_data );

/* Free the memory used by a specific entry */
void event_free_entry(gpointer data, gpointer user_data);

/* Force events between now and the next interrupt to happen */
static int event_force_events( void );

/* Set up the event list */
int event_init(void)
{
  event_list=NULL;
  event_next_event=event_no_events;
  return 0;
}

/* Add an event at the correct place in the event list */
int
event_add( libspectrum_dword event_time, int type )
{
  event_t *ptr;

  ptr=(event_t*)malloc(sizeof(event_t));
  if(!ptr) return 1;

  ptr->tstates= event_time;
  ptr->type=type;

  event_list=g_slist_insert_sorted(event_list,(gpointer)ptr,event_add_cmp);

  if( event_time < event_next_event ) event_next_event = event_time;

  return 0;
}

/* Comparision function so events stay in t-state and event type order */
static gint
event_add_cmp( gconstpointer a1, gconstpointer b1 )
{
  const event_t *a = a1, *b = b1;

  return a->tstates != b->tstates ? a->tstates - b->tstates
		                  : a->type - b->type;
}

/* Do all events which have passed */
int event_do_events(void)
{
  event_t *ptr;

  while(event_next_event <= tstates) {
    ptr= ( (event_t*) (event_list->data) );

    /* Remove the event from the list *before* processing */
    event_list=g_slist_remove(event_list,ptr);

    if( event_list == NULL ) {
      event_next_event = event_no_events;
    } else {
      event_next_event= ( (event_t*) (event_list->data) ) -> tstates;
    }

    switch(ptr->type) {

    case EVENT_TYPE_EDGE: tape_next_edge( ptr->tstates ); break;

    case EVENT_TYPE_ENABLE_INTERRUPTS: z80_enable_interrupts(); break;

    case EVENT_TYPE_INTERRUPT:
      if( rzx_playback ) event_force_events();
      rzx_frame();
      psg_frame();
      spectrum_interrupt();
      ui_event();
      break;

    case EVENT_TYPE_LINE: display_line(); break;
    case EVENT_TYPE_NMI: z80_nmi(); break;
    case EVENT_TYPE_NULL: /* Do nothing */ break;

    case EVENT_TYPE_TRDOS_CMD_DONE:
      trdos_event_cmd_done( ptr->tstates );
      break;

    case EVENT_TYPE_TRDOS_INDEX: trdos_event_index( ptr->tstates ); break;

    default:
      ui_error( UI_ERROR_ERROR, "unknown event type %d", ptr->type );
      break;
    }
    free(ptr);
  }

  return 0;
}

/* Called on interrupt to reduce T-state count of all entries */
int
event_interrupt( libspectrum_dword tstates_per_frame )
{
  g_slist_foreach(event_list, event_reduce_tstates, &tstates_per_frame );

  if( event_list == NULL ) {
    event_next_event = event_no_events;
  } else {
    event_next_event= ( (event_t*) (event_list->data) ) -> tstates;
  }

  return 0;
}

/* User function for event_interrupt(...) */
void event_reduce_tstates(gpointer data,gpointer user_data)
{
  event_t *ptr=(event_t*)data;
  libspectrum_dword *tstates_per_frame = (libspectrum_dword*)user_data;

  ptr->tstates -= (*tstates_per_frame) ;
}

/* Remove all events of a specific type from the stack */
int event_remove_type( int type )
{
  /* FIXME: this is an ugly hack. Just set all events of the given
     type to be of a null type, meaning they do nothing */
  g_slist_foreach( event_list, set_event_null, &type );
  return 0;
}

static void set_event_null( gpointer data, gpointer user_data )
{
  event_t *ptr = (event_t*)data;
  int type = *(int*)user_data;

  if( ptr->type == type ) ptr->type = EVENT_TYPE_NULL;
}

/* Clear the event stack */
int event_reset(void)
{
  g_slist_foreach(event_list,event_free_entry,NULL);
  g_slist_free(event_list);
  event_list=NULL;
  event_next_event=event_no_events;
  return 0;
}

/* Free the memory used by a specific entry */
void
event_free_entry( gpointer data, gpointer user_data GCC_UNUSED )
{
  event_t *ptr=(event_t*)data;
  free(ptr);
}

/* Do all events that would happen between the current time and when
   the next interrupt will occur; called only when RZX playback is in
   effect */
static int event_force_events( void )
{
  while( event_next_event < machine_current->timings.tstates_per_frame ) {

    /* Jump along to the next event */
    tstates = event_next_event;
    
    /* And do that event */
    event_do_events();

  }

  /* Finally, jump to the interrupt time */
  tstates = machine_current->timings.tstates_per_frame;

  return 0;
}

/* Tidy-up function called at end of emulation */
int event_end(void)
{
  return event_reset();
}
