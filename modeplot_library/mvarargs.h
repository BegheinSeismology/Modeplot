#ident	"@(#)varargs.h	1.5"	1/7/91

/* ************************************************************************
Copyright (C) 1988, by Ardent Computer Corp.
        All Rights Reserved
This program is a  trade secret of  Ardent Computer Corp. and it is not to be
reproduced, published, disclosed  to others, copied, adapted, distributed,
or displayed without the prior authorization of Ardent Computer Corp.  Licensee
agrees  to  attach  or  embed  this  Notice on all copies  of the program,
including partial copies or modified versions thereof.
**************************************************************************/

/******************************************************************************/
/*       COPYRIGHT (C)  DANA COMPUTER, INC. 1987                              *//******************************************************************************/
#ifndef va_alist

#define va_dcl
#define va_end(x)
#define va_alist ...

/*	this relies on two special pragmas implemented in the front end */
/*	_pragma_(-1) returns the address of the first argument */
/*	_pragma_(-2) returns the number of (presumed integer) arguments
		that precede the ellipsis */
/*	if I had it to do over again, I'd consider making &... legal in
		the front end... */

#define va_list struct { char __ni,__nf; short __off; int *__p; }

#define va_start(x) {x.__ni=_pragma_(-2);x.__nf=0;x.__off=4; \
						x.__p=(int *)_pragma_(-1); }

	/* eo is 1 if the offset is 4 mod 8, 0 otherwise */
	/* since this has ellipsis, no float arguments (whew!) */

	/* the & of the first arg causes the registers to be stored */

#define va_int(x) ( (x).__ni<4 ? &(x).__p[(x).__ni++] : &(x).__p[(x).__off++] )
#define va_bump(x) (((x).__off += ((x).__off&1)+2), (&(x).__p[(x).__off-2 ]) )

#if defined(__P3__) || defined(__STILETTO__)
#define va_dbl(x) ( (x).__nf<2 ? &(x).__p[-8+2*(x).__nf++] : (va_bump(x)) )
#else
#define va_dbl(x) ( (x).__nf<4 ? &(x).__p[-8+2*(x).__nf++] : (va_bump(x)) )
#endif

#define va_arg(x,ty)  ( *( ty *) ( sizeof(ty)==4 ? va_int(x) : va_dbl(x)) )

	/* pointer version returns pointer to next arg */
#define va_argp(x,ty)  ( ( ty *) ( sizeof(ty)==4 ? va_int(x) : va_dbl(x)) )

	/* va_next is like va_arg, but doesn't work with floats */
	/* consequently, it is faster */
#define va_next(x,ty) ( *(ty *) va_int(x) )

	/* va_nextp is the pointer version of va_next */
#define va_nextp(x,ty) ( (ty *) va_int(x) )

	/* va_doublep is used when #pragma NO_FLOAT has been used  */
	/* it gets the double precision values from their secret locations */
	/* it exists only in the pointer form */

double *va_sto_FPU(int *);
#define va_doublep(x) ( (x).__nf==0 ? (++(x).__nf,va_sto_FPU((x).__p)) : (double *) va_dbl(x) )

#endif
