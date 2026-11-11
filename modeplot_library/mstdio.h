#ident	"@(#)stdio.h	1.5"	6/7/90

/* ************************************************************************
Copyright (C) 1988, by Ardent Computer Corp.
        All Rights Reserved
This program is a  trade secret of  Ardent Computer Corp. and it is not to be
reproduced, published, disclosed  to others, copied, adapted, distributed,
or displayed without the prior authorization of Ardent Computer Corp.  Licensee
agrees  to  attach  or  embed  this  Notice on all copies  of the program,
including partial copies or modified versions thereof.
**************************************************************************/

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifndef _NFILE

	/* needed to declare printf */
#include "mvarargs.h"

#define _NFILE	64

#define BUFSIZ	4096

/* buffer size for multi-character output to unbuffered files */
#define _SBFSIZ 8

typedef struct {
	int	_cnt;
	unsigned char	*_ptr;
	unsigned char	*_base;
	char	_flag;
	char	_file;
} FILE;

/*
 * _IOLBF means that a file's output will be buffered line by line
 * In addition to being flags, _IONBF, _IOLBF and _IOFBF are possible
 * values for "type" in setvbuf.
 */
#define _IOFBF		0000
#define _IOREAD		0001
#define _IOWRT		0002
#define _IONBF		0004
#define _IOMYBUF	0010
#define _IOEOF		0020
#define _IOERR		0040
#define _IOLBF		0100
#define _IORW		0200

#ifndef NULL
#define NULL		0
#endif
#ifndef EOF
#define EOF		(-1)
#endif

#define stdin		(&_iob[0])
#define stdout		(&_iob[1])
#define stderr		(&_iob[2])

#define _bufend(p)	_bufendtab[(p)->_file]
#define _bufsiz(p)	(_bufend(p) - (p)->_base)

#ifndef lint
#define getc(p)		(--(p)->_cnt < 0 ? _filbuf(p) : (int) *(p)->_ptr++)
#define putc(x, p)	(--(p)->_cnt < 0 ? \
			_flsbuf((unsigned char) (x), (p)) : \
			(int) (*(p)->_ptr++ = (unsigned char) (x)))
#define getchar()	getc(stdin)
#define putchar(x)	putc((x), stdout)
#define clearerr(p)	((void) ((p)->_flag &= ~(_IOERR | _IOEOF)))
#define feof(p)		((p)->_flag & _IOEOF)
#define ferror(p)	((p)->_flag & _IOERR)
#define fileno(p)	(p)->_file
#endif

extern FILE	_iob[_NFILE + 1];
extern FILE	*fopen(char *,char *);
extern FILE	*fdopen(int,char *);
extern FILE	*freopen(char *,char *,FILE *);
extern FILE	*popen(char *,char *);
extern FILE	*tmpfile(void);
extern long	ftell(FILE *);
extern void	rewind(FILE *);
extern void	setbuf(FILE *,void *);
extern char	*ctermid(char *);
extern char	*cuserid(char *);
extern char	*fgets(char *,int,FILE *);
extern char	*gets(char *);
extern char	*tempnam(char *,char *);
extern char	*tmpnam(char *);
extern int	fclose(FILE *);
extern int	fflush(FILE *);
extern int	fread(void *,unsigned,int,FILE *);
extern int	fwrite(void *,unsigned,int,FILE *);
extern int	fseek(FILE *,int,int);
extern int	fgetc(FILE *);
extern int	getw(FILE *);
extern int	pclose(FILE *);
extern int	vprintf(char *,va_list);
extern int	vfprintf(FILE *,char *,va_list);
extern int	vsprintf(char *,char *,va_list);
extern int	fputc(int,FILE *);
extern int	putw(int,FILE *);
extern int	puts(char *);
extern int	fputs(char *,FILE *);
extern int	scanf(char *,va_alist);
extern int	fscanf(FILE *,char *,va_alist);
extern int	sscanf(char *,char *,va_alist);
extern int	fprintf(FILE *,char *,va_alist);
extern int	sprintf(char *,char *,va_alist);
extern int	printf(char *,va_alist);
extern int	setvbuf(FILE *,void *,int,int);
extern int	system(char *);
extern int	ungetc(int,FILE *);

extern unsigned char *_bufendtab[];

#define L_ctermid	9
#define L_cuserid	9
#define P_tmpdir	"/usr/tmp/"
#define L_tmpnam	(sizeof(P_tmpdir) + 15)
#endif
