#ifndef lint
static char *RCSid = "$Header: rmtlib.c,v 1.2 86/10/09 16:38:53 root Locked $";
#endif

/*
 * $Log:	rmtlib.c,v $
 * Revision 1.2  86/10/09  16:38:53  root
 * Changed to reflect 4.3BSD rcp syntax. ADR.
 * 
 * Revision 1.1  86/10/09  16:17:35  root
 * Initial revision
 * 
 */

/*
 *	rmt --- remote tape emulator subroutines
 *
 *	Originally written by Jeff Lee, modified some by Arnold Robbins
 *
 *	WARNING:  The man page rmt(8) for /etc/rmt documents the remote mag
 *	tape protocol which rdump and rrestore use.  Unfortunately, the man
 *	page is *WRONG*.  The author of the routines I'm including originally
 *	wrote his code just based on the man page, and it didn't work, so he
 *	went to the rdump source to figure out why.  The only thing he had to
 *	change was to check for the 'F' return code in addition to the 'E',
 *	and to separate the various arguments with \n instead of a space.  I
 *	personally don't think that this is much of a problem, but I wanted to
 *	point it out.
 *	-- Arnold Robbins
 *
 *	Redone as a library that can replace open, read, write, etc, by
 *	Fred Fish, with some additional work by Arnold Robbins.
 */
 
/*
 *	MAXUNIT --- Maximum number of remote tape file units
 *
 *	READ --- Return the number of the read side file descriptor
 *	WRITE --- Return the number of the write side file descriptor
 */

#define RMTIOCTL	1

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef RMTIOCTL
#include <sys/ioctl.h>
#include <sys/mtio.h>
#endif

#include <errno.h>
#include <setjmp.h>
#include <sys/stat.h>

#define BUFMAGIC	64	/* a magic number for buffer sizes */
#define MAXUNIT	4

#define READ(fd)	(Ctp[fd][0])
#define WRITE(fd)	(Ptc[fd][1])

static int Ctp[MAXUNIT][2] = { -1, -1, -1, -1, -1, -1, -1, -1 };
static int Ptc[MAXUNIT][2] = { -1, -1, -1, -1, -1, -1, -1, -1 };

static jmp_buf Jmpbuf;
extern int errno;

/*
 *	abort --- close off a remote tape connection
 */

static void abort(fildes)
int fildes;
{
	close(READ(fildes));
	close(WRITE(fildes));
	READ(fildes) = -1;
	WRITE(fildes) = -1;
}



/*
 *	command --- attempt to perform a remote tape command
 */

static int command(fildes, buf)
int fildes;
char *buf;
{
	register int blen;
	int (*pstat)();
	char *cp ; int knt;
/*
 *	save current pipe status and try to make the request
 */

	blen = strlen(buf);
	pstat = signal(SIGPIPE, SIG_IGN);
/*	printf("%c ",*buf);
	for (cp=buf , knt=0 ; knt < blen ; cp++ , knt++)
		printf(" %d",*cp);
	printf("\n command length: %d\n",blen); */

	if (write(WRITE(fildes), buf, blen) == blen)
	{
		signal(SIGPIPE, pstat);
		return(0);
	}

/*
 *	something went wrong. close down and go home
 */

	signal(SIGPIPE, pstat);
	abort(fildes);

	errno = EIO;
	return(-1);
}



/*
 *	status --- retrieve the status from the pipe
 */

static int status(fildes)
int fildes;
{
	int i;
	char c, *cp;
	char buffer[BUFMAGIC];

/*
 *	read the reply command line
 */

	for (i = 0, cp = buffer; i < BUFMAGIC; i++, cp++)
	{
		if (read(READ(fildes), cp, 1) != 1)
		{
			abort(fildes);
			errno = EIO;
			return(-1);
		}
		if (*cp == '\n')
		{
			*cp = 0;
			break;
		}
	}

	if (i == BUFMAGIC)
	{
		abort(fildes);
		errno = EIO;
		return(-1);
	}

/*
 *	check the return status
 */

	for (cp = buffer; *cp; cp++)
		if (*cp != ' ')
			break;

	if (*cp == 'E' || *cp == 'F')
	{
		errno = atoi(cp + 1);
		while (read(READ(fildes), &c, 1) == 1)
			if (c == '\n')
				break;

		if (*cp == 'F')
			abort(fildes);

		return(-1);
	}

/*
 *	check for mis-synced pipes
 */

	if (*cp != 'A')
	{
		abort(fildes);
		errno = EIO;
		return(-1);
	}

	return(atoi(cp + 1));
}



/*
 *	_rmt_open --- open a magtape device on system specified, as given user
 *
 *	file name has the form system[.user]:/dev/????
 */

#define MAXHOSTLEN	257	/* BSD allows very long host names... */

static int _rmt_open (path, oflag, mode)
char *path;
int oflag;
int mode;
{
	int i, rc;
	char buffer[BUFMAGIC];
	char system[MAXHOSTLEN];
	char device[BUFMAGIC];
	char login[BUFMAGIC];
	char *sys, *dev, *user;
	int ii;

	sys = system;
	dev = device;
	user = login;

/*
 *	first, find an open pair of file descriptors
 */

	for (i = 0; i < MAXUNIT; i++)
		if (READ(i) == -1 && WRITE(i) == -1)
			break;

	if (i == MAXUNIT)
	{
		errno = EMFILE;
		return(-1);
	}

/*
 *	pull apart system and device, and optional user
 *	don't munge original string
 *	handle both old host.person and new person@site notations
 */
	while (*path != '@' && *path != '.' && *path != ':') {
		*sys++ = *path++;
	}
	*sys = '\0';
	path++;

	if (*(path - 1) == '@')
	{
		(void) strcpy (user, sys);	/* saw user part of user@host */
		sys = system;			/* start over */
		while (*path != ':') {
			*sys++ = *path++;
		}
		*sys = '\0';
		path++;
	}
	else if (*(path - 1) == '.')
	{
		while (*path != ':') {
			*user++ = *path++;
		}
		*user = '\0';
		path++;
	}
	else
		*user = '\0';

	while (*path) {
		*dev++ = *path++;
	}
	*dev = '\0';

/*
 *	setup the pipes for the 'rsh' command and fork
 */

	if (pipe(Ptc[i]) == -1 || pipe(Ctp[i]) == -1)
		return(-1);


	if ((rc = fork()) == -1)
		return(-1);

	if (rc == 0)
	{
		char *rmtoas; extern char *getenv();
		rmtoas = getenv("ROAS_PATH");
		if( rmtoas == (char *)0 ) rmtoas = "/usr/local/bin/rmtoas" ;
		close(0);
		dup(Ptc[i][0]);
		close(Ptc[i][0]); close(Ptc[i][1]);
		close(1);
		dup(Ctp[i][1]);
		close(Ctp[i][0]); close(Ctp[i][1]);
		(void) setuid (getuid ());
		(void) setgid (getgid ());
		if (*user)
		{
			execl("/usr/ucb/rsh", "rsh", system, "-l", login,
				rmtoas, (char *) 0);
			execl("/usr/bin/remsh", "remsh", system, "-l", login,
				rmtoas, (char *) 0);
		}
		else
		{
			execl("/usr/ucb/rsh", "rsh", system,
				rmtoas, (char *) 0);
			execl("/usr/bin/remsh", "remsh", system,
				rmtoas, (char *) 0);
		}

/*
 *	bad problems if we get here
 */

		perror("exec");
		exit(1);
	}

	close(Ptc[i][0]); close(Ctp[i][1]);

/*
 *	now attempt to open the tape device
 */

	sprintf(buffer, "O%s\n%d\n%d\n", device, oflag,mode);
	if (command(i, buffer) == -1 || (ii=status(i)) == -1)
	{
	 	int (*pstat)();
		pstat = signal(SIGPIPE, SIG_IGN);
		signal(SIGPIPE, pstat);
		abort(i);
		return(-1);
	}
	return(i);
}



/*
 *	_rmt_close --- close a remote magtape unit and shut down
 */

static int _rmt_close(fildes)
int fildes;
{
	int rc;

	if (command(fildes, "C\n") != -1)
	{
	 	int (*pstat)();
		rc = status(fildes);

		pstat = signal(SIGPIPE, SIG_IGN);
		signal(SIGPIPE, pstat);
		abort(fildes);
		return(rc);
	}

	return(-1);
}



/*
 *	_rmt_read --- read a buffer from a remote tape
 */

static int _rmt_read(fildes, buf, nbyte)
int fildes;
char *buf;
unsigned int nbyte;
{
	int rc, i;
	char buffer[BUFMAGIC];

	sprintf(buffer, "R%d\n", nbyte);
	if (command(fildes, buffer) == -1 || (rc = status(fildes)) == -1)
		return(-1);

	for (i = 0; i < rc; i += nbyte, buf += nbyte)
	{
		nbyte = read(READ(fildes), buf, rc);
		if (nbyte <= 0)
		{
			abort(fildes);
			errno = EIO;
			return(-1);
		}
	}

	return(rc);
}



/*
 *	_rmt_write --- write a buffer to the remote tape
 */

static int _rmt_write(fildes, buf, nbyte)
int fildes;
char *buf;
unsigned int nbyte;
{
	int rc;
	char buffer[BUFMAGIC];
	int (*pstat)();

	sprintf(buffer, "W%d\n", nbyte);
	if (command(fildes, buffer) == -1)
		return(-1);

	pstat = signal(SIGPIPE, SIG_IGN);
	if (write(WRITE(fildes), buf, nbyte) == nbyte)
	{
		signal (SIGPIPE, pstat);
		return(status(fildes));
	}

	signal (SIGPIPE, pstat);
	abort(fildes);
	errno = EIO;
	return(-1);
}



/*
 *	_rmt_lseek --- perform an imitation lseek operation remotely
 */

static long _rmt_lseek(fildes, offset, whence)
int fildes;
long offset;
int whence;
{
	char buffer[BUFMAGIC];

	sprintf(buffer, "L%d\n%d\n", offset, whence);
	if (command(fildes, buffer) == -1)
		return(-1);

	return(status(fildes));
}


/*
 *	_rmt_ioctl --- perform raw tape operations remotely
 */

static _rmt_mtio(fildes, op, knt)
int fildes, op, knt;
{
	char buffer[BUFMAGIC];

	sprintf(buffer, "I%d\n%d\n", op, knt);
	if (command(fildes, buffer) == -1)
		return(-1);
	return(status(fildes));

}

/*
 *	Added routines to replace open(), close(), lseek(), ioctl(), etc.
 *	The preprocessor can be used to remap these the rmtopen(), etc
 *	thus minimizing source changes:
 *
 *		#ifdef <something>
 *		#  define access rmtaccess
 *		#  define close rmtclose
 *		#  define creat rmtcreat
 *		#  define dup rmtdup
 *		#  define fcntl rmtfcntl
 *		#  define fstat rmtfstat
 *		#  define ioctl rmtioctl
 *		#  define isatty rmtisatty
 *		#  define lseek rmtlseek
 *		#  define lstat rmtlstat
 *		#  define open rmtopen
 *		#  define read rmtread
 *		#  define stat rmtstat
 *		#  define write rmtwrite
 *		#  define access rmtaccess
 *		#  define close rmtclose
 *		#  define creat rmtcreat
 *		#  define dup rmtdup
 *		#  define fcntl rmtfcntl
 *		#  define fstat rmtfstat
 *		#  define ioctl rmtioctl
 *		#  define lseek rmtlseek
 *		#  define open rmtopen
 *		#  define read rmtread
 *		#  define stat rmtstat
 *		#  define write rmtwrite
 *		#endif
 *
 *	-- Fred Fish
 *
 *	ADR --- I set up a <rmt.h> include file for this
 *
 */

/*
 *	Note that local vs remote file descriptors are distinquished
 *	by adding a bias to the remote descriptors.  This is a quick
 *	and dirty trick that may not be portable to some systems.
 */

#define REM_BIAS 128


/*
 *	Test pathname to see if it is local or remote.  A remote device
 *	is any string that contains ":/dev/".  Returns 1 if remote,
 *	0 otherwise.
 */
 
static int remdev (path)
register char *path;
{
/*  #define strchr	index
	extern char *strchr ();

	if ((path = strchr (path, ':')) != NULL) */
	for ( ; *path != ':' & *path != '\0' ; path++ );
	if( *path != '\0' )
	{
		if (strncmp (path + 1, "/dev/", /* 5 */ 1) == 0) 
		{  
			return (1);         
		}     
	}
	return (0);
}


/*
 *	Open a local or remote file.  Looks just like open(2) to
 *	caller.
 */
 
int rmtopen (path, oflag, mode)
char *path;
int oflag;
int mode;
{
	if (remdev (path))
	{
		return (_rmt_open (path, oflag, mode) + REM_BIAS);
	}
	else
	{
		return (open (path, oflag, mode));
	}
}

/*
 *	Test pathname for specified access.  Looks just like access(2)
 *	to caller.
 */
 
int rmtaccess (path, amode)
char *path;
int amode;
{
	if (remdev (path))
	{
		return (0);		/* Let /etc/rmt find out */
	}
	else
	{
		return (access (path, amode));
	}
}


/*
 *	Read from stream.  Looks just like read(2) to caller.
 */
  
int rmtread (fildes, buf, nbyte)
int fildes;
char *buf;
unsigned int nbyte;
{
	if (isrmt (fildes))
	{
		return (_rmt_read (fildes - REM_BIAS, buf, nbyte));
	}
	else
	{
		return (read (fildes, buf, nbyte));
	}
}


/*
 *	Write to stream.  Looks just like write(2) to caller.
 */
 
int rmtwrite (fildes, buf, nbyte)
int fildes;
char *buf;
unsigned int nbyte;
{
	if (isrmt (fildes))
	{
		return (_rmt_write (fildes - REM_BIAS, buf, nbyte));
	}
	else
	{
		return (write (fildes, buf, nbyte));
	}
}

/*
 *	Perform lseek on file.  Looks just like lseek(2) to caller.
 */

long rmtlseek (fildes, offset, whence)
int fildes;
long offset;
int whence;
{
	if (isrmt (fildes))
	{
		return (_rmt_lseek (fildes - REM_BIAS, offset, whence));
	}
	else
	{
		return (lseek (fildes, offset, whence));
	}
}


/*
 *	Close a file.  Looks just like close(2) to caller.
 */
 
int rmtclose (fildes)
int fildes;
{
	if (isrmt (fildes))
	{
		return (_rmt_close (fildes - REM_BIAS));
	}
	else
	{
		return (close (fildes));
	}
}

/*
 *	Do ioctl on file.  Looks just like ioctl(2) to caller.
 */
 
int rmtmtio(fildes, request, arg)
int fildes, request, arg;
{
	if (isrmt (fildes))
	{
		return (_rmt_mtio (fildes - REM_BIAS, request, arg));
	}
	else
	{
		return (mtio (fildes, request, arg));
	}
}


/*
 *	Duplicate an open file descriptor.  Looks just like dup(2)
 *	to caller.
 */
 
int rmtdup (fildes)
int fildes;
{
	if (isrmt (fildes))
	{
		errno = EOPNOTSUPP;
		return (-1);		/* For now (fnf) */
	}
	else
	{
		return (dup (fildes));
	}
}

/*
 *	Get file status.  Looks just like fstat(2) to caller.
 */
 
int rmtfstat (fildes, buf)
int fildes;
struct stat *buf;
{
	if (isrmt (fildes))
	{
		errno = EOPNOTSUPP;
		return (-1);		/* For now (fnf) */
	}
	else
	{
		return (fstat (fildes, buf));
	}
}


/*
 *	Get file status.  Looks just like stat(2) to caller.
 */
 
int rmtstat (path, buf)
char *path;
struct stat *buf;
{
	if (remdev (path))
	{
		errno = EOPNOTSUPP;
		return (-1);		/* For now (fnf) */
	}
	else
	{
		return (stat (path, buf));
	}
}



/*
 *	Create a file from scratch.  Looks just like creat(2) to the caller.
 */

#include <sys/file.h>		/* BSD DEPENDANT!!! */
/* #include <fcntl.h>		/* use this one for S5 with remote stuff */

int rmtcreat (path, mode)
char *path;
int mode;
{
	if (remdev (path))
	{
		return (rmtopen (path, 1 | O_CREAT, mode));
	}
	else
	{
		return (creat (path, mode));
	}
}

/*
 *	Isrmt. Let a programmer know he has a remote device.
 */

int isrmt (fd)
int fd;
{
	return (fd >= REM_BIAS);
}

/*
 *	Rmtfcntl. Do a remote fcntl operation.
 */

int rmtfcntl (fd, cmd, arg)
int fd, cmd, arg;
{
	if (isrmt (fd))
	{
		errno = EOPNOTSUPP;
		return (-1);
	}
	else
	{
		return (fcntl (fd, cmd, arg));
	}
}

/*
 *	Rmtisatty.  Do the isatty function.
 */

int rmtisatty (fd)
int fd;
{
	if (isrmt (fd))
		return (0);
	else
		return (isatty (fd));
}


/*
 *	Get file status, even if symlink.  Looks just like lstat(2) to caller.
 */
 
int rmtlstat (path, buf)
char *path;
struct stat *buf;
{
	if (remdev (path))
	{
		errno = EOPNOTSUPP;
		return (-1);		/* For now (fnf) */
	}
	else
	{
		return (lstat (path, buf));
	}
}

static int _rmtsetupoas(fildes)
int fildes;
{	char c;
	int rc, cnt, st;
	char buffer[BUFMAGIC];

	sprintf(buffer, "P\n");
	if (command(fildes, buffer) == -1)
		return(-1);
	st=status(fildes);
	return(st);
}


static int _rmtsendoas(fildes,comm,response,lr,iwt,lencom,lenresp)
int fildes, *lr, iwt, lencom, lenresp;
char *comm, *response;
{
	char buffer[BUFMAGIC];
	int (*pstat)();
	int nbyte;

	sprintf(buffer, "T%d\n%d\n", lencom, iwt);
	if (command(fildes, buffer) == -1)
		return(-1);

	pstat = signal(SIGPIPE, SIG_IGN);

	if (write(WRITE(fildes), comm, lencom) == lencom)
	{
		signal (SIGPIPE, pstat);
		*lr = status(fildes);
		if( *lr > 0 ) 
		{
			nbyte = read(READ(fildes), response, *lr);
			if (nbyte != *lr)
			{
				abort(fildes);
				errno = EIO;
				return(-1);
			}
		}
		return(0);
	}

	signal (SIGPIPE, pstat);
	abort(fildes);
	errno = EIO;
	return(-1);
}

static int _rmtstatf(fildes,size,istat)
int fildes, *size, *istat;
{
	char buffer[BUFMAGIC];
	int (*pstat)();
	int nbyte;

	sprintf(buffer, "F\n");
	if (command(fildes, buffer) == -1)
		return(-1);
	{
	 	int (*pstat)();
		pstat = signal(SIGPIPE, SIG_IGN);
		signal(SIGPIPE, pstat);
		*size = status(fildes);
		if( *size < 0 ) return(-1);
		(void) read(READ(fildes), istat, 4);
		return(0);

	}


}

int rmtstatf(fd,size,istat)
int fd, *size, *istat;
{	
	if (isrmt (fd))
	{
		return( _rmtstatf(fd - REM_BIAS,size,istat) );
	}
	else
	{
		return( statf(fd,size,istat) );
	}
}


int rmtsendoas(fd,comm,response,lr,iwt,lencom,lenresp)
int fd, *lr, iwt, lencom, lenresp;
char *comm, *response;
{	
	if (isrmt (fd))
	{
		return( _rmtsendoas(fd - REM_BIAS,comm,response,lr,iwt,lencom,lenresp) );
	}
	else
	{
		return( sendoas(fd,comm,response,lr,iwt,lencom,lenresp) );
	}
}


int rmtsetupoas(fd)
int fd;
{
	if (isrmt (fd))
	{
		return(_rmtsetupoas(fd - REM_BIAS));
	}
	else
	{
		return( setupoas(fd));
	}
}
