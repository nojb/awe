/* awe.h -- Algol W runtime library 

--

This file is part of Awe. Copyright 2012 Glyn Webster.

This file is free software: you can redistribute it and/or modify it
under the terms of the GNU Limited General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Awe is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU Limited General Public
License along with Awe.  If not, see <http://www.gnu.org/licenses/>.

*/

#ifndef __AWE_H
#define __AWE_H


/* Awe library procedures begin with "_" because they must be in scope everywhere 
   in the C code for an Algol program. Algol W identifiers cannot begin with "_". */


/* Entering and exiting the program, runtime messages. - - - - - - - - - - - - - - - - - - - - - - - - - -  */


/* This represents a location in an Algol W source file. */
/* Every C function that implements an action that can cause a runtime error has an _awe_loc argument. */

typedef struct _awe_LOC {
  const char *file; 
  int line; 
  int column;
} *_awe_loc;


/* _awe_src_<nnn> must be a string variable valid at the point of call, so this macro will work. */

#define _awe_at(filenum, line, column) (&(struct _awe_LOC){_awe_src_##filenum, line, column}) 


/* A macro for _awe_loc locations, to be used in C functions that implement external procedures. */

#define _awe_HERE (&(struct _awe_LOC){__FILE__, __LINE__, 0})


/* The 'main' function will always copy its 'argv' and 'argv' values into these
   variables, they will make the values available to external procedures. */

extern int _awe_argc;
extern char **_awe_argv;


/* Initialize and shutdown the Algol W runtime library. */

void _awe_init (_awe_loc loc);
void _awe_finalize (_awe_loc loc);


/* Issue a run-time error, reporting the Algol W source location, and halt. */

void _awe_error(_awe_loc l, const char *format, ...);


/* Issue a run-time error, reporting the Algol W source location. Don't halt. */

void _awe_warning(_awe_loc l, const char *format, ...);


/* These read environment variables. */
/* An out-of-range integer or an unrecognizable boolean flag is a runtime error. */

int _awe_env_int  (_awe_loc l, const char *variable, int default_, int max, int min);
int _awe_env_bool (_awe_loc l, const char *variable, int default_);



/* References. - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

 /* This allocates storage for a record. */

void *_awe_allocate_record(_awe_loc l, int size);


/* The compiler initializes all references variables to point at this dummy location. 
   Its allows the runtime to tell if a field designator is being called on an 
   uninitialized reference. */

extern int _awe_you_should_not_pointing_here;  
#define _awe_uninitialized_reference ((void*)&_awe_you_should_not_pointing_here)


/* The header of  all Algol records

   Records contain pointers to the names of their record class, which
   serve as class tags.

   Records are numbered in order of allocation. That information is
   given out with Awe error messages. (Hendrik Boom says this is quite
   useful when debugging.)

   Note that references to Awe "external records" are pointers to
   stuctures without this header, so they are not compatible with
   regular Algol records. */

struct _awe_any_record {
    const char *_class; 
    int _number;          
};

#define _awe_class(ref) (((struct _awe_any_record *)ref)->_class)
#define _awe_record_number(ref) (((struct _awe_any_record *)ref)->_number)

extern int _awe_record_counter;  /* for numbering records */

/* '_awe_ref_cast' is used in some reference assignments and actual parameters.
   It returns a reference if it refers to a record that belongs in 'classes'
   otherwise it raises a run-time reference type error.
   This is only called in places where such an error is possible.  
   'classes' is NULL-terminated array of pointers to class names. */

#define _awe_ref_cast(loc, reference, classes...) (_awe_ref_cast_check(loc, reference, (const char *[]){classes, (const char *)0}))
void *_awe_ref_cast_check (_awe_loc loc, void *reference, const char **classes);

/* This is used in field designator functions; it raises a run-time
   reference type error if 'reference' does not belong in 'class'. */

void _awe_ref_field_check (_awe_loc loc, void *reference, const char *class, const char *field_name);

/* The IS operator. */

int _awe_is (void *ref, const char *class);



/* Arrays. - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* The '_awe_array_bounds_check' macro checks that an array dimension's bounds are valid. */
/* A lower bound one less than its upper bound is allowed, it creates an empty array. */

#define _awe_array_bounds_check(a, _awe_src_line, d)\
    if (_##a##_upb##d - _##a##_lwb##d + 1 < 0) { \
        _awe_array_bounds_error(_awe_src_line, #a, d, _##a##_lwb##d, _##a##_upb##d);\
    }
void _awe_array_bounds_error(_awe_loc l, const char *array, int subscript, int lwb, int upb);


/* Array designators use the _awe_array_range_check macro to check 
   that subscripts are within their dimensions' bounds. */
/* The gcc compiler flag -D AWE_NO_ARRAY_CHECKS turns off array subscript range checking. */

#ifdef AWE_NO_ARRAY_CHECKS
#define _awe_array_range_check(a, d)
#else
#define _awe_array_range_check(a, d)\
    if (!(_sub##d >= _##a##_lwb##d && _sub##d <= _##a##_upb##d)) {        \
        _awe_array_range_error(loc, #a, d, _##a##_lwb##d, _##a##_upb##d, _sub##d);\
    }
void _awe_array_range_error(_awe_loc l, const char *array, int subscript, int lwb, int upb, int sub);
#endif



/* Statements. - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


/* Performs an assertion. (Gives the Algol W source code location on failure.) */

void _awe_assert(_awe_loc l, int condition);


/* Checks that the STEP of a FOR statement is not zero. (An unending loop.) */

void _awe_check_for_step(_awe_loc l, int for_step);


/* Reports an error when CASE selector is out of range. */

void _awe_case_range_error(_awe_loc l, int selector);



/* Arithmetic. - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* The gcc compiler flag -D AWE_NO_ARITHMETIC_CHECKS turns off arithmetic bounds checking. */
/* (Only use this this if you are dead sure of what you are doing.) */

#ifdef AWE_NO_DIVZERO
#define _awe_div(l, a, b) ((a) / (b))
#define _awe_rem(l, a, b) ((a) % (b))
#define _awe_rdiv(l, a, b) ((a) / (b))
#define _awe_cdiv(l, a, b) ((a) / (b))
#else


/* Divide-by-zero is a runtime error. */

int _awe_div(_awe_loc l, int a, int b);
int _awe_rem(_awe_loc l, int a, int b);
double _awe_rdiv(_awe_loc loc, double dividend, double divisor);
_Complex double _awe_cdiv(_awe_loc loc, _Complex double dividend, _Complex double divisor);


/* The ABS operator */

#define _awe_abs(i) ({ int _t = i; _t >= 0 ? _t : -_t; })
double _awe_fabs(double r);
_Complex double _awe_cabs(_Complex double x);


/* bit shift operators */

#define _awe_shl(bits, shift) ((bits) << _awe_abs(shift))
#define _awe_shr(bits, shift) ((bits) >> _awe_abs(shift))


/* The ** operator. */

double _awe_rpwr(_awe_loc l, double r, int n);
_Complex double _awe_cpwr(_awe_loc l, _Complex double x, int n);

#endif


/* Strings. - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* STRING(1) is represented by C character rvalues (unsigned char).

   Longer Algol W strings are represented by pointers to arrays of characters (_awe_str).
   '_awe_strs' are NOT null-terminated, their lengths should be known at all times. 
   Each '_awe_str' has its own array, except the ones representing substring designators, 
   those point into other _awe_str's arrays. */

typedef unsigned char *_awe_str;


/* EBCDIC translation tables for the CODE and DECODE standard transfer functions.  
   The Algol W Description specifies EBCDIC encoding, 
   but Awe programs use Latin-1 internally, for the sake of sanity. */

extern unsigned char _awe_ebcdic_of_latin1 [256];
extern unsigned char _awe_latin1_of_ebcdic [256];


/* This is a buffer for strings returned from function procedures.
   (Awe arranges its string-handling code in such a way that only on
   such buffer is necessary.) */

extern unsigned char _awe_return_string [256];

/* Copy a styring into the buffer, and return a pointer to the buffer. 
   External procedures must call this to return string values. */

_awe_str _awe_string (_awe_str src, int srclen);
_awe_str _awe_str_cast (_awe_str src, int srclen, int dstlen);
_awe_str _awe_str_cast_c (unsigned char src, int length);


/* These two are not used by the Algol W runtime, they are use in for external C functions. */
/* "Unpadded" means "excluding the spaces on the righthand side". */

int _awe_str_unpadded_length (const _awe_str src, int srclen);


/* Make an unpadded, zero-terminated copy of an Algol string. */

void _awe_str_unpadded_copy (char * dst, const _awe_str src, int srclen);


/* Perform 'dst := src', return 'src': */

_awe_str _awe_str_cpy (_awe_str dst, int dstlen, const _awe_str src, int srclen);
unsigned char _awe_str_cpy_sc (_awe_str dst, int dstlen, unsigned char src);


/* Return a pointer to the substring 'src(index|len)'.
   There is a runtime error if the substring is not completely in the bounds of the string. */

_awe_str _awe_str_sub (_awe_loc loc, const _awe_str src, int srclen, int index, int length);


/* Compare two strings.  Spaces at the ends of strings are ignored. */

int _awe_str_cmp (const _awe_str str1, int str1len, const _awe_str str2, int str2len);
int _awe_str_cmp_cs (unsigned char c1, const _awe_str str2, int str2len);
int _awe_str_cmp_sc (const _awe_str str1, int str1len, unsigned char c2);
int _awe_str_cmp_cc (unsigned char c1, unsigned char c2);


/* Initialize a string by filling it with spaces. */

void _awe_str_init (_awe_str dst, int dstlen);



/* The standard procedure library - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/


void _awe_init_awestd (void);


/* These functions must have entries in the global Algol W scope defined in 'predeclared.ml' */

int truncate(double r);

int entier(double r);

double roundtoreal(double r);

int round_(double r);

int odd_(int i);

unsigned int bitstring(int i);

int number(unsigned int bits);
 
int decode (unsigned char s);
unsigned char code(int i);

double imagpart(_Complex double x);
double realpart(_Complex double x);
_Complex double imag(double r);
double longimagpart(_Complex double x);
double longrealpart(_Complex double x);
_Complex double longimag(double r);


_awe_str base10(double r);
_awe_str longbase10(double r);

_awe_str intbase10(int r);
_awe_str intbase16(int r);

int maxinteger;
double pi;
double epsilon;
double longepsilon;
double maxreal;

double _awe_sqrt (_awe_loc, double);
double _awe_exp (_awe_loc, double);
double _awe_ln (_awe_loc, double);
double _awe_log (_awe_loc, double);
double _awe_sin (_awe_loc, double);
double _awe_cos (_awe_loc, double);
double _awe_arctan (_awe_loc, double);
#define _awe_longsqrt _awe_sqrt  /* LONG REAL and REAL are identical */
#define _awe_longexp _awe_exp
#define _awe_longln _awe_ln
#define _awe_longlog _awe_log
#define _awe_longsin _awe_sin
#define _awe_longcos _awe_cos
#define _awe_longarctan _awe_arctan

int time_ (int n);


/* Exceptional conditions. - - -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void _awe_init_exceptions (_awe_loc loc);

void *exception( _awe_loc loc,    /* This is a RECORD structure, */
                 int xcpnoted, 
                 int xcplimit, 
                 int xcpaction, 
                 int xcpmark, 
                 _awe_str  xcpmsg );

int *xcpnoted (_awe_loc loc, void *ref);    /* and these are its field designator functions. */
int *xcplimit (_awe_loc loc, void *ref);
int *xcpaction (_awe_loc loc, void *ref);
int *xcpmark (_awe_loc loc, void *ref);
_awe_str xcpmsg (_awe_loc loc, void *ref);

extern void *divzero;
extern void *intdivzero;
extern void *sqrterr;
extern void *experr;
extern void *lnlogerr;
extern void *sincoserr;
extern void *endfile;


/* This is the PROCESSEXCEPTION procedure described in section 8.5,
   except that supplying a default value is the responsibility of the caller. */

void _awe_process_exception (_awe_loc loc, void *condition);



/* The Input/Output System - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

/* Initialize the Input/Output System. This needs to be called at the start of any Algol W program. */

void _awe_init_aweio (_awe_loc l);


/* Shutdown the Input/Output System. This needs to be called at least once at the end of any Algol W program. 
   (Only the first call will have any effect.) */

void _awe_exit_aweio (_awe_loc l);


/* Editing variables. 

   These control the formatting of parameters to WRITE statements. Values over 132 are silently treated as 132.

   Width of a INTEGER field                  I_W        14
   Number of spaces after a field            S_W         2
   Width of a REAL field                     R_W        14
   Number of decimal places in a REAL field  R_D         0
   REAL number output format                 R_FORMAT   "F" */

extern int   i_w;
extern int   s_w;
extern int   r_w;
extern int   r_d;
extern unsigned char r_format;  /* this is a STRING(1) */


/* The editing variables are saved at the start of an standard procedure statement 
   and restored afterwards, see section 7.9.4. */

typedef struct {
    int   i_w;
    int   s_w;
    int   r_w;
    int   r_d;
    unsigned char r_format;
} _awe_Editing_t;

void _awe_Editing_save (_awe_Editing_t* state);
void _awe_Editing_restore (_awe_Editing_t* state);


/* Actions for IOCONTROL statement parameters. 

   There will be a runtime error if the page estimate is exceeded by a line or 
   page break instruction, or if the argument to '_awe_iocontrol' is not a valid 
   IOCONTROL control code. */

void _awe_iocontrol (_awe_loc l, int code);


/* Actions for WRITE or WRITEON actual parameters.  

   There will be a runtime error if the "page estimate" is exceeded. (See the Awe manual.)  */

void _awe_write_integer      (_awe_loc l, int i);
void _awe_write_real         (_awe_loc l, double r);
void _awe_write_long_real    (_awe_loc l, double r);
void _awe_write_complex      (_awe_loc l, _Complex double r);
void _awe_write_long_complex (_awe_loc l, _Complex double r);
void _awe_write_logical      (_awe_loc l, int b);
void _awe_write_bits         (_awe_loc l, unsigned int b);
void _awe_write_string       (_awe_loc l, _awe_str s, int length);
void _awe_write_char         (_awe_loc l, unsigned char c);
void _awe_write_reference    (_awe_loc loc, void *ref);


/* Actions for READ, READON and READCARD actual parameters. */

void _awe_read_integer  (_awe_loc l, int *i);
void _awe_read_real     (_awe_loc l, double *r);
void _awe_read_complex  (_awe_loc l, _Complex double *r);
void _awe_read_logical  (_awe_loc l, int *b);
void _awe_read_bits     (_awe_loc l, unsigned int *b);
void _awe_read_string   (_awe_loc l, _awe_str s, int length);
void _awe_read_char     (_awe_loc l, unsigned char *c);
void _awe_readcard      (_awe_loc l, _awe_str string, int length);
void _awe_readcard_char (_awe_loc l, unsigned char *c);


#endif

/* end */
