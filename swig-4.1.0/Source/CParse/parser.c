/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 25 "parser.y"

#define yylex yylex

/* doh.h uses #pragma GCC poison with GCC to prevent direct calls to certain
 * standard C library functions being introduced, but those cause errors due
 * to checks like `#if defined YYMALLOC || defined malloc` in the bison
 * template code.  We can't easily arrange to include headers after that
 * template code, so instead we disable the problematic poisoning for this
 * file.
 */
#define DOH_NO_POISON_MALLOC_FREE

#include "swig.h"
#include "cparse.h"
#include "preprocessor.h"
#include <ctype.h>

/* We do this for portability */
#undef alloca
#define alloca Malloc

#define YYMALLOC Malloc
#define YYFREE Free

/* -----------------------------------------------------------------------------
 *                               Externals
 * ----------------------------------------------------------------------------- */

int  yyparse(void);

/* NEW Variables */

static Node    *top = 0;      /* Top of the generated parse tree */
static int      unnamed = 0;  /* Unnamed datatype counter */
static Hash    *classes = 0;        /* Hash table of classes */
static Hash    *classes_typedefs = 0; /* Hash table of typedef classes: typedef struct X {...} Y; */
static Symtab  *prev_symtab = 0;
static Node    *current_class = 0;
String  *ModuleName = 0;
static Node    *module_node = 0;
static String  *Classprefix = 0;  
static String  *Namespaceprefix = 0;
static int      inclass = 0;
static Node    *currentOuterClass = 0; /* for nested classes */
static const char *last_cpptype = 0;
static int      inherit_list = 0;
static Parm    *template_parameters = 0;
static int      parsing_template_declaration = 0;
static int      extendmode   = 0;
static int      compact_default_args = 0;
static int      template_reduce = 0;
static int      cparse_externc = 0;
int		ignore_nested_classes = 0;
int		kwargs_supported = 0;
/* -----------------------------------------------------------------------------
 *                            Doxygen Comment Globals
 * ----------------------------------------------------------------------------- */
static String *currentDeclComment = NULL; /* Comment of C/C++ declaration. */
static Node *previousNode = NULL; /* Pointer to the previous node (for post comments) */
static Node *currentNode = NULL; /* Pointer to the current node (for post comments) */

/* -----------------------------------------------------------------------------
 *                            Assist Functions
 * ----------------------------------------------------------------------------- */


 
/* Called by the parser (yyparse) when an error is found.*/
static void yyerror (const char *e) {
  (void)e;
}

static Node *new_node(const_String_or_char_ptr tag) {
  Node *n = Swig_cparse_new_node(tag);
  /* Remember the previous node in case it will need a post-comment */
  previousNode = currentNode;
  currentNode = n;
  return n;
}

/* Copies a node.  Does not copy tree links or symbol table data (except for
   sym:name) */

static Node *copy_node(Node *n) {
  Node *nn;
  Iterator k;
  nn = NewHash();
  Setfile(nn,Getfile(n));
  Setline(nn,Getline(n));
  for (k = First(n); k.key; k = Next(k)) {
    String *ci;
    String *key = k.key;
    char *ckey = Char(key);
    if ((strcmp(ckey,"nextSibling") == 0) ||
	(strcmp(ckey,"previousSibling") == 0) ||
	(strcmp(ckey,"parentNode") == 0) ||
	(strcmp(ckey,"lastChild") == 0)) {
      continue;
    }
    if (Strncmp(key,"csym:",5) == 0) continue;
    /* We do copy sym:name.  For templates */
    if ((strcmp(ckey,"sym:name") == 0) || 
	(strcmp(ckey,"sym:weak") == 0) ||
	(strcmp(ckey,"sym:typename") == 0)) {
      String *ci = Copy(k.item);
      Setattr(nn,key, ci);
      Delete(ci);
      continue;
    }
    if (strcmp(ckey,"sym:symtab") == 0) {
      Setattr(nn,"sym:needs_symtab", "1");
    }
    /* We don't copy any other symbol table attributes */
    if (strncmp(ckey,"sym:",4) == 0) {
      continue;
    }
    /* If children.  We copy them recursively using this function */
    if (strcmp(ckey,"firstChild") == 0) {
      /* Copy children */
      Node *cn = k.item;
      while (cn) {
	Node *copy = copy_node(cn);
	appendChild(nn,copy);
	Delete(copy);
	cn = nextSibling(cn);
      }
      continue;
    }
    /* We don't copy the symbol table.  But we drop an attribute 
       requires_symtab so that functions know it needs to be built */

    if (strcmp(ckey,"symtab") == 0) {
      /* Node defined a symbol table. */
      Setattr(nn,"requires_symtab","1");
      continue;
    }
    /* Can't copy nodes */
    if (strcmp(ckey,"node") == 0) {
      continue;
    }
    if ((strcmp(ckey,"parms") == 0) || (strcmp(ckey,"pattern") == 0) || (strcmp(ckey,"throws") == 0)
	|| (strcmp(ckey,"kwargs") == 0)) {
      ParmList *pl = CopyParmList(k.item);
      Setattr(nn,key,pl);
      Delete(pl);
      continue;
    }
    if (strcmp(ckey,"nested:outer") == 0) { /* don't copy outer classes links, they will be updated later */
      Setattr(nn, key, k.item);
      continue;
    }
    /* defaultargs will be patched back in later in update_defaultargs() */
    if (strcmp(ckey,"defaultargs") == 0) {
      Setattr(nn, "needs_defaultargs", "1");
      continue;
    }
    /* same for abstracts, which contains pointers to the source node children, and so will need to be patch too */
    if (strcmp(ckey,"abstracts") == 0) {
      SetFlag(nn, "needs_abstracts");
      continue;
    }
    /* Looks okay.  Just copy the data using Copy */
    ci = Copy(k.item);
    Setattr(nn, key, ci);
    Delete(ci);
  }
  return nn;
}

static void set_comment(Node *n, String *comment) {
  String *name;
  Parm *p;
  if (!n || !comment)
    return;

  if (Getattr(n, "doxygen"))
    Append(Getattr(n, "doxygen"), comment);
  else {
    Setattr(n, "doxygen", comment);
    /* This is the first comment, populate it with @params, if any */
    p = Getattr(n, "parms");
    while (p) {
      if (Getattr(p, "doxygen"))
	Printv(comment, "\n@param ", Getattr(p, "name"), Getattr(p, "doxygen"), NIL);
      p=nextSibling(p);
    }
  }
  
  /* Append same comment to every generated overload */
  name = Getattr(n, "name");
  if (!name)
    return;
  n = nextSibling(n);
  while (n && Getattr(n, "name") && Strcmp(Getattr(n, "name"), name) == 0) {
    Setattr(n, "doxygen", comment);
    n = nextSibling(n);
  }
}

/* -----------------------------------------------------------------------------
 *                              Variables
 * ----------------------------------------------------------------------------- */

static char  *typemap_lang = 0;    /* Current language setting */

static int cplus_mode  = 0;

/* C++ modes */

#define  CPLUS_PUBLIC    1
#define  CPLUS_PRIVATE   2
#define  CPLUS_PROTECTED 3

/* include types */
static int   import_mode = 0;

void SWIG_typemap_lang(const char *tm_lang) {
  typemap_lang = Swig_copy_string(tm_lang);
}

void SWIG_cparse_set_compact_default_args(int defargs) {
  compact_default_args = defargs;
}

int SWIG_cparse_template_reduce(int treduce) {
  template_reduce = treduce;
  return treduce;  
}

/* -----------------------------------------------------------------------------
 *                           Assist functions
 * ----------------------------------------------------------------------------- */

static int promote_type(int t) {
  if (t <= T_UCHAR || t == T_CHAR || t == T_WCHAR) return T_INT;
  return t;
}

/* Perform type-promotion for binary operators */
static int promote(int t1, int t2) {
  t1 = promote_type(t1);
  t2 = promote_type(t2);
  return t1 > t2 ? t1 : t2;
}

static String *yyrename = 0;

/* Forward renaming operator */

static String *resolve_create_node_scope(String *cname, int is_class_definition);


Hash *Swig_cparse_features(void) {
  static Hash   *features_hash = 0;
  if (!features_hash) features_hash = NewHash();
  return features_hash;
}

/* Fully qualify any template parameters */
static String *feature_identifier_fix(String *s) {
  String *tp = SwigType_istemplate_templateprefix(s);
  if (tp) {
    String *ts, *ta, *tq;
    ts = SwigType_templatesuffix(s);
    ta = SwigType_templateargs(s);
    tq = Swig_symbol_type_qualify(ta,0);
    Append(tp,tq);
    Append(tp,ts);
    Delete(ts);
    Delete(ta);
    Delete(tq);
    return tp;
  } else {
    return NewString(s);
  }
}

static void set_access_mode(Node *n) {
  if (cplus_mode == CPLUS_PUBLIC)
    Setattr(n, "access", "public");
  else if (cplus_mode == CPLUS_PROTECTED)
    Setattr(n, "access", "protected");
  else
    Setattr(n, "access", "private");
}

static void restore_access_mode(Node *n) {
  String *mode = Getattr(n, "access");
  if (Strcmp(mode, "private") == 0)
    cplus_mode = CPLUS_PRIVATE;
  else if (Strcmp(mode, "protected") == 0)
    cplus_mode = CPLUS_PROTECTED;
  else
    cplus_mode = CPLUS_PUBLIC;
}

/* Generate the symbol table name for an object */
/* This is a bit of a mess. Need to clean up */
static String *add_oldname = 0;



static String *make_name(Node *n, String *name,SwigType *decl) {
  String *made_name = 0;
  int destructor = name && (*(Char(name)) == '~');

  if (yyrename) {
    String *s = NewString(yyrename);
    Delete(yyrename);
    yyrename = 0;
    if (destructor  && (*(Char(s)) != '~')) {
      Insert(s,0,"~");
    }
    return s;
  }

  if (!name) return 0;

  if (parsing_template_declaration)
    SetFlag(n, "parsing_template_declaration");
  made_name = Swig_name_make(n, Namespaceprefix, name, decl, add_oldname);
  Delattr(n, "parsing_template_declaration");

  return made_name;
}

/* Generate an unnamed identifier */
static String *make_unnamed(void) {
  unnamed++;
  return NewStringf("$unnamed%d$",unnamed);
}

/* Return if the node is a friend declaration */
static int is_friend(Node *n) {
  return Cmp(Getattr(n,"storage"),"friend") == 0;
}

static int is_operator(String *name) {
  return Strncmp(name,"operator ", 9) == 0;
}


/* Add declaration list to symbol table */
static int  add_only_one = 0;

static void add_symbols(Node *n) {
  String *decl;
  String *wrn = 0;

  if (inclass && n) {
    cparse_normalize_void(n);
  }
  while (n) {
    String *symname = 0;
    /* for friends, we need to pop the scope once */
    String *old_prefix = 0;
    Symtab *old_scope = 0;
    int isfriend = inclass && is_friend(n);
    int iscdecl = Cmp(nodeType(n),"cdecl") == 0;
    int only_csymbol = 0;
    
    if (inclass) {
      String *name = Getattr(n, "name");
      if (isfriend) {
	/* for friends, we need to add the scopename if needed */
	String *prefix = name ? Swig_scopename_prefix(name) : 0;
	old_prefix = Namespaceprefix;
	old_scope = Swig_symbol_popscope();
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	if (!prefix) {
	  if (name && !is_operator(name) && Namespaceprefix) {
	    String *nname = NewStringf("%s::%s", Namespaceprefix, name);
	    Setattr(n,"name",nname);
	    Delete(nname);
	  }
	} else {
	  Symtab *st = Swig_symbol_getscope(prefix);
	  String *ns = st ? Getattr(st,"name") : prefix;
	  String *base  = Swig_scopename_last(name);
	  String *nname = NewStringf("%s::%s", ns, base);
	  Setattr(n,"name",nname);
	  Delete(nname);
	  Delete(base);
	  Delete(prefix);
	}
	Namespaceprefix = 0;
      } else {
	/* for member functions, we need to remove the redundant
	   class scope if provided, as in
	   
	   struct Foo {
	   int Foo::method(int a);
	   };
	   
	*/
	String *prefix = name ? Swig_scopename_prefix(name) : 0;
	if (prefix) {
	  if (Classprefix && (Equal(prefix,Classprefix))) {
	    String *base = Swig_scopename_last(name);
	    Setattr(n,"name",base);
	    Delete(base);
	  }
	  Delete(prefix);
	}
      }
    }

    if (!isfriend && (inclass || extendmode)) {
      Setattr(n,"ismember","1");
    }

    if (extendmode) {
      if (!Getattr(n, "template"))
        SetFlag(n,"isextendmember");
    }

    if (!isfriend && inclass) {
      if ((cplus_mode != CPLUS_PUBLIC)) {
	only_csymbol = 1;
	if (cplus_mode == CPLUS_PROTECTED) {
	  Setattr(n,"access", "protected");
	  only_csymbol = !Swig_need_protected(n);
	} else {
	  Setattr(n,"access", "private");
	  /* private are needed only when they are pure virtuals - why? */
	  if ((Cmp(Getattr(n,"storage"),"virtual") == 0) && (Cmp(Getattr(n,"value"),"0") == 0)) {
	    only_csymbol = 0;
	  }
	  if (Cmp(nodeType(n),"destructor") == 0) {
	    /* Needed for "unref" feature */
	    only_csymbol = 0;
	  }
	}
      } else {
	  Setattr(n,"access", "public");
      }
    }
    if (Getattr(n,"sym:name")) {
      n = nextSibling(n);
      continue;
    }
    decl = Getattr(n,"decl");
    if (!SwigType_isfunction(decl)) {
      String *name = Getattr(n,"name");
      String *makename = Getattr(n,"parser:makename");
      if (iscdecl) {	
	String *storage = Getattr(n, "storage");
	if (Cmp(storage,"typedef") == 0) {
	  Setattr(n,"kind","typedef");
	} else {
	  SwigType *type = Getattr(n,"type");
	  String *value = Getattr(n,"value");
	  Setattr(n,"kind","variable");
	  if (value && Len(value)) {
	    Setattr(n,"hasvalue","1");
	  }
	  if (type) {
	    SwigType *ty;
	    SwigType *tmp = 0;
	    if (decl) {
	      ty = tmp = Copy(type);
	      SwigType_push(ty,decl);
	    } else {
	      ty = type;
	    }
	    if (!SwigType_ismutable(ty) || (storage && Strstr(storage, "constexpr"))) {
	      SetFlag(n,"hasconsttype");
	      SetFlag(n,"feature:immutable");
	    }
	    if (tmp) Delete(tmp);
	  }
	  if (!type) {
	    Printf(stderr,"notype name %s\n", name);
	  }
	}
      }
      Swig_features_get(Swig_cparse_features(), Namespaceprefix, name, 0, n);
      if (makename) {
	symname = make_name(n, makename,0);
        Delattr(n,"parser:makename"); /* temporary information, don't leave it hanging around */
      } else {
        makename = name;
	symname = make_name(n, makename,0);
      }
      
      if (!symname) {
	symname = Copy(Getattr(n,"unnamed"));
      }
      if (symname) {
	if (parsing_template_declaration)
	  SetFlag(n, "parsing_template_declaration");
	wrn = Swig_name_warning(n, Namespaceprefix, symname,0);
	Delattr(n, "parsing_template_declaration");
      }
    } else {
      String *name = Getattr(n,"name");
      SwigType *fdecl = Copy(decl);
      SwigType *fun = SwigType_pop_function(fdecl);
      if (iscdecl) {	
	Setattr(n,"kind","function");
      }
      
      Swig_features_get(Swig_cparse_features(),Namespaceprefix,name,fun,n);

      symname = make_name(n, name,fun);
      if (parsing_template_declaration)
	SetFlag(n, "parsing_template_declaration");
      wrn = Swig_name_warning(n, Namespaceprefix,symname,fun);
      Delattr(n, "parsing_template_declaration");
      
      Delete(fdecl);
      Delete(fun);
      
    }
    if (!symname) {
      n = nextSibling(n);
      continue;
    }
    if (cparse_cplusplus) {
      String *value = Getattr(n, "value");
      if (value && Strcmp(value, "delete") == 0) {
	/* C++11 deleted definition / deleted function */
        SetFlag(n,"deleted");
        SetFlag(n,"feature:ignore");
      }
      if (SwigType_isrvalue_reference(Getattr(n, "refqualifier"))) {
	/* Ignore rvalue ref-qualifiers by default
	 * Use Getattr instead of GetFlag to handle explicit ignore and explicit not ignore */
	if (!(Getattr(n, "feature:ignore") || Strncmp(symname, "$ignore", 7) == 0)) {
	  SWIG_WARN_NODE_BEGIN(n);
	  Swig_warning(WARN_TYPE_RVALUE_REF_QUALIFIER_IGNORED, Getfile(n), Getline(n),
	      "Method with rvalue ref-qualifier %s ignored.\n", Swig_name_decl(n));
	  SWIG_WARN_NODE_END(n);
	  SetFlag(n, "feature:ignore");
	}
      }
    }
    if (only_csymbol || GetFlag(n, "feature:ignore") || Strncmp(symname, "$ignore", 7) == 0) {
      /* Only add to C symbol table and continue */
      Swig_symbol_add(0, n);
      if (!only_csymbol && !GetFlag(n, "feature:ignore")) {
	/* Print the warning attached to $ignore name, if any */
        char *c = Char(symname) + 7;
	if (strlen(c)) {
	  SWIG_WARN_NODE_BEGIN(n);
	  Swig_warning(0,Getfile(n), Getline(n), "%s\n",c+1);
	  SWIG_WARN_NODE_END(n);
	}
	/* If the symbol was ignored via "rename" and is visible, set also feature:ignore*/
	SetFlag(n, "feature:ignore");
      }
      if (!GetFlag(n, "feature:ignore") && Strcmp(symname,"$ignore") == 0) {
	/* Add feature:ignore if the symbol was explicitly ignored, regardless of visibility */
	SetFlag(n, "feature:ignore");
      }
    } else {
      Node *c;
      if ((wrn) && (Len(wrn))) {
	String *metaname = symname;
	if (!Getmeta(metaname,"already_warned")) {
	  SWIG_WARN_NODE_BEGIN(n);
	  Swig_warning(0,Getfile(n),Getline(n), "%s\n", wrn);
	  SWIG_WARN_NODE_END(n);
	  Setmeta(metaname,"already_warned","1");
	}
      }
      c = Swig_symbol_add(symname,n);

      if (c != n) {
        /* symbol conflict attempting to add in the new symbol */
        if (Getattr(n,"sym:weak")) {
          Setattr(n,"sym:name",symname);
        } else {
          String *e = NewStringEmpty();
          String *en = NewStringEmpty();
          String *ec = NewStringEmpty();
          int redefined = Swig_need_redefined_warn(n,c,inclass);
          if (redefined) {
            Printf(en,"Identifier '%s' redefined (ignored)",symname);
            Printf(ec,"previous definition of '%s'",symname);
          } else {
            Printf(en,"Redundant redeclaration of '%s'",symname);
            Printf(ec,"previous declaration of '%s'",symname);
          }
          if (Cmp(symname,Getattr(n,"name"))) {
            Printf(en," (Renamed from '%s')", SwigType_namestr(Getattr(n,"name")));
          }
          Printf(en,",");
          if (Cmp(symname,Getattr(c,"name"))) {
            Printf(ec," (Renamed from '%s')", SwigType_namestr(Getattr(c,"name")));
          }
          Printf(ec,".");
	  SWIG_WARN_NODE_BEGIN(n);
          if (redefined) {
            Swig_warning(WARN_PARSE_REDEFINED,Getfile(n),Getline(n),"%s\n",en);
            Swig_warning(WARN_PARSE_REDEFINED,Getfile(c),Getline(c),"%s\n",ec);
          } else if (!is_friend(n) && !is_friend(c)) {
            Swig_warning(WARN_PARSE_REDUNDANT,Getfile(n),Getline(n),"%s\n",en);
            Swig_warning(WARN_PARSE_REDUNDANT,Getfile(c),Getline(c),"%s\n",ec);
          }
	  SWIG_WARN_NODE_END(n);
          Printf(e,"%s:%d:%s\n%s:%d:%s\n",Getfile(n),Getline(n),en,
                 Getfile(c),Getline(c),ec);
          Setattr(n,"error",e);
	  Delete(e);
          Delete(en);
          Delete(ec);
        }
      }
    }
    /* restore the class scope if needed */
    if (isfriend) {
      Swig_symbol_setscope(old_scope);
      if (old_prefix) {
	Delete(Namespaceprefix);
	Namespaceprefix = old_prefix;
      }
    }
    Delete(symname);

    if (add_only_one) return;
    n = nextSibling(n);
  }
}


/* add symbols a parse tree node copy */

static void add_symbols_copy(Node *n) {
  String *name;
  int    emode = 0;
  while (n) {
    char *cnodeType = Char(nodeType(n));

    if (strcmp(cnodeType,"access") == 0) {
      String *kind = Getattr(n,"kind");
      if (Strcmp(kind,"public") == 0) {
	cplus_mode = CPLUS_PUBLIC;
      } else if (Strcmp(kind,"private") == 0) {
	cplus_mode = CPLUS_PRIVATE;
      } else if (Strcmp(kind,"protected") == 0) {
	cplus_mode = CPLUS_PROTECTED;
      }
      n = nextSibling(n);
      continue;
    }

    add_oldname = Getattr(n,"sym:name");
    if ((add_oldname) || (Getattr(n,"sym:needs_symtab"))) {
      int old_inclass = -1;
      Node *old_current_class = 0;
      if (add_oldname) {
	DohIncref(add_oldname);
	/*  Disable this, it prevents %rename to work with templates */
	/* If already renamed, we used that name  */
	/*
	if (Strcmp(add_oldname, Getattr(n,"name")) != 0) {
	  Delete(yyrename);
	  yyrename = Copy(add_oldname);
	}
	*/
      }
      Delattr(n,"sym:needs_symtab");
      Delattr(n,"sym:name");

      add_only_one = 1;
      add_symbols(n);

      if (Getattr(n,"partialargs")) {
	Swig_symbol_cadd(Getattr(n,"partialargs"),n);
      }
      add_only_one = 0;
      name = Getattr(n,"name");
      if (Getattr(n,"requires_symtab")) {
	Swig_symbol_newscope();
	Swig_symbol_setscopename(name);
	Delete(Namespaceprefix);
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
      }
      if (strcmp(cnodeType,"class") == 0) {
	old_inclass = inclass;
	inclass = 1;
	old_current_class = current_class;
	current_class = n;
	if (Strcmp(Getattr(n,"kind"),"class") == 0) {
	  cplus_mode = CPLUS_PRIVATE;
	} else {
	  cplus_mode = CPLUS_PUBLIC;
	}
      }
      if (strcmp(cnodeType,"extend") == 0) {
	emode = cplus_mode;
	cplus_mode = CPLUS_PUBLIC;
      }
      add_symbols_copy(firstChild(n));
      if (strcmp(cnodeType,"extend") == 0) {
	cplus_mode = emode;
      }
      if (Getattr(n,"requires_symtab")) {
	Setattr(n,"symtab", Swig_symbol_popscope());
	Delattr(n,"requires_symtab");
	Delete(Namespaceprefix);
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
      }
      if (add_oldname) {
	Delete(add_oldname);
	add_oldname = 0;
      }
      if (strcmp(cnodeType,"class") == 0) {
	inclass = old_inclass;
	current_class = old_current_class;
      }
    } else {
      if (strcmp(cnodeType,"extend") == 0) {
	emode = cplus_mode;
	cplus_mode = CPLUS_PUBLIC;
      }
      add_symbols_copy(firstChild(n));
      if (strcmp(cnodeType,"extend") == 0) {
	cplus_mode = emode;
      }
    }
    n = nextSibling(n);
  }
}

/* Add in the "defaultargs" attribute for functions in instantiated templates.
 * n should be any instantiated template (class or start of linked list of functions). */
static void update_defaultargs(Node *n) {
  if (n) {
    Node *firstdefaultargs = n;
    update_defaultargs(firstChild(n));
    n = nextSibling(n);
    /* recursively loop through nodes of all types, but all we really need are the overloaded functions */
    while (n) {
      update_defaultargs(firstChild(n));
      if (!Getattr(n, "defaultargs")) {
	if (Getattr(n, "needs_defaultargs")) {
	  Setattr(n, "defaultargs", firstdefaultargs);
	  Delattr(n, "needs_defaultargs");
	} else {
	  firstdefaultargs = n;
	}
      } else {
	/* Functions added in with %extend (for specialized template classes) will already have default args patched up */
	assert(Getattr(n, "defaultargs") == firstdefaultargs);
      }
      n = nextSibling(n);
    }
  }
}

/* Check a set of declarations to see if any are pure-abstract */

static List *pure_abstracts(Node *n) {
  List *abstracts = 0;
  while (n) {
    if (Cmp(nodeType(n),"cdecl") == 0) {
      String *decl = Getattr(n,"decl");
      if (SwigType_isfunction(decl)) {
	String *init = Getattr(n,"value");
	if (Cmp(init,"0") == 0) {
	  if (!abstracts) {
	    abstracts = NewList();
	  }
	  Append(abstracts,n);
	  SetFlag(n,"abstract");
	}
      }
    } else if (Cmp(nodeType(n),"destructor") == 0) {
      if (Cmp(Getattr(n,"value"),"0") == 0) {
	if (!abstracts) {
	  abstracts = NewList();
	}
	Append(abstracts,n);
	SetFlag(n,"abstract");
      }
    }
    n = nextSibling(n);
  }
  return abstracts;
}

/* Recompute the "abstracts" attribute for the classes in instantiated templates, similarly to update_defaultargs() above. */
static void update_abstracts(Node *n) {
  for (; n; n = nextSibling(n)) {
    Node* const child = firstChild(n);
    if (!child)
      continue;

    update_abstracts(child);

    if (Getattr(n, "needs_abstracts")) {
      Setattr(n, "abstracts", pure_abstracts(child));
      Delattr(n, "needs_abstracts");
    }
  }
}

/* Make a classname */

static String *make_class_name(String *name) {
  String *nname = 0;
  String *prefix;
  if (Namespaceprefix) {
    nname= NewStringf("%s::%s", Namespaceprefix, name);
  } else {
    nname = NewString(name);
  }
  prefix = SwigType_istemplate_templateprefix(nname);
  if (prefix) {
    String *args, *qargs;
    args   = SwigType_templateargs(nname);
    qargs  = Swig_symbol_type_qualify(args,0);
    Append(prefix,qargs);
    Delete(nname);
    Delete(args);
    Delete(qargs);
    nname = prefix;
  }
  return nname;
}

/* Use typedef name as class name */

static void add_typedef_name(Node *n, Node *declnode, String *oldName, Symtab *cscope, String *scpname) {
  String *class_rename = 0;
  SwigType *decl = Getattr(declnode, "decl");
  if (!decl || !Len(decl)) {
    String *cname;
    String *tdscopename;
    String *class_scope = Swig_symbol_qualifiedscopename(cscope);
    String *name = Getattr(declnode, "name");
    cname = Copy(name);
    Setattr(n, "tdname", cname);
    tdscopename = class_scope ? NewStringf("%s::%s", class_scope, name) : Copy(name);
    class_rename = Getattr(n, "class_rename");
    if (class_rename && (Strcmp(class_rename, oldName) == 0))
      Setattr(n, "class_rename", NewString(name));
    if (!classes_typedefs) classes_typedefs = NewHash();
    if (!Equal(scpname, tdscopename) && !Getattr(classes_typedefs, tdscopename)) {
      Setattr(classes_typedefs, tdscopename, n);
    }
    Setattr(n, "decl", decl);
    Delete(class_scope);
    Delete(cname);
    Delete(tdscopename);
  }
}

/* If the class name is qualified.  We need to create or lookup namespace entries */

static Symtab *set_scope_to_global(void) {
  Symtab *symtab = Swig_symbol_global_scope();
  Swig_symbol_setscope(symtab);
  return symtab;
}
 
/* Remove the block braces, { and }, if the 'noblock' attribute is set.
 * Node *kw can be either a Hash or Parmlist. */
static String *remove_block(Node *kw, const String *inputcode) {
  String *modified_code = 0;
  while (kw) {
   String *name = Getattr(kw,"name");
   if (name && (Cmp(name,"noblock") == 0)) {
     char *cstr = Char(inputcode);
     int len = Len(inputcode);
     if (len && cstr[0] == '{') {
       --len; ++cstr; 
       if (len && cstr[len - 1] == '}') { --len; }
       /* we now remove the extra spaces */
       while (len && isspace((int)cstr[0])) { --len; ++cstr; }
       while (len && isspace((int)cstr[len - 1])) { --len; }
       modified_code = NewStringWithSize(cstr, len);
       break;
     }
   }
   kw = nextSibling(kw);
  }
  return modified_code;
}

/*
#define RESOLVE_DEBUG 1
*/
static Node *nscope = 0;
static Node *nscope_inner = 0;

/* Remove the scope prefix from cname and return the base name without the prefix.
 * The scopes required for the symbol name are resolved and/or created, if required.
 * For example AA::BB::CC as input returns CC and creates the namespace AA then inner 
 * namespace BB in the current scope. */
static String *resolve_create_node_scope(String *cname, int is_class_definition) {
  Symtab *gscope = 0;
  Node *cname_node = 0;
  String *last = Swig_scopename_last(cname);
  nscope = 0;
  nscope_inner = 0;  

  if (Strncmp(cname,"::" ,2) != 0) {
    if (is_class_definition) {
      /* Only lookup symbols which are in scope via a using declaration but not via a using directive.
         For example find y via 'using x::y' but not y via a 'using namespace x'. */
      cname_node = Swig_symbol_clookup_no_inherit(cname, 0);
      if (!cname_node) {
	Node *full_lookup_node = Swig_symbol_clookup(cname, 0);
	if (full_lookup_node) {
	 /* This finds a symbol brought into scope via both a using directive and a using declaration. */
	  Node *last_node = Swig_symbol_clookup_no_inherit(last, 0);
	  if (last_node == full_lookup_node)
	    cname_node = last_node;
	}
      }
    } else {
      /* For %template, the template needs to be in scope via any means. */
      cname_node = Swig_symbol_clookup(cname, 0);
    }
  }
#if RESOLVE_DEBUG
  if (!cname_node)
    Printf(stdout, "symbol does not yet exist (%d): [%s]\n", is_class_definition, cname);
  else
    Printf(stdout, "symbol does exist (%d): [%s]\n", is_class_definition, cname);
#endif

  if (cname_node) {
    /* The symbol has been defined already or is in another scope.
       If it is a weak symbol, it needs replacing and if it was brought into the current scope,
       the scope needs adjusting appropriately for the new symbol.
       Similarly for defined templates. */
    Symtab *symtab = Getattr(cname_node, "sym:symtab");
    Node *sym_weak = Getattr(cname_node, "sym:weak");
    if ((symtab && sym_weak) || Equal(nodeType(cname_node), "template")) {
      /* Check if the scope is the current scope */
      String *current_scopename = Swig_symbol_qualifiedscopename(0);
      String *found_scopename = Swig_symbol_qualifiedscopename(symtab);
      if (!current_scopename)
	current_scopename = NewString("");
      if (!found_scopename)
	found_scopename = NewString("");

      {
	int fail = 1;
	List *current_scopes = Swig_scopename_tolist(current_scopename);
	List *found_scopes = Swig_scopename_tolist(found_scopename);
        Iterator cit = First(current_scopes);
	Iterator fit = First(found_scopes);
#if RESOLVE_DEBUG
Printf(stdout, "comparing current: [%s] found: [%s]\n", current_scopename, found_scopename);
#endif
	for (; fit.item && cit.item; fit = Next(fit), cit = Next(cit)) {
	  String *current = cit.item;
	  String *found = fit.item;
#if RESOLVE_DEBUG
	  Printf(stdout, "  looping %s %s\n", current, found);
#endif
	  if (Strcmp(current, found) != 0)
	    break;
	}

	if (!cit.item) {
	  String *subscope = NewString("");
	  for (; fit.item; fit = Next(fit)) {
	    if (Len(subscope) > 0)
	      Append(subscope, "::");
	    Append(subscope, fit.item);
	  }
	  if (Len(subscope) > 0)
	    cname = NewStringf("%s::%s", subscope, last);
	  else
	    cname = Copy(last);
#if RESOLVE_DEBUG
	  Printf(stdout, "subscope to create: [%s] cname: [%s]\n", subscope, cname);
#endif
	  fail = 0;
	  Delete(subscope);
	} else {
	  if (is_class_definition) {
	    if (!fit.item) {
	      /* It is valid to define a new class with the same name as one forward declared in a parent scope */
	      fail = 0;
	    } else if (Swig_scopename_check(cname)) {
	      /* Classes defined with scope qualifiers must have a matching forward declaration in matching scope */
	      fail = 1;
	    } else {
	      /* This may let through some invalid cases */
	      fail = 0;
	    }
#if RESOLVE_DEBUG
	    Printf(stdout, "scope for class definition, fail: %d\n", fail);
#endif
	  } else {
#if RESOLVE_DEBUG
	    Printf(stdout, "no matching base scope for template\n");
#endif
	    fail = 1;
	  }
	}

	Delete(found_scopes);
	Delete(current_scopes);

	if (fail) {
	  String *cname_resolved = NewStringf("%s::%s", found_scopename, last);
	  Swig_error(cparse_file, cparse_line, "'%s' resolves to '%s' and was incorrectly instantiated in scope '%s' instead of within scope '%s'.\n", cname, cname_resolved, current_scopename, found_scopename);
	  cname = Copy(last);
	  Delete(cname_resolved);
	}
      }

      Delete(current_scopename);
      Delete(found_scopename);
    }
  } else if (!is_class_definition) {
    /* A template instantiation requires a template to be found in scope... fail here too?
    Swig_error(cparse_file, cparse_line, "No template found to instantiate '%s' with %%template.\n", cname);
     */
  }

  if (Swig_scopename_check(cname)) {
    Node   *ns;
    String *prefix = Swig_scopename_prefix(cname);
    if (prefix && (Strncmp(prefix,"::",2) == 0)) {
/* I don't think we can use :: global scope to declare classes and hence neither %template. - consider reporting error instead - wsfulton. */
      /* Use the global scope */
      String *nprefix = NewString(Char(prefix)+2);
      Delete(prefix);
      prefix= nprefix;
      gscope = set_scope_to_global();
    }
    if (Len(prefix) == 0) {
      String *base = Copy(last);
      /* Use the global scope, but we need to add a 'global' namespace.  */
      if (!gscope) gscope = set_scope_to_global();
      /* note that this namespace is not the "unnamed" one,
	 and we don't use Setattr(nscope,"name", ""),
	 because the unnamed namespace is private */
      nscope = new_node("namespace");
      Setattr(nscope,"symtab", gscope);;
      nscope_inner = nscope;
      Delete(last);
      return base;
    }
    /* Try to locate the scope */
    ns = Swig_symbol_clookup(prefix,0);
    if (!ns) {
      Swig_error(cparse_file,cparse_line,"Undefined scope '%s'\n", prefix);
    } else {
      Symtab *nstab = Getattr(ns,"symtab");
      if (!nstab) {
	Swig_error(cparse_file,cparse_line, "'%s' is not defined as a valid scope.\n", prefix);
	ns = 0;
      } else {
	/* Check if the node scope is the current scope */
	String *tname = Swig_symbol_qualifiedscopename(0);
	String *nname = Swig_symbol_qualifiedscopename(nstab);
	if (tname && (Strcmp(tname,nname) == 0)) {
	  ns = 0;
	  cname = Copy(last);
	}
	Delete(tname);
	Delete(nname);
      }
      if (ns) {
	/* we will try to create a new node using the namespaces we
	   can find in the scope name */
	List *scopes = Swig_scopename_tolist(prefix);
	String *sname;
	Iterator si;

	for (si = First(scopes); si.item; si = Next(si)) {
	  Node *ns1,*ns2;
	  sname = si.item;
	  ns1 = Swig_symbol_clookup(sname,0);
	  assert(ns1);
	  if (Strcmp(nodeType(ns1),"namespace") == 0) {
	    if (Getattr(ns1,"alias")) {
	      ns1 = Getattr(ns1,"namespace");
	    }
	  } else {
	    /* now this last part is a class */
	    si = Next(si);
	    /*  or a nested class tree, which is unrolled here */
	    for (; si.item; si = Next(si)) {
	      if (si.item) {
		Printf(sname,"::%s",si.item);
	      }
	    }
	    /* we get the 'inner' class */
	    nscope_inner = Swig_symbol_clookup(sname,0);
	    /* set the scope to the inner class */
	    Swig_symbol_setscope(Getattr(nscope_inner,"symtab"));
	    /* save the last namespace prefix */
	    Delete(Namespaceprefix);
	    Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	    /* and return the node name, including the inner class prefix */
	    break;
	  }
	  /* here we just populate the namespace tree as usual */
	  ns2 = new_node("namespace");
	  Setattr(ns2,"name",sname);
	  Setattr(ns2,"symtab", Getattr(ns1,"symtab"));
	  add_symbols(ns2);
	  Swig_symbol_setscope(Getattr(ns1,"symtab"));
	  Delete(Namespaceprefix);
	  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	  if (nscope_inner) {
	    if (Getattr(nscope_inner,"symtab") != Getattr(ns2,"symtab")) {
	      appendChild(nscope_inner,ns2);
	      Delete(ns2);
	    }
	  }
	  nscope_inner = ns2;
	  if (!nscope) nscope = ns2;
	}
	cname = Copy(last);
	Delete(scopes);
      }
    }
    Delete(prefix);
  }
  Delete(last);

  return cname;
}
 
/* look for simple typedef name in typedef list */
static String *try_to_find_a_name_for_unnamed_structure(const char *storage, Node *decls) {
  String *name = 0;
  Node *n = decls;
  if (storage && (strcmp(storage, "typedef") == 0)) {
    for (; n; n = nextSibling(n)) {
      if (!Len(Getattr(n, "decl"))) {
	name = Copy(Getattr(n, "name"));
	break;
      }
    }
  }
  return name;
}

/* traverse copied tree segment, and update outer class links*/
static void update_nested_classes(Node *n)
{
  Node *c = firstChild(n);
  while (c) {
    if (Getattr(c, "nested:outer"))
      Setattr(c, "nested:outer", n);
    update_nested_classes(c);
    c = nextSibling(c);
  }
}

/* -----------------------------------------------------------------------------
 * nested_forward_declaration()
 * 
 * Nested struct handling for C++ code if the nested classes are disabled.
 * Create the nested class/struct/union as a forward declaration.
 * ----------------------------------------------------------------------------- */

static Node *nested_forward_declaration(const char *storage, const char *kind, String *sname, String *name, Node *cpp_opt_declarators) {
  Node *nn = 0;

  if (sname) {
    /* Add forward declaration of the nested type */
    Node *n = new_node("classforward");
    Setattr(n, "kind", kind);
    Setattr(n, "name", sname);
    Setattr(n, "storage", storage);
    Setattr(n, "sym:weak", "1");
    add_symbols(n);
    nn = n;
  }

  /* Add any variable instances. Also add in any further typedefs of the nested type.
     Note that anonymous typedefs (eg typedef struct {...} a, b;) are treated as class forward declarations */
  if (cpp_opt_declarators) {
    int storage_typedef = (storage && (strcmp(storage, "typedef") == 0));
    int variable_of_anonymous_type = !sname && !storage_typedef;
    if (!variable_of_anonymous_type) {
      int anonymous_typedef = !sname && (storage && (strcmp(storage, "typedef") == 0));
      Node *n = cpp_opt_declarators;
      SwigType *type = name;
      while (n) {
	Setattr(n, "type", type);
	Setattr(n, "storage", storage);
	if (anonymous_typedef) {
	  Setattr(n, "nodeType", "classforward");
	  Setattr(n, "sym:weak", "1");
	}
	n = nextSibling(n);
      }
      add_symbols(cpp_opt_declarators);

      if (nn) {
	set_nextSibling(nn, cpp_opt_declarators);
      } else {
	nn = cpp_opt_declarators;
      }
    }
  }

  if (!currentOuterClass || !GetFlag(currentOuterClass, "nested")) {
    if (nn && Equal(nodeType(nn), "classforward")) {
      Node *n = nn;
      if (!GetFlag(n, "feature:ignore")) {
	SWIG_WARN_NODE_BEGIN(n);
	Swig_warning(WARN_PARSE_NAMED_NESTED_CLASS, cparse_file, cparse_line,"Nested %s not currently supported (%s ignored)\n", kind, sname ? sname : name);
	SWIG_WARN_NODE_END(n);
      }
    } else {
      Swig_warning(WARN_PARSE_UNNAMED_NESTED_CLASS, cparse_file, cparse_line, "Nested %s not currently supported (ignored).\n", kind);
    }
  }

  return nn;
}


Node *Swig_cparse(File *f) {
  scanner_file(f);
  top = 0;
  yyparse();
  return top;
}

static void single_new_feature(const char *featurename, String *val, Hash *featureattribs, char *declaratorid, SwigType *type, ParmList *declaratorparms, String *qualifier) {
  String *fname;
  String *name;
  String *fixname;
  SwigType *t = Copy(type);

  /* Printf(stdout, "single_new_feature: [%s] [%s] [%s] [%s] [%s] [%s]\n", featurename, val, declaratorid, t, ParmList_str_defaultargs(declaratorparms), qualifier); */

  /* Warn about deprecated features */
  if (strcmp(featurename, "nestedworkaround") == 0)
    Swig_warning(WARN_DEPRECATED_NESTED_WORKAROUND, cparse_file, cparse_line, "The 'nestedworkaround' feature is deprecated.\n");

  fname = NewStringf("feature:%s",featurename);
  if (declaratorid) {
    fixname = feature_identifier_fix(declaratorid);
  } else {
    fixname = NewStringEmpty();
  }
  if (Namespaceprefix) {
    name = NewStringf("%s::%s",Namespaceprefix, fixname);
  } else {
    name = fixname;
  }

  if (declaratorparms) Setmeta(val,"parms",declaratorparms);
  if (!Len(t)) t = 0;
  if (t) {
    if (qualifier) SwigType_push(t,qualifier);
    if (SwigType_isfunction(t)) {
      SwigType *decl = SwigType_pop_function(t);
      if (SwigType_ispointer(t)) {
	String *nname = NewStringf("*%s",name);
	Swig_feature_set(Swig_cparse_features(), nname, decl, fname, val, featureattribs);
	Delete(nname);
      } else {
	Swig_feature_set(Swig_cparse_features(), name, decl, fname, val, featureattribs);
      }
      Delete(decl);
    } else if (SwigType_ispointer(t)) {
      String *nname = NewStringf("*%s",name);
      Swig_feature_set(Swig_cparse_features(),nname,0,fname,val, featureattribs);
      Delete(nname);
    }
  } else {
    /* Global feature, that is, feature not associated with any particular symbol */
    Swig_feature_set(Swig_cparse_features(),name,0,fname,val, featureattribs);
  }
  Delete(fname);
  Delete(name);
}

/* Add a new feature to the Hash. Additional features are added if the feature has a parameter list (declaratorparms)
 * and one or more of the parameters have a default argument. An extra feature is added for each defaulted parameter,
 * simulating the equivalent overloaded method. */
static void new_feature(const char *featurename, String *val, Hash *featureattribs, char *declaratorid, SwigType *type, ParmList *declaratorparms, String *qualifier) {

  ParmList *declparms = declaratorparms;

  /* remove the { and } braces if the noblock attribute is set */
  String *newval = remove_block(featureattribs, val);
  val = newval ? newval : val;

  /* Add the feature */
  single_new_feature(featurename, val, featureattribs, declaratorid, type, declaratorparms, qualifier);

  /* Add extra features if there are default parameters in the parameter list */
  if (type) {
    while (declparms) {
      if (ParmList_has_defaultargs(declparms)) {

        /* Create a parameter list for the new feature by copying all
           but the last (defaulted) parameter */
        ParmList* newparms = CopyParmListMax(declparms, ParmList_len(declparms)-1);

        /* Create new declaration - with the last parameter removed */
        SwigType *newtype = Copy(type);
        Delete(SwigType_pop_function(newtype)); /* remove the old parameter list from newtype */
        SwigType_add_function(newtype,newparms);

        single_new_feature(featurename, Copy(val), featureattribs, declaratorid, newtype, newparms, qualifier);
        declparms = newparms;
      } else {
        declparms = 0;
      }
    }
  }
}

/* check if a function declaration is a plain C object */
static int is_cfunction(Node *n) {
  if (!cparse_cplusplus || cparse_externc)
    return 1;
  if (Swig_storage_isexternc(n)) {
    return 1;
  }
  return 0;
}

/* If the Node is a function with parameters, check to see if any of the parameters
 * have default arguments. If so create a new function for each defaulted argument. 
 * The additional functions form a linked list of nodes with the head being the original Node n. */
static void default_arguments(Node *n) {
  Node *function = n;

  if (function) {
    ParmList *varargs = Getattr(function,"feature:varargs");
    if (varargs) {
      /* Handles the %varargs directive by looking for "feature:varargs" and 
       * substituting ... with an alternative set of arguments.  */
      Parm     *p = Getattr(function,"parms");
      Parm     *pp = 0;
      while (p) {
	SwigType *t = Getattr(p,"type");
	if (Strcmp(t,"v(...)") == 0) {
	  if (pp) {
	    ParmList *cv = Copy(varargs);
	    set_nextSibling(pp,cv);
	    Delete(cv);
	  } else {
	    ParmList *cv =  Copy(varargs);
	    Setattr(function,"parms", cv);
	    Delete(cv);
	  }
	  break;
	}
	pp = p;
	p = nextSibling(p);
      }
    }

    /* Do not add in functions if kwargs is being used or if user wants old default argument wrapping
       (one wrapped method per function irrespective of number of default arguments) */
    if (compact_default_args 
	|| is_cfunction(function) 
	|| GetFlag(function,"feature:compactdefaultargs") 
	|| (GetFlag(function,"feature:kwargs") && kwargs_supported)) {
      ParmList *p = Getattr(function,"parms");
      if (p) 
        Setattr(p,"compactdefargs", "1"); /* mark parameters for special handling */
      function = 0; /* don't add in extra methods */
    }
  }

  while (function) {
    ParmList *parms = Getattr(function,"parms");
    if (ParmList_has_defaultargs(parms)) {

      /* Create a parameter list for the new function by copying all
         but the last (defaulted) parameter */
      ParmList* newparms = CopyParmListMax(parms,ParmList_len(parms)-1);

      /* Create new function and add to symbol table */
      {
	SwigType *ntype = Copy(nodeType(function));
	char *cntype = Char(ntype);
        Node *new_function = new_node(ntype);
        SwigType *decl = Copy(Getattr(function,"decl"));
        int constqualifier = SwigType_isconst(decl);
	String *ccode = Copy(Getattr(function,"code"));
	String *cstorage = Copy(Getattr(function,"storage"));
	String *cvalue = Copy(Getattr(function,"value"));
	SwigType *ctype = Copy(Getattr(function,"type"));
	String *cthrow = Copy(Getattr(function,"throw"));

        Delete(SwigType_pop_function(decl)); /* remove the old parameter list from decl */
        SwigType_add_function(decl,newparms);
        if (constqualifier)
          SwigType_add_qualifier(decl,"const");

        Setattr(new_function,"name", Getattr(function,"name"));
        Setattr(new_function,"code", ccode);
        Setattr(new_function,"decl", decl);
        Setattr(new_function,"parms", newparms);
        Setattr(new_function,"storage", cstorage);
        Setattr(new_function,"value", cvalue);
        Setattr(new_function,"type", ctype);
        Setattr(new_function,"throw", cthrow);

	Delete(ccode);
	Delete(cstorage);
	Delete(cvalue);
	Delete(ctype);
	Delete(cthrow);
	Delete(decl);

        {
          Node *throws = Getattr(function,"throws");
	  ParmList *pl = CopyParmList(throws);
          if (throws) Setattr(new_function,"throws",pl);
	  Delete(pl);
        }

        /* copy specific attributes for global (or in a namespace) template functions - these are not templated class methods */
        if (strcmp(cntype,"template") == 0) {
          Node *templatetype = Getattr(function,"templatetype");
          Node *symtypename = Getattr(function,"sym:typename");
          Parm *templateparms = Getattr(function,"templateparms");
          if (templatetype) {
	    Node *tmp = Copy(templatetype);
	    Setattr(new_function,"templatetype",tmp);
	    Delete(tmp);
	  }
          if (symtypename) {
	    Node *tmp = Copy(symtypename);
	    Setattr(new_function,"sym:typename",tmp);
	    Delete(tmp);
	  }
          if (templateparms) {
	    Parm *tmp = CopyParmList(templateparms);
	    Setattr(new_function,"templateparms",tmp);
	    Delete(tmp);
	  }
        } else if (strcmp(cntype,"constructor") == 0) {
          /* only copied for constructors as this is not a user defined feature - it is hard coded in the parser */
          if (GetFlag(function,"feature:new")) SetFlag(new_function,"feature:new");
        }

        add_symbols(new_function);
        /* mark added functions as ones with overloaded parameters and point to the parsed method */
        Setattr(new_function,"defaultargs", n);

        /* Point to the new function, extending the linked list */
        set_nextSibling(function, new_function);
	Delete(new_function);
        function = new_function;
	
	Delete(ntype);
      }
    } else {
      function = 0;
    }
  }
}

/* -----------------------------------------------------------------------------
 * mark_nodes_as_extend()
 *
 * Used by the %extend to mark subtypes with "feature:extend".
 * template instances declared within %extend are skipped
 * ----------------------------------------------------------------------------- */

static void mark_nodes_as_extend(Node *n) {
  for (; n; n = nextSibling(n)) {
    if (Getattr(n, "template") && Strcmp(nodeType(n), "class") == 0)
      continue;
    /* Fix me: extend is not a feature. Replace with isextendmember? */
    Setattr(n, "feature:extend", "1");
    mark_nodes_as_extend(firstChild(n));
  }
}

/* -----------------------------------------------------------------------------
 * add_qualifier_to_declarator()
 *
 * Normally the qualifier is pushed on to the front of the type.
 * Adding a qualifier to a pointer to member function is a special case.
 * For example       : typedef double (Cls::*pmf)(void) const;
 * The qualifier is  : q(const).
 * The declarator is : m(Cls).f(void).
 * We need           : m(Cls).q(const).f(void).
 * ----------------------------------------------------------------------------- */

static String *add_qualifier_to_declarator(SwigType *type, SwigType *qualifier) {
  int is_pointer_to_member_function = 0;
  String *decl = Copy(type);
  String *poppedtype = NewString("");
  assert(qualifier);

  while (decl) {
    if (SwigType_ismemberpointer(decl)) {
      String *memberptr = SwigType_pop(decl);
      if (SwigType_isfunction(decl)) {
	is_pointer_to_member_function = 1;
	SwigType_push(decl, qualifier);
	SwigType_push(decl, memberptr);
	Insert(decl, 0, poppedtype);
	Delete(memberptr);
	break;
      } else {
	Append(poppedtype, memberptr);
      }
      Delete(memberptr);
    } else {
      String *popped = SwigType_pop(decl);
      if (!popped)
	break;
      Append(poppedtype, popped);
      Delete(popped);
    }
  }

  if (!is_pointer_to_member_function) {
    Delete(decl);
    decl = Copy(type);
    SwigType_push(decl, qualifier);
  }

  Delete(poppedtype);
  return decl;
}


#line 1600 "y.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    END = 0,                       /* END  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    ID = 258,                      /* ID  */
    HBLOCK = 259,                  /* HBLOCK  */
    POUND = 260,                   /* POUND  */
    STRING = 261,                  /* STRING  */
    WSTRING = 262,                 /* WSTRING  */
    INCLUDE = 263,                 /* INCLUDE  */
    IMPORT = 264,                  /* IMPORT  */
    INSERT = 265,                  /* INSERT  */
    CHARCONST = 266,               /* CHARCONST  */
    WCHARCONST = 267,              /* WCHARCONST  */
    NUM_INT = 268,                 /* NUM_INT  */
    NUM_FLOAT = 269,               /* NUM_FLOAT  */
    NUM_UNSIGNED = 270,            /* NUM_UNSIGNED  */
    NUM_LONG = 271,                /* NUM_LONG  */
    NUM_ULONG = 272,               /* NUM_ULONG  */
    NUM_LONGLONG = 273,            /* NUM_LONGLONG  */
    NUM_ULONGLONG = 274,           /* NUM_ULONGLONG  */
    NUM_BOOL = 275,                /* NUM_BOOL  */
    TYPEDEF = 276,                 /* TYPEDEF  */
    TYPE_INT = 277,                /* TYPE_INT  */
    TYPE_UNSIGNED = 278,           /* TYPE_UNSIGNED  */
    TYPE_SHORT = 279,              /* TYPE_SHORT  */
    TYPE_LONG = 280,               /* TYPE_LONG  */
    TYPE_FLOAT = 281,              /* TYPE_FLOAT  */
    TYPE_DOUBLE = 282,             /* TYPE_DOUBLE  */
    TYPE_CHAR = 283,               /* TYPE_CHAR  */
    TYPE_WCHAR = 284,              /* TYPE_WCHAR  */
    TYPE_VOID = 285,               /* TYPE_VOID  */
    TYPE_SIGNED = 286,             /* TYPE_SIGNED  */
    TYPE_BOOL = 287,               /* TYPE_BOOL  */
    TYPE_COMPLEX = 288,            /* TYPE_COMPLEX  */
    TYPE_TYPEDEF = 289,            /* TYPE_TYPEDEF  */
    TYPE_RAW = 290,                /* TYPE_RAW  */
    TYPE_NON_ISO_INT8 = 291,       /* TYPE_NON_ISO_INT8  */
    TYPE_NON_ISO_INT16 = 292,      /* TYPE_NON_ISO_INT16  */
    TYPE_NON_ISO_INT32 = 293,      /* TYPE_NON_ISO_INT32  */
    TYPE_NON_ISO_INT64 = 294,      /* TYPE_NON_ISO_INT64  */
    LPAREN = 295,                  /* LPAREN  */
    RPAREN = 296,                  /* RPAREN  */
    COMMA = 297,                   /* COMMA  */
    SEMI = 298,                    /* SEMI  */
    EXTERN = 299,                  /* EXTERN  */
    INIT = 300,                    /* INIT  */
    LBRACE = 301,                  /* LBRACE  */
    RBRACE = 302,                  /* RBRACE  */
    PERIOD = 303,                  /* PERIOD  */
    ELLIPSIS = 304,                /* ELLIPSIS  */
    CONST_QUAL = 305,              /* CONST_QUAL  */
    VOLATILE = 306,                /* VOLATILE  */
    REGISTER = 307,                /* REGISTER  */
    STRUCT = 308,                  /* STRUCT  */
    UNION = 309,                   /* UNION  */
    EQUAL = 310,                   /* EQUAL  */
    SIZEOF = 311,                  /* SIZEOF  */
    MODULE = 312,                  /* MODULE  */
    LBRACKET = 313,                /* LBRACKET  */
    RBRACKET = 314,                /* RBRACKET  */
    BEGINFILE = 315,               /* BEGINFILE  */
    ENDOFFILE = 316,               /* ENDOFFILE  */
    ILLEGAL = 317,                 /* ILLEGAL  */
    CONSTANT = 318,                /* CONSTANT  */
    NAME = 319,                    /* NAME  */
    RENAME = 320,                  /* RENAME  */
    NAMEWARN = 321,                /* NAMEWARN  */
    EXTEND = 322,                  /* EXTEND  */
    PRAGMA = 323,                  /* PRAGMA  */
    FEATURE = 324,                 /* FEATURE  */
    VARARGS = 325,                 /* VARARGS  */
    ENUM = 326,                    /* ENUM  */
    CLASS = 327,                   /* CLASS  */
    TYPENAME = 328,                /* TYPENAME  */
    PRIVATE = 329,                 /* PRIVATE  */
    PUBLIC = 330,                  /* PUBLIC  */
    PROTECTED = 331,               /* PROTECTED  */
    COLON = 332,                   /* COLON  */
    STATIC = 333,                  /* STATIC  */
    VIRTUAL = 334,                 /* VIRTUAL  */
    FRIEND = 335,                  /* FRIEND  */
    THROW = 336,                   /* THROW  */
    CATCH = 337,                   /* CATCH  */
    EXPLICIT = 338,                /* EXPLICIT  */
    STATIC_ASSERT = 339,           /* STATIC_ASSERT  */
    CONSTEXPR = 340,               /* CONSTEXPR  */
    THREAD_LOCAL = 341,            /* THREAD_LOCAL  */
    DECLTYPE = 342,                /* DECLTYPE  */
    AUTO = 343,                    /* AUTO  */
    NOEXCEPT = 344,                /* NOEXCEPT  */
    OVERRIDE = 345,                /* OVERRIDE  */
    FINAL = 346,                   /* FINAL  */
    USING = 347,                   /* USING  */
    NAMESPACE = 348,               /* NAMESPACE  */
    NATIVE = 349,                  /* NATIVE  */
    INLINE = 350,                  /* INLINE  */
    TYPEMAP = 351,                 /* TYPEMAP  */
    EXCEPT = 352,                  /* EXCEPT  */
    ECHO = 353,                    /* ECHO  */
    APPLY = 354,                   /* APPLY  */
    CLEAR = 355,                   /* CLEAR  */
    SWIGTEMPLATE = 356,            /* SWIGTEMPLATE  */
    FRAGMENT = 357,                /* FRAGMENT  */
    WARN = 358,                    /* WARN  */
    LESSTHAN = 359,                /* LESSTHAN  */
    GREATERTHAN = 360,             /* GREATERTHAN  */
    DELETE_KW = 361,               /* DELETE_KW  */
    DEFAULT = 362,                 /* DEFAULT  */
    LESSTHANOREQUALTO = 363,       /* LESSTHANOREQUALTO  */
    GREATERTHANOREQUALTO = 364,    /* GREATERTHANOREQUALTO  */
    EQUALTO = 365,                 /* EQUALTO  */
    NOTEQUALTO = 366,              /* NOTEQUALTO  */
    LESSEQUALGREATER = 367,        /* LESSEQUALGREATER  */
    ARROW = 368,                   /* ARROW  */
    QUESTIONMARK = 369,            /* QUESTIONMARK  */
    TYPES = 370,                   /* TYPES  */
    PARMS = 371,                   /* PARMS  */
    NONID = 372,                   /* NONID  */
    DSTAR = 373,                   /* DSTAR  */
    DCNOT = 374,                   /* DCNOT  */
    TEMPLATE = 375,                /* TEMPLATE  */
    OPERATOR = 376,                /* OPERATOR  */
    CONVERSIONOPERATOR = 377,      /* CONVERSIONOPERATOR  */
    PARSETYPE = 378,               /* PARSETYPE  */
    PARSEPARM = 379,               /* PARSEPARM  */
    PARSEPARMS = 380,              /* PARSEPARMS  */
    DOXYGENSTRING = 381,           /* DOXYGENSTRING  */
    DOXYGENPOSTSTRING = 382,       /* DOXYGENPOSTSTRING  */
    CAST = 383,                    /* CAST  */
    LOR = 384,                     /* LOR  */
    LAND = 385,                    /* LAND  */
    OR = 386,                      /* OR  */
    XOR = 387,                     /* XOR  */
    AND = 388,                     /* AND  */
    LSHIFT = 389,                  /* LSHIFT  */
    RSHIFT = 390,                  /* RSHIFT  */
    PLUS = 391,                    /* PLUS  */
    MINUS = 392,                   /* MINUS  */
    STAR = 393,                    /* STAR  */
    SLASH = 394,                   /* SLASH  */
    MODULO = 395,                  /* MODULO  */
    UMINUS = 396,                  /* UMINUS  */
    NOT = 397,                     /* NOT  */
    LNOT = 398,                    /* LNOT  */
    DCOLON = 399                   /* DCOLON  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define END 0
#define YYerror 256
#define YYUNDEF 257
#define ID 258
#define HBLOCK 259
#define POUND 260
#define STRING 261
#define WSTRING 262
#define INCLUDE 263
#define IMPORT 264
#define INSERT 265
#define CHARCONST 266
#define WCHARCONST 267
#define NUM_INT 268
#define NUM_FLOAT 269
#define NUM_UNSIGNED 270
#define NUM_LONG 271
#define NUM_ULONG 272
#define NUM_LONGLONG 273
#define NUM_ULONGLONG 274
#define NUM_BOOL 275
#define TYPEDEF 276
#define TYPE_INT 277
#define TYPE_UNSIGNED 278
#define TYPE_SHORT 279
#define TYPE_LONG 280
#define TYPE_FLOAT 281
#define TYPE_DOUBLE 282
#define TYPE_CHAR 283
#define TYPE_WCHAR 284
#define TYPE_VOID 285
#define TYPE_SIGNED 286
#define TYPE_BOOL 287
#define TYPE_COMPLEX 288
#define TYPE_TYPEDEF 289
#define TYPE_RAW 290
#define TYPE_NON_ISO_INT8 291
#define TYPE_NON_ISO_INT16 292
#define TYPE_NON_ISO_INT32 293
#define TYPE_NON_ISO_INT64 294
#define LPAREN 295
#define RPAREN 296
#define COMMA 297
#define SEMI 298
#define EXTERN 299
#define INIT 300
#define LBRACE 301
#define RBRACE 302
#define PERIOD 303
#define ELLIPSIS 304
#define CONST_QUAL 305
#define VOLATILE 306
#define REGISTER 307
#define STRUCT 308
#define UNION 309
#define EQUAL 310
#define SIZEOF 311
#define MODULE 312
#define LBRACKET 313
#define RBRACKET 314
#define BEGINFILE 315
#define ENDOFFILE 316
#define ILLEGAL 317
#define CONSTANT 318
#define NAME 319
#define RENAME 320
#define NAMEWARN 321
#define EXTEND 322
#define PRAGMA 323
#define FEATURE 324
#define VARARGS 325
#define ENUM 326
#define CLASS 327
#define TYPENAME 328
#define PRIVATE 329
#define PUBLIC 330
#define PROTECTED 331
#define COLON 332
#define STATIC 333
#define VIRTUAL 334
#define FRIEND 335
#define THROW 336
#define CATCH 337
#define EXPLICIT 338
#define STATIC_ASSERT 339
#define CONSTEXPR 340
#define THREAD_LOCAL 341
#define DECLTYPE 342
#define AUTO 343
#define NOEXCEPT 344
#define OVERRIDE 345
#define FINAL 346
#define USING 347
#define NAMESPACE 348
#define NATIVE 349
#define INLINE 350
#define TYPEMAP 351
#define EXCEPT 352
#define ECHO 353
#define APPLY 354
#define CLEAR 355
#define SWIGTEMPLATE 356
#define FRAGMENT 357
#define WARN 358
#define LESSTHAN 359
#define GREATERTHAN 360
#define DELETE_KW 361
#define DEFAULT 362
#define LESSTHANOREQUALTO 363
#define GREATERTHANOREQUALTO 364
#define EQUALTO 365
#define NOTEQUALTO 366
#define LESSEQUALGREATER 367
#define ARROW 368
#define QUESTIONMARK 369
#define TYPES 370
#define PARMS 371
#define NONID 372
#define DSTAR 373
#define DCNOT 374
#define TEMPLATE 375
#define OPERATOR 376
#define CONVERSIONOPERATOR 377
#define PARSETYPE 378
#define PARSEPARM 379
#define PARSEPARMS 380
#define DOXYGENSTRING 381
#define DOXYGENPOSTSTRING 382
#define CAST 383
#define LOR 384
#define LAND 385
#define OR 386
#define XOR 387
#define AND 388
#define LSHIFT 389
#define RSHIFT 390
#define PLUS 391
#define MINUS 392
#define STAR 393
#define SLASH 394
#define MODULO 395
#define UMINUS 396
#define NOT 397
#define LNOT 398
#define DCOLON 399

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 1554 "parser.y"

  const char  *id;
  List  *bases;
  struct Define {
    String *val;
    String *rawval;
    int     type;
    String *qualifier;
    String *refqualifier;
    String *bitfield;
    Parm   *throws;
    String *throwf;
    String *nexcept;
    String *final;
  } dtype;
  struct {
    const char *type;
    String *filename;
    int   line;
  } loc;
  struct {
    char      *id;
    SwigType  *type;
    String    *defarg;
    ParmList  *parms;
    short      have_parms;
    ParmList  *throws;
    String    *throwf;
    String    *nexcept;
    String    *final;
  } decl;
  Parm         *tparms;
  struct {
    String     *method;
    Hash       *kwargs;
  } tmap;
  struct {
    String     *type;
    String     *us;
  } ptype;
  SwigType     *type;
  String       *str;
  Parm         *p;
  ParmList     *pl;
  int           intvalue;
  Node         *node;

#line 1989 "y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* END  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_ID = 3,                         /* ID  */
  YYSYMBOL_HBLOCK = 4,                     /* HBLOCK  */
  YYSYMBOL_POUND = 5,                      /* POUND  */
  YYSYMBOL_STRING = 6,                     /* STRING  */
  YYSYMBOL_WSTRING = 7,                    /* WSTRING  */
  YYSYMBOL_INCLUDE = 8,                    /* INCLUDE  */
  YYSYMBOL_IMPORT = 9,                     /* IMPORT  */
  YYSYMBOL_INSERT = 10,                    /* INSERT  */
  YYSYMBOL_CHARCONST = 11,                 /* CHARCONST  */
  YYSYMBOL_WCHARCONST = 12,                /* WCHARCONST  */
  YYSYMBOL_NUM_INT = 13,                   /* NUM_INT  */
  YYSYMBOL_NUM_FLOAT = 14,                 /* NUM_FLOAT  */
  YYSYMBOL_NUM_UNSIGNED = 15,              /* NUM_UNSIGNED  */
  YYSYMBOL_NUM_LONG = 16,                  /* NUM_LONG  */
  YYSYMBOL_NUM_ULONG = 17,                 /* NUM_ULONG  */
  YYSYMBOL_NUM_LONGLONG = 18,              /* NUM_LONGLONG  */
  YYSYMBOL_NUM_ULONGLONG = 19,             /* NUM_ULONGLONG  */
  YYSYMBOL_NUM_BOOL = 20,                  /* NUM_BOOL  */
  YYSYMBOL_TYPEDEF = 21,                   /* TYPEDEF  */
  YYSYMBOL_TYPE_INT = 22,                  /* TYPE_INT  */
  YYSYMBOL_TYPE_UNSIGNED = 23,             /* TYPE_UNSIGNED  */
  YYSYMBOL_TYPE_SHORT = 24,                /* TYPE_SHORT  */
  YYSYMBOL_TYPE_LONG = 25,                 /* TYPE_LONG  */
  YYSYMBOL_TYPE_FLOAT = 26,                /* TYPE_FLOAT  */
  YYSYMBOL_TYPE_DOUBLE = 27,               /* TYPE_DOUBLE  */
  YYSYMBOL_TYPE_CHAR = 28,                 /* TYPE_CHAR  */
  YYSYMBOL_TYPE_WCHAR = 29,                /* TYPE_WCHAR  */
  YYSYMBOL_TYPE_VOID = 30,                 /* TYPE_VOID  */
  YYSYMBOL_TYPE_SIGNED = 31,               /* TYPE_SIGNED  */
  YYSYMBOL_TYPE_BOOL = 32,                 /* TYPE_BOOL  */
  YYSYMBOL_TYPE_COMPLEX = 33,              /* TYPE_COMPLEX  */
  YYSYMBOL_TYPE_TYPEDEF = 34,              /* TYPE_TYPEDEF  */
  YYSYMBOL_TYPE_RAW = 35,                  /* TYPE_RAW  */
  YYSYMBOL_TYPE_NON_ISO_INT8 = 36,         /* TYPE_NON_ISO_INT8  */
  YYSYMBOL_TYPE_NON_ISO_INT16 = 37,        /* TYPE_NON_ISO_INT16  */
  YYSYMBOL_TYPE_NON_ISO_INT32 = 38,        /* TYPE_NON_ISO_INT32  */
  YYSYMBOL_TYPE_NON_ISO_INT64 = 39,        /* TYPE_NON_ISO_INT64  */
  YYSYMBOL_LPAREN = 40,                    /* LPAREN  */
  YYSYMBOL_RPAREN = 41,                    /* RPAREN  */
  YYSYMBOL_COMMA = 42,                     /* COMMA  */
  YYSYMBOL_SEMI = 43,                      /* SEMI  */
  YYSYMBOL_EXTERN = 44,                    /* EXTERN  */
  YYSYMBOL_INIT = 45,                      /* INIT  */
  YYSYMBOL_LBRACE = 46,                    /* LBRACE  */
  YYSYMBOL_RBRACE = 47,                    /* RBRACE  */
  YYSYMBOL_PERIOD = 48,                    /* PERIOD  */
  YYSYMBOL_ELLIPSIS = 49,                  /* ELLIPSIS  */
  YYSYMBOL_CONST_QUAL = 50,                /* CONST_QUAL  */
  YYSYMBOL_VOLATILE = 51,                  /* VOLATILE  */
  YYSYMBOL_REGISTER = 52,                  /* REGISTER  */
  YYSYMBOL_STRUCT = 53,                    /* STRUCT  */
  YYSYMBOL_UNION = 54,                     /* UNION  */
  YYSYMBOL_EQUAL = 55,                     /* EQUAL  */
  YYSYMBOL_SIZEOF = 56,                    /* SIZEOF  */
  YYSYMBOL_MODULE = 57,                    /* MODULE  */
  YYSYMBOL_LBRACKET = 58,                  /* LBRACKET  */
  YYSYMBOL_RBRACKET = 59,                  /* RBRACKET  */
  YYSYMBOL_BEGINFILE = 60,                 /* BEGINFILE  */
  YYSYMBOL_ENDOFFILE = 61,                 /* ENDOFFILE  */
  YYSYMBOL_ILLEGAL = 62,                   /* ILLEGAL  */
  YYSYMBOL_CONSTANT = 63,                  /* CONSTANT  */
  YYSYMBOL_NAME = 64,                      /* NAME  */
  YYSYMBOL_RENAME = 65,                    /* RENAME  */
  YYSYMBOL_NAMEWARN = 66,                  /* NAMEWARN  */
  YYSYMBOL_EXTEND = 67,                    /* EXTEND  */
  YYSYMBOL_PRAGMA = 68,                    /* PRAGMA  */
  YYSYMBOL_FEATURE = 69,                   /* FEATURE  */
  YYSYMBOL_VARARGS = 70,                   /* VARARGS  */
  YYSYMBOL_ENUM = 71,                      /* ENUM  */
  YYSYMBOL_CLASS = 72,                     /* CLASS  */
  YYSYMBOL_TYPENAME = 73,                  /* TYPENAME  */
  YYSYMBOL_PRIVATE = 74,                   /* PRIVATE  */
  YYSYMBOL_PUBLIC = 75,                    /* PUBLIC  */
  YYSYMBOL_PROTECTED = 76,                 /* PROTECTED  */
  YYSYMBOL_COLON = 77,                     /* COLON  */
  YYSYMBOL_STATIC = 78,                    /* STATIC  */
  YYSYMBOL_VIRTUAL = 79,                   /* VIRTUAL  */
  YYSYMBOL_FRIEND = 80,                    /* FRIEND  */
  YYSYMBOL_THROW = 81,                     /* THROW  */
  YYSYMBOL_CATCH = 82,                     /* CATCH  */
  YYSYMBOL_EXPLICIT = 83,                  /* EXPLICIT  */
  YYSYMBOL_STATIC_ASSERT = 84,             /* STATIC_ASSERT  */
  YYSYMBOL_CONSTEXPR = 85,                 /* CONSTEXPR  */
  YYSYMBOL_THREAD_LOCAL = 86,              /* THREAD_LOCAL  */
  YYSYMBOL_DECLTYPE = 87,                  /* DECLTYPE  */
  YYSYMBOL_AUTO = 88,                      /* AUTO  */
  YYSYMBOL_NOEXCEPT = 89,                  /* NOEXCEPT  */
  YYSYMBOL_OVERRIDE = 90,                  /* OVERRIDE  */
  YYSYMBOL_FINAL = 91,                     /* FINAL  */
  YYSYMBOL_USING = 92,                     /* USING  */
  YYSYMBOL_NAMESPACE = 93,                 /* NAMESPACE  */
  YYSYMBOL_NATIVE = 94,                    /* NATIVE  */
  YYSYMBOL_INLINE = 95,                    /* INLINE  */
  YYSYMBOL_TYPEMAP = 96,                   /* TYPEMAP  */
  YYSYMBOL_EXCEPT = 97,                    /* EXCEPT  */
  YYSYMBOL_ECHO = 98,                      /* ECHO  */
  YYSYMBOL_APPLY = 99,                     /* APPLY  */
  YYSYMBOL_CLEAR = 100,                    /* CLEAR  */
  YYSYMBOL_SWIGTEMPLATE = 101,             /* SWIGTEMPLATE  */
  YYSYMBOL_FRAGMENT = 102,                 /* FRAGMENT  */
  YYSYMBOL_WARN = 103,                     /* WARN  */
  YYSYMBOL_LESSTHAN = 104,                 /* LESSTHAN  */
  YYSYMBOL_GREATERTHAN = 105,              /* GREATERTHAN  */
  YYSYMBOL_DELETE_KW = 106,                /* DELETE_KW  */
  YYSYMBOL_DEFAULT = 107,                  /* DEFAULT  */
  YYSYMBOL_LESSTHANOREQUALTO = 108,        /* LESSTHANOREQUALTO  */
  YYSYMBOL_GREATERTHANOREQUALTO = 109,     /* GREATERTHANOREQUALTO  */
  YYSYMBOL_EQUALTO = 110,                  /* EQUALTO  */
  YYSYMBOL_NOTEQUALTO = 111,               /* NOTEQUALTO  */
  YYSYMBOL_LESSEQUALGREATER = 112,         /* LESSEQUALGREATER  */
  YYSYMBOL_ARROW = 113,                    /* ARROW  */
  YYSYMBOL_QUESTIONMARK = 114,             /* QUESTIONMARK  */
  YYSYMBOL_TYPES = 115,                    /* TYPES  */
  YYSYMBOL_PARMS = 116,                    /* PARMS  */
  YYSYMBOL_NONID = 117,                    /* NONID  */
  YYSYMBOL_DSTAR = 118,                    /* DSTAR  */
  YYSYMBOL_DCNOT = 119,                    /* DCNOT  */
  YYSYMBOL_TEMPLATE = 120,                 /* TEMPLATE  */
  YYSYMBOL_OPERATOR = 121,                 /* OPERATOR  */
  YYSYMBOL_CONVERSIONOPERATOR = 122,       /* CONVERSIONOPERATOR  */
  YYSYMBOL_PARSETYPE = 123,                /* PARSETYPE  */
  YYSYMBOL_PARSEPARM = 124,                /* PARSEPARM  */
  YYSYMBOL_PARSEPARMS = 125,               /* PARSEPARMS  */
  YYSYMBOL_DOXYGENSTRING = 126,            /* DOXYGENSTRING  */
  YYSYMBOL_DOXYGENPOSTSTRING = 127,        /* DOXYGENPOSTSTRING  */
  YYSYMBOL_CAST = 128,                     /* CAST  */
  YYSYMBOL_LOR = 129,                      /* LOR  */
  YYSYMBOL_LAND = 130,                     /* LAND  */
  YYSYMBOL_OR = 131,                       /* OR  */
  YYSYMBOL_XOR = 132,                      /* XOR  */
  YYSYMBOL_AND = 133,                      /* AND  */
  YYSYMBOL_LSHIFT = 134,                   /* LSHIFT  */
  YYSYMBOL_RSHIFT = 135,                   /* RSHIFT  */
  YYSYMBOL_PLUS = 136,                     /* PLUS  */
  YYSYMBOL_MINUS = 137,                    /* MINUS  */
  YYSYMBOL_STAR = 138,                     /* STAR  */
  YYSYMBOL_SLASH = 139,                    /* SLASH  */
  YYSYMBOL_MODULO = 140,                   /* MODULO  */
  YYSYMBOL_UMINUS = 141,                   /* UMINUS  */
  YYSYMBOL_NOT = 142,                      /* NOT  */
  YYSYMBOL_LNOT = 143,                     /* LNOT  */
  YYSYMBOL_DCOLON = 144,                   /* DCOLON  */
  YYSYMBOL_YYACCEPT = 145,                 /* $accept  */
  YYSYMBOL_program = 146,                  /* program  */
  YYSYMBOL_interface = 147,                /* interface  */
  YYSYMBOL_declaration = 148,              /* declaration  */
  YYSYMBOL_swig_directive = 149,           /* swig_directive  */
  YYSYMBOL_extend_directive = 150,         /* extend_directive  */
  YYSYMBOL_151_1 = 151,                    /* $@1  */
  YYSYMBOL_apply_directive = 152,          /* apply_directive  */
  YYSYMBOL_clear_directive = 153,          /* clear_directive  */
  YYSYMBOL_constant_directive = 154,       /* constant_directive  */
  YYSYMBOL_echo_directive = 155,           /* echo_directive  */
  YYSYMBOL_except_directive = 156,         /* except_directive  */
  YYSYMBOL_stringtype = 157,               /* stringtype  */
  YYSYMBOL_fname = 158,                    /* fname  */
  YYSYMBOL_fragment_directive = 159,       /* fragment_directive  */
  YYSYMBOL_include_directive = 160,        /* include_directive  */
  YYSYMBOL_161_2 = 161,                    /* $@2  */
  YYSYMBOL_includetype = 162,              /* includetype  */
  YYSYMBOL_inline_directive = 163,         /* inline_directive  */
  YYSYMBOL_insert_directive = 164,         /* insert_directive  */
  YYSYMBOL_module_directive = 165,         /* module_directive  */
  YYSYMBOL_name_directive = 166,           /* name_directive  */
  YYSYMBOL_native_directive = 167,         /* native_directive  */
  YYSYMBOL_pragma_directive = 168,         /* pragma_directive  */
  YYSYMBOL_pragma_arg = 169,               /* pragma_arg  */
  YYSYMBOL_pragma_lang = 170,              /* pragma_lang  */
  YYSYMBOL_rename_directive = 171,         /* rename_directive  */
  YYSYMBOL_rename_namewarn = 172,          /* rename_namewarn  */
  YYSYMBOL_feature_directive = 173,        /* feature_directive  */
  YYSYMBOL_stringbracesemi = 174,          /* stringbracesemi  */
  YYSYMBOL_featattr = 175,                 /* featattr  */
  YYSYMBOL_varargs_directive = 176,        /* varargs_directive  */
  YYSYMBOL_varargs_parms = 177,            /* varargs_parms  */
  YYSYMBOL_typemap_directive = 178,        /* typemap_directive  */
  YYSYMBOL_typemap_type = 179,             /* typemap_type  */
  YYSYMBOL_tm_list = 180,                  /* tm_list  */
  YYSYMBOL_tm_tail = 181,                  /* tm_tail  */
  YYSYMBOL_typemap_parm = 182,             /* typemap_parm  */
  YYSYMBOL_types_directive = 183,          /* types_directive  */
  YYSYMBOL_template_directive = 184,       /* template_directive  */
  YYSYMBOL_warn_directive = 185,           /* warn_directive  */
  YYSYMBOL_c_declaration = 186,            /* c_declaration  */
  YYSYMBOL_187_3 = 187,                    /* $@3  */
  YYSYMBOL_c_decl = 188,                   /* c_decl  */
  YYSYMBOL_c_decl_tail = 189,              /* c_decl_tail  */
  YYSYMBOL_initializer = 190,              /* initializer  */
  YYSYMBOL_cpp_alternate_rettype = 191,    /* cpp_alternate_rettype  */
  YYSYMBOL_cpp_lambda_decl = 192,          /* cpp_lambda_decl  */
  YYSYMBOL_lambda_introducer = 193,        /* lambda_introducer  */
  YYSYMBOL_lambda_template = 194,          /* lambda_template  */
  YYSYMBOL_lambda_body = 195,              /* lambda_body  */
  YYSYMBOL_lambda_tail = 196,              /* lambda_tail  */
  YYSYMBOL_197_4 = 197,                    /* $@4  */
  YYSYMBOL_c_enum_key = 198,               /* c_enum_key  */
  YYSYMBOL_c_enum_inherit = 199,           /* c_enum_inherit  */
  YYSYMBOL_c_enum_forward_decl = 200,      /* c_enum_forward_decl  */
  YYSYMBOL_c_enum_decl = 201,              /* c_enum_decl  */
  YYSYMBOL_c_constructor_decl = 202,       /* c_constructor_decl  */
  YYSYMBOL_cpp_declaration = 203,          /* cpp_declaration  */
  YYSYMBOL_cpp_class_decl = 204,           /* cpp_class_decl  */
  YYSYMBOL_205_5 = 205,                    /* @5  */
  YYSYMBOL_206_6 = 206,                    /* @6  */
  YYSYMBOL_cpp_opt_declarators = 207,      /* cpp_opt_declarators  */
  YYSYMBOL_cpp_forward_class_decl = 208,   /* cpp_forward_class_decl  */
  YYSYMBOL_cpp_template_decl = 209,        /* cpp_template_decl  */
  YYSYMBOL_210_7 = 210,                    /* $@7  */
  YYSYMBOL_cpp_template_possible = 211,    /* cpp_template_possible  */
  YYSYMBOL_template_parms = 212,           /* template_parms  */
  YYSYMBOL_templateparameters = 213,       /* templateparameters  */
  YYSYMBOL_templateparameter = 214,        /* templateparameter  */
  YYSYMBOL_templateparameterstail = 215,   /* templateparameterstail  */
  YYSYMBOL_cpp_using_decl = 216,           /* cpp_using_decl  */
  YYSYMBOL_cpp_namespace_decl = 217,       /* cpp_namespace_decl  */
  YYSYMBOL_218_8 = 218,                    /* @8  */
  YYSYMBOL_219_9 = 219,                    /* $@9  */
  YYSYMBOL_cpp_members = 220,              /* cpp_members  */
  YYSYMBOL_221_10 = 221,                   /* $@10  */
  YYSYMBOL_222_11 = 222,                   /* $@11  */
  YYSYMBOL_223_12 = 223,                   /* $@12  */
  YYSYMBOL_cpp_member_no_dox = 224,        /* cpp_member_no_dox  */
  YYSYMBOL_cpp_member = 225,               /* cpp_member  */
  YYSYMBOL_cpp_constructor_decl = 226,     /* cpp_constructor_decl  */
  YYSYMBOL_cpp_destructor_decl = 227,      /* cpp_destructor_decl  */
  YYSYMBOL_cpp_conversion_operator = 228,  /* cpp_conversion_operator  */
  YYSYMBOL_cpp_catch_decl = 229,           /* cpp_catch_decl  */
  YYSYMBOL_cpp_static_assert = 230,        /* cpp_static_assert  */
  YYSYMBOL_cpp_protection_decl = 231,      /* cpp_protection_decl  */
  YYSYMBOL_cpp_swig_directive = 232,       /* cpp_swig_directive  */
  YYSYMBOL_cpp_end = 233,                  /* cpp_end  */
  YYSYMBOL_cpp_vend = 234,                 /* cpp_vend  */
  YYSYMBOL_anonymous_bitfield = 235,       /* anonymous_bitfield  */
  YYSYMBOL_anon_bitfield_type = 236,       /* anon_bitfield_type  */
  YYSYMBOL_extern_string = 237,            /* extern_string  */
  YYSYMBOL_storage_class = 238,            /* storage_class  */
  YYSYMBOL_parms = 239,                    /* parms  */
  YYSYMBOL_rawparms = 240,                 /* rawparms  */
  YYSYMBOL_ptail = 241,                    /* ptail  */
  YYSYMBOL_parm_no_dox = 242,              /* parm_no_dox  */
  YYSYMBOL_parm = 243,                     /* parm  */
  YYSYMBOL_valparms = 244,                 /* valparms  */
  YYSYMBOL_rawvalparms = 245,              /* rawvalparms  */
  YYSYMBOL_valptail = 246,                 /* valptail  */
  YYSYMBOL_valparm = 247,                  /* valparm  */
  YYSYMBOL_callparms = 248,                /* callparms  */
  YYSYMBOL_callptail = 249,                /* callptail  */
  YYSYMBOL_def_args = 250,                 /* def_args  */
  YYSYMBOL_parameter_declarator = 251,     /* parameter_declarator  */
  YYSYMBOL_plain_declarator = 252,         /* plain_declarator  */
  YYSYMBOL_declarator = 253,               /* declarator  */
  YYSYMBOL_notso_direct_declarator = 254,  /* notso_direct_declarator  */
  YYSYMBOL_direct_declarator = 255,        /* direct_declarator  */
  YYSYMBOL_abstract_declarator = 256,      /* abstract_declarator  */
  YYSYMBOL_direct_abstract_declarator = 257, /* direct_abstract_declarator  */
  YYSYMBOL_pointer = 258,                  /* pointer  */
  YYSYMBOL_cv_ref_qualifier = 259,         /* cv_ref_qualifier  */
  YYSYMBOL_ref_qualifier = 260,            /* ref_qualifier  */
  YYSYMBOL_type_qualifier = 261,           /* type_qualifier  */
  YYSYMBOL_type_qualifier_raw = 262,       /* type_qualifier_raw  */
  YYSYMBOL_type = 263,                     /* type  */
  YYSYMBOL_rawtype = 264,                  /* rawtype  */
  YYSYMBOL_type_right = 265,               /* type_right  */
  YYSYMBOL_decltype = 266,                 /* decltype  */
  YYSYMBOL_primitive_type = 267,           /* primitive_type  */
  YYSYMBOL_primitive_type_list = 268,      /* primitive_type_list  */
  YYSYMBOL_type_specifier = 269,           /* type_specifier  */
  YYSYMBOL_definetype = 270,               /* definetype  */
  YYSYMBOL_271_13 = 271,                   /* $@13  */
  YYSYMBOL_default_delete = 272,           /* default_delete  */
  YYSYMBOL_deleted_definition = 273,       /* deleted_definition  */
  YYSYMBOL_explicit_default = 274,         /* explicit_default  */
  YYSYMBOL_ename = 275,                    /* ename  */
  YYSYMBOL_constant_directives = 276,      /* constant_directives  */
  YYSYMBOL_optional_ignored_defines = 277, /* optional_ignored_defines  */
  YYSYMBOL_enumlist = 278,                 /* enumlist  */
  YYSYMBOL_enumlist_item = 279,            /* enumlist_item  */
  YYSYMBOL_edecl_with_dox = 280,           /* edecl_with_dox  */
  YYSYMBOL_edecl = 281,                    /* edecl  */
  YYSYMBOL_etype = 282,                    /* etype  */
  YYSYMBOL_expr = 283,                     /* expr  */
  YYSYMBOL_exprmem = 284,                  /* exprmem  */
  YYSYMBOL_exprsimple = 285,               /* exprsimple  */
  YYSYMBOL_valexpr = 286,                  /* valexpr  */
  YYSYMBOL_exprnum = 287,                  /* exprnum  */
  YYSYMBOL_exprcompound = 288,             /* exprcompound  */
  YYSYMBOL_variadic = 289,                 /* variadic  */
  YYSYMBOL_inherit = 290,                  /* inherit  */
  YYSYMBOL_raw_inherit = 291,              /* raw_inherit  */
  YYSYMBOL_292_14 = 292,                   /* $@14  */
  YYSYMBOL_base_list = 293,                /* base_list  */
  YYSYMBOL_base_specifier = 294,           /* base_specifier  */
  YYSYMBOL_295_15 = 295,                   /* @15  */
  YYSYMBOL_296_16 = 296,                   /* @16  */
  YYSYMBOL_access_specifier = 297,         /* access_specifier  */
  YYSYMBOL_templcpptype = 298,             /* templcpptype  */
  YYSYMBOL_cpptype = 299,                  /* cpptype  */
  YYSYMBOL_classkey = 300,                 /* classkey  */
  YYSYMBOL_classkeyopt = 301,              /* classkeyopt  */
  YYSYMBOL_opt_virtual = 302,              /* opt_virtual  */
  YYSYMBOL_virt_specifier_seq = 303,       /* virt_specifier_seq  */
  YYSYMBOL_virt_specifier_seq_opt = 304,   /* virt_specifier_seq_opt  */
  YYSYMBOL_class_virt_specifier_opt = 305, /* class_virt_specifier_opt  */
  YYSYMBOL_exception_specification = 306,  /* exception_specification  */
  YYSYMBOL_qualifiers_exception_specification = 307, /* qualifiers_exception_specification  */
  YYSYMBOL_cpp_const = 308,                /* cpp_const  */
  YYSYMBOL_ctor_end = 309,                 /* ctor_end  */
  YYSYMBOL_ctor_initializer = 310,         /* ctor_initializer  */
  YYSYMBOL_mem_initializer_list = 311,     /* mem_initializer_list  */
  YYSYMBOL_mem_initializer = 312,          /* mem_initializer  */
  YYSYMBOL_less_valparms_greater = 313,    /* less_valparms_greater  */
  YYSYMBOL_identifier = 314,               /* identifier  */
  YYSYMBOL_idstring = 315,                 /* idstring  */
  YYSYMBOL_idstringopt = 316,              /* idstringopt  */
  YYSYMBOL_idcolon = 317,                  /* idcolon  */
  YYSYMBOL_idcolontail = 318,              /* idcolontail  */
  YYSYMBOL_idtemplate = 319,               /* idtemplate  */
  YYSYMBOL_idtemplatetemplate = 320,       /* idtemplatetemplate  */
  YYSYMBOL_idcolonnt = 321,                /* idcolonnt  */
  YYSYMBOL_idcolontailnt = 322,            /* idcolontailnt  */
  YYSYMBOL_string = 323,                   /* string  */
  YYSYMBOL_wstring = 324,                  /* wstring  */
  YYSYMBOL_stringbrace = 325,              /* stringbrace  */
  YYSYMBOL_options = 326,                  /* options  */
  YYSYMBOL_kwargs = 327,                   /* kwargs  */
  YYSYMBOL_stringnum = 328,                /* stringnum  */
  YYSYMBOL_empty = 329                     /* empty  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  62
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   6373

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  145
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  185
/* YYNRULES -- Number of rules.  */
#define YYNRULES  632
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1203

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   399


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,  1730,  1730,  1742,  1746,  1749,  1752,  1755,  1758,  1763,
    1772,  1776,  1783,  1788,  1789,  1790,  1791,  1792,  1802,  1818,
    1828,  1829,  1830,  1831,  1832,  1833,  1834,  1835,  1836,  1837,
    1838,  1839,  1840,  1841,  1842,  1843,  1844,  1845,  1846,  1847,
    1848,  1855,  1855,  1937,  1947,  1961,  1981,  2005,  2029,  2033,
    2044,  2053,  2072,  2078,  2084,  2089,  2096,  2103,  2107,  2120,
    2129,  2144,  2157,  2157,  2213,  2214,  2221,  2240,  2271,  2275,
    2285,  2290,  2308,  2351,  2357,  2370,  2376,  2402,  2408,  2415,
    2416,  2419,  2420,  2427,  2473,  2519,  2530,  2533,  2560,  2566,
    2572,  2578,  2586,  2592,  2598,  2604,  2612,  2613,  2614,  2617,
    2622,  2632,  2668,  2669,  2704,  2721,  2729,  2742,  2764,  2770,
    2774,  2777,  2788,  2793,  2806,  2818,  3117,  3127,  3134,  3135,
    3139,  3139,  3172,  3178,  3188,  3200,  3209,  3289,  3352,  3356,
    3381,  3385,  3396,  3401,  3402,  3403,  3407,  3408,  3409,  3413,
    3424,  3429,  3434,  3441,  3447,  3451,  3454,  3459,  3462,  3462,
    3475,  3478,  3481,  3490,  3493,  3500,  3522,  3551,  3649,  3702,
    3703,  3704,  3705,  3706,  3707,  3716,  3716,  3965,  3965,  4112,
    4113,  4125,  4143,  4143,  4404,  4410,  4416,  4422,  4428,  4431,
    4434,  4437,  4440,  4443,  4448,  4484,  4488,  4491,  4494,  4499,
    4503,  4508,  4518,  4549,  4549,  4607,  4607,  4629,  4656,  4673,
    4678,  4673,  4686,  4687,  4688,  4688,  4702,  4703,  4720,  4721,
    4722,  4723,  4724,  4725,  4726,  4727,  4728,  4729,  4730,  4731,
    4732,  4733,  4734,  4735,  4737,  4740,  4744,  4756,  4785,  4815,
    4848,  4867,  4888,  4910,  4933,  4956,  4964,  4971,  4978,  4986,
    4994,  4997,  5001,  5004,  5005,  5006,  5007,  5008,  5009,  5010,
    5011,  5014,  5025,  5036,  5049,  5060,  5071,  5085,  5088,  5091,
    5092,  5096,  5098,  5106,  5118,  5119,  5120,  5127,  5128,  5129,
    5130,  5131,  5132,  5133,  5134,  5135,  5136,  5137,  5138,  5139,
    5140,  5141,  5142,  5143,  5150,  5161,  5165,  5172,  5176,  5181,
    5185,  5197,  5207,  5217,  5220,  5224,  5230,  5243,  5247,  5250,
    5254,  5258,  5286,  5294,  5298,  5301,  5305,  5308,  5321,  5337,
    5348,  5358,  5370,  5374,  5378,  5385,  5407,  5424,  5443,  5462,
    5469,  5477,  5486,  5495,  5499,  5508,  5519,  5530,  5542,  5552,
    5566,  5574,  5583,  5592,  5596,  5605,  5616,  5627,  5639,  5649,
    5659,  5670,  5683,  5690,  5698,  5714,  5722,  5733,  5744,  5755,
    5774,  5782,  5799,  5807,  5814,  5821,  5832,  5844,  5855,  5867,
    5878,  5889,  5909,  5930,  5936,  5942,  5949,  5956,  5965,  5974,
    5977,  5986,  5995,  6002,  6009,  6016,  6024,  6034,  6045,  6056,
    6067,  6074,  6081,  6084,  6101,  6119,  6129,  6136,  6142,  6147,
    6154,  6158,  6163,  6170,  6174,  6180,  6184,  6190,  6191,  6192,
    6198,  6204,  6208,  6209,  6213,  6220,  6223,  6224,  6228,  6229,
    6231,  6234,  6237,  6242,  6253,  6278,  6281,  6335,  6339,  6343,
    6347,  6351,  6355,  6359,  6363,  6367,  6371,  6375,  6379,  6383,
    6387,  6393,  6393,  6409,  6414,  6417,  6423,  6438,  6454,  6455,
    6458,  6459,  6463,  6464,  6474,  6478,  6483,  6493,  6504,  6509,
    6514,  6517,  6523,  6531,  6543,  6558,  6559,  6579,  6583,  6587,
    6591,  6595,  6599,  6603,  6607,  6614,  6617,  6620,  6624,  6629,
    6641,  6649,  6653,  6658,  6672,  6689,  6690,  6693,  6703,  6721,
    6728,  6735,  6742,  6750,  6758,  6762,  6768,  6769,  6770,  6771,
    6772,  6773,  6774,  6775,  6778,  6782,  6786,  6790,  6794,  6798,
    6802,  6806,  6810,  6814,  6818,  6822,  6826,  6830,  6838,  6848,
    6852,  6856,  6860,  6869,  6875,  6879,  6883,  6887,  6891,  6907,
    6910,  6915,  6920,  6920,  6921,  6924,  6941,  6950,  6950,  6968,
    6968,  6986,  6987,  6988,  6991,  6995,  6999,  7003,  7009,  7012,
    7016,  7022,  7026,  7030,  7036,  7039,  7044,  7045,  7048,  7051,
    7054,  7057,  7062,  7065,  7070,  7073,  7078,  7084,  7090,  7096,
    7102,  7108,  7116,  7124,  7129,  7136,  7139,  7149,  7160,  7171,
    7181,  7191,  7199,  7211,  7212,  7215,  7216,  7217,  7218,  7221,
    7233,  7239,  7248,  7249,  7250,  7253,  7254,  7255,  7258,  7259,
    7262,  7267,  7271,  7274,  7277,  7280,  7283,  7288,  7292,  7295,
    7302,  7308,  7311,  7316,  7319,  7325,  7330,  7334,  7337,  7340,
    7343,  7348,  7352,  7355,  7358,  7364,  7367,  7370,  7378,  7381,
    7384,  7388,  7393,  7406,  7410,  7415,  7421,  7425,  7430,  7434,
    7441,  7444,  7449
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "END", "error", "\"invalid token\"", "ID", "HBLOCK", "POUND", "STRING",
  "WSTRING", "INCLUDE", "IMPORT", "INSERT", "CHARCONST", "WCHARCONST",
  "NUM_INT", "NUM_FLOAT", "NUM_UNSIGNED", "NUM_LONG", "NUM_ULONG",
  "NUM_LONGLONG", "NUM_ULONGLONG", "NUM_BOOL", "TYPEDEF", "TYPE_INT",
  "TYPE_UNSIGNED", "TYPE_SHORT", "TYPE_LONG", "TYPE_FLOAT", "TYPE_DOUBLE",
  "TYPE_CHAR", "TYPE_WCHAR", "TYPE_VOID", "TYPE_SIGNED", "TYPE_BOOL",
  "TYPE_COMPLEX", "TYPE_TYPEDEF", "TYPE_RAW", "TYPE_NON_ISO_INT8",
  "TYPE_NON_ISO_INT16", "TYPE_NON_ISO_INT32", "TYPE_NON_ISO_INT64",
  "LPAREN", "RPAREN", "COMMA", "SEMI", "EXTERN", "INIT", "LBRACE",
  "RBRACE", "PERIOD", "ELLIPSIS", "CONST_QUAL", "VOLATILE", "REGISTER",
  "STRUCT", "UNION", "EQUAL", "SIZEOF", "MODULE", "LBRACKET", "RBRACKET",
  "BEGINFILE", "ENDOFFILE", "ILLEGAL", "CONSTANT", "NAME", "RENAME",
  "NAMEWARN", "EXTEND", "PRAGMA", "FEATURE", "VARARGS", "ENUM", "CLASS",
  "TYPENAME", "PRIVATE", "PUBLIC", "PROTECTED", "COLON", "STATIC",
  "VIRTUAL", "FRIEND", "THROW", "CATCH", "EXPLICIT", "STATIC_ASSERT",
  "CONSTEXPR", "THREAD_LOCAL", "DECLTYPE", "AUTO", "NOEXCEPT", "OVERRIDE",
  "FINAL", "USING", "NAMESPACE", "NATIVE", "INLINE", "TYPEMAP", "EXCEPT",
  "ECHO", "APPLY", "CLEAR", "SWIGTEMPLATE", "FRAGMENT", "WARN", "LESSTHAN",
  "GREATERTHAN", "DELETE_KW", "DEFAULT", "LESSTHANOREQUALTO",
  "GREATERTHANOREQUALTO", "EQUALTO", "NOTEQUALTO", "LESSEQUALGREATER",
  "ARROW", "QUESTIONMARK", "TYPES", "PARMS", "NONID", "DSTAR", "DCNOT",
  "TEMPLATE", "OPERATOR", "CONVERSIONOPERATOR", "PARSETYPE", "PARSEPARM",
  "PARSEPARMS", "DOXYGENSTRING", "DOXYGENPOSTSTRING", "CAST", "LOR",
  "LAND", "OR", "XOR", "AND", "LSHIFT", "RSHIFT", "PLUS", "MINUS", "STAR",
  "SLASH", "MODULO", "UMINUS", "NOT", "LNOT", "DCOLON", "$accept",
  "program", "interface", "declaration", "swig_directive",
  "extend_directive", "$@1", "apply_directive", "clear_directive",
  "constant_directive", "echo_directive", "except_directive", "stringtype",
  "fname", "fragment_directive", "include_directive", "$@2", "includetype",
  "inline_directive", "insert_directive", "module_directive",
  "name_directive", "native_directive", "pragma_directive", "pragma_arg",
  "pragma_lang", "rename_directive", "rename_namewarn",
  "feature_directive", "stringbracesemi", "featattr", "varargs_directive",
  "varargs_parms", "typemap_directive", "typemap_type", "tm_list",
  "tm_tail", "typemap_parm", "types_directive", "template_directive",
  "warn_directive", "c_declaration", "$@3", "c_decl", "c_decl_tail",
  "initializer", "cpp_alternate_rettype", "cpp_lambda_decl",
  "lambda_introducer", "lambda_template", "lambda_body", "lambda_tail",
  "$@4", "c_enum_key", "c_enum_inherit", "c_enum_forward_decl",
  "c_enum_decl", "c_constructor_decl", "cpp_declaration", "cpp_class_decl",
  "@5", "@6", "cpp_opt_declarators", "cpp_forward_class_decl",
  "cpp_template_decl", "$@7", "cpp_template_possible", "template_parms",
  "templateparameters", "templateparameter", "templateparameterstail",
  "cpp_using_decl", "cpp_namespace_decl", "@8", "$@9", "cpp_members",
  "$@10", "$@11", "$@12", "cpp_member_no_dox", "cpp_member",
  "cpp_constructor_decl", "cpp_destructor_decl", "cpp_conversion_operator",
  "cpp_catch_decl", "cpp_static_assert", "cpp_protection_decl",
  "cpp_swig_directive", "cpp_end", "cpp_vend", "anonymous_bitfield",
  "anon_bitfield_type", "extern_string", "storage_class", "parms",
  "rawparms", "ptail", "parm_no_dox", "parm", "valparms", "rawvalparms",
  "valptail", "valparm", "callparms", "callptail", "def_args",
  "parameter_declarator", "plain_declarator", "declarator",
  "notso_direct_declarator", "direct_declarator", "abstract_declarator",
  "direct_abstract_declarator", "pointer", "cv_ref_qualifier",
  "ref_qualifier", "type_qualifier", "type_qualifier_raw", "type",
  "rawtype", "type_right", "decltype", "primitive_type",
  "primitive_type_list", "type_specifier", "definetype", "$@13",
  "default_delete", "deleted_definition", "explicit_default", "ename",
  "constant_directives", "optional_ignored_defines", "enumlist",
  "enumlist_item", "edecl_with_dox", "edecl", "etype", "expr", "exprmem",
  "exprsimple", "valexpr", "exprnum", "exprcompound", "variadic",
  "inherit", "raw_inherit", "$@14", "base_list", "base_specifier", "@15",
  "@16", "access_specifier", "templcpptype", "cpptype", "classkey",
  "classkeyopt", "opt_virtual", "virt_specifier_seq",
  "virt_specifier_seq_opt", "class_virt_specifier_opt",
  "exception_specification", "qualifiers_exception_specification",
  "cpp_const", "ctor_end", "ctor_initializer", "mem_initializer_list",
  "mem_initializer", "less_valparms_greater", "identifier", "idstring",
  "idstringopt", "idcolon", "idcolontail", "idtemplate",
  "idtemplatetemplate", "idcolonnt", "idcolontailnt", "string", "wstring",
  "stringbrace", "options", "kwargs", "stringnum", "empty", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-939)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-633)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     701,  4755,  4860,   229,   223,  4189,  -939,  -939,  -939,  -939,
    -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,
    -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,
    -939,  -939,   272,    32,   198,    27,  -939,  -939,   120,   155,
     236,  5583,    69,   169,   319,  5870,   792,   194,   792,  -939,
    -939,  -939,  2742,  -939,    69,   236,  -939,   -34,  -939,   342,
     346,  5176,  -939,   271,  -939,  -939,  -939,   369,  -939,  -939,
      57,   414,  5253,   451,  -939,  -939,   414,   472,   484,   491,
     433,  -939,  -939,   503,   466,   520,    19,   259,   103,  1052,
     528,    65,   547,   442,   641,  5655,  5655,   555,   579,   630,
     600,  5941,  -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,
    -939,  -939,  -939,  -939,   414,  -939,  -939,  -939,  -939,  -939,
    -939,  -939,  2034,  -939,  -939,  -939,  -939,  -939,  -939,  -939,
    -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,
    -939,  -939,  -939,   189,  5727,  -939,  -939,  -939,  -939,  -939,
      69,   171,   540,  2517,  -939,  -939,  -939,  -939,  -939,   792,
    -939,  3804,   248,   222,  2655,  3483,    72,    67,   508,   115,
      69,  -939,  -939,    35,   394,    35,   426,  1234,   537,  -939,
    -939,  -939,  -939,  -939,   146,   301,  -939,  -939,  -939,   619,
    -939,   624,  -939,  -939,   211,  -939,  -939,  6141,    76,   211,
     211,  -939,   227,  2093,  -939,    23,  1179,   376,   146,   146,
    -939,   211,  5071,  -939,  -939,  5176,  -939,  -939,  -939,  -939,
    -939,  -939,    69,    53,  -939,   250,   622,   146,  -939,  -939,
     211,   146,  -939,  -939,  -939,   670,  5176,   653,  1595,   661,
     667,   211,   630,   670,  5176,  -939,  -939,  -939,  5176,    69,
    -939,  -939,    69,   588,   630,  2488,   112,   914,   211,   455,
     942,   596,  -939,  -939,  2093,    69,  2147,   354,   689,   146,
    -939,  -939,   -34,   631,    25,  -939,  -939,  -939,  -939,  -939,
    -939,  -939,  -939,  -939,  -939,  -939,  3483,  2316,  3483,  3483,
    3483,  3483,  3483,  3483,  -939,   639,  -939,   700,   706,   432,
    3449,   165,  -939,   190,  -939,  -939,   670,   744,  -939,  -939,
    3923,  1186,  1186,   746,   748,   283,   683,   125,   455,  -939,
    -939,  -939,  -939,   739,  3483,  -939,  -939,  -939,  -939,  6134,
    -939,  3449,   764,  3923,   835,    69,   494,   426,  -939,   835,
     494,   426,  -939,   681,  -939,  -939,  5176,  2793,  -939,  5176,
    2931,   835,   897,  1003,   494,   426,   690,  1346,  -939,  -939,
     -34,   784,  4965,  -939,  -939,  -939,  -939,   789,   670,    69,
      69,  -939,  -939,   377,   794,  -939,  -939,  -939,    35,   500,
     581,  -939,   799,  -939,  -939,  -939,  -939,    69,  -939,   804,
     793,   723,   811,   816,  -939,   822,   823,  -939,  5799,  -939,
      69,  -939,   830,   836,  -939,   838,   841,  5655,  -939,  -939,
     575,  -939,  -939,  -939,  5655,  -939,  -939,  -939,   858,  -939,
    -939,   730,    83,   864,   771,  -939,   866,  -939,   215,  -939,
     869,  -939,  -939,   273,   248,   248,   248,   385,   796,   871,
     192,   873,  5176,  1486,  1506,   800,  2608,  1190,   225,   842,
     338,  -939,  4042,  1190,  -939,   874,  -939,   359,  -939,  -939,
     236,  -939,   540,   918,   919,  2900,   819,    25,  5510,   888,
    -939,  1955,  -939,  -939,  -939,  -939,  -939,  -939,  2517,  -939,
    -939,  -939,  3483,  3483,  3483,  3483,  3483,  3483,  3483,  3483,
    3483,  3483,  3483,  3483,  3483,  3483,  3483,  3483,  3483,  3483,
     926,   929,  -939,   655,   655,   411,   815,   505,   570,  -939,
    -939,   655,   655,   590,   818,   996,  3483,  3449,  -939,  5176,
    1391,    90,   494,  -939,  5176,  3069,   494,  -939,   893,  -939,
    6167,   898,  -939,  6179,   494,   835,   494,   426,   835,   494,
     426,  1551,   835,   891,  1799,   494,  -939,  -939,  -939,  5176,
     624,   355,   901,  -939,  -939,   211,  2425,  -939,   899,  5176,
     900,  -939,   905,  -939,   645,  1291,  1480,   912,  5176,  2093,
     913,  -939,  1595,  4304,   915,  -939,  1267,  5655,   545,   920,
     921,  5176,   667,   544,   922,   211,  5176,    52,   870,  5176,
    -939,  -939,  5176,  -939,   996,  1618,   835,   159,  -939,   928,
    2746,   930,   281,   880,   883,  -939,  -939,   626,  -939,   487,
    -939,  -939,  -939,   862,  -939,   925,  5870,   637,  -939,   937,
     796,    35,   907,  -939,  -939,  -939,   902,  -939,  -939,    69,
     940,   950,  3483,  3483,  3207,  3345,  3621,    92,  3483,   194,
     952,  5799,   700,  1790,  1790,  1861,  1861,  1253,  3311,  4826,
    3594,  4931,  3181,  1955,   752,   752,   721,   721,  -939,  -939,
    -939,   955,   956,   818,   792,  -939,  -939,  -939,   655,   957,
     962,  1595,  6221,   966,   632,   818,  -939,   316,   968,  -939,
    6233,   475,  -939,   475,  -939,   494,   494,   835,   948,  2099,
     494,   426,   494,   835,   835,   494,   624,  -939,  -939,  -939,
     670,  5176,  4419,  -939,   973,  -939,    83,   974,  -939,   976,
    -939,  -939,  -939,  -939,   670,  -939,  -939,  -939,   979,  -939,
    1190,   670,  -939,   982,    85,   791,  1291,  -939,  1190,  -939,
     978,  -939,  -939,  4534,    55,  5799,   437,  -939,  -939,  5176,
    -939,   987,  -939,   890,  -939,   284,   934,  -939,  1001,   997,
    -939,    69,  1167,   866,  1008,  -939,  1595,  1190,   244,   835,
    -939,  5176,  3483,  -939,  -939,  -939,  -939,  -939,  3880,  -939,
     946,  -939,  -939,   988,  1859,   496,  -939,  -939,  1011,  -939,
     821,  -939,  2383,  1014,    35,  3483,  3483,  3449,  2622,  3483,
    3483,  3621,  4113,  3483,  1016,  1021,  2760,  1023,  -939,   194,
    -939,  3483,  3483,  3483,  -939,  -939,  1025,  1026,  -939,  -939,
    -939,   652,  -939,  -939,  -939,  -939,   494,   835,   835,   494,
     494,   494,  -939,  1027,  -939,   211,   211,   475,  2383,  5176,
      52,  2425,  1746,   211,  1028,  -939,  1190,  1030,  -939,  -939,
     670,  2093,   140,  -939,  5655,  -939,  1033,   475,   114,   146,
     373,  -939,  2517,   348,  -939,  1022,    57,  6041,  -939,  -939,
    -939,  -939,  -939,  -939,  -939,  5325,  -939,  -939,  4649,  1039,
    -939,  1035,  3036,   760,  -939,   415,  -939,   988,  -939,    56,
    1024,    94,  -939,  5176,   581,  1034,  1006,  -939,  -939,  2093,
    -939,  -939,  -939,   907,  -939,  -939,  -939,    69,  -939,  -939,
    -939,  1042,  1015,  1018,  1029,   943,  3703,   146,  -939,  -939,
    -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,
    -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,  -939,  1044,
     975,  2383,  -939,  -939,  -939,  -939,  -939,  -939,  -939,  5398,
    1053,  -939,  -939,  1062,   861,  -939,  1063,  -939,  3449,  3449,
    3449,  3483,  3483,  -939,  -939,  1066,  3894,  1068,  1069,  -939,
    -939,  -939,   494,   494,  -939,  -939,  -939,    35,  1061,  1071,
    -939,   670,  1073,  -939,  1190,  1822,    52,  -939,  1075,  -939,
    1082,  -939,  -939,  -939,   284,  -939,  -939,   284,  1031,  -939,
    -939,  5799,  5176,  2093,  5799,  2179,  -939,  -939,   760,  -939,
    -939,    35,  -939,  5176,  -939,   642,  -939,   146,   988,  -939,
    1072,  1842,    42,  -939,  1087,  1088,   581,    69,   679,  -939,
    1190,  -939,  1081,   907,  2383,  -939,  -939,  -939,  -939,   146,
    -939,  1076,  1939,  -939,  -939,  1055,  1058,  1064,  1074,  1077,
     269,  2383,  -939,  3483,  -939,  -939,  -939,  3449,  3449,  -939,
    -939,  -939,  1097,  -939,  1101,  -939,  1106,  -939,  1190,  -939,
    -939,  -939,  -939,  -939,  1107,  1595,  1048,   249,  4042,  -939,
     496,  1114,  -939,  -939,  -939,  -939,  -939,  3483,  -939,  1190,
     988,  -939,   688,  -939,  1113,  1116,  1111,   416,  -939,  -939,
      35,  -939,  -939,  -939,    69,  -939,  2383,  1122,  5176,  -939,
    -939,  1190,  3483,  -939,  1117,   861,  -939,  -939,  -939,  1120,
    -939,  1123,  -939,  5176,  1125,  1127,    15,  1129,  -939,  1190,
    1131,  -939,  3449,    35,  -939,  -939,  -939,  -939,    69,  -939,
    -939,  -939,   496,  1081,  1128,  5176,  1142,    35,  3173,  1939,
    -939,  -939,  -939,  1143,  5176,  5176,  5176,  1146,  1859,    11,
    -939,   496,  1141,  -939,  -939,  -939,  1156,  1190,   496,  -939,
    -939,  1190,  1160,  1162,  1163,  5176,  -939,  5799,   642,  -939,
    -939,  2383,  1190,  -939,   462,  -939,  -939,   598,  1190,  1190,
    1190,  1164,  1161,  -939,  -939,  -939,  -939,  -939,   581,  -939,
    -939,   581,  -939,  -939,  -939,  1190,   642,  1165,  1166,  -939,
    -939,  -939,  -939
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
     632,     0,     0,     0,     0,     0,    12,     4,   582,   417,
     425,   418,   419,   422,   423,   420,   421,   407,   424,   406,
     426,   409,   427,   428,   429,   430,   292,   397,   398,   399,
     539,   540,   150,   534,   535,     0,   583,   584,     0,     0,
     594,     0,     0,   293,     0,     0,   395,   632,   402,   412,
     405,   414,   415,   538,     0,   601,   410,   592,     6,     0,
       0,   632,     1,    17,    68,    64,    65,     0,   269,    16,
     264,   632,     0,     0,    86,    87,   632,   632,     0,     0,
     268,   270,   271,     0,   272,     0,   273,   278,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    10,    11,     9,    13,    20,    21,    22,    23,
      24,    25,    26,    27,   632,    28,    29,    30,    31,    32,
      33,    34,     0,    35,    36,    37,    38,    39,    40,    14,
     117,   122,   119,   118,    18,    15,   159,   160,   161,   162,
     163,   164,   125,   265,     0,   283,   152,   151,   536,   537,
       0,     0,     0,   632,   595,   294,   408,   295,     3,   401,
     396,   632,     0,   431,     0,     0,   594,   373,   372,   389,
       0,   314,   290,   632,   323,   632,   369,   363,   350,   311,
     403,   416,   411,   602,     0,     0,   590,     5,     8,     0,
     284,   632,   286,    19,     0,   616,   281,     0,   263,     0,
       0,   623,     0,     0,   400,   601,     0,   632,     0,     0,
      82,     0,   632,   276,   280,   632,   274,   236,   277,   275,
     282,   279,     0,     0,   195,   601,     0,     0,    66,    67,
       0,     0,    55,    53,    50,    51,   632,     0,   632,     0,
     632,   632,     0,   116,   632,   135,   134,   136,   632,     0,
     139,   133,     0,   137,     0,     0,     0,     0,     0,   323,
       0,   350,   267,   266,     0,   632,     0,   632,     0,     0,
     596,   603,   593,     0,   582,   618,   473,   474,   486,   487,
     488,   489,   490,   491,   492,   493,     0,     0,     0,     0,
       0,     0,     0,     0,   301,     0,   296,   632,   456,   400,
       0,   466,   475,   455,   465,   476,   467,   472,   298,   404,
     632,   373,   372,     0,     0,   363,   410,     0,   333,   350,
     309,   436,   437,   307,     0,   433,   434,   435,   380,     0,
     455,   310,     0,   632,     0,     0,   325,   371,   342,     0,
     324,   370,   387,   388,   351,   312,   632,     0,   313,   632,
       0,     0,   366,   365,   320,   364,   342,   374,   600,   599,
     598,     0,     0,   285,   289,   586,   585,     0,   587,     0,
       0,   615,   120,   626,     0,    72,    49,    48,   632,   323,
     431,    74,     0,   542,   543,   541,   544,     0,   545,     0,
      78,     0,     0,     0,   102,     0,     0,   191,     0,   632,
       0,   193,     0,     0,   107,     0,     0,     0,   111,   316,
     323,   317,   319,    44,     0,   108,   110,   588,     0,   589,
      58,     0,    57,     0,     0,   184,   632,   188,   538,   186,
       0,   174,   138,     0,     0,     0,     0,   585,     0,     0,
       0,     0,   632,     0,     0,   342,     0,   632,   350,   632,
     601,   439,   632,   632,   522,     0,   521,   411,   524,   413,
       0,   591,     0,     0,     0,     0,   475,     0,     0,     0,
     471,   484,   515,   514,   485,   516,   517,   581,     0,   297,
     300,   518,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   617,   373,   372,   363,   410,     0,     0,   385,
     382,   366,   365,     0,   350,   374,     0,   432,   381,   632,
     363,   410,   335,   343,   632,     0,   334,   386,     0,   359,
       0,     0,   378,     0,   330,     0,   322,   368,     0,   321,
     367,   376,     0,     0,     0,   326,   375,   597,     7,     0,
     632,     0,     0,   176,   632,     0,     0,   622,     0,   632,
       0,    73,     0,    81,     0,     0,     0,     0,     0,     0,
       0,   192,   632,     0,     0,   632,   632,     0,     0,   112,
       0,   632,   632,     0,     0,     0,     0,     0,   172,     0,
     185,   190,   632,    62,     0,     0,     0,     0,    83,     0,
       0,     0,   557,   548,   549,   394,   393,   562,   392,   390,
     558,   563,   565,     0,   566,     0,     0,     0,   154,     0,
     410,   632,   632,   167,   171,   554,   632,   555,   604,     0,
     461,   457,   477,     0,     0,     0,   389,     0,     0,   632,
       0,     0,   632,   511,   510,   506,   507,   512,     0,   505,
     504,   500,   501,   499,   502,   503,   494,   495,   496,   497,
     498,   463,   459,     0,   374,   354,   353,   352,   376,     0,
       0,   375,     0,     0,     0,   342,   344,   374,     0,   347,
       0,   361,   360,   383,   379,   332,   331,     0,     0,     0,
     327,   377,   336,     0,     0,   329,   632,   287,    70,    71,
      69,   632,     0,   627,   628,   631,   630,   624,    46,     0,
      45,    41,    80,    77,    79,   621,    97,   620,     0,    92,
     632,   619,    96,     0,   630,     0,     0,   103,   632,   235,
       0,   196,   197,     0,   264,     0,     0,    54,    52,   632,
      43,     0,   109,     0,   609,   607,     0,    61,     0,     0,
     114,     0,   632,   632,     0,   632,     0,   632,     0,     0,
     361,   632,     0,   560,   551,   550,   564,   391,     0,   143,
     632,   153,   155,   632,   632,     0,   132,   546,   523,   525,
     527,   547,     0,     0,   632,   632,   632,   478,     0,     0,
       0,   389,   388,     0,     0,     0,     0,     0,   470,   632,
     299,     0,   632,   632,   355,   357,     0,     0,   308,   362,
     345,     0,   349,   348,   315,   384,   337,     0,     0,   328,
     341,   340,   288,     0,   121,     0,     0,   361,     0,   632,
       0,     0,     0,     0,     0,    94,   632,     0,   123,   194,
     263,     0,   601,   105,     0,   104,     0,   361,     0,     0,
       0,   605,   632,     0,    56,     0,   264,     0,   178,   179,
     182,   181,   173,   180,   183,     0,   189,   175,     0,     0,
      85,     0,     0,   632,   144,     0,   145,   440,   442,   448,
       0,   444,   443,   632,   431,   563,   632,   158,   131,     0,
     128,   130,   126,   632,   532,   531,   533,     0,   529,   204,
     223,     0,     0,     0,     0,   270,   632,     0,   248,   249,
     241,   250,   221,   202,   246,   242,   240,   243,   244,   245,
     247,   222,   218,   219,   206,   213,   212,   216,   215,     0,
     224,     0,   207,   208,   211,   217,   209,   210,   220,     0,
     283,   165,   291,     0,   455,   304,     0,   508,   481,   480,
     479,     0,     0,   509,   468,     0,   513,     0,     0,   356,
     358,   346,   339,   338,   177,   629,   625,   632,     0,     0,
      88,   630,    99,    93,   632,     0,     0,   101,     0,    75,
       0,   113,   318,   610,   608,   614,   613,   612,     0,    59,
      60,     0,   632,     0,     0,     0,    63,    84,   556,   561,
     552,   632,   553,   632,   146,     0,   441,     0,   632,   450,
     452,     0,   632,   445,     0,     0,     0,     0,     0,   574,
     632,   526,   632,   632,     0,   199,   238,   237,   239,     0,
     225,     0,     0,   226,   198,   407,   406,   409,     0,   405,
     410,     0,   462,     0,   303,   306,   458,   483,   482,   469,
     464,   460,     0,    42,     0,   100,     0,    95,   632,    90,
      76,   106,   606,   611,     0,   632,     0,     0,   632,   559,
       0,     0,   148,   147,   142,   451,   449,     0,   156,   632,
     632,   446,     0,   571,     0,   573,   575,     0,   567,   568,
     632,   519,   528,   520,     0,   205,     0,     0,   632,   169,
     168,   632,     0,   214,     0,   455,    47,    98,    89,     0,
     115,     0,   172,   632,     0,     0,     0,     0,   127,   632,
       0,   453,   454,   632,   447,   569,   570,   572,     0,   577,
     579,   580,     0,   632,     0,   632,     0,   632,     0,     0,
     305,    91,   124,     0,   632,   632,   632,     0,   632,     0,
     149,     0,   576,   129,   530,   200,     0,   632,     0,   257,
     166,   632,     0,     0,     0,   632,   227,     0,     0,   157,
     578,     0,   632,   228,     0,   170,   234,     0,   632,   632,
     632,     0,     0,   140,   201,   229,   251,   253,     0,   254,
     256,   431,   232,   231,   230,   632,     0,     0,     0,   233,
     141,   252,   255
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -939,  -939,  -369,  -939,  -939,  -939,  -939,    -4,     4,    17,
      30,  -939,   654,  -939,    33,    36,  -939,  -939,  -939,    45,
    -939,    59,  -939,    79,  -939,  -939,    81,  -939,    89,  -556,
    -691,    96,  -939,    98,  -939,  -365,   634,   -69,   104,   108,
     113,   116,  -939,   460,  -672,  -936,  -184,  -939,  -939,  -939,
    -833,  -750,  -939,  -132,  -939,  -939,  -939,  -939,  -939,     6,
    -939,  -939,    75,    10,    43,  -939,  -939,   226,  -939,   636,
     468,   121,  -939,  -939,  -939,  -796,  -939,  -939,  -939,   313,
    -939,   477,  -939,   478,   132,  -939,  -939,  -939,  -939,  -702,
    -939,  -939,  -939,    16,   119,  -939,  -504,  1192,    18,   379,
    -939,   592,   757,  -222,   133,   -43,  -588,  -555,   747,  1701,
     -39,  -155,    -5,  1067,  -629,   640,    20,  -939,   -56,    78,
     -38,   -41,   -77,  1187,  -939,  -370,  -939,  -161,  -939,  -939,
    -939,   371,   243,  -938,  -939,  -939,   251,  -939,  1525,  -939,
    -242,  -128,  -523,  -939,   123,   633,  -939,  -939,  -939,   364,
    -939,  -939,  -939,  -225,   -52,  -939,  -939,   237,  -575,  -939,
    -939,  -593,  -939,   749,   130,  -939,  -939,   161,   -22,  1353,
    -107,  -939,  1105,  -210,  -148,  1083,  -939,  -191,  1520,  -939,
     525,   294,  -202,  -537,     0
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     4,     5,   104,   105,   106,   828,   908,   909,   910,
     911,   111,   420,   421,   912,   913,   755,   114,   115,   914,
     117,   915,   119,   916,   713,   209,   917,   122,   918,   719,
     567,   919,   393,   920,   403,   239,   415,   240,   921,   922,
     923,   924,   554,   130,   892,   775,   249,   131,   770,   875,
    1005,  1074,  1120,    42,   617,   132,   133,   134,   135,   925,
    1041,   782,  1100,   926,   927,   752,   862,   424,   425,   426,
     590,   928,   140,   575,   399,   929,  1096,  1171,  1024,   930,
     931,   932,   933,   934,   935,   142,   936,   937,  1173,  1176,
     938,  1038,   143,   939,   313,   190,   363,    43,   191,   295,
     296,   479,   297,   943,  1044,   776,   172,   408,   173,   336,
     259,   175,   176,   260,   607,   608,    45,    46,   298,   204,
      48,    49,    50,    51,    52,   323,   324,   365,   326,   327,
     449,   878,   879,   880,   881,  1008,  1009,  1121,   300,   301,
     302,   330,   304,   305,  1092,   455,   456,   622,   778,   779,
     897,  1023,   898,    53,    54,   386,   387,   780,   610,  1001,
     626,   611,   612,  1177,   887,  1018,  1085,  1086,   183,    55,
     373,   418,    56,   186,    57,   272,   746,   851,   306,   307,
     722,   200,   374,   707,   192
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
       6,   107,   325,   271,   171,   145,   314,   159,   174,   108,
     560,   136,   265,   369,   766,   137,   203,   730,   154,    44,
      59,   144,   109,   428,   251,   303,   237,   763,   404,   725,
     573,   750,   968,   705,   834,   110,   358,   271,   112,   238,
     238,   113,   580,   705,   466,   470,   697,   179,   138,   252,
     116,   797,   814,   439,   815,  1146,   715,  1004,   195,     8,
     250,   195,   461,   195,   118,  1070,   160,   150,   180,   228,
       8,   201,     8,   463,  1081,   332,   201,   210,   380,    47,
      47,   148,   371,   411,   120,   184,   121,   367,   266,   371,
     163,   371,   267,   375,   123,   716,   397,   218,   717,   382,
     273,   124,   219,   125,   391,    72,     8,   333,   398,   126,
     185,   229,   165,   127,   201,     8,   334,     8,   128,    47,
     251,   129,   372,   318,  1167,   164,   139,   153,     8,   586,
     345,   676,   348,   793,   417,  1034,  1012,   141,   464,    47,
    -587,   196,  1124,   196,   154,   370,    36,    37,  1147,     8,
     547,   441,   440,   308,  1132,   314,   250,    36,    37,    36,
      37,   334,   337,   341,   379,    27,    28,    29,   718,  1080,
     835,   294,   355,   179,     8,   179,   153,   197,   314,   309,
     189,   885,  1007,   979,    38,   702,    38,  1151,    40,   343,
      40,   364,   822,    36,    37,     8,   222,     8,   967,   410,
     676,  1158,    36,    37,    36,    37,   733,   388,   677,   335,
     262,   955,   736,   500,     8,    36,    37,   195,   982,   325,
      38,  1013,   794,    62,    40,   795,   640,   376,  1095,    38,
      60,   299,  -302,    40,   161,   983,    36,    37,   412,    47,
     416,   419,    38,   162,   153,  1104,    40,   149,   429,   163,
     371,     8,   164,   169,   335,   434,   171,  -187,   435,   152,
     174,    36,    37,   169,   151,   451,   427,   458,   320,    61,
     377,   165,   507,   508,   970,   263,   513,   759,   501,   371,
     615,  1055,    36,    37,    36,    37,     8,   870,   317,  1113,
      47,   269,   270,    47,   972,  -302,   157,   480,  1000,   179,
    1134,    36,    37,   220,     8,   400,   337,   341,   705,    38,
     355,    38,  1103,    40,    47,   166,  1168,   321,   322,     8,
    -187,   762,    47,   161,   167,   146,    47,   168,   321,   322,
     169,   394,   169,   593,   395,   558,   170,   221,    36,    37,
     153,   164,   572,   446,   147,   582,  -262,   537,   540,  1196,
     303,   238,   989,   703,   153,   406,   440,     8,   238,   698,
     670,   195,   158,   423,   428,    38,    27,    28,    29,   166,
     207,   603,   604,    36,    37,  1184,     8,   546,   179,  1114,
     550,  -438,  1115,   748,  -438,   187,   868,   169,    47,   188,
     170,    36,    37,   193,   990,   507,   508,   513,  1118,     6,
      38,   699,   624,   849,   166,  -632,    36,    37,   254,   194,
     629,    47,   639,   511,     8,  -438,   512,   411,  1183,   555,
    1059,   269,   359,  1069,    47,   170,   591,    47,   850,   383,
     384,   454,   556,    38,   346,     8,  -632,    40,   628,   670,
      47,   715,   153,   195,    36,    37,  1200,   614,   385,   618,
     625,   310,   347,   614,   199,  1003,  1130,   627,   335,   723,
    1153,  1004,  1131,    36,    37,   528,   349,   609,   531,   164,
    1185,    38,   161,   609,  -632,    40,  1192,  1193,  1194,  1169,
     843,   162,   231,   717,   350,   232,  1175,   163,   233,   153,
     164,   206,   844,  1199,   986,   442,   294,   888,   337,   341,
     355,    36,    37,  -601,  -601,  1186,   537,   540,  1187,   165,
    1111,     8,   208,   347,  1015,   355,   807,  1188,   213,   214,
      47,   238,    36,    37,   211,    27,    28,    29,    38,  -601,
      47,   212,    40,   410,   524,   671,   691,  -632,   889,   890,
     559,   511,   891,   215,   512,   442,   665,     8,   333,    38,
     364,   216,   525,   166,     6,   885,   299,   339,   347,  -632,
     217,   599,   167,   347,   946,   168,   164,   696,   227,   107,
     169,   619,   412,   145,   170,     6,   145,   108,   771,   136,
     957,   958,   416,   137,   873,   799,   727,   230,   737,   144,
     109,   738,   735,    30,    31,   241,   171,    47,    36,    37,
     174,   807,    47,   110,   749,   605,   112,   427,   606,   113,
     442,   666,    33,    34,   756,   581,   138,   605,   116,   242,
     606,   179,   781,   965,   966,    38,   458,    47,   347,    40,
     442,   667,   118,   347,    36,    37,   195,    47,   673,   179,
     244,  1189,   480,   678,  1190,   234,    47,   195,   347,   712,
     335,   195,   120,  1191,   121,   357,   792,   944,   944,    47,
     361,   743,   123,   691,    47,   744,   362,    47,   401,   124,
      47,   125,   524,   810,   944,   944,   371,   126,   709,   841,
     772,   127,  1072,   773,   546,  1073,   128,   321,   322,   129,
     525,   251,   524,   961,   139,   310,   364,   546,   107,   407,
     741,   609,   145,   609,   413,   141,   108,   601,   136,   414,
     525,   754,   137,   164,   446,   602,   603,   604,   144,   109,
     614,   432,  1088,   325,   303,  1089,   723,   250,   614,   107,
     459,  1125,   110,   145,  1126,   112,   462,   108,   113,   136,
     609,   942,   478,   137,   477,   138,   481,   116,   609,   144,
     109,   502,   145,   591,   159,     6,   171,   614,   859,  1031,
     174,   118,   860,   110,   565,   566,   112,   428,   865,   113,
     876,   584,   585,   882,   614,   980,   138,   609,   116,    47,
     251,   120,   940,   121,   179,   945,   945,   509,   238,   510,
     877,   123,   118,  1062,   609,   861,  1063,   516,   124,   179,
     125,   515,   945,   945,   519,   252,   126,   265,   541,   995,
     127,   792,   120,   267,   121,   128,   250,    47,   129,   169,
     823,  1198,   123,   139,     1,     2,     3,   548,   940,   124,
     551,   125,   832,   833,   141,   557,   614,   126,     8,    47,
     561,   127,    27,    28,    29,   563,   128,   609,   564,   129,
     603,   604,   308,   568,   139,  1084,   609,   569,   846,   497,
     498,   499,  1039,   570,   107,   141,   571,   609,   145,   258,
     294,   576,   108,  1002,   136,   440,   588,   577,   137,   578,
     871,  1097,   579,   995,   144,   109,  1019,   267,   495,   496,
     497,   498,   499,   781,   877,   894,   895,   896,   110,   583,
       8,   112,  -632,  1043,   113,   587,   145,    47,   589,   592,
     411,   138,   595,   116,   594,  1105,   598,     8,   600,   616,
     623,   630,   631,   638,  1052,    36,    37,   118,   641,   661,
     299,   940,   662,   664,   681,  1065,   668,   333,  1067,   683,
     693,   701,   708,   710,  1045,     8,   535,   120,   969,   121,
     378,   711,    38,   726,   440,   164,    40,   123,   732,   729,
     739,    47,   751,   339,   124,   747,   125,   179,   740,   760,
     761,   764,   126,   765,   614,   768,   127,   335,   774,   454,
     785,   128,   440,   769,   129,   409,   777,    36,    37,   139,
     786,   351,   429,   798,   609,   802,   803,   817,   804,     8,
     141,   179,  1014,   805,    36,    37,     8,   809,   882,   812,
     427,   447,   882,   453,    38,   825,   826,   827,    40,   829,
     614,   838,  1093,   781,   940,   877,   410,  1197,   847,   877,
     325,    38,    36,    37,   848,    40,   161,   831,   852,   335,
     609,   940,   853,   333,   854,   162,    27,    28,    29,   867,
     874,    72,   538,   893,   164,     8,   335,   951,   614,    38,
     941,   164,   952,    40,   954,   412,   959,   960,   964,   975,
      47,  1011,   443,   977,   981,   444,   998,   991,   609,   614,
     882,    47,   997,  1017,   335,  1029,    36,    37,  1025,  1016,
     179,  1032,  1026,    36,    37,  1027,   940,   877,   224,   609,
    -203,   614,  1033,  1042,  1046,  1045,  1028,  1049,  1053,  1050,
    1051,  1182,  1054,    38,   177,   833,  1098,   166,  1060,   614,
      38,   609,  1071,   179,    40,  1061,   167,  1077,  1082,   168,
    1091,  1083,  -260,  1093,   169,  -259,  1064,   179,   170,   609,
    1106,  -261,    36,    37,  1107,   335,    47,   156,   614,  1108,
    1110,  1102,   178,  1112,  -258,  1119,  1127,   614,  1128,   182,
    1129,   614,  1135,  1141,  1139,  1144,  1142,  1145,   609,    38,
    1148,   940,   614,    40,  1150,  1155,    47,   609,   614,   614,
     614,   609,     8,  1157,  1161,   195,  1165,  1117,    68,     8,
    1170,    47,   609,   223,   226,   614,   613,  1172,   609,   609,
     609,  1178,   621,  1179,  1180,  1195,   253,  1004,  1201,  1202,
     704,   856,   858,    47,  1160,   609,   742,  1136,  1066,  1030,
     381,   866,    47,    47,    47,   753,   161,   261,   315,   863,
     864,   988,  1143,   155,   800,   642,   342,     8,  1140,   181,
      27,    28,    29,    47,   164,    80,    81,    82,  1006,   767,
      84,  1076,    86,    87,  1156,   268,  1154,  1021,  1075,   783,
    1094,   845,   669,  1162,  1163,  1164,   316,   319,   360,    36,
      37,   601,   338,   338,   333,   344,    36,    37,  1166,   602,
     603,   604,   356,   351,  1181,   321,   322,   857,    68,  1152,
       0,     0,   164,     0,     8,   715,     0,   195,     0,     0,
       0,     0,   253,    38,     0,   177,     0,   166,   261,     0,
       0,   734,   720,     0,     0,     0,   728,     0,     0,   409,
     605,     0,   436,   606,    36,    37,     0,   396,   170,     0,
       0,   317,     0,     0,   716,     0,     0,   717,     0,     0,
     162,   669,   757,   178,     0,    80,    81,    82,     0,     8,
      84,    38,    86,    87,   430,    40,     0,   431,     0,     0,
     438,   338,   338,     0,   352,   445,   177,   353,     0,   448,
     156,   261,   457,     0,     0,     0,   335,   505,     0,     0,
       0,    36,    37,     0,   436,     0,   440,   493,   494,   495,
     496,   497,   498,   499,     8,   542,    27,    28,    29,     0,
     520,     0,     0,     0,   178,     0,     0,   718,    38,     0,
     527,     0,   166,     0,     0,   506,   319,   319,   806,     0,
     514,   256,   438,     0,   257,   205,     0,     0,     0,   169,
       0,   333,     0,   170,     0,     0,    36,    37,   521,   338,
     523,     0,   225,     0,   338,     0,     0,     0,     0,   164,
       0,     0,     0,     0,     0,     0,   338,   338,   338,     0,
       0,     0,   338,    38,     0,     0,     0,    40,     0,   830,
       0,     0,     0,   836,   552,   553,   543,   837,     0,   544,
       0,    36,    37,     8,     0,     0,   195,     0,   335,     8,
       0,     0,   562,   278,   279,   280,   281,   282,   283,   284,
     285,     0,     0,   806,     0,   574,   869,   596,    38,     8,
       0,     0,    40,     0,     0,     0,     0,     0,     0,   436,
       0,   511,     0,   886,   512,     0,   440,     0,     0,     0,
       0,     0,   637,   335,     0,   535,     0,     0,     0,   319,
     319,   319,     0,     0,     0,   597,   440,   366,   338,   338,
       0,   338,   366,   366,     8,   538,     0,   620,     0,   366,
       0,   389,   390,     0,   366,     0,     0,     0,     0,     0,
      36,    37,     0,     0,     0,     0,    36,    37,     0,   974,
     402,     0,   177,   366,   405,   976,   321,   322,   978,     0,
     198,   333,     0,     0,   366,     0,    36,    37,     8,     0,
     687,     0,     0,    38,     0,     0,     0,    40,   437,   164,
     663,   366,     0,     0,   235,     0,     0,     0,   450,   243,
     178,     8,   460,    38,   195,   675,     0,    40,   335,     0,
       0,     0,     0,     0,     0,   161,  1020,     0,     0,   177,
     338,    36,    37,   338,   162,     0,   338,   338,   335,   338,
       0,     0,     0,   164,     0,     0,     0,     0,   317,     0,
       0,   177,     0,     0,     0,     0,     0,   162,    38,     0,
     261,     0,    40,     0,   261,     0,     0,   178,     0,     0,
       0,   688,     0,     0,   689,    36,    37,     0,     0,   329,
     331,     0,     0,   335,     0,     0,     0,     0,     0,   178,
     261,   338,     0,   342,     0,   338,   177,     0,    36,    37,
       0,     0,    38,     0,   368,     0,   166,     0,     0,   368,
     368,     0,  1058,  1056,     0,   167,   368,     0,   168,     0,
       0,   368,     0,   169,   784,    38,     0,   170,   177,   166,
     447,     0,   453,     0,   178,     0,     0,     0,   256,     8,
     368,   257,     0,     0,     0,     0,   169,     0,  1079,     0,
     170,   368,   422,     0,     0,     0,     0,     0,     0,  1090,
       0,     0,     0,     0,   433,   368,   178,     0,   368,  1101,
       0,     0,   338,     0,     0,     0,   317,     0,     0,   973,
       0,     0,   338,     0,   338,   162,     0,     0,   338,   338,
       0,     0,     8,     0,     0,     0,     0,  1109,     0,     0,
       0,   465,   409,   471,   472,   473,   474,   475,   476,     0,
       0,     0,     0,   177,     0,     8,     0,     0,  1123,     0,
       0,   261,     0,     0,     0,     0,    36,    37,     0,   440,
       0,     0,     0,     0,     0,     8,     0,     0,   694,   517,
    1137,     0,     0,     0,     0,     0,   855,     0,   342,   527,
       0,   178,   317,    38,   338,  1057,   177,   166,  1149,   340,
       0,   162,   530,   253,     0,   533,   256,     0,   354,   257,
       0,     0,   317,     0,   169,  1078,  1101,     0,   170,    36,
      37,   162,     0,     0,     0,     0,     0,   886,     0,   883,
       0,     0,   486,     0,   178,     0,  1174,     0,   366,    27,
      28,    29,    36,    37,   884,     0,    38,     0,     0,   366,
      40,     0,   338,   338,   493,   494,   495,   496,   497,   498,
     499,     0,    36,    37,     0,     0,   745,   261,   366,    38,
     601,   335,     8,   166,     0,     0,   261,     0,   602,   603,
     604,     0,   256,     0,     0,   257,     0,     0,   340,    38,
     169,   354,   253,   166,   170,     0,     0,     0,     0,   482,
     483,     0,   256,   486,     0,   257,     0,     0,     0,   317,
     169,     0,  1099,     0,   170,     0,     0,     0,   162,   605,
       0,     0,   606,     0,   261,   493,   494,   495,   496,   497,
     498,   499,  1022,     0,     0,     0,     0,   643,   644,   645,
     646,   647,   648,   649,   650,   651,   652,   653,   654,   655,
     656,   657,   658,   659,   660,     0,     0,     0,     0,    36,
      37,     0,     0,     0,     0,   522,     0,     8,     0,     0,
     526,   672,     0,     0,  1040,     0,     0,     0,     0,     0,
     680,     0,   534,   536,   539,     0,    38,     0,   545,     0,
     166,     0,     0,   482,   483,   484,   485,   486,     0,   256,
       0,   700,   257,     0,   255,   368,   706,   169,     0,     0,
     261,   170,     0,   162,   714,   721,   724,     0,   842,   493,
     494,   495,   496,   497,   498,   499,     8,     0,   261,     0,
     261,     0,     8,     0,     0,   368,     0,   721,     0,     0,
       0,     0,     0,     0,     0,   758,   261,     0,     0,     0,
       0,     0,  1087,     0,    36,    37,     0,     0,     0,     0,
       0,     0,   177,   317,  1116,   436,     0,   261,     0,   440,
       0,     0,   162,     0,   536,   539,     0,   545,   818,     0,
       8,    38,     0,     0,     0,   166,     0,   787,   788,   650,
     653,   658,     0,   796,   256,     0,     0,   257,     0,     0,
     178,     0,   169,   620,     0,     0,   170,     0,   366,   366,
       0,     0,     8,    36,    37,     0,   366,   452,     0,    36,
      37,     0,     0,     0,     0,     0,   162,     0,     0,  1133,
       0,   984,   985,   987,     0,     0,     0,     0,     0,     0,
      38,     0,     0,     0,   166,     0,    38,     0,     0,  1068,
      40,   674,     0,   256,     0,     0,   257,     0,   162,     0,
       0,   169,  1010,  1087,     0,   170,   685,    36,    37,   686,
       0,   335,   690,   692,   261,   695,   721,     0,     0,     0,
       0,     0,     0,     0,   840,     0,   721,     0,     0,     0,
       0,     0,     0,     0,    38,     0,     0,     0,   166,    36,
      37,     0,     0,     0,     0,     0,     0,   256,     0,     0,
     257,     0,     0,     0,     0,   169,     0,   872,     0,   170,
       0,     0,     0,     0,     0,     0,    38,   674,     0,     0,
     166,   690,     0,     0,     0,     0,     0,     0,     0,   256,
       0,     0,   257,     0,   948,   949,   474,   169,   950,   467,
       0,   170,   195,   275,     0,     0,   956,   276,   277,   278,
     279,   280,   281,   282,   283,   284,   285,     0,     0,     0,
       0,     0,     0,     0,     0,   368,   368,     0,     0,     0,
     721,   971,     0,   368,     0,     0,   468,     0,     0,     0,
    1010,     0,     0,     0,     0,   469,     0,     0,     0,     0,
       0,     0,   287,     0,     0,     0,   840,     0,   811,     0,
       0,     0,     0,     0,   899,     0,  -632,    64,   816,     0,
     819,    65,    66,    67,   820,   821,     0,     0,     0,     0,
       0,     0,     0,     0,    68,  -632,  -632,  -632,  -632,  -632,
    -632,  -632,  -632,  -632,  -632,  -632,  -632,     0,  -632,  -632,
    -632,  -632,  -632,     0,     0,     0,   900,    70,     0,     0,
    -632,   195,     0,  -632,  -632,  -632,  -632,  -632,   278,   279,
     280,   281,   282,   283,   284,   285,    72,    73,    74,    75,
     901,    77,    78,    79,  -632,  -632,  -632,   902,   903,   904,
     811,    80,   905,    82,     0,    83,    84,    85,    86,    87,
    -632,  -632,     0,  -632,  -632,    88,  1047,  1048,     0,    92,
       0,    94,    95,    96,    97,    98,    99,     0,     0,     0,
       0,     8,     0,     0,   195,     0,   721,     0,   100,     0,
    -632,     0,     0,   101,  -632,  -632,     0,     0,     0,   906,
       0,     0,     0,     0,     0,     0,     0,     0,   962,   963,
     274,     0,     0,   195,   275,   907,     0,     0,   276,   277,
     278,   279,   280,   281,   282,   283,   284,   285,     0,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,     0,    21,    22,    23,    24,    25,   286,     0,     0,
       0,     0,     0,     0,     0,     0,    26,    27,    28,    29,
      30,    31,     0,   287,     0,     0,     0,     0,    36,    37,
       0,     0,     0,     0,     0,     0,     0,     0,    32,    33,
      34,     0,     0,     0,   321,   322,     0,     0,     0,     0,
       0,     0,  1122,     0,    35,    38,     0,    36,    37,    40,
       0,     8,     0,     0,     0,     0,     0,     0,   434,     0,
       0,   435,     0,     0,     0,     0,   169,  1138,     0,     0,
       0,     0,     0,     0,    38,     0,     0,    39,    40,     0,
       0,     0,     0,    41,     0,     0,     0,     0,   440,     0,
     288,     0,     0,   289,   290,   291,     0,   542,   274,   292,
     293,   195,   275,   947,     0,     0,   276,   277,   278,   279,
     280,   281,   282,   283,   284,   285,     0,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,     0,
      21,    22,    23,    24,    25,   286,     0,     0,    36,    37,
       0,     0,     0,     0,     0,    27,    28,    29,    30,    31,
       0,   287,     0,     0,   328,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    38,    32,    33,    34,    40,
     482,   483,   484,   485,   486,     0,   487,     0,   543,     0,
       0,   544,    35,     0,     0,    36,    37,     0,     0,     8,
     335,   488,   489,   490,   491,   492,   493,   494,   495,   496,
     497,   498,   499,     0,     9,    10,    11,    12,    13,    14,
      15,    16,    38,    18,     0,    20,    40,     0,    22,    23,
      24,    25,     0,     0,     0,     0,   440,     0,   288,     0,
       0,   289,   290,   291,     0,   687,   274,   292,   293,   195,
     275,   953,     0,     0,   276,   277,   278,   279,   280,   281,
     282,   283,   284,   285,     0,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,     0,    21,    22,
      23,    24,    25,   286,     0,     0,    36,    37,     0,     0,
       0,     0,     0,    27,    28,    29,    30,    31,     0,   287,
       0,     0,   529,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    38,    32,    33,    34,    40,   482,   483,
     484,   485,   486,     0,   487,     0,   688,     0,     0,   689,
      35,     0,     0,    36,    37,     0,     0,     0,   335,   488,
     489,   490,   491,   492,   493,   494,   495,   496,   497,   498,
     499,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      38,     0,     0,     0,    40,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   288,     0,     0,   289,
     290,   291,     0,     0,   274,   292,   293,   195,   275,     0,
       0,   632,   276,   277,   278,   279,   280,   281,   282,   283,
     284,   285,     0,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,     0,    21,    22,    23,    24,
      25,   286,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    27,    28,    29,    30,    31,     0,   287,     0,     0,
     532,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    32,    33,    34,   633,     0,     0,   482,   483,
     484,   485,   486,     0,   487,     0,     0,     0,    35,     0,
       0,    36,    37,     0,     0,     0,     0,     0,     0,   488,
     634,   490,   491,   635,   493,   494,   495,   496,   636,   498,
     499,     0,     0,     0,     0,     0,     0,     0,    38,     0,
       0,     0,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   288,     0,     0,   289,   290,   291,
       0,     0,   274,   292,   293,   195,   275,   999,     0,     0,
     276,   277,   278,   279,   280,   281,   282,   283,   284,   285,
       0,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,     0,    21,    22,    23,    24,    25,   286,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    27,
      28,    29,    30,    31,     0,   287,     0,     0,   679,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      32,    33,    34,     0,   482,   483,   484,   485,   486,     0,
     487,     0,     0,     0,     0,     0,    35,     0,     0,    36,
      37,     0,     0,     0,     0,   488,   489,   490,   491,   492,
     493,   494,   495,   496,   497,   498,   499,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    38,     0,     0,     0,
      40,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   288,     0,     0,   289,   290,   291,     0,     0,
     274,   292,   293,   195,   275,     0,  1159,     0,   276,   277,
     278,   279,   280,   281,   282,   283,   284,   285,     0,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,     0,    21,    22,    23,    24,    25,   286,   789,     0,
       0,     0,     0,     0,     0,     0,     0,    27,    28,    29,
      30,    31,     0,   287,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    32,    33,
      34,   482,   483,   484,   485,   486,     0,   487,     0,   482,
     483,   484,   485,   486,    35,     0,     0,    36,    37,     0,
       0,     0,   488,   489,   490,   491,   492,   493,   494,   495,
     496,   497,   498,   499,   492,   493,   494,   495,   496,   497,
     498,   499,     0,     0,    38,     0,     0,     0,    40,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     288,     0,     0,   289,   290,   291,     0,     0,   274,   292,
     293,   195,   275,     0,     0,     0,   276,   277,   278,   279,
     280,   281,   282,   283,   284,   285,     0,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,     0,
      21,    22,    23,    24,    25,   286,   790,     0,   801,     0,
       0,     0,     0,     0,     0,    27,    28,    29,    30,    31,
       0,   287,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    32,    33,    34,   482,
     483,   484,   485,   486,     0,   487,     0,     0,     0,     0,
       0,     0,    35,     0,     0,    36,    37,     0,     0,     0,
     488,   489,   490,   491,   492,   493,   494,   495,   496,   497,
     498,   499,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    38,     0,     0,     0,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   288,     0,
       0,   289,   290,   291,     0,     0,   274,   292,   293,   195,
     275,     0,     0,     0,   276,   277,   278,   279,   280,   281,
     282,   283,   284,   285,     0,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,     0,    21,    22,
      23,    24,    25,   286,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    27,    28,    29,    30,    31,     0,   287,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    32,    33,    34,   482,   483,   484,
     485,   486,     0,   487,     0,     0,     0,     0,     0,     0,
      35,     0,     0,    36,    37,     0,     0,     0,   488,   489,
     490,   491,   492,   493,   494,   495,   496,   497,   498,   499,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      38,     0,     0,     0,    40,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   288,     0,     0,   289,
     290,   291,     0,     0,   274,   292,   293,   195,   275,     0,
       0,     0,   276,   277,   278,   279,   280,   281,   282,   283,
     284,   285,     0,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,     0,    21,    22,    23,    24,
      25,   286,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    27,    28,    29,    30,    31,     0,   287,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    32,    33,    34,     0,     0,     0,     0,     0,
       0,     0,   482,   483,   484,   485,   486,    64,    35,     0,
       0,    36,    37,    67,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    68,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,     0,     0,     0,    38,     0,
       0,     0,    40,     0,     0,     0,   900,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   289,   290,   791,
       0,     0,     0,   292,   293,     0,    72,    73,    74,    75,
       0,    77,    78,    79,     0,     0,     0,   902,   903,   904,
       0,    80,   905,    82,     0,    83,    84,    85,    86,    87,
       0,     0,     0,     0,     0,    88,     0,     0,     0,    92,
       0,    94,    95,    96,    97,    98,    99,     8,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   100,     0,
       0,     0,     0,   101,     0,     0,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,     0,    21,
      22,    23,    24,    25,   310,   907,     0,     0,     0,     0,
       0,     0,     0,    26,    27,    28,    29,    30,    31,     0,
       0,     0,   164,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    32,    33,    34,     0,     0,
       0,     0,     0,     8,     0,     0,     0,     0,     0,     0,
       0,    35,     0,     0,    36,    37,     0,     0,     0,     0,
       0,     0,     9,    10,    11,    12,    13,    14,    15,    16,
     245,    18,   246,    20,     0,   247,    22,    23,    24,    25,
       0,    38,     0,     0,    39,    40,     8,     0,     0,     0,
      41,     0,     0,     0,   311,     0,     0,   312,     0,     0,
       0,     0,   169,     0,     0,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,     0,    21,    22,
      23,    24,    25,   310,     0,     0,     0,    35,     0,     0,
      36,    37,    26,    27,    28,    29,    30,    31,     0,     0,
       0,   164,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    32,    33,    34,    38,     0,     0,
       0,    40,   482,   483,   484,   485,   486,     0,     0,     0,
      35,     0,     0,    36,    37,     0,     0,     0,     0,     0,
       0,     0,     0,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,     0,     0,     0,     0,     0,
      38,     0,     0,    39,    40,     8,     0,     0,     0,    41,
       0,     0,     0,   503,     0,     0,   504,     0,     0,     0,
       0,   169,     0,     0,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,     0,    21,    22,    23,
      24,    25,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    26,    27,    28,    29,    30,    31,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    32,    33,    34,     8,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    35,
       0,     0,    36,    37,     0,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,     0,    21,    22,
      23,    24,    25,     0,     0,     0,     0,     0,     0,    38,
       0,     0,    39,    40,     0,     0,    30,    31,    41,     0,
       0,     0,   434,     0,     0,   435,     0,     0,     0,     0,
     169,     0,     0,     0,    32,    33,    34,     0,     0,    -2,
      63,     0,  -632,    64,     0,     0,     0,    65,    66,    67,
      35,     0,     0,    36,    37,     0,     0,     0,     0,     0,
      68,  -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,
    -632,  -632,  -632,     0,  -632,  -632,  -632,  -632,  -632,     0,
      38,     0,    69,    70,    40,     0,     0,     0,     0,  -632,
    -632,  -632,  -632,  -632,     0,     0,    71,     0,     0,     0,
       0,   169,    72,    73,    74,    75,    76,    77,    78,    79,
    -632,  -632,  -632,     0,     0,     0,     0,    80,    81,    82,
       0,    83,    84,    85,    86,    87,  -632,  -632,     0,  -632,
    -632,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   100,    63,  -632,  -632,    64,   101,
    -632,     0,    65,    66,    67,   102,   103,     0,     0,     0,
       0,     0,     0,     0,     0,    68,  -632,  -632,  -632,  -632,
    -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,     0,  -632,
    -632,  -632,  -632,  -632,     0,     0,     0,    69,    70,     0,
       0,   731,     0,     0,  -632,  -632,  -632,  -632,  -632,     0,
       0,    71,     0,     0,     0,     0,     0,    72,    73,    74,
      75,    76,    77,    78,    79,  -632,  -632,  -632,     0,     0,
       0,     0,    80,    81,    82,     0,    83,    84,    85,    86,
      87,  -632,  -632,     0,  -632,  -632,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   100,
      63,  -632,  -632,    64,   101,  -632,     0,    65,    66,    67,
     102,   103,     0,     0,     0,     0,     0,     0,     0,     0,
      68,  -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,
    -632,  -632,  -632,     0,  -632,  -632,  -632,  -632,  -632,     0,
       0,     0,    69,    70,     0,     0,   824,     0,     0,  -632,
    -632,  -632,  -632,  -632,     0,     0,    71,     0,     0,     0,
       0,     0,    72,    73,    74,    75,    76,    77,    78,    79,
    -632,  -632,  -632,     0,     0,     0,     0,    80,    81,    82,
       0,    83,    84,    85,    86,    87,  -632,  -632,     0,  -632,
    -632,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   100,    63,  -632,  -632,    64,   101,
    -632,     0,    65,    66,    67,   102,   103,     0,     0,     0,
       0,     0,     0,     0,     0,    68,  -632,  -632,  -632,  -632,
    -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,     0,  -632,
    -632,  -632,  -632,  -632,     0,     0,     0,    69,    70,     0,
       0,   839,     0,     0,  -632,  -632,  -632,  -632,  -632,     0,
       0,    71,     0,     0,     0,     0,     0,    72,    73,    74,
      75,    76,    77,    78,    79,  -632,  -632,  -632,     0,     0,
       0,     0,    80,    81,    82,     0,    83,    84,    85,    86,
      87,  -632,  -632,     0,  -632,  -632,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   100,
      63,  -632,  -632,    64,   101,  -632,     0,    65,    66,    67,
     102,   103,     0,     0,     0,     0,     0,     0,     0,     0,
      68,  -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,
    -632,  -632,  -632,     0,  -632,  -632,  -632,  -632,  -632,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,  -632,
    -632,  -632,  -632,  -632,     0,     0,    71,     0,     0,     0,
     996,     0,    72,    73,    74,    75,    76,    77,    78,    79,
    -632,  -632,  -632,     0,     0,     0,     0,    80,    81,    82,
       0,    83,    84,    85,    86,    87,  -632,  -632,     0,  -632,
    -632,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,     0,     0,     0,     7,     0,     8,     0,
       0,     0,     0,     0,   100,     0,  -632,     0,     0,   101,
    -632,     0,     0,     0,     0,   102,   103,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,     0,
      21,    22,    23,    24,    25,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    26,    27,    28,    29,    30,    31,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    32,    33,    34,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    35,     0,     0,    36,    37,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     8,     0,     0,     0,     0,     0,     0,
       0,     0,    38,     0,     0,    39,    40,     0,     0,     0,
       0,    41,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,     0,    21,    22,    23,    24,    25,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    26,
      27,    28,    29,    30,    31,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    32,    33,    34,   482,   483,   484,   485,   486,     0,
       0,     0,     0,     0,     0,     0,     0,    35,     0,     0,
      36,    37,     0,     0,     0,     0,   489,   490,   491,   492,
     493,   494,   495,   496,   497,   498,   499,     0,     8,     0,
       0,     0,     0,     0,     0,     0,     0,    38,     0,     0,
      39,    40,     0,     0,     0,     0,    41,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,     0,
      21,    22,    23,    24,    25,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    26,    27,    28,    29,    30,    31,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    32,    33,    34,   482,
     483,   484,   485,   486,     0,     0,     0,     0,     0,     0,
       0,     0,    35,     0,     0,    36,    37,     0,     0,     0,
       0,     0,     0,   491,   492,   493,   494,   495,   496,   497,
     498,   499,     0,     0,     8,     0,     0,     0,     0,     0,
       0,     0,    38,     0,   392,    39,    40,     0,     0,     0,
       0,    41,   549,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,     0,    21,    22,    23,    24,
      25,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      26,    27,    28,    29,    30,    31,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    32,    33,    34,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    35,     0,
       0,    36,    37,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     8,
       0,     0,     0,     0,     0,     0,     0,     0,    38,     0,
       0,    39,    40,     0,     0,     0,     0,    41,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
       0,    21,    22,    23,    24,    25,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    26,    27,    28,    29,    30,
      31,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    32,    33,    34,
       0,     0,     0,     0,   202,     0,     8,     0,     0,     0,
       0,     0,     0,    35,     0,     0,    36,    37,     0,     0,
       0,     0,     0,     0,     0,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,     0,    21,    22,
      23,    24,    25,    38,     0,     0,    39,    40,     0,     0,
       0,     0,    41,    27,    28,    29,    30,    31,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    32,    33,    34,     0,     8,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      35,     0,     0,    36,    37,     0,     0,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,     0,
      21,    22,    23,    24,    25,     0,     0,     0,     0,     0,
      38,     0,     0,     0,    40,    27,    28,    29,    30,    31,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    32,    33,    34,     0,
       0,     8,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    35,   993,     0,    36,    37,     0,     0,     0,
       9,    10,    11,    12,    13,    14,    15,    16,  1035,    18,
    1036,    20,     0,  1037,    22,    23,    24,    25,     0,     0,
       0,     0,    38,     0,     0,     0,    40,   994,    27,    28,
      29,    30,    31,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    32,
      33,    34,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    35,   264,     0,    36,    37,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   274,     0,    38,   195,   275,     0,    40,
     994,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,     0,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,     0,    21,    22,    23,    24,    25,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      27,    28,    29,    30,    31,     0,   287,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    32,    33,    34,     0,     0,     8,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    35,     0,     0,
      36,    37,     0,     0,     0,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,     0,    21,    22,
      23,    24,    25,     0,     0,     0,     0,    38,     0,     0,
       0,    40,    26,    27,    28,    29,    30,    31,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    32,    33,    34,     0,     8,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      35,     0,     0,    36,    37,     0,     0,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,     0,
      21,    22,    23,    24,    25,   236,     0,     0,     0,     0,
      38,     0,     0,    39,    40,    27,    28,    29,    30,    31,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    32,    33,    34,     0,
       8,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    35,     0,     0,    36,    37,     0,     0,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,     0,    21,    22,    23,    24,    25,     0,     0,     0,
       0,     0,    38,     0,     0,     0,    40,    27,    28,    29,
      30,    31,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    32,    33,
      34,     0,     8,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    35,   264,     0,    36,    37,     0,
       0,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,     0,    21,    22,    23,    24,    25,     0,
       0,     0,     0,     0,    38,     0,     0,     0,    40,    27,
      28,    29,    30,    31,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      32,    33,    34,     8,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    35,     0,     0,    36,
      37,     0,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,     0,    21,    22,    23,    24,    25,
       0,     0,     0,     0,     0,     0,    38,     0,     0,     0,
      40,     0,     0,    30,    31,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    32,    33,    34,     8,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    35,     0,     0,
      36,    37,     0,     9,    10,    11,    12,    13,    14,    15,
      16,   245,    18,   246,    20,     0,   247,    22,    23,    24,
      25,     0,     0,     0,     0,     0,     0,    38,     0,     0,
       0,    40,     0,     0,    30,    31,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    33,    34,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    35,     0,
       0,    36,    37,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     8,   248,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    38,     0,
       0,     0,    40,     9,    10,    11,    12,    13,    14,    15,
      16,   245,    18,   246,    20,     0,   247,    22,    23,    24,
      25,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    33,    34,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    35,     0,
       0,    36,    37,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     8,   992,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    38,     0,
       0,     0,    40,     9,    10,    11,    12,    13,    14,    15,
      16,   245,    18,   246,    20,     0,   247,    22,    23,    24,
      25,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   518,    30,    31,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    33,    34,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   682,     0,    35,     0,
       0,    36,    37,     0,     0,     0,     0,     0,   684,     0,
       0,     0,   482,   483,   484,   485,   486,     0,   487,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    38,     0,
       0,     0,    40,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   482,   483,   484,   485,   486,
     808,   487,     0,     0,     0,     0,     0,   482,   483,   484,
     485,   486,   813,   487,     0,     0,   488,   489,   490,   491,
     492,   493,   494,   495,   496,   497,   498,   499,   488,   489,
     490,   491,   492,   493,   494,   495,   496,   497,   498,   499,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   482,
     483,   484,   485,   486,     0,   487,     0,     0,     0,     0,
       0,   482,   483,   484,   485,   486,     0,   487,     0,     0,
     488,   489,   490,   491,   492,   493,   494,   495,   496,   497,
     498,   499,   488,   489,   490,   491,   492,   493,   494,   495,
     496,   497,   498,   499
};

static const yytype_int16 yycheck[] =
{
       0,     5,   163,   151,    47,     5,   161,    45,    47,     5,
     380,     5,   144,   197,   607,     5,    72,   572,    40,     1,
       2,     5,     5,   248,   101,   153,    95,   602,   230,   566,
     399,   587,   828,   556,   725,     5,   184,   185,     5,    95,
      96,     5,   407,   566,   286,   287,   550,    47,     5,   101,
       5,   639,   681,   255,   683,    40,     4,    46,     6,     3,
     101,     6,   272,     6,     5,  1001,    46,    40,    48,     4,
       3,    71,     3,    48,  1012,     3,    76,    77,    55,     1,
       2,    49,     6,   238,     5,   119,     5,   194,   144,     6,
      55,     6,   144,   200,     5,    43,    43,    78,    46,   206,
     152,     5,    83,     5,   211,    63,     3,    40,    55,     5,
     144,    46,    77,     5,   114,     3,    49,     3,     5,    41,
     197,     5,    46,   162,   113,    58,     5,   104,     3,    46,
     173,    41,   175,    41,   241,   931,    42,     5,   113,    61,
      55,    86,  1080,    86,   166,   197,    90,    91,   133,     3,
     360,   258,    40,   153,  1090,   310,   197,    90,    91,    90,
      91,    49,   167,   168,   203,    50,    51,    52,   116,   127,
     726,   153,   177,   173,     3,   175,   104,   120,   333,   159,
      61,   774,   126,    43,   117,   554,   117,  1123,   121,   169,
     121,   191,   696,    90,    91,     3,    93,     3,   827,   238,
      41,  1137,    90,    91,    90,    91,   575,   207,   118,   142,
      21,   799,   577,    48,     3,    90,    91,     6,   847,   380,
     117,   127,   130,     0,   121,   133,   468,     0,  1024,   117,
       1,   153,    42,   121,    40,   121,    90,    91,   238,   161,
     240,   241,   117,    49,   104,  1041,   121,    49,   248,    55,
       6,     3,    58,   138,   142,   130,   299,    42,   133,   104,
     299,    90,    91,   138,   144,   265,   248,   267,    46,    40,
      43,    77,   311,   312,   830,    86,   315,   118,   113,     6,
      55,   972,    90,    91,    90,    91,     3,    43,    40,    40,
     212,   120,   121,   215,   831,   105,   127,   297,   873,   299,
    1096,    90,    91,    44,     3,    55,   311,   312,   831,   117,
     315,   117,    43,   121,   236,   121,  1149,   106,   107,     3,
     105,    40,   244,    40,   130,    53,   248,   133,   106,   107,
     138,   212,   138,    60,   215,   378,   142,    78,    90,    91,
     104,    58,   398,   118,    72,   414,    77,   352,   353,  1182,
     478,   407,     4,   555,   104,   236,    40,     3,   414,     4,
     515,     6,    43,   244,   589,   117,    50,    51,    52,   121,
      76,    90,    91,    90,    91,  1171,     3,   357,   378,   130,
     362,    43,   133,   585,    46,    43,   755,   138,   310,    43,
     142,    90,    91,   122,    46,   434,   435,   436,  1070,   399,
     117,    46,    43,   119,   121,    46,    90,    91,   114,    40,
     462,   333,   468,   130,     3,    77,   133,   572,  1168,    42,
     976,   120,   121,   998,   346,   142,   426,   349,   144,    53,
      54,    77,    55,   117,    40,     3,    77,   121,   460,   594,
     362,     4,   104,     6,    90,    91,  1196,   447,    72,   449,
      91,    40,    58,   453,    40,    40,    40,   457,   142,   566,
    1132,    46,    46,    90,    91,   346,    40,   447,   349,    58,
    1172,   117,    40,   453,    42,   121,  1178,  1179,  1180,  1151,
      43,    49,    40,    46,    58,    43,  1158,    55,    46,   104,
      58,    40,    55,  1195,   121,    40,   478,     1,   503,   504,
     505,    90,    91,   118,   119,    43,   511,   512,    46,    77,
    1065,     3,    40,    58,   884,   520,   671,    55,    85,    86,
     442,   577,    90,    91,    40,    50,    51,    52,   117,   144,
     452,    40,   121,   572,    40,   515,   541,   105,    42,    43,
      40,   130,    46,    40,   133,    40,    41,     3,    40,   117,
     550,    85,    58,   121,   554,  1148,   478,    49,    58,   127,
      40,   442,   130,    58,   786,   133,    58,   549,    40,   573,
     138,   452,   572,   573,   142,   575,   576,   573,   616,   573,
     802,   803,   582,   573,   768,   641,   568,    40,    43,   573,
     573,    46,   576,    53,    54,    40,   639,   519,    90,    91,
     639,   756,   524,   573,   586,   130,   573,   589,   133,   573,
      40,    41,    72,    73,   594,    40,   573,   130,   573,    40,
     133,   621,   622,   825,   826,   117,   626,   549,    58,   121,
      40,    41,   573,    58,    90,    91,     6,   559,   519,   639,
      40,    43,   642,   524,    46,     4,   568,     6,    58,     4,
     142,     6,   573,    55,   573,   118,   636,   785,   786,   581,
      41,   117,   573,   668,   586,   121,    42,   589,    46,   573,
     592,   573,    40,    41,   802,   803,     6,   573,   559,   735,
      43,   573,    40,    46,   664,    43,   573,   106,   107,   573,
      58,   768,    40,    41,   573,    40,   696,   677,   702,    46,
     581,   681,   702,   683,    43,   573,   702,    81,   702,    42,
      58,   592,   702,    58,   118,    89,    90,    91,   702,   702,
     720,   133,    43,   884,   852,    46,   833,   768,   728,   733,
      41,    43,   702,   733,    46,   702,   105,   733,   702,   733,
     720,   784,    42,   733,   105,   702,    40,   702,   728,   733,
     733,     7,   752,   753,   792,   755,   799,   757,   752,   907,
     799,   702,   752,   733,    41,    42,   733,   992,   752,   733,
     770,    41,    42,   773,   774,   844,   733,   757,   733,   701,
     857,   702,   782,   702,   784,   785,   786,    41,   844,    41,
     773,   702,   733,   984,   774,   752,   987,    58,   702,   799,
     702,   118,   802,   803,    40,   857,   702,   939,   118,   865,
     702,   791,   733,   865,   733,   702,   857,   739,   702,   138,
     701,  1191,   733,   702,   123,   124,   125,    43,   828,   733,
      41,   733,    41,    42,   702,    41,   836,   733,     3,   761,
      41,   733,    50,    51,    52,    41,   733,   827,    55,   733,
      90,    91,   852,    42,   733,  1016,   836,    41,   739,   138,
     139,   140,   939,    41,   868,   733,    43,   847,   868,   122,
     852,    41,   868,   873,   868,    40,   105,    41,   868,    41,
     761,  1029,    41,   939,   868,   868,   886,   939,   136,   137,
     138,   139,   140,   893,   877,    74,    75,    76,   868,    41,
       3,   868,    41,    42,   868,    41,   906,   829,    42,    40,
    1065,   868,    41,   868,   118,  1043,    43,     3,   118,    77,
      46,     3,     3,   104,   967,    90,    91,   868,    40,     3,
     852,   931,     3,   118,    41,   991,   118,    40,   994,    41,
      49,    40,    43,    43,   944,     3,    49,   868,   829,   868,
     203,    46,   117,    41,    40,    58,   121,   868,    43,    46,
      40,   883,    92,    49,   868,    43,   868,   967,    47,    41,
      40,    91,   868,    90,   974,   113,   868,   142,    41,    77,
      40,   868,    40,    58,   868,   238,    79,    90,    91,   868,
      40,    49,   992,    41,   974,    40,    40,    49,    41,     3,
     868,  1001,   883,    41,    90,    91,     3,    41,  1008,    41,
     992,   264,  1012,   266,   117,    42,    42,    41,   121,    40,
    1020,    43,  1022,  1023,  1024,  1008,  1065,  1188,    41,  1012,
    1191,   117,    90,    91,   144,   121,    40,    55,   104,   142,
    1020,  1041,    41,    40,    47,    49,    50,    51,    52,    41,
     104,    63,    49,    42,    58,     3,   142,    41,  1058,   117,
      46,    58,    41,   121,    41,  1065,    41,    41,    41,    41,
     992,    47,   130,    43,    41,   133,    41,    55,  1058,  1079,
    1080,  1003,    43,    77,   142,   142,    90,    91,    46,    55,
    1090,    47,    77,    90,    91,    77,  1096,  1080,    46,  1079,
      47,  1101,   127,    41,    41,  1105,    77,    41,    47,    41,
      41,  1167,    41,   117,    47,    42,    40,   121,    43,  1119,
     117,  1101,  1003,  1123,   121,    43,   130,    55,    41,   133,
      49,    43,    77,  1133,   138,    77,   105,  1137,   142,  1119,
      43,    77,    90,    91,    43,   142,  1068,    42,  1148,    43,
      43,    77,    47,   105,    77,    41,    43,  1157,    42,    54,
      49,  1161,    40,    43,    47,    40,    43,    40,  1148,   117,
      41,  1171,  1172,   121,    43,    47,  1098,  1157,  1178,  1179,
    1180,  1161,     3,    41,    41,     6,    40,  1068,    21,     3,
      49,  1113,  1172,    88,    89,  1195,   447,    41,  1178,  1179,
    1180,    41,   453,    41,    41,    41,   101,    46,    43,    43,
     556,    44,   752,  1135,  1139,  1195,   582,  1098,   992,   906,
      41,   753,  1144,  1145,  1146,   589,    40,   122,   161,   752,
     752,   852,  1113,    41,   642,   478,   169,     3,  1105,    52,
      50,    51,    52,  1165,    58,    78,    79,    80,   877,   609,
      83,  1008,    85,    86,  1135,   150,  1133,   893,  1007,   626,
    1023,   736,   515,  1144,  1145,  1146,   161,   162,   185,    90,
      91,    81,   167,   168,    40,   170,    90,    91,  1148,    89,
      90,    91,   177,    49,  1165,   106,   107,   120,    21,  1128,
      -1,    -1,    58,    -1,     3,     4,    -1,     6,    -1,    -1,
      -1,    -1,   197,   117,    -1,   238,    -1,   121,   203,    -1,
      -1,    44,   565,    -1,    -1,    -1,   569,    -1,    -1,   572,
     130,    -1,   255,   133,    90,    91,    -1,   222,   142,    -1,
      -1,    40,    -1,    -1,    43,    -1,    -1,    46,    -1,    -1,
      49,   594,   595,   238,    -1,    78,    79,    80,    -1,     3,
      83,   117,    85,    86,   249,   121,    -1,   252,    -1,    -1,
     255,   256,   257,    -1,   130,   260,   299,   133,    -1,   264,
     265,   266,   267,    -1,    -1,    -1,   142,   310,    -1,    -1,
      -1,    90,    91,    -1,   317,    -1,    40,   134,   135,   136,
     137,   138,   139,   140,     3,    49,    50,    51,    52,    -1,
     333,    -1,    -1,    -1,   299,    -1,    -1,   116,   117,    -1,
     343,    -1,   121,    -1,    -1,   310,   311,   312,   671,    -1,
     315,   130,   317,    -1,   133,    72,    -1,    -1,    -1,   138,
      -1,    40,    -1,   142,    -1,    -1,    90,    91,   333,   334,
     335,    -1,    89,    -1,   339,    -1,    -1,    -1,    -1,    58,
      -1,    -1,    -1,    -1,    -1,    -1,   351,   352,   353,    -1,
      -1,    -1,   357,   117,    -1,    -1,    -1,   121,    -1,   720,
      -1,    -1,    -1,   726,   369,   370,   130,   728,    -1,   133,
      -1,    90,    91,     3,    -1,    -1,     6,    -1,   142,     3,
      -1,    -1,   387,    13,    14,    15,    16,    17,    18,    19,
      20,    -1,    -1,   756,    -1,   400,   757,   440,   117,     3,
      -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,   452,
      -1,   130,    -1,   774,   133,    -1,    40,    -1,    -1,    -1,
      -1,    -1,   465,   142,    -1,    49,    -1,    -1,    -1,   434,
     435,   436,    -1,    -1,    -1,   440,    40,   194,   443,   444,
      -1,   446,   199,   200,     3,    49,    -1,   452,    -1,   206,
      -1,   208,   209,    -1,   211,    -1,    -1,    -1,    -1,    -1,
      90,    91,    -1,    -1,    -1,    -1,    90,    91,    -1,   832,
     227,    -1,   515,   230,   231,   836,   106,   107,   841,    -1,
      70,    40,    -1,    -1,   241,    -1,    90,    91,     3,    -1,
      49,    -1,    -1,   117,    -1,    -1,    -1,   121,   255,    58,
     505,   258,    -1,    -1,    94,    -1,    -1,    -1,   265,    99,
     515,     3,   269,   117,     6,   520,    -1,   121,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    40,   889,    -1,    -1,   572,
     535,    90,    91,   538,    49,    -1,   541,   542,   142,   544,
      -1,    -1,    -1,    58,    -1,    -1,    -1,    -1,    40,    -1,
      -1,   594,    -1,    -1,    -1,    -1,    -1,    49,   117,    -1,
     565,    -1,   121,    -1,   569,    -1,    -1,   572,    -1,    -1,
      -1,   130,    -1,    -1,   133,    90,    91,    -1,    -1,   164,
     165,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,   594,
     595,   596,    -1,   636,    -1,   600,   639,    -1,    90,    91,
      -1,    -1,   117,    -1,   194,    -1,   121,    -1,    -1,   199,
     200,    -1,   975,   974,    -1,   130,   206,    -1,   133,    -1,
      -1,   211,    -1,   138,   629,   117,    -1,   142,   671,   121,
     993,    -1,   995,    -1,   639,    -1,    -1,    -1,   130,     3,
     230,   133,    -1,    -1,    -1,    -1,   138,    -1,  1011,    -1,
     142,   241,   242,    -1,    -1,    -1,    -1,    -1,    -1,  1020,
      -1,    -1,    -1,    -1,   254,   255,   671,    -1,   258,  1032,
      -1,    -1,   677,    -1,    -1,    -1,    40,    -1,    -1,    43,
      -1,    -1,   687,    -1,   689,    49,    -1,    -1,   693,   694,
      -1,    -1,     3,    -1,    -1,    -1,    -1,  1058,    -1,    -1,
      -1,   286,  1065,   288,   289,   290,   291,   292,   293,    -1,
      -1,    -1,    -1,   756,    -1,     3,    -1,    -1,  1079,    -1,
      -1,   726,    -1,    -1,    -1,    -1,    90,    91,    -1,    40,
      -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    49,   324,
    1101,    -1,    -1,    -1,    -1,    -1,   751,    -1,   791,   792,
      -1,   756,    40,   117,   759,    43,   799,   121,  1119,   168,
      -1,    49,   347,   768,    -1,   350,   130,    -1,   177,   133,
      -1,    -1,    40,    -1,   138,    43,  1139,    -1,   142,    90,
      91,    49,    -1,    -1,    -1,    -1,    -1,  1148,    -1,    40,
      -1,    -1,   112,    -1,   799,    -1,  1157,    -1,   555,    50,
      51,    52,    90,    91,    55,    -1,   117,    -1,    -1,   566,
     121,    -1,   817,   818,   134,   135,   136,   137,   138,   139,
     140,    -1,    90,    91,    -1,    -1,   583,   832,   585,   117,
      81,   142,     3,   121,    -1,    -1,   841,    -1,    89,    90,
      91,    -1,   130,    -1,    -1,   133,    -1,    -1,   257,   117,
     138,   260,   857,   121,   142,    -1,    -1,    -1,    -1,   108,
     109,    -1,   130,   112,    -1,   133,    -1,    -1,    -1,    40,
     138,    -1,    43,    -1,   142,    -1,    -1,    -1,    49,   130,
      -1,    -1,   133,    -1,   889,   134,   135,   136,   137,   138,
     139,   140,   897,    -1,    -1,    -1,    -1,   482,   483,   484,
     485,   486,   487,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,    -1,    -1,    -1,    -1,    90,
      91,    -1,    -1,    -1,    -1,   334,    -1,     3,    -1,    -1,
     339,   516,    -1,    -1,   939,    -1,    -1,    -1,    -1,    -1,
     525,    -1,   351,   352,   353,    -1,   117,    -1,   357,    -1,
     121,    -1,    -1,   108,   109,   110,   111,   112,    -1,   130,
      -1,   551,   133,    -1,    40,   555,   556,   138,    -1,    -1,
     975,   142,    -1,    49,   564,   565,   566,    -1,   735,   134,
     135,   136,   137,   138,   139,   140,     3,    -1,   993,    -1,
     995,    -1,     3,    -1,    -1,   585,    -1,   587,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   595,  1011,    -1,    -1,    -1,
      -1,    -1,  1017,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,  1065,    40,  1067,  1068,    -1,  1032,    -1,    40,
      -1,    -1,    49,    -1,   443,   444,    -1,   446,    49,    -1,
       3,   117,    -1,    -1,    -1,   121,    -1,   632,   633,   634,
     635,   636,    -1,   638,   130,    -1,    -1,   133,    -1,    -1,
    1065,    -1,   138,  1068,    -1,    -1,   142,    -1,   825,   826,
      -1,    -1,     3,    90,    91,    -1,   833,    40,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,  1094,
      -1,   848,   849,   850,    -1,    -1,    -1,    -1,    -1,    -1,
     117,    -1,    -1,    -1,   121,    -1,   117,    -1,    -1,    40,
     121,   520,    -1,   130,    -1,    -1,   133,    -1,    49,    -1,
      -1,   138,   879,  1128,    -1,   142,   535,    90,    91,   538,
      -1,   142,   541,   542,  1139,   544,   726,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   734,    -1,   736,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,    -1,    -1,    -1,   121,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,
     133,    -1,    -1,    -1,    -1,   138,    -1,   762,    -1,   142,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   596,    -1,    -1,
     121,   600,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,
      -1,    -1,   133,    -1,   789,   790,   791,   138,   793,     3,
      -1,   142,     6,     7,    -1,    -1,   801,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   825,   826,    -1,    -1,    -1,
     830,   831,    -1,   833,    -1,    -1,    40,    -1,    -1,    -1,
    1007,    -1,    -1,    -1,    -1,    49,    -1,    -1,    -1,    -1,
      -1,    -1,    56,    -1,    -1,    -1,   856,    -1,   677,    -1,
      -1,    -1,    -1,    -1,     1,    -1,     3,     4,   687,    -1,
     689,     8,     9,    10,   693,   694,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    -1,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    43,    44,    -1,    -1,
      47,     6,    -1,    50,    51,    52,    53,    54,    13,    14,
      15,    16,    17,    18,    19,    20,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
     759,    78,    79,    80,    -1,    82,    83,    84,    85,    86,
      87,    88,    -1,    90,    91,    92,   951,   952,    -1,    96,
      -1,    98,    99,   100,   101,   102,   103,    -1,    -1,    -1,
      -1,     3,    -1,    -1,     6,    -1,   976,    -1,   115,    -1,
     117,    -1,    -1,   120,   121,   122,    -1,    -1,    -1,   126,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   817,   818,
       3,    -1,    -1,     6,     7,   142,    -1,    -1,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    -1,    35,    36,    37,    38,    39,    40,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    50,    51,    52,
      53,    54,    -1,    56,    -1,    -1,    -1,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,
      73,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,  1077,    -1,    87,   117,    -1,    90,    91,   121,
      -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,
      -1,   133,    -1,    -1,    -1,    -1,   138,  1102,    -1,    -1,
      -1,    -1,    -1,    -1,   117,    -1,    -1,   120,   121,    -1,
      -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,    40,    -1,
     133,    -1,    -1,   136,   137,   138,    -1,    49,     3,   142,
     143,     6,     7,    41,    -1,    -1,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
      35,    36,    37,    38,    39,    40,    -1,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    50,    51,    52,    53,    54,
      -1,    56,    -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   117,    71,    72,    73,   121,
     108,   109,   110,   111,   112,    -1,   114,    -1,   130,    -1,
      -1,   133,    87,    -1,    -1,    90,    91,    -1,    -1,     3,
     142,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,   117,    31,    -1,    33,   121,    -1,    36,    37,
      38,    39,    -1,    -1,    -1,    -1,    40,    -1,   133,    -1,
      -1,   136,   137,   138,    -1,    49,     3,   142,   143,     6,
       7,    41,    -1,    -1,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    -1,    35,    36,
      37,    38,    39,    40,    -1,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    50,    51,    52,    53,    54,    -1,    56,
      -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117,    71,    72,    73,   121,   108,   109,
     110,   111,   112,    -1,   114,    -1,   130,    -1,    -1,   133,
      87,    -1,    -1,    90,    91,    -1,    -1,    -1,   142,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,    -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   133,    -1,    -1,   136,
     137,   138,    -1,    -1,     3,   142,   143,     6,     7,    -1,
      -1,    41,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    -1,    35,    36,    37,    38,
      39,    40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    50,    51,    52,    53,    54,    -1,    56,    -1,    -1,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    73,   105,    -1,    -1,   108,   109,
     110,   111,   112,    -1,   114,    -1,    -1,    -1,    87,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,
      -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   133,    -1,    -1,   136,   137,   138,
      -1,    -1,     3,   142,   143,     6,     7,    41,    -1,    -1,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    -1,    35,    36,    37,    38,    39,    40,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,
      51,    52,    53,    54,    -1,    56,    -1,    -1,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    72,    73,    -1,   108,   109,   110,   111,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    90,
      91,    -1,    -1,    -1,    -1,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,    -1,    -1,
     121,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   133,    -1,    -1,   136,   137,   138,    -1,    -1,
       3,   142,   143,     6,     7,    -1,    43,    -1,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    -1,    35,    36,    37,    38,    39,    40,    41,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    52,
      53,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,
      73,   108,   109,   110,   111,   112,    -1,   114,    -1,   108,
     109,   110,   111,   112,    87,    -1,    -1,    90,    91,    -1,
      -1,    -1,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   133,   134,   135,   136,   137,   138,
     139,   140,    -1,    -1,   117,    -1,    -1,    -1,   121,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     133,    -1,    -1,   136,   137,   138,    -1,    -1,     3,   142,
     143,     6,     7,    -1,    -1,    -1,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
      35,    36,    37,    38,    39,    40,    41,    -1,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    50,    51,    52,    53,    54,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,   108,
     109,   110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    90,    91,    -1,    -1,    -1,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,    -1,    -1,    -1,   121,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   133,    -1,
      -1,   136,   137,   138,    -1,    -1,     3,   142,   143,     6,
       7,    -1,    -1,    -1,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    -1,    35,    36,
      37,    38,    39,    40,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    50,    51,    52,    53,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    72,    73,   108,   109,   110,
     111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    90,    91,    -1,    -1,    -1,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,    -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   133,    -1,    -1,   136,
     137,   138,    -1,    -1,     3,   142,   143,     6,     7,    -1,
      -1,    -1,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    -1,    35,    36,    37,    38,
      39,    40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    50,    51,    52,    53,    54,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,   109,   110,   111,   112,     4,    87,    -1,
      -1,    90,    91,    10,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    21,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,    -1,    -1,    -1,   117,    -1,
      -1,    -1,   121,    -1,    -1,    -1,    43,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,   137,   138,
      -1,    -1,    -1,   142,   143,    -1,    63,    64,    65,    66,
      -1,    68,    69,    70,    -1,    -1,    -1,    74,    75,    76,
      -1,    78,    79,    80,    -1,    82,    83,    84,    85,    86,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,    96,
      -1,    98,    99,   100,   101,   102,   103,     3,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,
      -1,    -1,    -1,   120,    -1,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    -1,    35,
      36,    37,    38,    39,    40,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    50,    51,    52,    53,    54,    -1,
      -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,    -1,    -1,
      -1,    -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    -1,    35,    36,    37,    38,    39,
      -1,   117,    -1,    -1,   120,   121,     3,    -1,    -1,    -1,
     126,    -1,    -1,    -1,   130,    -1,    -1,   133,    -1,    -1,
      -1,    -1,   138,    -1,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    -1,    35,    36,
      37,    38,    39,    40,    -1,    -1,    -1,    87,    -1,    -1,
      90,    91,    49,    50,    51,    52,    53,    54,    -1,    -1,
      -1,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    72,    73,   117,    -1,    -1,
      -1,   121,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      87,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,    -1,    -1,    -1,    -1,    -1,
     117,    -1,    -1,   120,   121,     3,    -1,    -1,    -1,   126,
      -1,    -1,    -1,   130,    -1,    -1,   133,    -1,    -1,    -1,
      -1,   138,    -1,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    -1,    35,    36,    37,
      38,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    50,    51,    52,    53,    54,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    73,     3,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,    90,    91,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    -1,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    -1,   117,
      -1,    -1,   120,   121,    -1,    -1,    53,    54,   126,    -1,
      -1,    -1,   130,    -1,    -1,   133,    -1,    -1,    -1,    -1,
     138,    -1,    -1,    -1,    71,    72,    73,    -1,    -1,     0,
       1,    -1,     3,     4,    -1,    -1,    -1,     8,     9,    10,
      87,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    -1,    35,    36,    37,    38,    39,    -1,
     117,    -1,    43,    44,   121,    -1,    -1,    -1,    -1,    50,
      51,    52,    53,    54,    -1,    -1,    57,    -1,    -1,    -1,
      -1,   138,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    -1,    -1,    -1,    -1,    78,    79,    80,
      -1,    82,    83,    84,    85,    86,    87,    88,    -1,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,     1,   117,     3,     4,   120,
     121,    -1,     8,     9,    10,   126,   127,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    -1,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    -1,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    -1,    -1,
      -1,    -1,    78,    79,    80,    -1,    82,    83,    84,    85,
      86,    87,    88,    -1,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
       1,   117,     3,     4,   120,   121,    -1,     8,     9,    10,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    -1,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    -1,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    -1,    -1,    -1,    -1,    78,    79,    80,
      -1,    82,    83,    84,    85,    86,    87,    88,    -1,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,     1,   117,     3,     4,   120,
     121,    -1,     8,     9,    10,   126,   127,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    -1,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    -1,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    -1,    -1,
      -1,    -1,    78,    79,    80,    -1,    82,    83,    84,    85,
      86,    87,    88,    -1,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
       1,   117,     3,     4,   120,   121,    -1,     8,     9,    10,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    -1,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    43,    44,    -1,    -1,    -1,    -1,    -1,    50,
      51,    52,    53,    54,    -1,    -1,    57,    -1,    -1,    -1,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    -1,    -1,    -1,    -1,    78,    79,    80,
      -1,    82,    83,    84,    85,    86,    87,    88,    -1,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,    -1,    -1,    -1,     1,    -1,     3,    -1,
      -1,    -1,    -1,    -1,   115,    -1,   117,    -1,    -1,   120,
     121,    -1,    -1,    -1,    -1,   126,   127,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,    -1,    -1,   120,   121,    -1,    -1,    -1,
      -1,   126,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    -1,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      50,    51,    52,    53,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,
      90,    91,    -1,    -1,    -1,    -1,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,    -1,     3,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,    -1,
     120,   121,    -1,    -1,    -1,    -1,   126,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,   132,   133,   134,   135,   136,   137,   138,
     139,   140,    -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,    -1,    13,   120,   121,    -1,    -1,    -1,
      -1,   126,   127,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    -1,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    50,    51,    52,    53,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,
      -1,   120,   121,    -1,    -1,    -1,    -1,   126,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      -1,    35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
      -1,    -1,    -1,    -1,     1,    -1,     3,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    -1,    35,    36,
      37,    38,    39,   117,    -1,    -1,   120,   121,    -1,    -1,
      -1,    -1,   126,    50,    51,    52,    53,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    72,    73,    -1,     3,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    90,    91,    -1,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
     117,    -1,    -1,    -1,   121,    50,    51,    52,    53,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,    -1,
      -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    88,    -1,    90,    91,    -1,    -1,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    -1,    35,    36,    37,    38,    39,    -1,    -1,
      -1,    -1,   117,    -1,    -1,    -1,   121,   122,    50,    51,
      52,    53,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    88,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,    -1,   117,     6,     7,    -1,   121,
     122,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    -1,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      50,    51,    52,    53,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,    -1,    -1,     3,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,
      90,    91,    -1,    -1,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    -1,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,   117,    -1,    -1,
      -1,   121,    49,    50,    51,    52,    53,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    72,    73,    -1,     3,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    90,    91,    -1,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    -1,
      35,    36,    37,    38,    39,    40,    -1,    -1,    -1,    -1,
     117,    -1,    -1,   120,   121,    50,    51,    52,    53,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,    -1,
       3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    90,    91,    -1,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    -1,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,    -1,   117,    -1,    -1,    -1,   121,    50,    51,    52,
      53,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,
      73,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    87,    88,    -1,    90,    91,    -1,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    -1,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    -1,    -1,   117,    -1,    -1,    -1,   121,    50,
      51,    52,    53,    54,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    72,    73,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    90,
      91,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    -1,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,    -1,    -1,
     121,    -1,    -1,    53,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,     3,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,
      90,    91,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    -1,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,    -1,
      -1,   121,    -1,    -1,    53,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,   104,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,
      -1,    -1,   121,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    -1,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    53,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,   104,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,
      -1,    -1,   121,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    -1,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    59,    53,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,    87,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    59,    -1,
      -1,    -1,   108,   109,   110,   111,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,
      -1,    -1,   121,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,   108,   109,   110,   111,   112,
      59,   114,    -1,    -1,    -1,    -1,    -1,   108,   109,   110,
     111,   112,    59,   114,    -1,    -1,   129,   130,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
     109,   110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,   108,   109,   110,   111,   112,    -1,   114,    -1,    -1,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   123,   124,   125,   146,   147,   329,     1,     3,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    35,    36,    37,    38,    39,    49,    50,    51,    52,
      53,    54,    71,    72,    73,    87,    90,    91,   117,   120,
     121,   126,   198,   242,   243,   261,   262,   264,   265,   266,
     267,   268,   269,   298,   299,   314,   317,   319,     1,   243,
       1,    40,     0,     1,     4,     8,     9,    10,    21,    43,
      44,    57,    63,    64,    65,    66,    67,    68,    69,    70,
      78,    79,    80,    82,    83,    84,    85,    86,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     115,   120,   126,   127,   148,   149,   150,   152,   153,   154,
     155,   156,   159,   160,   162,   163,   164,   165,   166,   167,
     168,   171,   172,   173,   176,   178,   183,   184,   185,   186,
     188,   192,   200,   201,   202,   203,   204,   208,   209,   216,
     217,   229,   230,   237,   238,   329,    53,    72,    49,    49,
      40,   144,   104,   104,   313,   242,   317,   127,    43,   265,
     261,    40,    49,    55,    58,    77,   121,   130,   133,   138,
     142,   250,   251,   253,   255,   256,   257,   258,   317,   329,
     261,   268,   317,   313,   119,   144,   318,    43,    43,   239,
     240,   243,   329,   122,    40,     6,    86,   120,   323,    40,
     326,   329,     1,   263,   264,   314,    40,   326,    40,   170,
     329,    40,    40,    85,    86,    40,    85,    40,    78,    83,
      44,    78,    93,   317,    46,   314,   317,    40,     4,    46,
      40,    40,    43,    46,     4,   323,    40,   182,   263,   180,
     182,    40,    40,   323,    40,    30,    32,    35,   104,   191,
     266,   267,   299,   317,   326,    40,   130,   133,   253,   255,
     258,   317,    21,    86,    88,   198,   263,   299,   317,   120,
     121,   319,   320,   299,     3,     7,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    40,    56,   133,   136,
     137,   138,   142,   143,   243,   244,   245,   247,   263,   264,
     283,   284,   285,   286,   287,   288,   323,   324,   329,   261,
      40,   130,   133,   239,   256,   258,   317,    40,   255,   317,
      46,   106,   107,   270,   271,   272,   273,   274,    59,   283,
     286,   283,     3,    40,    49,   142,   254,   257,   317,    49,
     254,   257,   258,   261,   317,   250,    40,    58,   250,    40,
      58,    49,   130,   133,   254,   257,   317,   118,   319,   121,
     320,    41,    42,   241,   329,   272,   314,   315,   323,   191,
     299,     6,    46,   315,   327,   315,     0,    43,   253,   255,
      55,    41,   315,    53,    54,    72,   300,   301,   329,   314,
     314,   315,    13,   177,   239,   239,   317,    43,    55,   219,
      55,    46,   314,   179,   327,   314,   239,    46,   252,   253,
     255,   256,   329,    43,    42,   181,   329,   315,   316,   329,
     157,   158,   323,   239,   212,   213,   214,   243,   298,   329,
     317,   317,   133,   323,   130,   133,   258,   314,   317,   327,
      40,   315,    40,   130,   133,   317,   118,   253,   317,   275,
     314,   329,    40,   253,    77,   290,   291,   317,   329,    41,
     314,   318,   105,    48,   113,   283,   285,     3,    40,    49,
     285,   283,   283,   283,   283,   283,   283,   105,    42,   246,
     329,    40,   108,   109,   110,   111,   112,   114,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
      48,   113,     7,   130,   133,   258,   317,   255,   255,    41,
      41,   130,   133,   255,   317,   118,    58,   283,    59,    40,
     258,   317,   254,   317,    40,    58,   254,   258,   239,    59,
     283,   239,    59,   283,   254,    49,   254,   257,    49,   254,
     257,   118,    49,   130,   133,   254,   261,   318,    43,   127,
     243,    41,   317,   317,   187,    42,    55,    41,   250,    40,
     270,    41,   317,    41,    55,    41,    42,   175,    42,    41,
      41,    43,   263,   147,   317,   218,    41,    41,    41,    41,
     180,    40,   182,    41,    41,    42,    46,    41,   105,    42,
     215,   329,    40,    60,   118,    41,   258,   317,    43,   239,
     118,    81,    89,    90,    91,   130,   133,   259,   260,   261,
     303,   306,   307,   308,   329,    55,    77,   199,   329,   239,
     317,   308,   292,    46,    43,    91,   305,   329,   313,   299,
       3,     3,    41,   105,   130,   133,   138,   258,   104,   263,
     285,    40,   247,   283,   283,   283,   283,   283,   283,   283,
     283,   283,   283,   283,   283,   283,   283,   283,   283,   283,
     283,     3,     3,   317,   118,    41,    41,    41,   118,   253,
     256,   261,   283,   239,   254,   317,    41,   118,   239,    59,
     283,    41,    59,    41,    59,   254,   254,    49,   130,   133,
     254,   257,   254,    49,    49,   254,   243,   241,     4,    46,
     323,    40,   147,   327,   157,   287,   323,   328,    43,   239,
      43,    46,     4,   169,   323,     4,    43,    46,   116,   174,
     253,   323,   325,   315,   323,   328,    41,   243,   253,    46,
     252,    47,    43,   147,    44,   238,   180,    43,    46,    40,
      47,   239,   181,   117,   121,   314,   321,    43,   327,   243,
     174,    92,   210,   214,   239,   161,   261,   253,   323,   118,
      41,    40,    40,   303,    91,    90,   306,   260,   113,    58,
     193,   265,    43,    46,    41,   190,   250,    79,   293,   294,
     302,   329,   206,   290,   317,    40,    40,   283,   283,    41,
      41,   138,   261,    41,   130,   133,   283,   251,    41,   263,
     246,    77,    40,    40,    41,    41,   253,   256,    59,    41,
      41,   254,    41,    59,   259,   259,   254,    49,    49,   254,
     254,   254,   241,   239,    47,    42,    42,    41,   151,    40,
     308,    55,    41,    42,   175,   174,   253,   308,    43,    47,
     323,   263,   314,    43,    55,   325,   239,    41,   144,   119,
     144,   322,   104,    41,    47,   317,    44,   120,   188,   204,
     208,   209,   211,   226,   228,   238,   215,    41,   147,   308,
      43,   239,   283,   191,   104,   194,   329,   154,   276,   277,
     278,   279,   329,    40,    55,   306,   308,   309,     1,    42,
      43,    46,   189,    42,    74,    75,    76,   295,   297,     1,
      43,    67,    74,    75,    76,    79,   126,   142,   152,   153,
     154,   155,   159,   160,   164,   166,   168,   171,   173,   176,
     178,   183,   184,   185,   186,   204,   208,   209,   216,   220,
     224,   225,   226,   227,   228,   229,   231,   232,   235,   238,
     329,    46,   250,   248,   286,   329,   248,    41,   283,   283,
     283,    41,    41,    41,    41,   251,   283,   248,   248,    41,
      41,    41,   254,   254,    41,   327,   327,   259,   220,   239,
     174,   323,   328,    43,   253,    41,   308,    43,   253,    43,
     182,    41,   259,   121,   314,   314,   121,   314,   244,     4,
      46,    55,   104,    88,   122,   263,    61,    43,    41,    41,
     303,   304,   329,    40,    46,   195,   276,   126,   280,   281,
     314,    47,    42,   127,   239,   270,    55,    77,   310,   329,
     253,   294,   317,   296,   223,    46,    77,    77,    77,   142,
     224,   319,    47,   127,   220,    30,    32,    35,   236,   267,
     317,   205,    41,    42,   249,   329,    41,   283,   283,    41,
      41,    41,   250,    47,    41,   175,   308,    43,   253,   174,
      43,    43,   322,   322,   105,   263,   212,   263,    40,   303,
     190,   239,    40,    43,   196,   281,   277,    55,    43,   253,
     127,   278,    41,    43,   272,   311,   312,   317,    43,    46,
     308,    49,   289,   329,   302,   220,   221,   319,    40,    43,
     207,   253,    77,    43,   220,   286,    43,    43,    43,   308,
      43,   252,   105,    40,   130,   133,   258,   239,   189,    41,
     197,   282,   283,   308,   278,    43,    46,    43,    42,    49,
      40,    46,   190,   317,   220,    40,   239,   308,   283,    47,
     249,    43,    43,   239,    40,    40,    40,   133,    41,   308,
      43,   190,   312,   189,   289,    47,   239,    41,   190,    43,
     207,    41,   239,   239,   239,    40,   309,   113,   195,   189,
      49,   222,    41,   233,   308,   189,   234,   308,    41,    41,
      41,   239,   263,   196,   220,   234,    43,    46,    55,    43,
      46,    55,   234,   234,   234,    41,   195,   272,   270,   234,
     196,    43,    43
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,   145,   146,   146,   146,   146,   146,   146,   146,   147,
     147,   147,   147,   148,   148,   148,   148,   148,   148,   148,
     149,   149,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   151,   150,   152,   153,   154,   154,   154,   154,   154,
     155,   155,   156,   156,   156,   156,   157,   158,   158,   159,
     159,   159,   161,   160,   162,   162,   163,   163,   164,   164,
     164,   164,   165,   166,   166,   167,   167,   168,   168,   169,
     169,   170,   170,   171,   171,   171,   172,   172,   173,   173,
     173,   173,   173,   173,   173,   173,   174,   174,   174,   175,
     175,   176,   177,   177,   178,   178,   178,   179,   180,   181,
     181,   182,   182,   182,   183,   184,   185,   186,   186,   186,
     187,   186,   186,   186,   186,   186,   188,   188,   189,   189,
     189,   189,   190,   191,   191,   191,   191,   191,   191,   191,
     192,   192,   192,   193,   194,   194,   195,   196,   197,   196,
     198,   198,   198,   199,   199,   200,   201,   201,   202,   203,
     203,   203,   203,   203,   203,   205,   204,   206,   204,   207,
     207,   208,   210,   209,   209,   209,   209,   209,   211,   211,
     211,   211,   211,   211,   212,   213,   213,   214,   214,   215,
     215,   216,   216,   218,   217,   219,   217,   217,   220,   221,
     222,   220,   220,   220,   223,   220,   224,   224,   224,   224,
     224,   224,   224,   224,   224,   224,   224,   224,   224,   224,
     224,   224,   224,   224,   225,   225,   225,   226,   227,   227,
     228,   228,   228,   228,   228,   229,   230,   231,   231,   231,
     232,   232,   232,   232,   232,   232,   232,   232,   232,   232,
     232,   233,   233,   233,   234,   234,   234,   235,   236,   236,
     236,   236,   236,   237,   238,   238,   238,   238,   238,   238,
     238,   238,   238,   238,   238,   238,   238,   238,   238,   238,
     238,   238,   238,   238,   239,   240,   240,   241,   241,   241,
     242,   242,   242,   243,   243,   243,   244,   245,   245,   246,
     246,   247,   247,   248,   248,   249,   249,   250,   250,   250,
     250,   250,   251,   251,   251,   251,   252,   252,   252,   252,
     253,   253,   253,   253,   253,   253,   253,   253,   253,   253,
     253,   253,   253,   253,   253,   253,   253,   253,   253,   253,
     253,   253,   254,   254,   254,   254,   254,   254,   254,   254,
     255,   255,   255,   255,   255,   255,   255,   255,   255,   255,
     255,   255,   255,   256,   256,   256,   256,   256,   256,   256,
     256,   256,   256,   256,   256,   256,   256,   256,   257,   257,
     257,   257,   257,   257,   257,   257,   258,   258,   258,   258,
     259,   259,   259,   260,   260,   261,   261,   262,   262,   262,
     263,   264,   264,   264,   264,   265,   265,   265,   265,   265,
     265,   265,   265,   266,   267,   268,   268,   269,   269,   269,
     269,   269,   269,   269,   269,   269,   269,   269,   269,   269,
     269,   271,   270,   270,   272,   272,   273,   274,   275,   275,
     276,   276,   277,   277,   278,   278,   278,   278,   278,   279,
     280,   280,   281,   281,   282,   283,   283,   284,   284,   284,
     284,   284,   284,   284,   284,   285,   285,   285,   285,   285,
     285,   285,   285,   285,   285,   286,   286,   286,   286,   286,
     286,   286,   286,   286,   286,   286,   287,   287,   287,   287,
     287,   287,   287,   287,   288,   288,   288,   288,   288,   288,
     288,   288,   288,   288,   288,   288,   288,   288,   288,   288,
     288,   288,   288,   288,   288,   288,   288,   288,   288,   289,
     289,   290,   292,   291,   291,   293,   293,   295,   294,   296,
     294,   297,   297,   297,   298,   298,   298,   298,   299,   299,
     299,   300,   300,   300,   301,   301,   302,   302,   303,   303,
     303,   303,   304,   304,   305,   305,   306,   306,   306,   306,
     306,   306,   307,   307,   307,   308,   308,   309,   309,   309,
     309,   309,   309,   310,   310,   311,   311,   311,   311,   312,
     312,   313,   314,   314,   314,   315,   315,   315,   316,   316,
     317,   317,   317,   317,   317,   317,   317,   318,   318,   318,
     318,   319,   319,   320,   320,   321,   321,   321,   321,   321,
     321,   322,   322,   322,   322,   323,   323,   324,   324,   325,
     325,   325,   326,   326,   327,   327,   327,   327,   327,   327,
     328,   328,   329
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     3,     2,     3,     2,     5,     3,     2,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     8,     5,     3,     5,     5,     9,     3,     3,
       2,     2,     5,     2,     5,     2,     4,     1,     1,     7,
       7,     5,     0,     7,     1,     1,     2,     2,     1,     5,
       5,     5,     3,     4,     3,     7,     8,     5,     3,     1,
       1,     3,     1,     4,     7,     6,     1,     1,     7,     9,
       8,    10,     5,     7,     6,     8,     1,     1,     5,     4,
       5,     7,     1,     3,     6,     6,     8,     1,     2,     3,
       1,     2,     3,     6,     5,     9,     2,     1,     1,     1,
       0,     6,     1,     6,    10,     1,     6,     9,     1,     5,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     1,
      12,    14,     8,     1,     1,     1,     1,     1,     0,     3,
       1,     2,     2,     2,     1,     5,     8,    11,     6,     1,
       1,     1,     1,     1,     1,     0,    10,     0,     8,     1,
       4,     4,     0,     6,     3,     6,     4,     7,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     1,     3,
       1,     3,     4,     0,     6,     0,     5,     5,     2,     0,
       0,     7,     1,     1,     0,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     6,     6,     7,
       8,     8,     8,     9,     7,     5,     2,     2,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     4,     2,     2,     4,     2,     5,     1,     1,
       1,     1,     1,     2,     1,     1,     2,     2,     1,     1,
       1,     1,     1,     1,     2,     2,     2,     2,     1,     2,
       2,     2,     2,     1,     1,     2,     1,     3,     4,     1,
       2,     7,     1,     1,     2,     2,     1,     2,     1,     3,
       1,     1,     1,     2,     1,     3,     1,     2,     5,     2,
       2,     1,     2,     2,     1,     5,     1,     1,     5,     1,
       2,     3,     3,     1,     2,     2,     3,     4,     5,     4,
       3,     4,     4,     2,     3,     3,     4,     5,     6,     6,
       5,     5,     1,     2,     3,     4,     5,     3,     4,     4,
       1,     2,     4,     4,     4,     5,     6,     5,     6,     3,
       4,     4,     5,     1,     2,     2,     2,     3,     3,     1,
       2,     2,     1,     1,     2,     3,     3,     4,     3,     4,
       2,     3,     3,     4,     5,     3,     3,     2,     2,     1,
       1,     2,     1,     1,     1,     1,     2,     1,     1,     1,
       1,     2,     1,     2,     3,     1,     1,     1,     2,     1,
       1,     2,     1,     4,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     1,     1,     2,     3,     4,     1,     3,
       1,     2,     1,     3,     1,     1,     1,     3,     6,     3,
       6,     3,     6,     3,     6,     1,     1,     1,     5,     6,
       4,     2,     1,     1,     1,     1,     1,     3,     4,     5,
       5,     5,     6,     6,     2,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     5,     5,
       3,     3,     3,     5,     2,     2,     2,     2,     2,     1,
       1,     1,     0,     3,     1,     1,     3,     0,     4,     0,
       6,     1,     1,     1,     1,     1,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     1,     1,     1,     1,     4,     1,     1,     5,
       2,     4,     1,     1,     2,     1,     1,     3,     3,     4,
       4,     3,     4,     2,     1,     1,     3,     2,     4,     2,
       2,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     4,     1,     3,     1,     2,     3,     3,     2,     2,
       2,     1,     2,     1,     3,     2,     4,     1,     3,     1,
       3,     3,     2,     2,     2,     2,     1,     2,     1,     1,
       1,     1,     3,     1,     3,     5,     1,     3,     3,     5,
       1,     1,     0
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= END)
    {
      yychar = END;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* program: interface  */
#line 1730 "parser.y"
                            {
                   if (!classes) classes = NewHash();
		   Setattr((yyvsp[0].node),"classes",classes); 
		   Setattr((yyvsp[0].node),"name",ModuleName);
		   
		   if ((!module_node) && ModuleName) {
		     module_node = new_node("module");
		     Setattr(module_node,"name",ModuleName);
		   }
		   Setattr((yyvsp[0].node),"module",module_node);
	           top = (yyvsp[0].node);
               }
#line 5236 "y.tab.c"
    break;

  case 3: /* program: PARSETYPE parm SEMI  */
#line 1742 "parser.y"
                                     {
                 top = Copy(Getattr((yyvsp[-1].p),"type"));
		 Delete((yyvsp[-1].p));
               }
#line 5245 "y.tab.c"
    break;

  case 4: /* program: PARSETYPE error  */
#line 1746 "parser.y"
                                 {
                 top = 0;
               }
#line 5253 "y.tab.c"
    break;

  case 5: /* program: PARSEPARM parm SEMI  */
#line 1749 "parser.y"
                                     {
                 top = (yyvsp[-1].p);
               }
#line 5261 "y.tab.c"
    break;

  case 6: /* program: PARSEPARM error  */
#line 1752 "parser.y"
                                 {
                 top = 0;
               }
#line 5269 "y.tab.c"
    break;

  case 7: /* program: PARSEPARMS LPAREN parms RPAREN SEMI  */
#line 1755 "parser.y"
                                                     {
                 top = (yyvsp[-2].pl);
               }
#line 5277 "y.tab.c"
    break;

  case 8: /* program: PARSEPARMS error SEMI  */
#line 1758 "parser.y"
                                       {
                 top = 0;
               }
#line 5285 "y.tab.c"
    break;

  case 9: /* interface: interface declaration  */
#line 1763 "parser.y"
                                       {  
                   /* add declaration to end of linked list (the declaration isn't always a single declaration, sometimes it is a linked list itself) */
                   if (currentDeclComment != NULL) {
		     set_comment((yyvsp[0].node), currentDeclComment);
		     currentDeclComment = NULL;
                   }                                      
                   appendChild((yyvsp[-1].node),(yyvsp[0].node));
                   (yyval.node) = (yyvsp[-1].node);
               }
#line 5299 "y.tab.c"
    break;

  case 10: /* interface: interface DOXYGENSTRING  */
#line 1772 "parser.y"
                                         {
                   currentDeclComment = (yyvsp[0].str); 
                   (yyval.node) = (yyvsp[-1].node);
               }
#line 5308 "y.tab.c"
    break;

  case 11: /* interface: interface DOXYGENPOSTSTRING  */
#line 1776 "parser.y"
                                             {
                   Node *node = lastChild((yyvsp[-1].node));
                   if (node) {
                     set_comment(node, (yyvsp[0].str));
                   }
                   (yyval.node) = (yyvsp[-1].node);
               }
#line 5320 "y.tab.c"
    break;

  case 12: /* interface: empty  */
#line 1783 "parser.y"
                       {
                   (yyval.node) = new_node("top");
               }
#line 5328 "y.tab.c"
    break;

  case 13: /* declaration: swig_directive  */
#line 1788 "parser.y"
                                { (yyval.node) = (yyvsp[0].node); }
#line 5334 "y.tab.c"
    break;

  case 14: /* declaration: c_declaration  */
#line 1789 "parser.y"
                               { (yyval.node) = (yyvsp[0].node); }
#line 5340 "y.tab.c"
    break;

  case 15: /* declaration: cpp_declaration  */
#line 1790 "parser.y"
                                 { (yyval.node) = (yyvsp[0].node); }
#line 5346 "y.tab.c"
    break;

  case 16: /* declaration: SEMI  */
#line 1791 "parser.y"
                      { (yyval.node) = 0; }
#line 5352 "y.tab.c"
    break;

  case 17: /* declaration: error  */
#line 1792 "parser.y"
                       {
                  (yyval.node) = 0;
		  if (cparse_unknown_directive) {
		      Swig_error(cparse_file, cparse_line, "Unknown directive '%s'.\n", cparse_unknown_directive);
		  } else {
		      Swig_error(cparse_file, cparse_line, "Syntax error in input(1).\n");
		  }
		  Exit(EXIT_FAILURE);
               }
#line 5366 "y.tab.c"
    break;

  case 18: /* declaration: c_constructor_decl  */
#line 1802 "parser.y"
                                    { 
                  if ((yyval.node)) {
   		      add_symbols((yyval.node));
                  }
                  (yyval.node) = (yyvsp[0].node); 
	       }
#line 5377 "y.tab.c"
    break;

  case 19: /* declaration: error CONVERSIONOPERATOR  */
#line 1818 "parser.y"
                                          {
                  (yyval.node) = 0;
                  skip_decl();
               }
#line 5386 "y.tab.c"
    break;

  case 20: /* swig_directive: extend_directive  */
#line 1828 "parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 5392 "y.tab.c"
    break;

  case 21: /* swig_directive: apply_directive  */
#line 1829 "parser.y"
                                 { (yyval.node) = (yyvsp[0].node); }
#line 5398 "y.tab.c"
    break;

  case 22: /* swig_directive: clear_directive  */
#line 1830 "parser.y"
                                 { (yyval.node) = (yyvsp[0].node); }
#line 5404 "y.tab.c"
    break;

  case 23: /* swig_directive: constant_directive  */
#line 1831 "parser.y"
                                    { (yyval.node) = (yyvsp[0].node); }
#line 5410 "y.tab.c"
    break;

  case 24: /* swig_directive: echo_directive  */
#line 1832 "parser.y"
                                { (yyval.node) = (yyvsp[0].node); }
#line 5416 "y.tab.c"
    break;

  case 25: /* swig_directive: except_directive  */
#line 1833 "parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 5422 "y.tab.c"
    break;

  case 26: /* swig_directive: fragment_directive  */
#line 1834 "parser.y"
                                    { (yyval.node) = (yyvsp[0].node); }
#line 5428 "y.tab.c"
    break;

  case 27: /* swig_directive: include_directive  */
#line 1835 "parser.y"
                                   { (yyval.node) = (yyvsp[0].node); }
#line 5434 "y.tab.c"
    break;

  case 28: /* swig_directive: inline_directive  */
#line 1836 "parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 5440 "y.tab.c"
    break;

  case 29: /* swig_directive: insert_directive  */
#line 1837 "parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 5446 "y.tab.c"
    break;

  case 30: /* swig_directive: module_directive  */
#line 1838 "parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 5452 "y.tab.c"
    break;

  case 31: /* swig_directive: name_directive  */
#line 1839 "parser.y"
                                { (yyval.node) = (yyvsp[0].node); }
#line 5458 "y.tab.c"
    break;

  case 32: /* swig_directive: native_directive  */
#line 1840 "parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 5464 "y.tab.c"
    break;

  case 33: /* swig_directive: pragma_directive  */
#line 1841 "parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 5470 "y.tab.c"
    break;

  case 34: /* swig_directive: rename_directive  */
#line 1842 "parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 5476 "y.tab.c"
    break;

  case 35: /* swig_directive: feature_directive  */
#line 1843 "parser.y"
                                   { (yyval.node) = (yyvsp[0].node); }
#line 5482 "y.tab.c"
    break;

  case 36: /* swig_directive: varargs_directive  */
#line 1844 "parser.y"
                                   { (yyval.node) = (yyvsp[0].node); }
#line 5488 "y.tab.c"
    break;

  case 37: /* swig_directive: typemap_directive  */
#line 1845 "parser.y"
                                   { (yyval.node) = (yyvsp[0].node); }
#line 5494 "y.tab.c"
    break;

  case 38: /* swig_directive: types_directive  */
#line 1846 "parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 5500 "y.tab.c"
    break;

  case 39: /* swig_directive: template_directive  */
#line 1847 "parser.y"
                                    { (yyval.node) = (yyvsp[0].node); }
#line 5506 "y.tab.c"
    break;

  case 40: /* swig_directive: warn_directive  */
#line 1848 "parser.y"
                                { (yyval.node) = (yyvsp[0].node); }
#line 5512 "y.tab.c"
    break;

  case 41: /* $@1: %empty  */
#line 1855 "parser.y"
                                                             {
               Node *cls;
	       String *clsname;
	       extendmode = 1;
	       cplus_mode = CPLUS_PUBLIC;
	       if (!classes) classes = NewHash();
	       if (!classes_typedefs) classes_typedefs = NewHash();
	       clsname = make_class_name((yyvsp[-1].str));
	       cls = Getattr(classes,clsname);
	       if (!cls) {
	         cls = Getattr(classes_typedefs, clsname);
		 if (!cls) {
		   /* No previous definition. Create a new scope */
		   Node *am = Getattr(Swig_extend_hash(),clsname);
		   if (!am) {
		     Swig_symbol_newscope();
		     Swig_symbol_setscopename((yyvsp[-1].str));
		     prev_symtab = 0;
		   } else {
		     prev_symtab = Swig_symbol_setscope(Getattr(am,"symtab"));
		   }
		   current_class = 0;
		 } else {
		   /* Previous typedef class definition.  Use its symbol table.
		      Deprecated, just the real name should be used. 
		      Note that %extend before the class typedef never worked, only %extend after the class typedef. */
		   prev_symtab = Swig_symbol_setscope(Getattr(cls, "symtab"));
		   current_class = cls;
		   SWIG_WARN_NODE_BEGIN(cls);
		   Swig_warning(WARN_PARSE_EXTEND_NAME, cparse_file, cparse_line, "Deprecated %%extend name used - the %s name '%s' should be used instead of the typedef name '%s'.\n", Getattr(cls, "kind"), SwigType_namestr(Getattr(cls, "name")), (yyvsp[-1].str));
		   SWIG_WARN_NODE_END(cls);
		 }
	       } else {
		 /* Previous class definition.  Use its symbol table */
		 prev_symtab = Swig_symbol_setscope(Getattr(cls,"symtab"));
		 current_class = cls;
	       }
	       Classprefix = NewString((yyvsp[-1].str));
	       Namespaceprefix= Swig_symbol_qualifiedscopename(0);
	       Delete(clsname);
	     }
#line 5558 "y.tab.c"
    break;

  case 42: /* extend_directive: EXTEND options classkeyopt idcolon LBRACE $@1 cpp_members RBRACE  */
#line 1895 "parser.y"
                                  {
               String *clsname;
	       extendmode = 0;
               (yyval.node) = new_node("extend");
	       Setattr((yyval.node),"symtab",Swig_symbol_popscope());
	       if (prev_symtab) {
		 Swig_symbol_setscope(prev_symtab);
	       }
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
               clsname = make_class_name((yyvsp[-4].str));
	       Setattr((yyval.node),"name",clsname);

	       mark_nodes_as_extend((yyvsp[-1].node));
	       if (current_class) {
		 /* We add the extension to the previously defined class */
		 appendChild((yyval.node), (yyvsp[-1].node));
		 appendChild(current_class,(yyval.node));
	       } else {
		 /* We store the extensions in the extensions hash */
		 Node *am = Getattr(Swig_extend_hash(),clsname);
		 if (am) {
		   /* Append the members to the previous extend methods */
		   appendChild(am, (yyvsp[-1].node));
		 } else {
		   appendChild((yyval.node), (yyvsp[-1].node));
		   Setattr(Swig_extend_hash(),clsname,(yyval.node));
		 }
	       }
	       current_class = 0;
	       Delete(Classprefix);
	       Delete(clsname);
	       Classprefix = 0;
	       prev_symtab = 0;
	       (yyval.node) = 0;

	     }
#line 5599 "y.tab.c"
    break;

  case 43: /* apply_directive: APPLY typemap_parm LBRACE tm_list RBRACE  */
#line 1937 "parser.y"
                                                           {
                    (yyval.node) = new_node("apply");
                    Setattr((yyval.node),"pattern",Getattr((yyvsp[-3].p),"pattern"));
		    appendChild((yyval.node),(yyvsp[-1].p));
               }
#line 5609 "y.tab.c"
    break;

  case 44: /* clear_directive: CLEAR tm_list SEMI  */
#line 1947 "parser.y"
                                     {
		 (yyval.node) = new_node("clear");
		 appendChild((yyval.node),(yyvsp[-1].p));
               }
#line 5618 "y.tab.c"
    break;

  case 45: /* constant_directive: CONSTANT identifier EQUAL definetype SEMI  */
#line 1961 "parser.y"
                                                                {
		   if (((yyvsp[-1].dtype).type != T_ERROR) && ((yyvsp[-1].dtype).type != T_SYMBOL)) {
		     SwigType *type = NewSwigType((yyvsp[-1].dtype).type);
		     (yyval.node) = new_node("constant");
		     Setattr((yyval.node),"name",(yyvsp[-3].id));
		     Setattr((yyval.node),"type",type);
		     Setattr((yyval.node),"value",(yyvsp[-1].dtype).val);
		     if ((yyvsp[-1].dtype).rawval) Setattr((yyval.node),"rawval", (yyvsp[-1].dtype).rawval);
		     Setattr((yyval.node),"storage","%constant");
		     SetFlag((yyval.node),"feature:immutable");
		     add_symbols((yyval.node));
		     Delete(type);
		   } else {
		     if ((yyvsp[-1].dtype).type == T_ERROR) {
		       Swig_warning(WARN_PARSE_UNSUPPORTED_VALUE,cparse_file,cparse_line,"Unsupported constant value (ignored)\n");
		     }
		     (yyval.node) = 0;
		   }

	       }
#line 5643 "y.tab.c"
    break;

  case 46: /* constant_directive: CONSTANT type declarator def_args SEMI  */
#line 1981 "parser.y"
                                                        {
		 if (((yyvsp[-1].dtype).type != T_ERROR) && ((yyvsp[-1].dtype).type != T_SYMBOL)) {
		   SwigType_push((yyvsp[-3].type),(yyvsp[-2].decl).type);
		   /* Sneaky callback function trick */
		   if (SwigType_isfunction((yyvsp[-3].type))) {
		     SwigType_add_pointer((yyvsp[-3].type));
		   }
		   (yyval.node) = new_node("constant");
		   Setattr((yyval.node),"name",(yyvsp[-2].decl).id);
		   Setattr((yyval.node),"type",(yyvsp[-3].type));
		   Setattr((yyval.node),"value",(yyvsp[-1].dtype).val);
		   if ((yyvsp[-1].dtype).rawval) Setattr((yyval.node),"rawval", (yyvsp[-1].dtype).rawval);
		   Setattr((yyval.node),"storage","%constant");
		   SetFlag((yyval.node),"feature:immutable");
		   add_symbols((yyval.node));
		 } else {
		   if ((yyvsp[-1].dtype).type == T_ERROR) {
		     Swig_warning(WARN_PARSE_UNSUPPORTED_VALUE,cparse_file,cparse_line, "Unsupported constant value\n");
		   }
		   (yyval.node) = 0;
		 }
               }
#line 5670 "y.tab.c"
    break;

  case 47: /* constant_directive: CONSTANT type direct_declarator LPAREN parms RPAREN cv_ref_qualifier def_args SEMI  */
#line 2005 "parser.y"
                                                                                                    {
		 if (((yyvsp[-1].dtype).type != T_ERROR) && ((yyvsp[-1].dtype).type != T_SYMBOL)) {
		   SwigType_add_function((yyvsp[-7].type), (yyvsp[-4].pl));
		   SwigType_push((yyvsp[-7].type), (yyvsp[-2].dtype).qualifier);
		   SwigType_push((yyvsp[-7].type), (yyvsp[-6].decl).type);
		   /* Sneaky callback function trick */
		   if (SwigType_isfunction((yyvsp[-7].type))) {
		     SwigType_add_pointer((yyvsp[-7].type));
		   }
		   (yyval.node) = new_node("constant");
		   Setattr((yyval.node), "name", (yyvsp[-6].decl).id);
		   Setattr((yyval.node), "type", (yyvsp[-7].type));
		   Setattr((yyval.node), "value", (yyvsp[-1].dtype).val);
		   if ((yyvsp[-1].dtype).rawval) Setattr((yyval.node), "rawval", (yyvsp[-1].dtype).rawval);
		   Setattr((yyval.node), "storage", "%constant");
		   SetFlag((yyval.node), "feature:immutable");
		   add_symbols((yyval.node));
		 } else {
		   if ((yyvsp[-1].dtype).type == T_ERROR) {
		     Swig_warning(WARN_PARSE_UNSUPPORTED_VALUE,cparse_file,cparse_line, "Unsupported constant value\n");
		   }
		   (yyval.node) = 0;
		 }
	       }
#line 5699 "y.tab.c"
    break;

  case 48: /* constant_directive: CONSTANT error SEMI  */
#line 2029 "parser.y"
                                     {
		 Swig_warning(WARN_PARSE_BAD_VALUE,cparse_file,cparse_line,"Bad constant value (ignored).\n");
		 (yyval.node) = 0;
	       }
#line 5708 "y.tab.c"
    break;

  case 49: /* constant_directive: CONSTANT error END  */
#line 2033 "parser.y"
                                    {
		 Swig_error(cparse_file,cparse_line,"Missing semicolon (';') after %%constant.\n");
		 Exit(EXIT_FAILURE);
	       }
#line 5717 "y.tab.c"
    break;

  case 50: /* echo_directive: ECHO HBLOCK  */
#line 2044 "parser.y"
                             {
		 char temp[64];
		 Replace((yyvsp[0].str),"$file",cparse_file, DOH_REPLACE_ANY);
		 sprintf(temp,"%d", cparse_line);
		 Replace((yyvsp[0].str),"$line",temp,DOH_REPLACE_ANY);
		 Printf(stderr,"%s\n", (yyvsp[0].str));
		 Delete((yyvsp[0].str));
                 (yyval.node) = 0;
	       }
#line 5731 "y.tab.c"
    break;

  case 51: /* echo_directive: ECHO string  */
#line 2053 "parser.y"
                             {
		 char temp[64];
		 String *s = (yyvsp[0].str);
		 Replace(s,"$file",cparse_file, DOH_REPLACE_ANY);
		 sprintf(temp,"%d", cparse_line);
		 Replace(s,"$line",temp,DOH_REPLACE_ANY);
		 Printf(stderr,"%s\n", s);
		 Delete(s);
                 (yyval.node) = 0;
               }
#line 5746 "y.tab.c"
    break;

  case 52: /* except_directive: EXCEPT LPAREN identifier RPAREN LBRACE  */
#line 2072 "parser.y"
                                                          {
                    skip_balanced('{','}');
		    (yyval.node) = 0;
		    Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
	       }
#line 5756 "y.tab.c"
    break;

  case 53: /* except_directive: EXCEPT LBRACE  */
#line 2078 "parser.y"
                               {
                    skip_balanced('{','}');
		    (yyval.node) = 0;
		    Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
               }
#line 5766 "y.tab.c"
    break;

  case 54: /* except_directive: EXCEPT LPAREN identifier RPAREN SEMI  */
#line 2084 "parser.y"
                                                      {
		 (yyval.node) = 0;
		 Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
               }
#line 5775 "y.tab.c"
    break;

  case 55: /* except_directive: EXCEPT SEMI  */
#line 2089 "parser.y"
                             {
		 (yyval.node) = 0;
		 Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
	       }
#line 5784 "y.tab.c"
    break;

  case 56: /* stringtype: string LBRACE parm RBRACE  */
#line 2096 "parser.y"
                                          {		 
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"value",(yyvsp[-3].str));
		 Setattr((yyval.node),"type",Getattr((yyvsp[-1].p),"type"));
               }
#line 5794 "y.tab.c"
    break;

  case 57: /* fname: string  */
#line 2103 "parser.y"
                       {
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"value",(yyvsp[0].str));
              }
#line 5803 "y.tab.c"
    break;

  case 58: /* fname: stringtype  */
#line 2107 "parser.y"
                           {
                (yyval.node) = (yyvsp[0].node);
              }
#line 5811 "y.tab.c"
    break;

  case 59: /* fragment_directive: FRAGMENT LPAREN fname COMMA kwargs RPAREN HBLOCK  */
#line 2120 "parser.y"
                                                                     {
                   Hash *p = (yyvsp[-2].node);
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[-4].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[-4].node),"type"));
		   Setattr((yyval.node),"section",Getattr(p,"name"));
		   Setattr((yyval.node),"kwargs",nextSibling(p));
		   Setattr((yyval.node),"code",(yyvsp[0].str));
                 }
#line 5825 "y.tab.c"
    break;

  case 60: /* fragment_directive: FRAGMENT LPAREN fname COMMA kwargs RPAREN LBRACE  */
#line 2129 "parser.y"
                                                                    {
		   Hash *p = (yyvsp[-2].node);
		   String *code;
                   skip_balanced('{','}');
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[-4].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[-4].node),"type"));
		   Setattr((yyval.node),"section",Getattr(p,"name"));
		   Setattr((yyval.node),"kwargs",nextSibling(p));
		   Delitem(scanner_ccode,0);
		   Delitem(scanner_ccode,DOH_END);
		   code = Copy(scanner_ccode);
		   Setattr((yyval.node),"code",code);
		   Delete(code);
                 }
#line 5845 "y.tab.c"
    break;

  case 61: /* fragment_directive: FRAGMENT LPAREN fname RPAREN SEMI  */
#line 2144 "parser.y"
                                                     {
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[-2].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[-2].node),"type"));
		   Setattr((yyval.node),"emitonly","1");
		 }
#line 5856 "y.tab.c"
    break;

  case 62: /* $@2: %empty  */
#line 2157 "parser.y"
                                                        {
                     (yyvsp[-3].loc).filename = Copy(cparse_file);
		     (yyvsp[-3].loc).line = cparse_line;
		     scanner_set_location((yyvsp[-1].str),1);
                     if ((yyvsp[-2].node)) { 
		       String *maininput = Getattr((yyvsp[-2].node), "maininput");
		       if (maininput)
		         scanner_set_main_input_file(NewString(maininput));
		     }
               }
#line 5871 "y.tab.c"
    break;

  case 63: /* include_directive: includetype options string BEGINFILE $@2 interface ENDOFFILE  */
#line 2166 "parser.y"
                                     {
                     String *mname = 0;
                     (yyval.node) = (yyvsp[-1].node);
		     scanner_set_location((yyvsp[-6].loc).filename,(yyvsp[-6].loc).line+1);
		     if (strcmp((yyvsp[-6].loc).type,"include") == 0) set_nodeType((yyval.node),"include");
		     if (strcmp((yyvsp[-6].loc).type,"import") == 0) {
		       mname = (yyvsp[-5].node) ? Getattr((yyvsp[-5].node),"module") : 0;
		       set_nodeType((yyval.node),"import");
		       if (import_mode) --import_mode;
		     }
		     
		     Setattr((yyval.node),"name",(yyvsp[-4].str));
		     /* Search for the module (if any) */
		     {
			 Node *n = firstChild((yyval.node));
			 while (n) {
			     if (Strcmp(nodeType(n),"module") == 0) {
			         if (mname) {
				   Setattr(n,"name", mname);
				   mname = 0;
				 }
				 Setattr((yyval.node),"module",Getattr(n,"name"));
				 break;
			     }
			     n = nextSibling(n);
			 }
			 if (mname) {
			   /* There is no module node in the import
			      node, ie, you imported a .h file
			      directly.  We are forced then to create
			      a new import node with a module node.
			   */			      
			   Node *nint = new_node("import");
			   Node *mnode = new_node("module");
			   Setattr(mnode,"name", mname);
                           Setattr(mnode,"options",(yyvsp[-5].node));
			   appendChild(nint,mnode);
			   Delete(mnode);
			   appendChild(nint,firstChild((yyval.node)));
			   (yyval.node) = nint;
			   Setattr((yyval.node),"module",mname);
			 }
		     }
		     Setattr((yyval.node),"options",(yyvsp[-5].node));
               }
#line 5921 "y.tab.c"
    break;

  case 64: /* includetype: INCLUDE  */
#line 2213 "parser.y"
                         { (yyval.loc).type = "include"; }
#line 5927 "y.tab.c"
    break;

  case 65: /* includetype: IMPORT  */
#line 2214 "parser.y"
                         { (yyval.loc).type = "import"; ++import_mode;}
#line 5933 "y.tab.c"
    break;

  case 66: /* inline_directive: INLINE HBLOCK  */
#line 2221 "parser.y"
                                 {
                 String *cpps;
		 if (Namespaceprefix) {
		   Swig_error(cparse_file, cparse_start_line, "%%inline directive inside a namespace is disallowed.\n");
		   (yyval.node) = 0;
		 } else {
		   (yyval.node) = new_node("insert");
		   Setattr((yyval.node),"code",(yyvsp[0].str));
		   /* Need to run through the preprocessor */
		   Seek((yyvsp[0].str),0,SEEK_SET);
		   Setline((yyvsp[0].str),cparse_start_line);
		   Setfile((yyvsp[0].str),cparse_file);
		   cpps = Preprocessor_parse((yyvsp[0].str));
		   start_inline(Char(cpps), cparse_start_line);
		   Delete((yyvsp[0].str));
		   Delete(cpps);
		 }
		 
	       }
#line 5957 "y.tab.c"
    break;

  case 67: /* inline_directive: INLINE LBRACE  */
#line 2240 "parser.y"
                               {
                 String *cpps;
		 int start_line = cparse_line;
		 skip_balanced('{','}');
		 if (Namespaceprefix) {
		   Swig_error(cparse_file, cparse_start_line, "%%inline directive inside a namespace is disallowed.\n");
		   
		   (yyval.node) = 0;
		 } else {
		   String *code;
                   (yyval.node) = new_node("insert");
		   Delitem(scanner_ccode,0);
		   Delitem(scanner_ccode,DOH_END);
		   code = Copy(scanner_ccode);
		   Setattr((yyval.node),"code", code);
		   Delete(code);		   
		   cpps=Copy(scanner_ccode);
		   start_inline(Char(cpps), start_line);
		   Delete(cpps);
		 }
               }
#line 5983 "y.tab.c"
    break;

  case 68: /* insert_directive: HBLOCK  */
#line 2271 "parser.y"
                          {
                 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"code",(yyvsp[0].str));
	       }
#line 5992 "y.tab.c"
    break;

  case 69: /* insert_directive: INSERT LPAREN idstring RPAREN string  */
#line 2275 "parser.y"
                                                      {
		 String *code = NewStringEmpty();
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[-2].id));
		 Setattr((yyval.node),"code",code);
		 if (Swig_insert_file((yyvsp[0].str),code) < 0) {
		   Swig_error(cparse_file, cparse_line, "Couldn't find '%s'.\n", (yyvsp[0].str));
		   (yyval.node) = 0;
		 } 
               }
#line 6007 "y.tab.c"
    break;

  case 70: /* insert_directive: INSERT LPAREN idstring RPAREN HBLOCK  */
#line 2285 "parser.y"
                                                      {
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[-2].id));
		 Setattr((yyval.node),"code",(yyvsp[0].str));
               }
#line 6017 "y.tab.c"
    break;

  case 71: /* insert_directive: INSERT LPAREN idstring RPAREN LBRACE  */
#line 2290 "parser.y"
                                                      {
		 String *code;
                 skip_balanced('{','}');
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[-2].id));
		 Delitem(scanner_ccode,0);
		 Delitem(scanner_ccode,DOH_END);
		 code = Copy(scanner_ccode);
		 Setattr((yyval.node),"code", code);
		 Delete(code);
	       }
#line 6033 "y.tab.c"
    break;

  case 72: /* module_directive: MODULE options idstring  */
#line 2308 "parser.y"
                                          {
                 (yyval.node) = new_node("module");
		 if ((yyvsp[-1].node)) {
		   Setattr((yyval.node),"options",(yyvsp[-1].node));
		   if (Getattr((yyvsp[-1].node),"directors")) {
		     Wrapper_director_mode_set(1);
		     if (!cparse_cplusplus) {
		       Swig_error(cparse_file, cparse_line, "Directors are not supported for C code and require the -c++ option\n");
		     }
		   } 
		   if (Getattr((yyvsp[-1].node),"dirprot")) {
		     Wrapper_director_protected_mode_set(1);
		   } 
		   if (Getattr((yyvsp[-1].node),"allprotected")) {
		     Wrapper_all_protected_mode_set(1);
		   } 
		   if (Getattr((yyvsp[-1].node),"templatereduce")) {
		     template_reduce = 1;
		   }
		   if (Getattr((yyvsp[-1].node),"notemplatereduce")) {
		     template_reduce = 0;
		   }
		 }
		 if (!ModuleName) ModuleName = NewString((yyvsp[0].id));
		 if (!import_mode) {
		   /* first module included, we apply global
		      ModuleName, which can be modify by -module */
		   String *mname = Copy(ModuleName);
		   Setattr((yyval.node),"name",mname);
		   Delete(mname);
		 } else { 
		   /* import mode, we just pass the idstring */
		   Setattr((yyval.node),"name",(yyvsp[0].id));   
		 }		 
		 if (!module_node) module_node = (yyval.node);
	       }
#line 6074 "y.tab.c"
    break;

  case 73: /* name_directive: NAME LPAREN idstring RPAREN  */
#line 2351 "parser.y"
                                             {
                 Swig_warning(WARN_DEPRECATED_NAME,cparse_file,cparse_line, "%%name is deprecated.  Use %%rename instead.\n");
		 Delete(yyrename);
                 yyrename = NewString((yyvsp[-1].id));
		 (yyval.node) = 0;
               }
#line 6085 "y.tab.c"
    break;

  case 74: /* name_directive: NAME LPAREN RPAREN  */
#line 2357 "parser.y"
                                    {
		 Swig_warning(WARN_DEPRECATED_NAME,cparse_file,cparse_line, "%%name is deprecated.  Use %%rename instead.\n");
		 (yyval.node) = 0;
		 Swig_error(cparse_file,cparse_line,"Missing argument to %%name directive.\n");
	       }
#line 6095 "y.tab.c"
    break;

  case 75: /* native_directive: NATIVE LPAREN identifier RPAREN storage_class identifier SEMI  */
#line 2370 "parser.y"
                                                                                 {
                 (yyval.node) = new_node("native");
		 Setattr((yyval.node),"name",(yyvsp[-4].id));
		 Setattr((yyval.node),"wrap:name",(yyvsp[-1].id));
	         add_symbols((yyval.node));
	       }
#line 6106 "y.tab.c"
    break;

  case 76: /* native_directive: NATIVE LPAREN identifier RPAREN storage_class type declarator SEMI  */
#line 2376 "parser.y"
                                                                                    {
		 if (!SwigType_isfunction((yyvsp[-1].decl).type)) {
		   Swig_error(cparse_file,cparse_line,"%%native declaration '%s' is not a function.\n", (yyvsp[-1].decl).id);
		   (yyval.node) = 0;
		 } else {
		     Delete(SwigType_pop_function((yyvsp[-1].decl).type));
		     /* Need check for function here */
		     SwigType_push((yyvsp[-2].type),(yyvsp[-1].decl).type);
		     (yyval.node) = new_node("native");
	             Setattr((yyval.node),"name",(yyvsp[-5].id));
		     Setattr((yyval.node),"wrap:name",(yyvsp[-1].decl).id);
		     Setattr((yyval.node),"type",(yyvsp[-2].type));
		     Setattr((yyval.node),"parms",(yyvsp[-1].decl).parms);
		     Setattr((yyval.node),"decl",(yyvsp[-1].decl).type);
		 }
	         add_symbols((yyval.node));
	       }
#line 6128 "y.tab.c"
    break;

  case 77: /* pragma_directive: PRAGMA pragma_lang identifier EQUAL pragma_arg  */
#line 2402 "parser.y"
                                                                  {
                 (yyval.node) = new_node("pragma");
		 Setattr((yyval.node),"lang",(yyvsp[-3].id));
		 Setattr((yyval.node),"name",(yyvsp[-2].id));
		 Setattr((yyval.node),"value",(yyvsp[0].str));
	       }
#line 6139 "y.tab.c"
    break;

  case 78: /* pragma_directive: PRAGMA pragma_lang identifier  */
#line 2408 "parser.y"
                                              {
		(yyval.node) = new_node("pragma");
		Setattr((yyval.node),"lang",(yyvsp[-1].id));
		Setattr((yyval.node),"name",(yyvsp[0].id));
	      }
#line 6149 "y.tab.c"
    break;

  case 79: /* pragma_arg: string  */
#line 2415 "parser.y"
                       { (yyval.str) = (yyvsp[0].str); }
#line 6155 "y.tab.c"
    break;

  case 80: /* pragma_arg: HBLOCK  */
#line 2416 "parser.y"
                       { (yyval.str) = (yyvsp[0].str); }
#line 6161 "y.tab.c"
    break;

  case 81: /* pragma_lang: LPAREN identifier RPAREN  */
#line 2419 "parser.y"
                                         { (yyval.id) = (yyvsp[-1].id); }
#line 6167 "y.tab.c"
    break;

  case 82: /* pragma_lang: empty  */
#line 2420 "parser.y"
                      { (yyval.id) = (char *) "swig"; }
#line 6173 "y.tab.c"
    break;

  case 83: /* rename_directive: rename_namewarn declarator idstring SEMI  */
#line 2427 "parser.y"
                                                            {
                SwigType *t = (yyvsp[-2].decl).type;
		Hash *kws = NewHash();
		String *fixname;
		fixname = feature_identifier_fix((yyvsp[-2].decl).id);
		Setattr(kws,"name",(yyvsp[-1].id));
		if (!Len(t)) t = 0;
		/* Special declarator check */
		if (t) {
		  if (SwigType_isfunction(t)) {
		    SwigType *decl = SwigType_pop_function(t);
		    if (SwigType_ispointer(t)) {
		      String *nname = NewStringf("*%s",fixname);
		      if ((yyvsp[-3].intvalue)) {
			Swig_name_rename_add(Namespaceprefix, nname,decl,kws,(yyvsp[-2].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,nname,decl,kws);
		      }
		      Delete(nname);
		    } else {
		      if ((yyvsp[-3].intvalue)) {
			Swig_name_rename_add(Namespaceprefix,(fixname),decl,kws,(yyvsp[-2].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,(fixname),decl,kws);
		      }
		    }
		    Delete(decl);
		  } else if (SwigType_ispointer(t)) {
		    String *nname = NewStringf("*%s",fixname);
		    if ((yyvsp[-3].intvalue)) {
		      Swig_name_rename_add(Namespaceprefix,(nname),0,kws,(yyvsp[-2].decl).parms);
		    } else {
		      Swig_name_namewarn_add(Namespaceprefix,(nname),0,kws);
		    }
		    Delete(nname);
		  }
		} else {
		  if ((yyvsp[-3].intvalue)) {
		    Swig_name_rename_add(Namespaceprefix,(fixname),0,kws,(yyvsp[-2].decl).parms);
		  } else {
		    Swig_name_namewarn_add(Namespaceprefix,(fixname),0,kws);
		  }
		}
                (yyval.node) = 0;
		scanner_clear_rename();
              }
#line 6224 "y.tab.c"
    break;

  case 84: /* rename_directive: rename_namewarn LPAREN kwargs RPAREN declarator cpp_const SEMI  */
#line 2473 "parser.y"
                                                                               {
		String *fixname;
		Hash *kws = (yyvsp[-4].node);
		SwigType *t = (yyvsp[-2].decl).type;
		fixname = feature_identifier_fix((yyvsp[-2].decl).id);
		if (!Len(t)) t = 0;
		/* Special declarator check */
		if (t) {
		  if ((yyvsp[-1].dtype).qualifier) SwigType_push(t,(yyvsp[-1].dtype).qualifier);
		  if (SwigType_isfunction(t)) {
		    SwigType *decl = SwigType_pop_function(t);
		    if (SwigType_ispointer(t)) {
		      String *nname = NewStringf("*%s",fixname);
		      if ((yyvsp[-6].intvalue)) {
			Swig_name_rename_add(Namespaceprefix, nname,decl,kws,(yyvsp[-2].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,nname,decl,kws);
		      }
		      Delete(nname);
		    } else {
		      if ((yyvsp[-6].intvalue)) {
			Swig_name_rename_add(Namespaceprefix,(fixname),decl,kws,(yyvsp[-2].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,(fixname),decl,kws);
		      }
		    }
		    Delete(decl);
		  } else if (SwigType_ispointer(t)) {
		    String *nname = NewStringf("*%s",fixname);
		    if ((yyvsp[-6].intvalue)) {
		      Swig_name_rename_add(Namespaceprefix,(nname),0,kws,(yyvsp[-2].decl).parms);
		    } else {
		      Swig_name_namewarn_add(Namespaceprefix,(nname),0,kws);
		    }
		    Delete(nname);
		  }
		} else {
		  if ((yyvsp[-6].intvalue)) {
		    Swig_name_rename_add(Namespaceprefix,(fixname),0,kws,(yyvsp[-2].decl).parms);
		  } else {
		    Swig_name_namewarn_add(Namespaceprefix,(fixname),0,kws);
		  }
		}
                (yyval.node) = 0;
		scanner_clear_rename();
              }
#line 6275 "y.tab.c"
    break;

  case 85: /* rename_directive: rename_namewarn LPAREN kwargs RPAREN string SEMI  */
#line 2519 "parser.y"
                                                                 {
		if ((yyvsp[-5].intvalue)) {
		  Swig_name_rename_add(Namespaceprefix,(yyvsp[-1].str),0,(yyvsp[-3].node),0);
		} else {
		  Swig_name_namewarn_add(Namespaceprefix,(yyvsp[-1].str),0,(yyvsp[-3].node));
		}
		(yyval.node) = 0;
		scanner_clear_rename();
              }
#line 6289 "y.tab.c"
    break;

  case 86: /* rename_namewarn: RENAME  */
#line 2530 "parser.y"
                         {
		    (yyval.intvalue) = 1;
                }
#line 6297 "y.tab.c"
    break;

  case 87: /* rename_namewarn: NAMEWARN  */
#line 2533 "parser.y"
                           {
                    (yyval.intvalue) = 0;
                }
#line 6305 "y.tab.c"
    break;

  case 88: /* feature_directive: FEATURE LPAREN idstring RPAREN declarator cpp_const stringbracesemi  */
#line 2560 "parser.y"
                                                                                        {
                    String *val = (yyvsp[0].str) ? NewString((yyvsp[0].str)) : NewString("1");
                    new_feature((yyvsp[-4].id), val, 0, (yyvsp[-2].decl).id, (yyvsp[-2].decl).type, (yyvsp[-2].decl).parms, (yyvsp[-1].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 6316 "y.tab.c"
    break;

  case 89: /* feature_directive: FEATURE LPAREN idstring COMMA stringnum RPAREN declarator cpp_const SEMI  */
#line 2566 "parser.y"
                                                                                             {
                    String *val = Len((yyvsp[-4].str)) ? (yyvsp[-4].str) : 0;
                    new_feature((yyvsp[-6].id), val, 0, (yyvsp[-2].decl).id, (yyvsp[-2].decl).type, (yyvsp[-2].decl).parms, (yyvsp[-1].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 6327 "y.tab.c"
    break;

  case 90: /* feature_directive: FEATURE LPAREN idstring featattr RPAREN declarator cpp_const stringbracesemi  */
#line 2572 "parser.y"
                                                                                                 {
                    String *val = (yyvsp[0].str) ? NewString((yyvsp[0].str)) : NewString("1");
                    new_feature((yyvsp[-5].id), val, (yyvsp[-4].node), (yyvsp[-2].decl).id, (yyvsp[-2].decl).type, (yyvsp[-2].decl).parms, (yyvsp[-1].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 6338 "y.tab.c"
    break;

  case 91: /* feature_directive: FEATURE LPAREN idstring COMMA stringnum featattr RPAREN declarator cpp_const SEMI  */
#line 2578 "parser.y"
                                                                                                      {
                    String *val = Len((yyvsp[-5].str)) ? (yyvsp[-5].str) : 0;
                    new_feature((yyvsp[-7].id), val, (yyvsp[-4].node), (yyvsp[-2].decl).id, (yyvsp[-2].decl).type, (yyvsp[-2].decl).parms, (yyvsp[-1].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 6349 "y.tab.c"
    break;

  case 92: /* feature_directive: FEATURE LPAREN idstring RPAREN stringbracesemi  */
#line 2586 "parser.y"
                                                                   {
                    String *val = (yyvsp[0].str) ? NewString((yyvsp[0].str)) : NewString("1");
                    new_feature((yyvsp[-2].id), val, 0, 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 6360 "y.tab.c"
    break;

  case 93: /* feature_directive: FEATURE LPAREN idstring COMMA stringnum RPAREN SEMI  */
#line 2592 "parser.y"
                                                                        {
                    String *val = Len((yyvsp[-2].str)) ? (yyvsp[-2].str) : 0;
                    new_feature((yyvsp[-4].id), val, 0, 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 6371 "y.tab.c"
    break;

  case 94: /* feature_directive: FEATURE LPAREN idstring featattr RPAREN stringbracesemi  */
#line 2598 "parser.y"
                                                                            {
                    String *val = (yyvsp[0].str) ? NewString((yyvsp[0].str)) : NewString("1");
                    new_feature((yyvsp[-3].id), val, (yyvsp[-2].node), 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 6382 "y.tab.c"
    break;

  case 95: /* feature_directive: FEATURE LPAREN idstring COMMA stringnum featattr RPAREN SEMI  */
#line 2604 "parser.y"
                                                                                 {
                    String *val = Len((yyvsp[-3].str)) ? (yyvsp[-3].str) : 0;
                    new_feature((yyvsp[-5].id), val, (yyvsp[-2].node), 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 6393 "y.tab.c"
    break;

  case 96: /* stringbracesemi: stringbrace  */
#line 2612 "parser.y"
                              { (yyval.str) = (yyvsp[0].str); }
#line 6399 "y.tab.c"
    break;

  case 97: /* stringbracesemi: SEMI  */
#line 2613 "parser.y"
                       { (yyval.str) = 0; }
#line 6405 "y.tab.c"
    break;

  case 98: /* stringbracesemi: PARMS LPAREN parms RPAREN SEMI  */
#line 2614 "parser.y"
                                                 { (yyval.str) = (yyvsp[-2].pl); }
#line 6411 "y.tab.c"
    break;

  case 99: /* featattr: COMMA idstring EQUAL stringnum  */
#line 2617 "parser.y"
                                                 {
		  (yyval.node) = NewHash();
		  Setattr((yyval.node),"name",(yyvsp[-2].id));
		  Setattr((yyval.node),"value",(yyvsp[0].str));
                }
#line 6421 "y.tab.c"
    break;

  case 100: /* featattr: COMMA idstring EQUAL stringnum featattr  */
#line 2622 "parser.y"
                                                          {
		  (yyval.node) = NewHash();
		  Setattr((yyval.node),"name",(yyvsp[-3].id));
		  Setattr((yyval.node),"value",(yyvsp[-1].str));
                  set_nextSibling((yyval.node),(yyvsp[0].node));
                }
#line 6432 "y.tab.c"
    break;

  case 101: /* varargs_directive: VARARGS LPAREN varargs_parms RPAREN declarator cpp_const SEMI  */
#line 2632 "parser.y"
                                                                                  {
                 Parm *val;
		 String *name;
		 SwigType *t;
		 if (Namespaceprefix) name = NewStringf("%s::%s", Namespaceprefix, (yyvsp[-2].decl).id);
		 else name = NewString((yyvsp[-2].decl).id);
		 val = (yyvsp[-4].pl);
		 if ((yyvsp[-2].decl).parms) {
		   Setmeta(val,"parms",(yyvsp[-2].decl).parms);
		 }
		 t = (yyvsp[-2].decl).type;
		 if (!Len(t)) t = 0;
		 if (t) {
		   if ((yyvsp[-1].dtype).qualifier) SwigType_push(t,(yyvsp[-1].dtype).qualifier);
		   if (SwigType_isfunction(t)) {
		     SwigType *decl = SwigType_pop_function(t);
		     if (SwigType_ispointer(t)) {
		       String *nname = NewStringf("*%s",name);
		       Swig_feature_set(Swig_cparse_features(), nname, decl, "feature:varargs", val, 0);
		       Delete(nname);
		     } else {
		       Swig_feature_set(Swig_cparse_features(), name, decl, "feature:varargs", val, 0);
		     }
		     Delete(decl);
		   } else if (SwigType_ispointer(t)) {
		     String *nname = NewStringf("*%s",name);
		     Swig_feature_set(Swig_cparse_features(),nname,0,"feature:varargs",val, 0);
		     Delete(nname);
		   }
		 } else {
		   Swig_feature_set(Swig_cparse_features(),name,0,"feature:varargs",val, 0);
		 }
		 Delete(name);
		 (yyval.node) = 0;
              }
#line 6472 "y.tab.c"
    break;

  case 102: /* varargs_parms: parms  */
#line 2668 "parser.y"
                        { (yyval.pl) = (yyvsp[0].pl); }
#line 6478 "y.tab.c"
    break;

  case 103: /* varargs_parms: NUM_INT COMMA parm  */
#line 2669 "parser.y"
                                     { 
		  int i;
		  int n;
		  Parm *p;
		  n = atoi(Char((yyvsp[-2].dtype).val));
		  if (n <= 0) {
		    Swig_error(cparse_file, cparse_line,"Argument count in %%varargs must be positive.\n");
		    (yyval.pl) = 0;
		  } else {
		    String *name = Getattr((yyvsp[0].p), "name");
		    (yyval.pl) = Copy((yyvsp[0].p));
		    if (name)
		      Setattr((yyval.pl), "name", NewStringf("%s%d", name, n));
		    for (i = 1; i < n; i++) {
		      p = Copy((yyvsp[0].p));
		      name = Getattr(p, "name");
		      if (name)
		        Setattr(p, "name", NewStringf("%s%d", name, n-i));
		      set_nextSibling(p,(yyval.pl));
		      Delete((yyval.pl));
		      (yyval.pl) = p;
		    }
		  }
                }
#line 6507 "y.tab.c"
    break;

  case 104: /* typemap_directive: TYPEMAP LPAREN typemap_type RPAREN tm_list stringbrace  */
#line 2704 "parser.y"
                                                                            {
		   (yyval.node) = 0;
		   if ((yyvsp[-3].tmap).method) {
		     String *code = 0;
		     (yyval.node) = new_node("typemap");
		     Setattr((yyval.node),"method",(yyvsp[-3].tmap).method);
		     if ((yyvsp[-3].tmap).kwargs) {
		       ParmList *kw = (yyvsp[-3].tmap).kwargs;
                       code = remove_block(kw, (yyvsp[0].str));
		       Setattr((yyval.node),"kwargs", (yyvsp[-3].tmap).kwargs);
		     }
		     code = code ? code : NewString((yyvsp[0].str));
		     Setattr((yyval.node),"code", code);
		     Delete(code);
		     appendChild((yyval.node),(yyvsp[-1].p));
		   }
	       }
#line 6529 "y.tab.c"
    break;

  case 105: /* typemap_directive: TYPEMAP LPAREN typemap_type RPAREN tm_list SEMI  */
#line 2721 "parser.y"
                                                                 {
		 (yyval.node) = 0;
		 if ((yyvsp[-3].tmap).method) {
		   (yyval.node) = new_node("typemap");
		   Setattr((yyval.node),"method",(yyvsp[-3].tmap).method);
		   appendChild((yyval.node),(yyvsp[-1].p));
		 }
	       }
#line 6542 "y.tab.c"
    break;

  case 106: /* typemap_directive: TYPEMAP LPAREN typemap_type RPAREN tm_list EQUAL typemap_parm SEMI  */
#line 2729 "parser.y"
                                                                                    {
		   (yyval.node) = 0;
		   if ((yyvsp[-5].tmap).method) {
		     (yyval.node) = new_node("typemapcopy");
		     Setattr((yyval.node),"method",(yyvsp[-5].tmap).method);
		     Setattr((yyval.node),"pattern", Getattr((yyvsp[-1].p),"pattern"));
		     appendChild((yyval.node),(yyvsp[-3].p));
		   }
	       }
#line 6556 "y.tab.c"
    break;

  case 107: /* typemap_type: kwargs  */
#line 2742 "parser.y"
                        {
		 String *name = Getattr((yyvsp[0].node), "name");
		 Hash *p = nextSibling((yyvsp[0].node));
		 (yyval.tmap).method = name;
		 (yyval.tmap).kwargs = p;
		 if (Getattr((yyvsp[0].node), "value")) {
		   Swig_error(cparse_file, cparse_line,
			      "%%typemap method shouldn't have a value specified.\n");
		 }
		 while (p) {
		   if (!Getattr(p, "value")) {
		     Swig_error(cparse_file, cparse_line,
				"%%typemap attribute '%s' is missing its value.  If this is specifying the target language, that's no longer supported: use #ifdef SWIG<LANG> instead.\n",
				Getattr(p, "name"));
		     /* Set to empty value to avoid segfaults later. */
		     Setattr(p, "value", NewStringEmpty());
		   }
		   p = nextSibling(p);
		 }
                }
#line 6581 "y.tab.c"
    break;

  case 108: /* tm_list: typemap_parm tm_tail  */
#line 2764 "parser.y"
                                      {
                 (yyval.p) = (yyvsp[-1].p);
		 set_nextSibling((yyval.p),(yyvsp[0].p));
		}
#line 6590 "y.tab.c"
    break;

  case 109: /* tm_tail: COMMA typemap_parm tm_tail  */
#line 2770 "parser.y"
                                            {
                 (yyval.p) = (yyvsp[-1].p);
		 set_nextSibling((yyval.p),(yyvsp[0].p));
                }
#line 6599 "y.tab.c"
    break;

  case 110: /* tm_tail: empty  */
#line 2774 "parser.y"
                       { (yyval.p) = 0;}
#line 6605 "y.tab.c"
    break;

  case 111: /* typemap_parm: type plain_declarator  */
#line 2777 "parser.y"
                                       {
                  Parm *parm;
		  SwigType_push((yyvsp[-1].type),(yyvsp[0].decl).type);
		  (yyval.p) = new_node("typemapitem");
		  parm = NewParmWithoutFileLineInfo((yyvsp[-1].type),(yyvsp[0].decl).id);
		  Setattr((yyval.p),"pattern",parm);
		  Setattr((yyval.p),"parms", (yyvsp[0].decl).parms);
		  Delete(parm);
		  /*		  $$ = NewParmWithoutFileLineInfo($1,$2.id);
				  Setattr($$,"parms",$2.parms); */
                }
#line 6621 "y.tab.c"
    break;

  case 112: /* typemap_parm: LPAREN parms RPAREN  */
#line 2788 "parser.y"
                                     {
                  (yyval.p) = new_node("typemapitem");
		  Setattr((yyval.p),"pattern",(yyvsp[-1].pl));
		  /*		  Setattr($$,"multitype",$2); */
               }
#line 6631 "y.tab.c"
    break;

  case 113: /* typemap_parm: LPAREN parms RPAREN LPAREN parms RPAREN  */
#line 2793 "parser.y"
                                                         {
		 (yyval.p) = new_node("typemapitem");
		 Setattr((yyval.p),"pattern", (yyvsp[-4].pl));
		 /*                 Setattr($$,"multitype",$2); */
		 Setattr((yyval.p),"parms",(yyvsp[-1].pl));
               }
#line 6642 "y.tab.c"
    break;

  case 114: /* types_directive: TYPES LPAREN parms RPAREN stringbracesemi  */
#line 2806 "parser.y"
                                                            {
                   (yyval.node) = new_node("types");
		   Setattr((yyval.node),"parms",(yyvsp[-2].pl));
                   if ((yyvsp[0].str))
		     Setattr((yyval.node),"convcode",NewString((yyvsp[0].str)));
               }
#line 6653 "y.tab.c"
    break;

  case 115: /* template_directive: SWIGTEMPLATE LPAREN idstringopt RPAREN idcolonnt LESSTHAN valparms GREATERTHAN SEMI  */
#line 2818 "parser.y"
                                                                                                        {
                  Parm *p, *tp;
		  Node *n;
		  Node *outer_class = currentOuterClass;
		  Symtab *tscope = 0;
		  int     specialized = 0;
		  int     variadic = 0;

		  (yyval.node) = 0;

		  tscope = Swig_symbol_current();          /* Get the current scope */

		  /* If the class name is qualified, we need to create or lookup namespace entries */
		  (yyvsp[-4].str) = resolve_create_node_scope((yyvsp[-4].str), 0);

		  if (nscope_inner && Strcmp(nodeType(nscope_inner), "class") == 0) {
		    outer_class	= nscope_inner;
		  }

		  /*
		    We use the new namespace entry 'nscope' only to
		    emit the template node. The template parameters are
		    resolved in the current 'tscope'.

		    This is closer to the C++ (typedef) behavior.
		  */
		  n = Swig_cparse_template_locate((yyvsp[-4].str),(yyvsp[-2].p),tscope);

		  /* Patch the argument types to respect namespaces */
		  p = (yyvsp[-2].p);
		  while (p) {
		    SwigType *value = Getattr(p,"value");
		    if (!value) {
		      SwigType *ty = Getattr(p,"type");
		      if (ty) {
			SwigType *rty = 0;
			int reduce = template_reduce;
			if (reduce || !SwigType_ispointer(ty)) {
			  rty = Swig_symbol_typedef_reduce(ty,tscope);
			  if (!reduce) reduce = SwigType_ispointer(rty);
			}
			ty = reduce ? Swig_symbol_type_qualify(rty,tscope) : Swig_symbol_type_qualify(ty,tscope);
			Setattr(p,"type",ty);
			Delete(ty);
			Delete(rty);
		      }
		    } else {
		      value = Swig_symbol_type_qualify(value,tscope);
		      Setattr(p,"value",value);
		      Delete(value);
		    }

		    p = nextSibling(p);
		  }

		  /* Look for the template */
		  {
                    Node *nn = n;
                    Node *linklistend = 0;
                    Node *linkliststart = 0;
                    while (nn) {
                      Node *templnode = 0;
                      if (Strcmp(nodeType(nn),"template") == 0) {
                        int nnisclass = (Strcmp(Getattr(nn,"templatetype"),"class") == 0); /* if not a templated class it is a templated function */
                        Parm *tparms = Getattr(nn,"templateparms");
                        if (!tparms) {
                          specialized = 1;
                        } else if (Getattr(tparms,"variadic") && strncmp(Char(Getattr(tparms,"variadic")), "1", 1)==0) {
                          variadic = 1;
                        }
                        if (nnisclass && !variadic && !specialized && (ParmList_len((yyvsp[-2].p)) > ParmList_len(tparms))) {
                          Swig_error(cparse_file, cparse_line, "Too many template parameters. Maximum of %d.\n", ParmList_len(tparms));
                        } else if (nnisclass && !specialized && ((ParmList_len((yyvsp[-2].p)) < (ParmList_numrequired(tparms) - (variadic?1:0))))) { /* Variadic parameter is optional */
                          Swig_error(cparse_file, cparse_line, "Not enough template parameters specified. %d required.\n", (ParmList_numrequired(tparms)-(variadic?1:0)) );
                        } else if (!nnisclass && ((ParmList_len((yyvsp[-2].p)) != ParmList_len(tparms)))) {
                          /* must be an overloaded templated method - ignore it as it is overloaded with a different number of template parameters */
                          nn = Getattr(nn,"sym:nextSibling"); /* repeat for overloaded templated functions */
                          continue;
                        } else {
			  String *tname = Copy((yyvsp[-4].str));
                          int def_supplied = 0;
                          /* Expand the template */
			  Node *templ = Swig_symbol_clookup((yyvsp[-4].str),0);
			  Parm *targs = templ ? Getattr(templ,"templateparms") : 0;

                          ParmList *temparms;
                          if (specialized) temparms = CopyParmList((yyvsp[-2].p));
                          else temparms = CopyParmList(tparms);

                          /* Create typedef's and arguments */
                          p = (yyvsp[-2].p);
                          tp = temparms;
                          if (!p && ParmList_len(p) != ParmList_len(temparms)) {
                            /* we have no template parameters supplied in %template for a template that has default args*/
                            p = tp;
                            def_supplied = 1;
                          }

                          while (p) {
                            String *value = Getattr(p,"value");
                            if (def_supplied) {
                              Setattr(p,"default","1");
                            }
                            if (value) {
                              Setattr(tp,"value",value);
                            } else {
                              SwigType *ty = Getattr(p,"type");
                              if (ty) {
                                Setattr(tp,"type",ty);
                              }
                              Delattr(tp,"value");
                            }
			    /* fix default arg values */
			    if (targs) {
			      Parm *pi = temparms;
			      Parm *ti = targs;
			      String *tv = Getattr(tp,"value");
			      if (!tv) tv = Getattr(tp,"type");
			      while(pi != tp && ti && pi) {
				String *name = Getattr(ti,"name");
				String *value = Getattr(pi,"value");
				if (!value) value = Getattr(pi,"type");
				Replaceid(tv, name, value);
				pi = nextSibling(pi);
				ti = nextSibling(ti);
			      }
			    }
                            p = nextSibling(p);
                            tp = nextSibling(tp);
                            if (!p && tp) {
                              p = tp;
                              def_supplied = 1;
                            } else if (p && !tp) { /* Variadic template - tp < p */
			      SWIG_WARN_NODE_BEGIN(nn);
                              Swig_warning(WARN_CPP11_VARIADIC_TEMPLATE,cparse_file, cparse_line,"Only the first variadic template argument is currently supported.\n");
			      SWIG_WARN_NODE_END(nn);
                              break;
                            }
                          }

                          templnode = copy_node(nn);
			  update_nested_classes(templnode); /* update classes nested within template */
                          /* We need to set the node name based on name used to instantiate */
                          Setattr(templnode,"name",tname);
			  Delete(tname);
                          if (!specialized) {
                            Delattr(templnode,"sym:typename");
                          } else {
                            Setattr(templnode,"sym:typename","1");
                          }
			  /* for now, nested %template is allowed only in the same scope as the template declaration */
                          if ((yyvsp[-6].id) && !(nnisclass && ((outer_class && (outer_class != Getattr(nn, "nested:outer")))
			    ||(extendmode && current_class && (current_class != Getattr(nn, "nested:outer")))))) {
			    /*
			       Comment this out for 1.3.28. We need to
			       re-enable it later but first we need to
			       move %ignore from using %rename to use
			       %feature(ignore).

			       String *symname = Swig_name_make(templnode,0,$3,0,0);
			    */
			    String *symname = NewString((yyvsp[-6].id));
                            Swig_cparse_template_expand(templnode,symname,temparms,tscope);
                            Setattr(templnode,"sym:name",symname);
                          } else {
                            static int cnt = 0;
                            String *nname = NewStringf("__dummy_%d__", cnt++);
                            Swig_cparse_template_expand(templnode,nname,temparms,tscope);
                            Setattr(templnode,"sym:name",nname);
                            SetFlag(templnode,"hidden");
			    Delete(nname);
                            Setattr(templnode,"feature:onlychildren", "typemap,typemapitem,typemapcopy,typedef,types,fragment,apply");
			    if ((yyvsp[-6].id)) {
			      Swig_warning(WARN_PARSE_NESTED_TEMPLATE, cparse_file, cparse_line, "Named nested template instantiations not supported. Processing as if no name was given to %%template().\n");
			    }
                          }
                          Delattr(templnode,"templatetype");
                          Setattr(templnode,"template",nn);
                          Setfile(templnode,cparse_file);
                          Setline(templnode,cparse_line);
                          Delete(temparms);
			  if (outer_class && nnisclass) {
			    SetFlag(templnode, "nested");
			    Setattr(templnode, "nested:outer", outer_class);
			  }
                          add_symbols_copy(templnode);

                          if (Strcmp(nodeType(templnode),"class") == 0) {

                            /* Identify pure abstract methods */
                            Setattr(templnode,"abstracts", pure_abstracts(firstChild(templnode)));

                            /* Set up inheritance in symbol table */
                            {
                              Symtab  *csyms;
                              List *baselist = Getattr(templnode,"baselist");
                              csyms = Swig_symbol_current();
                              Swig_symbol_setscope(Getattr(templnode,"symtab"));
                              if (baselist) {
                                List *bases = Swig_make_inherit_list(Getattr(templnode,"name"),baselist, Namespaceprefix);
                                if (bases) {
                                  Iterator s;
                                  for (s = First(bases); s.item; s = Next(s)) {
                                    Symtab *st = Getattr(s.item,"symtab");
                                    if (st) {
				      Setfile(st,Getfile(s.item));
				      Setline(st,Getline(s.item));
                                      Swig_symbol_inherit(st);
                                    }
                                  }
				  Delete(bases);
                                }
                              }
                              Swig_symbol_setscope(csyms);
                            }

                            /* Merge in %extend methods for this class.
			       This only merges methods within %extend for a template specialized class such as
			         template<typename T> class K {}; %extend K<int> { ... }
			       The copy_node() call above has already added in the generic %extend methods such as
			         template<typename T> class K {}; %extend K { ... } */

			    /* !!! This may be broken.  We may have to add the
			       %extend methods at the beginning of the class */
                            {
                              String *stmp = 0;
                              String *clsname;
                              Node *am;
                              if (Namespaceprefix) {
                                clsname = stmp = NewStringf("%s::%s", Namespaceprefix, Getattr(templnode,"name"));
                              } else {
                                clsname = Getattr(templnode,"name");
                              }
                              am = Getattr(Swig_extend_hash(),clsname);
                              if (am) {
                                Symtab *st = Swig_symbol_current();
                                Swig_symbol_setscope(Getattr(templnode,"symtab"));
                                /*			    Printf(stdout,"%s: %s %p %p\n", Getattr(templnode,"name"), clsname, Swig_symbol_current(), Getattr(templnode,"symtab")); */
                                Swig_extend_merge(templnode,am);
                                Swig_symbol_setscope(st);
				Swig_extend_append_previous(templnode,am);
                                Delattr(Swig_extend_hash(),clsname);
                              }
			      if (stmp) Delete(stmp);
                            }

                            /* Add to classes hash */
			    if (!classes)
			      classes = NewHash();

			    if (Namespaceprefix) {
			      String *temp = NewStringf("%s::%s", Namespaceprefix, Getattr(templnode,"name"));
			      Setattr(classes,temp,templnode);
			      Delete(temp);
			    } else {
			      String *qs = Swig_symbol_qualifiedscopename(templnode);
			      Setattr(classes, qs,templnode);
			      Delete(qs);
			    }
                          }
                        }

                        /* all the overloaded templated functions are added into a linked list */
                        if (!linkliststart)
                          linkliststart = templnode;
                        if (nscope_inner) {
                          /* non-global namespace */
                          if (templnode) {
                            appendChild(nscope_inner,templnode);
			    Delete(templnode);
                            if (nscope) (yyval.node) = nscope;
                          }
                        } else {
                          /* global namespace */
                          if (!linklistend) {
                            (yyval.node) = templnode;
                          } else {
                            set_nextSibling(linklistend,templnode);
			    Delete(templnode);
                          }
                          linklistend = templnode;
                        }
                      }
                      nn = Getattr(nn,"sym:nextSibling"); /* repeat for overloaded templated functions. If a templated class there will never be a sibling. */
                    }
                    update_defaultargs(linkliststart);
                    update_abstracts(linkliststart);
		  }
	          Swig_symbol_setscope(tscope);
		  Delete(Namespaceprefix);
		  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
                }
#line 6950 "y.tab.c"
    break;

  case 116: /* warn_directive: WARN string  */
#line 3117 "parser.y"
                             {
		  Swig_warning(0,cparse_file, cparse_line,"%s\n", (yyvsp[0].str));
		  (yyval.node) = 0;
               }
#line 6959 "y.tab.c"
    break;

  case 117: /* c_declaration: c_decl  */
#line 3127 "parser.y"
                         {
                    (yyval.node) = (yyvsp[0].node); 
                    if ((yyval.node)) {
   		      add_symbols((yyval.node));
                      default_arguments((yyval.node));
   	            }
                }
#line 6971 "y.tab.c"
    break;

  case 118: /* c_declaration: c_enum_decl  */
#line 3134 "parser.y"
                              { (yyval.node) = (yyvsp[0].node); }
#line 6977 "y.tab.c"
    break;

  case 119: /* c_declaration: c_enum_forward_decl  */
#line 3135 "parser.y"
                                      { (yyval.node) = (yyvsp[0].node); }
#line 6983 "y.tab.c"
    break;

  case 120: /* $@3: %empty  */
#line 3139 "parser.y"
                                       {
		  if (Strcmp((yyvsp[-1].str),"C") == 0) {
		    cparse_externc = 1;
		  }
		}
#line 6993 "y.tab.c"
    break;

  case 121: /* c_declaration: EXTERN string LBRACE $@3 interface RBRACE  */
#line 3143 "parser.y"
                                   {
		  cparse_externc = 0;
		  if (Strcmp((yyvsp[-4].str),"C") == 0) {
		    Node *n = firstChild((yyvsp[-1].node));
		    (yyval.node) = new_node("extern");
		    Setattr((yyval.node),"name",(yyvsp[-4].str));
		    appendChild((yyval.node),n);
		    while (n) {
		      String *s = Getattr(n, "storage");
		      if (s) {
			if (Strstr(s, "thread_local")) {
			  Insert(s,0,"externc ");
			} else if (!Equal(s, "typedef")) {
			  Setattr(n,"storage","externc");
			}
		      } else {
			Setattr(n,"storage","externc");
		      }
		      n = nextSibling(n);
		    }
		  } else {
		    if (!Equal((yyvsp[-4].str),"C++")) {
		      Swig_warning(WARN_PARSE_UNDEFINED_EXTERN,cparse_file, cparse_line,"Unrecognized extern type \"%s\".\n", (yyvsp[-4].str));
		    }
		    (yyval.node) = new_node("extern");
		    Setattr((yyval.node),"name",(yyvsp[-4].str));
		    appendChild((yyval.node),firstChild((yyvsp[-1].node)));
		  }
                }
#line 7027 "y.tab.c"
    break;

  case 122: /* c_declaration: cpp_lambda_decl  */
#line 3172 "parser.y"
                                  {
		  (yyval.node) = (yyvsp[0].node);
		  SWIG_WARN_NODE_BEGIN((yyval.node));
		  Swig_warning(WARN_CPP11_LAMBDA, cparse_file, cparse_line, "Lambda expressions and closures are not fully supported yet.\n");
		  SWIG_WARN_NODE_END((yyval.node));
		}
#line 7038 "y.tab.c"
    break;

  case 123: /* c_declaration: USING idcolon EQUAL type plain_declarator SEMI  */
#line 3178 "parser.y"
                                                                 {
		  /* Convert using statement to a typedef statement */
		  (yyval.node) = new_node("cdecl");
		  Setattr((yyval.node),"type",(yyvsp[-2].type));
		  Setattr((yyval.node),"storage","typedef");
		  Setattr((yyval.node),"name",(yyvsp[-4].str));
		  Setattr((yyval.node),"decl",(yyvsp[-1].decl).type);
		  SetFlag((yyval.node),"typealias");
		  add_symbols((yyval.node));
		}
#line 7053 "y.tab.c"
    break;

  case 124: /* c_declaration: TEMPLATE LESSTHAN template_parms GREATERTHAN USING idcolon EQUAL type plain_declarator SEMI  */
#line 3188 "parser.y"
                                                                                                              {
		  /* Convert alias template to a "template" typedef statement */
		  (yyval.node) = new_node("template");
		  Setattr((yyval.node),"type",(yyvsp[-2].type));
		  Setattr((yyval.node),"storage","typedef");
		  Setattr((yyval.node),"name",(yyvsp[-4].str));
		  Setattr((yyval.node),"decl",(yyvsp[-1].decl).type);
		  Setattr((yyval.node),"templateparms",(yyvsp[-7].tparms));
		  Setattr((yyval.node),"templatetype","cdecl");
		  SetFlag((yyval.node),"aliastemplate");
		  add_symbols((yyval.node));
		}
#line 7070 "y.tab.c"
    break;

  case 125: /* c_declaration: cpp_static_assert  */
#line 3200 "parser.y"
                                    {
                   (yyval.node) = (yyvsp[0].node);
                }
#line 7078 "y.tab.c"
    break;

  case 126: /* c_decl: storage_class type declarator cpp_const initializer c_decl_tail  */
#line 3209 "parser.y"
                                                                          {
	      String *decl = (yyvsp[-3].decl).type;
              (yyval.node) = new_node("cdecl");
	      if ((yyvsp[-2].dtype).qualifier)
	        decl = add_qualifier_to_declarator((yyvsp[-3].decl).type, (yyvsp[-2].dtype).qualifier);
	      Setattr((yyval.node),"refqualifier",(yyvsp[-2].dtype).refqualifier);
	      Setattr((yyval.node),"type",(yyvsp[-4].type));
	      Setattr((yyval.node),"storage",(yyvsp[-5].id));
	      Setattr((yyval.node),"name",(yyvsp[-3].decl).id);
	      Setattr((yyval.node),"decl",decl);
	      Setattr((yyval.node),"parms",(yyvsp[-3].decl).parms);
	      Setattr((yyval.node),"value",(yyvsp[-1].dtype).val);
	      Setattr((yyval.node),"throws",(yyvsp[-2].dtype).throws);
	      Setattr((yyval.node),"throw",(yyvsp[-2].dtype).throwf);
	      Setattr((yyval.node),"noexcept",(yyvsp[-2].dtype).nexcept);
	      Setattr((yyval.node),"final",(yyvsp[-2].dtype).final);
	      if ((yyvsp[-1].dtype).val && (yyvsp[-1].dtype).type) {
		/* store initializer type as it might be different to the declared type */
		SwigType *valuetype = NewSwigType((yyvsp[-1].dtype).type);
		if (Len(valuetype) > 0)
		  Setattr((yyval.node),"valuetype",valuetype);
		else
		  Delete(valuetype);
	      }
	      if (!(yyvsp[0].node)) {
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
	      } else {
		Node *n = (yyvsp[0].node);
		/* Inherit attributes */
		while (n) {
		  String *type = Copy((yyvsp[-4].type));
		  Setattr(n,"type",type);
		  Setattr(n,"storage",(yyvsp[-5].id));
		  n = nextSibling(n);
		  Delete(type);
		}
	      }
	      if ((yyvsp[-1].dtype).bitfield) {
		Setattr((yyval.node),"bitfield", (yyvsp[-1].dtype).bitfield);
	      }

	      if ((yyvsp[-3].decl).id) {
		/* Look for "::" declarations (ignored) */
		if (Strstr((yyvsp[-3].decl).id, "::")) {
		  /* This is a special case. If the scope name of the declaration exactly
		     matches that of the declaration, then we will allow it. Otherwise, delete. */
		  String *p = Swig_scopename_prefix((yyvsp[-3].decl).id);
		  if (p) {
		    if ((Namespaceprefix && Strcmp(p, Namespaceprefix) == 0) ||
			(Classprefix && Strcmp(p, Classprefix) == 0)) {
		      String *lstr = Swig_scopename_last((yyvsp[-3].decl).id);
		      Setattr((yyval.node), "name", lstr);
		      Delete(lstr);
		      set_nextSibling((yyval.node), (yyvsp[0].node));
		    } else {
		      Delete((yyval.node));
		      (yyval.node) = (yyvsp[0].node);
		    }
		    Delete(p);
		  } else {
		    Delete((yyval.node));
		    (yyval.node) = (yyvsp[0].node);
		  }
		} else {
		  set_nextSibling((yyval.node), (yyvsp[0].node));
		}
	      } else {
		Swig_error(cparse_file, cparse_line, "Missing symbol name for global declaration\n");
		(yyval.node) = 0;
	      }

	      if ((yyvsp[-2].dtype).qualifier && (yyvsp[-5].id) && Strstr((yyvsp[-5].id), "static"))
		Swig_error(cparse_file, cparse_line, "Static function %s cannot have a qualifier.\n", Swig_name_decl((yyval.node)));
           }
#line 7161 "y.tab.c"
    break;

  case 127: /* c_decl: storage_class AUTO declarator cpp_const ARROW cpp_alternate_rettype virt_specifier_seq_opt initializer c_decl_tail  */
#line 3289 "parser.y"
                                                                                                                                {
              (yyval.node) = new_node("cdecl");
	      if ((yyvsp[-5].dtype).qualifier) SwigType_push((yyvsp[-6].decl).type, (yyvsp[-5].dtype).qualifier);
	      Setattr((yyval.node),"refqualifier",(yyvsp[-5].dtype).refqualifier);
	      Setattr((yyval.node),"type",(yyvsp[-3].node));
	      Setattr((yyval.node),"storage",(yyvsp[-8].id));
	      Setattr((yyval.node),"name",(yyvsp[-6].decl).id);
	      Setattr((yyval.node),"decl",(yyvsp[-6].decl).type);
	      Setattr((yyval.node),"parms",(yyvsp[-6].decl).parms);
	      Setattr((yyval.node),"value",(yyvsp[-5].dtype).val);
	      Setattr((yyval.node),"throws",(yyvsp[-5].dtype).throws);
	      Setattr((yyval.node),"throw",(yyvsp[-5].dtype).throwf);
	      Setattr((yyval.node),"noexcept",(yyvsp[-5].dtype).nexcept);
	      Setattr((yyval.node),"final",(yyvsp[-5].dtype).final);
	      if (!(yyvsp[0].node)) {
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
	      } else {
		Node *n = (yyvsp[0].node);
		while (n) {
		  String *type = Copy((yyvsp[-3].node));
		  Setattr(n,"type",type);
		  Setattr(n,"storage",(yyvsp[-8].id));
		  n = nextSibling(n);
		  Delete(type);
		}
	      }
	      if ((yyvsp[-5].dtype).bitfield) {
		Setattr((yyval.node),"bitfield", (yyvsp[-5].dtype).bitfield);
	      }

	      if (Strstr((yyvsp[-6].decl).id,"::")) {
                String *p = Swig_scopename_prefix((yyvsp[-6].decl).id);
		if (p) {
		  if ((Namespaceprefix && Strcmp(p, Namespaceprefix) == 0) ||
		      (Classprefix && Strcmp(p, Classprefix) == 0)) {
		    String *lstr = Swig_scopename_last((yyvsp[-6].decl).id);
		    Setattr((yyval.node),"name",lstr);
		    Delete(lstr);
		    set_nextSibling((yyval.node), (yyvsp[0].node));
		  } else {
		    Delete((yyval.node));
		    (yyval.node) = (yyvsp[0].node);
		  }
		  Delete(p);
		} else {
		  Delete((yyval.node));
		  (yyval.node) = (yyvsp[0].node);
		}
	      } else {
		set_nextSibling((yyval.node), (yyvsp[0].node));
	      }

	      if ((yyvsp[-5].dtype).qualifier && (yyvsp[-8].id) && Strstr((yyvsp[-8].id), "static"))
		Swig_error(cparse_file, cparse_line, "Static function %s cannot have a qualifier.\n", Swig_name_decl((yyval.node)));
           }
#line 7225 "y.tab.c"
    break;

  case 128: /* c_decl_tail: SEMI  */
#line 3352 "parser.y"
                      { 
                   (yyval.node) = 0;
                   Clear(scanner_ccode); 
               }
#line 7234 "y.tab.c"
    break;

  case 129: /* c_decl_tail: COMMA declarator cpp_const initializer c_decl_tail  */
#line 3356 "parser.y"
                                                                    {
		 (yyval.node) = new_node("cdecl");
		 if ((yyvsp[-2].dtype).qualifier) SwigType_push((yyvsp[-3].decl).type,(yyvsp[-2].dtype).qualifier);
		 Setattr((yyval.node),"refqualifier",(yyvsp[-2].dtype).refqualifier);
		 Setattr((yyval.node),"name",(yyvsp[-3].decl).id);
		 Setattr((yyval.node),"decl",(yyvsp[-3].decl).type);
		 Setattr((yyval.node),"parms",(yyvsp[-3].decl).parms);
		 Setattr((yyval.node),"value",(yyvsp[-1].dtype).val);
		 Setattr((yyval.node),"throws",(yyvsp[-2].dtype).throws);
		 Setattr((yyval.node),"throw",(yyvsp[-2].dtype).throwf);
		 Setattr((yyval.node),"noexcept",(yyvsp[-2].dtype).nexcept);
		 Setattr((yyval.node),"final",(yyvsp[-2].dtype).final);
		 if ((yyvsp[-1].dtype).bitfield) {
		   Setattr((yyval.node),"bitfield", (yyvsp[-1].dtype).bitfield);
		 }
		 if (!(yyvsp[0].node)) {
		   if (Len(scanner_ccode)) {
		     String *code = Copy(scanner_ccode);
		     Setattr((yyval.node),"code",code);
		     Delete(code);
		   }
		 } else {
		   set_nextSibling((yyval.node), (yyvsp[0].node));
		 }
	       }
#line 7264 "y.tab.c"
    break;

  case 130: /* c_decl_tail: LBRACE  */
#line 3381 "parser.y"
                        { 
                   skip_balanced('{','}');
                   (yyval.node) = 0;
               }
#line 7273 "y.tab.c"
    break;

  case 131: /* c_decl_tail: error  */
#line 3385 "parser.y"
                       {
		   (yyval.node) = 0;
		   if (yychar == RPAREN) {
		       Swig_error(cparse_file, cparse_line, "Unexpected closing parenthesis (')').\n");
		   } else {
		       Swig_error(cparse_file, cparse_line, "Syntax error - possibly a missing semicolon (';').\n");
		   }
		   Exit(EXIT_FAILURE);
               }
#line 7287 "y.tab.c"
    break;

  case 132: /* initializer: def_args  */
#line 3396 "parser.y"
                         {
                   (yyval.dtype) = (yyvsp[0].dtype);
              }
#line 7295 "y.tab.c"
    break;

  case 133: /* cpp_alternate_rettype: primitive_type  */
#line 3401 "parser.y"
                                       { (yyval.node) = (yyvsp[0].type); }
#line 7301 "y.tab.c"
    break;

  case 134: /* cpp_alternate_rettype: TYPE_BOOL  */
#line 3402 "parser.y"
                          { (yyval.node) = (yyvsp[0].type); }
#line 7307 "y.tab.c"
    break;

  case 135: /* cpp_alternate_rettype: TYPE_VOID  */
#line 3403 "parser.y"
                          { (yyval.node) = (yyvsp[0].type); }
#line 7313 "y.tab.c"
    break;

  case 136: /* cpp_alternate_rettype: TYPE_RAW  */
#line 3407 "parser.y"
                         { (yyval.node) = (yyvsp[0].type); }
#line 7319 "y.tab.c"
    break;

  case 137: /* cpp_alternate_rettype: idcolon  */
#line 3408 "parser.y"
                        { (yyval.node) = (yyvsp[0].str); }
#line 7325 "y.tab.c"
    break;

  case 138: /* cpp_alternate_rettype: idcolon AND  */
#line 3409 "parser.y"
                            {
                (yyval.node) = (yyvsp[-1].str);
                SwigType_add_reference((yyval.node));
              }
#line 7334 "y.tab.c"
    break;

  case 139: /* cpp_alternate_rettype: decltype  */
#line 3413 "parser.y"
                         { (yyval.node) = (yyvsp[0].type); }
#line 7340 "y.tab.c"
    break;

  case 140: /* cpp_lambda_decl: storage_class AUTO idcolon EQUAL lambda_introducer lambda_template LPAREN parms RPAREN cpp_const lambda_body lambda_tail  */
#line 3424 "parser.y"
                                                                                                                                           {
		  (yyval.node) = new_node("lambda");
		  Setattr((yyval.node),"name",(yyvsp[-9].str));
		  add_symbols((yyval.node));
	        }
#line 7350 "y.tab.c"
    break;

  case 141: /* cpp_lambda_decl: storage_class AUTO idcolon EQUAL lambda_introducer lambda_template LPAREN parms RPAREN cpp_const ARROW type lambda_body lambda_tail  */
#line 3429 "parser.y"
                                                                                                                                                      {
		  (yyval.node) = new_node("lambda");
		  Setattr((yyval.node),"name",(yyvsp[-11].str));
		  add_symbols((yyval.node));
		}
#line 7360 "y.tab.c"
    break;

  case 142: /* cpp_lambda_decl: storage_class AUTO idcolon EQUAL lambda_introducer lambda_template lambda_body lambda_tail  */
#line 3434 "parser.y"
                                                                                                             {
		  (yyval.node) = new_node("lambda");
		  Setattr((yyval.node),"name",(yyvsp[-5].str));
		  add_symbols((yyval.node));
		}
#line 7370 "y.tab.c"
    break;

  case 143: /* lambda_introducer: LBRACKET  */
#line 3441 "parser.y"
                             {
		  skip_balanced('[',']');
		  (yyval.node) = 0;
	        }
#line 7379 "y.tab.c"
    break;

  case 144: /* lambda_template: LESSTHAN  */
#line 3447 "parser.y"
                           {
		  skip_balanced('<','>');
		  (yyval.node) = 0;
		}
#line 7388 "y.tab.c"
    break;

  case 145: /* lambda_template: empty  */
#line 3451 "parser.y"
                        { (yyval.node) = 0; }
#line 7394 "y.tab.c"
    break;

  case 146: /* lambda_body: LBRACE  */
#line 3454 "parser.y"
                     {
		  skip_balanced('{','}');
		  (yyval.node) = 0;
		}
#line 7403 "y.tab.c"
    break;

  case 147: /* lambda_tail: SEMI  */
#line 3459 "parser.y"
                     {
		  (yyval.pl) = 0;
		}
#line 7411 "y.tab.c"
    break;

  case 148: /* $@4: %empty  */
#line 3462 "parser.y"
                         {
		  skip_balanced('(',')');
		}
#line 7419 "y.tab.c"
    break;

  case 149: /* lambda_tail: LPAREN $@4 SEMI  */
#line 3464 "parser.y"
                       {
		  (yyval.pl) = 0;
		}
#line 7427 "y.tab.c"
    break;

  case 150: /* c_enum_key: ENUM  */
#line 3475 "parser.y"
                  {
		   (yyval.node) = (char *)"enum";
	      }
#line 7435 "y.tab.c"
    break;

  case 151: /* c_enum_key: ENUM CLASS  */
#line 3478 "parser.y"
                           {
		   (yyval.node) = (char *)"enum class";
	      }
#line 7443 "y.tab.c"
    break;

  case 152: /* c_enum_key: ENUM STRUCT  */
#line 3481 "parser.y"
                            {
		   (yyval.node) = (char *)"enum struct";
	      }
#line 7451 "y.tab.c"
    break;

  case 153: /* c_enum_inherit: COLON type_right  */
#line 3490 "parser.y"
                                  {
                   (yyval.node) = (yyvsp[0].type);
              }
#line 7459 "y.tab.c"
    break;

  case 154: /* c_enum_inherit: empty  */
#line 3493 "parser.y"
                      { (yyval.node) = 0; }
#line 7465 "y.tab.c"
    break;

  case 155: /* c_enum_forward_decl: storage_class c_enum_key ename c_enum_inherit SEMI  */
#line 3500 "parser.y"
                                                                         {
		   SwigType *ty = 0;
		   int scopedenum = (yyvsp[-2].id) && !Equal((yyvsp[-3].node), "enum");
		   (yyval.node) = new_node("enumforward");
		   ty = NewStringf("enum %s", (yyvsp[-2].id));
		   Setattr((yyval.node),"enumkey",(yyvsp[-3].node));
		   if (scopedenum)
		     SetFlag((yyval.node), "scopedenum");
		   Setattr((yyval.node),"name",(yyvsp[-2].id));
		   Setattr((yyval.node),"inherit",(yyvsp[-1].node));
		   Setattr((yyval.node),"type",ty);
		   Setattr((yyval.node),"sym:weak", "1");
		   add_symbols((yyval.node));
	      }
#line 7484 "y.tab.c"
    break;

  case 156: /* c_enum_decl: storage_class c_enum_key ename c_enum_inherit LBRACE enumlist RBRACE SEMI  */
#line 3522 "parser.y"
                                                                                         {
		  SwigType *ty = 0;
		  int scopedenum = (yyvsp[-5].id) && !Equal((yyvsp[-6].node), "enum");
                  (yyval.node) = new_node("enum");
		  ty = NewStringf("enum %s", (yyvsp[-5].id));
		  Setattr((yyval.node),"enumkey",(yyvsp[-6].node));
		  if (scopedenum)
		    SetFlag((yyval.node), "scopedenum");
		  Setattr((yyval.node),"name",(yyvsp[-5].id));
		  Setattr((yyval.node),"inherit",(yyvsp[-4].node));
		  Setattr((yyval.node),"type",ty);
		  appendChild((yyval.node),(yyvsp[-2].node));
		  add_symbols((yyval.node));      /* Add to tag space */

		  if (scopedenum) {
		    Swig_symbol_newscope();
		    Swig_symbol_setscopename((yyvsp[-5].id));
		    Delete(Namespaceprefix);
		    Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		  }

		  add_symbols((yyvsp[-2].node));      /* Add enum values to appropriate enum or enum class scope */

		  if (scopedenum) {
		    Setattr((yyval.node),"symtab", Swig_symbol_popscope());
		    Delete(Namespaceprefix);
		    Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		  }
               }
#line 7518 "y.tab.c"
    break;

  case 157: /* c_enum_decl: storage_class c_enum_key ename c_enum_inherit LBRACE enumlist RBRACE declarator cpp_const initializer c_decl_tail  */
#line 3551 "parser.y"
                                                                                                                                   {
		 Node *n;
		 SwigType *ty = 0;
		 String   *unnamed = 0;
		 int       unnamedinstance = 0;
		 int scopedenum = (yyvsp[-8].id) && !Equal((yyvsp[-9].node), "enum");

		 (yyval.node) = new_node("enum");
		 Setattr((yyval.node),"enumkey",(yyvsp[-9].node));
		 if (scopedenum)
		   SetFlag((yyval.node), "scopedenum");
		 Setattr((yyval.node),"inherit",(yyvsp[-7].node));
		 if ((yyvsp[-8].id)) {
		   Setattr((yyval.node),"name",(yyvsp[-8].id));
		   ty = NewStringf("enum %s", (yyvsp[-8].id));
		 } else if ((yyvsp[-3].decl).id) {
		   unnamed = make_unnamed();
		   ty = NewStringf("enum %s", unnamed);
		   Setattr((yyval.node),"unnamed",unnamed);
                   /* name is not set for unnamed enum instances, e.g. enum { foo } Instance; */
		   if ((yyvsp[-10].id) && Cmp((yyvsp[-10].id),"typedef") == 0) {
		     Setattr((yyval.node),"name",(yyvsp[-3].decl).id);
                   } else {
                     unnamedinstance = 1;
                   }
		   Setattr((yyval.node),"storage",(yyvsp[-10].id));
		 }
		 if ((yyvsp[-3].decl).id && Cmp((yyvsp[-10].id),"typedef") == 0) {
		   Setattr((yyval.node),"tdname",(yyvsp[-3].decl).id);
                   Setattr((yyval.node),"allows_typedef","1");
                 }
		 appendChild((yyval.node),(yyvsp[-5].node));
		 n = new_node("cdecl");
		 Setattr(n,"type",ty);
		 Setattr(n,"name",(yyvsp[-3].decl).id);
		 Setattr(n,"storage",(yyvsp[-10].id));
		 Setattr(n,"decl",(yyvsp[-3].decl).type);
		 Setattr(n,"parms",(yyvsp[-3].decl).parms);
		 Setattr(n,"unnamed",unnamed);

                 if (unnamedinstance) {
		   SwigType *cty = NewString("enum ");
		   Setattr((yyval.node),"type",cty);
		   SetFlag((yyval.node),"unnamedinstance");
		   SetFlag(n,"unnamedinstance");
		   Delete(cty);
                 }
		 if ((yyvsp[0].node)) {
		   Node *p = (yyvsp[0].node);
		   set_nextSibling(n,p);
		   while (p) {
		     SwigType *cty = Copy(ty);
		     Setattr(p,"type",cty);
		     Setattr(p,"unnamed",unnamed);
		     Setattr(p,"storage",(yyvsp[-10].id));
		     Delete(cty);
		     p = nextSibling(p);
		   }
		 } else {
		   if (Len(scanner_ccode)) {
		     String *code = Copy(scanner_ccode);
		     Setattr(n,"code",code);
		     Delete(code);
		   }
		 }

                 /* Ensure that typedef enum ABC {foo} XYZ; uses XYZ for sym:name, like structs.
                  * Note that class_rename/yyrename are bit of a mess so used this simple approach to change the name. */
                 if ((yyvsp[-3].decl).id && (yyvsp[-8].id) && Cmp((yyvsp[-10].id),"typedef") == 0) {
		   String *name = NewString((yyvsp[-3].decl).id);
                   Setattr((yyval.node), "parser:makename", name);
		   Delete(name);
                 }

		 add_symbols((yyval.node));       /* Add enum to tag space */
		 set_nextSibling((yyval.node),n);
		 Delete(n);

		 if (scopedenum) {
		   Swig_symbol_newscope();
		   Swig_symbol_setscopename((yyvsp[-8].id));
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		 }

		 add_symbols((yyvsp[-5].node));      /* Add enum values to appropriate enum or enum class scope */

		 if (scopedenum) {
		   Setattr((yyval.node),"symtab", Swig_symbol_popscope());
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		 }

	         add_symbols(n);
		 Delete(unnamed);
	       }
#line 7619 "y.tab.c"
    break;

  case 158: /* c_constructor_decl: storage_class type LPAREN parms RPAREN ctor_end  */
#line 3649 "parser.y"
                                                                     {
                   /* This is a sick hack.  If the ctor_end has parameters,
                      and the parms parameter only has 1 parameter, this
                      could be a declaration of the form:

                         type (id)(parms)

			 Otherwise it's an error. */
                    int err = 0;
                    (yyval.node) = 0;

		    if ((ParmList_len((yyvsp[-2].pl)) == 1) && (!Swig_scopename_check((yyvsp[-4].type)))) {
		      SwigType *ty = Getattr((yyvsp[-2].pl),"type");
		      String *name = Getattr((yyvsp[-2].pl),"name");
		      err = 1;
		      if (!name) {
			(yyval.node) = new_node("cdecl");
			Setattr((yyval.node),"type",(yyvsp[-4].type));
			Setattr((yyval.node),"storage",(yyvsp[-5].id));
			Setattr((yyval.node),"name",ty);

			if ((yyvsp[0].decl).have_parms) {
			  SwigType *decl = NewStringEmpty();
			  SwigType_add_function(decl,(yyvsp[0].decl).parms);
			  Setattr((yyval.node),"decl",decl);
			  Setattr((yyval.node),"parms",(yyvsp[0].decl).parms);
			  if (Len(scanner_ccode)) {
			    String *code = Copy(scanner_ccode);
			    Setattr((yyval.node),"code",code);
			    Delete(code);
			  }
			}
			if ((yyvsp[0].decl).defarg) {
			  Setattr((yyval.node),"value",(yyvsp[0].decl).defarg);
			}
			Setattr((yyval.node),"throws",(yyvsp[0].decl).throws);
			Setattr((yyval.node),"throw",(yyvsp[0].decl).throwf);
			Setattr((yyval.node),"noexcept",(yyvsp[0].decl).nexcept);
			Setattr((yyval.node),"final",(yyvsp[0].decl).final);
			err = 0;
		      }
		    }
		    if (err) {
		      Swig_error(cparse_file,cparse_line,"Syntax error in input(2).\n");
		      Exit(EXIT_FAILURE);
		    }
                }
#line 7671 "y.tab.c"
    break;

  case 159: /* cpp_declaration: cpp_class_decl  */
#line 3702 "parser.y"
                                 {  (yyval.node) = (yyvsp[0].node); }
#line 7677 "y.tab.c"
    break;

  case 160: /* cpp_declaration: cpp_forward_class_decl  */
#line 3703 "parser.y"
                                         { (yyval.node) = (yyvsp[0].node); }
#line 7683 "y.tab.c"
    break;

  case 161: /* cpp_declaration: cpp_template_decl  */
#line 3704 "parser.y"
                                    { (yyval.node) = (yyvsp[0].node); }
#line 7689 "y.tab.c"
    break;

  case 162: /* cpp_declaration: cpp_using_decl  */
#line 3705 "parser.y"
                                 { (yyval.node) = (yyvsp[0].node); }
#line 7695 "y.tab.c"
    break;

  case 163: /* cpp_declaration: cpp_namespace_decl  */
#line 3706 "parser.y"
                                     { (yyval.node) = (yyvsp[0].node); }
#line 7701 "y.tab.c"
    break;

  case 164: /* cpp_declaration: cpp_catch_decl  */
#line 3707 "parser.y"
                                 { (yyval.node) = 0; }
#line 7707 "y.tab.c"
    break;

  case 165: /* @5: %empty  */
#line 3716 "parser.y"
                                                                                      {
                   String *prefix;
                   List *bases = 0;
		   Node *scope = 0;
		   String *code;
		   (yyval.node) = new_node("class");
		   Setline((yyval.node),cparse_start_line);
		   Setattr((yyval.node),"kind",(yyvsp[-4].id));
		   if ((yyvsp[-1].bases)) {
		     Setattr((yyval.node),"baselist", Getattr((yyvsp[-1].bases),"public"));
		     Setattr((yyval.node),"protectedbaselist", Getattr((yyvsp[-1].bases),"protected"));
		     Setattr((yyval.node),"privatebaselist", Getattr((yyvsp[-1].bases),"private"));
		   }
		   Setattr((yyval.node),"allows_typedef","1");

		   /* preserve the current scope */
		   Setattr((yyval.node),"prev_symtab",Swig_symbol_current());
		  
		   /* If the class name is qualified.  We need to create or lookup namespace/scope entries */
		   scope = resolve_create_node_scope((yyvsp[-3].str), 1);
		   /* save nscope_inner to the class - it may be overwritten in nested classes*/
		   Setattr((yyval.node), "nested:innerscope", nscope_inner);
		   Setattr((yyval.node), "nested:nscope", nscope);
		   Setfile(scope,cparse_file);
		   Setline(scope,cparse_line);
		   (yyvsp[-3].str) = scope;
		   Setattr((yyval.node),"name",(yyvsp[-3].str));

		   if (currentOuterClass) {
		     SetFlag((yyval.node), "nested");
		     Setattr((yyval.node), "nested:outer", currentOuterClass);
		     set_access_mode((yyval.node));
		   }
		   Swig_features_get(Swig_cparse_features(), Namespaceprefix, Getattr((yyval.node), "name"), 0, (yyval.node));
		   /* save yyrename to the class attribute, to be used later in add_symbols()*/
		   Setattr((yyval.node), "class_rename", make_name((yyval.node), (yyvsp[-3].str), 0));
		   Setattr((yyval.node), "Classprefix", (yyvsp[-3].str));
		   Classprefix = NewString((yyvsp[-3].str));
		   /* Deal with inheritance  */
		   if ((yyvsp[-1].bases))
		     bases = Swig_make_inherit_list((yyvsp[-3].str),Getattr((yyvsp[-1].bases),"public"),Namespaceprefix);
		   prefix = SwigType_istemplate_templateprefix((yyvsp[-3].str));
		   if (prefix) {
		     String *fbase, *tbase;
		     if (Namespaceprefix) {
		       fbase = NewStringf("%s::%s", Namespaceprefix,(yyvsp[-3].str));
		       tbase = NewStringf("%s::%s", Namespaceprefix, prefix);
		     } else {
		       fbase = Copy((yyvsp[-3].str));
		       tbase = Copy(prefix);
		     }
		     Swig_name_inherit(tbase,fbase);
		     Delete(fbase);
		     Delete(tbase);
		   }
                   if (strcmp((yyvsp[-4].id),"class") == 0) {
		     cplus_mode = CPLUS_PRIVATE;
		   } else {
		     cplus_mode = CPLUS_PUBLIC;
		   }
		   if (!cparse_cplusplus) {
		     set_scope_to_global();
		   }
		   Swig_symbol_newscope();
		   Swig_symbol_setscopename((yyvsp[-3].str));
		   Swig_inherit_base_symbols(bases);
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   cparse_start_line = cparse_line;

		   /* If there are active template parameters, we need to make sure they are
                      placed in the class symbol table so we can catch shadows */

		   if (template_parameters) {
		     Parm *tp = template_parameters;
		     while(tp) {
		       String *tpname = Copy(Getattr(tp,"name"));
		       Node *tn = new_node("templateparm");
		       Setattr(tn,"name",tpname);
		       Swig_symbol_cadd(tpname,tn);
		       tp = nextSibling(tp);
		       Delete(tpname);
		     }
		   }
		   Delete(prefix);
		   inclass = 1;
		   currentOuterClass = (yyval.node);
		   if (cparse_cplusplusout) {
		     /* save the structure declaration to declare it in global scope for C++ to see */
		     code = get_raw_text_balanced('{', '}');
		     Setattr((yyval.node), "code", code);
		     Delete(code);
		   }
               }
#line 7806 "y.tab.c"
    break;

  case 166: /* cpp_class_decl: storage_class cpptype idcolon class_virt_specifier_opt inherit LBRACE @5 cpp_members RBRACE cpp_opt_declarators  */
#line 3809 "parser.y"
                                                        {
		   Node *p;
		   SwigType *ty;
		   Symtab *cscope;
		   Node *am = 0;
		   String *scpname = 0;
		   (void) (yyvsp[-4].node);
		   (yyval.node) = currentOuterClass;
		   currentOuterClass = Getattr((yyval.node), "nested:outer");
		   nscope_inner = Getattr((yyval.node), "nested:innerscope");
		   nscope = Getattr((yyval.node), "nested:nscope");
		   Delattr((yyval.node), "nested:innerscope");
		   Delattr((yyval.node), "nested:nscope");
		   if (nscope_inner && Strcmp(nodeType(nscope_inner), "class") == 0) { /* actual parent class for this class */
		     Node* forward_declaration = Swig_symbol_clookup_no_inherit(Getattr((yyval.node),"name"), Getattr(nscope_inner, "symtab"));
		     if (forward_declaration) {
		       Setattr((yyval.node), "access", Getattr(forward_declaration, "access"));
		     }
		     Setattr((yyval.node), "nested:outer", nscope_inner);
		     SetFlag((yyval.node), "nested");
                   }
		   if (!currentOuterClass)
		     inclass = 0;
		   cscope = Getattr((yyval.node), "prev_symtab");
		   Delattr((yyval.node), "prev_symtab");
		   
		   /* Check for pure-abstract class */
		   Setattr((yyval.node),"abstracts", pure_abstracts((yyvsp[-2].node)));
		   
		   /* This bit of code merges in a previously defined %extend directive (if any) */
		   {
		     String *clsname = Swig_symbol_qualifiedscopename(0);
		     am = Getattr(Swig_extend_hash(), clsname);
		     if (am) {
		       Swig_extend_merge((yyval.node), am);
		       Delattr(Swig_extend_hash(), clsname);
		     }
		     Delete(clsname);
		   }
		   if (!classes) classes = NewHash();
		   scpname = Swig_symbol_qualifiedscopename(0);
		   Setattr(classes, scpname, (yyval.node));

		   appendChild((yyval.node), (yyvsp[-2].node));
		   
		   if (am) 
		     Swig_extend_append_previous((yyval.node), am);

		   p = (yyvsp[0].node);
		   if (p && !nscope_inner) {
		     if (!cparse_cplusplus && currentOuterClass)
		       appendChild(currentOuterClass, p);
		     else
		      appendSibling((yyval.node), p);
		   }
		   
		   if (nscope_inner) {
		     ty = NewString(scpname); /* if the class is declared out of scope, let the declarator use fully qualified type*/
		   } else if (cparse_cplusplus && !cparse_externc) {
		     ty = NewString((yyvsp[-7].str));
		   } else {
		     ty = NewStringf("%s %s", (yyvsp[-8].id), (yyvsp[-7].str));
		   }
		   while (p) {
		     Setattr(p, "storage", (yyvsp[-9].id));
		     Setattr(p, "type" ,ty);
		     if (!cparse_cplusplus && currentOuterClass && (!Getattr(currentOuterClass, "name"))) {
		       SetFlag(p, "hasconsttype");
		       SetFlag(p, "feature:immutable");
		     }
		     p = nextSibling(p);
		   }
		   if ((yyvsp[0].node) && Cmp((yyvsp[-9].id),"typedef") == 0)
		     add_typedef_name((yyval.node), (yyvsp[0].node), (yyvsp[-7].str), cscope, scpname);
		   Delete(scpname);

		   if (cplus_mode != CPLUS_PUBLIC) {
		   /* we 'open' the class at the end, to allow %template
		      to add new members */
		     Node *pa = new_node("access");
		     Setattr(pa, "kind", "public");
		     cplus_mode = CPLUS_PUBLIC;
		     appendChild((yyval.node), pa);
		     Delete(pa);
		   }
		   if (currentOuterClass)
		     restore_access_mode((yyval.node));
		   Setattr((yyval.node), "symtab", Swig_symbol_popscope());
		   Classprefix = Getattr((yyval.node), "Classprefix");
		   Delattr((yyval.node), "Classprefix");
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   if (cplus_mode == CPLUS_PRIVATE) {
		     (yyval.node) = 0; /* skip private nested classes */
		   } else if (cparse_cplusplus && currentOuterClass && ignore_nested_classes && !GetFlag((yyval.node), "feature:flatnested")) {
		     (yyval.node) = nested_forward_declaration((yyvsp[-9].id), (yyvsp[-8].id), (yyvsp[-7].str), Copy((yyvsp[-7].str)), (yyvsp[0].node));
		   } else if (nscope_inner) {
		     /* this is tricky */
		     /* we add the declaration in the original namespace */
		     if (Strcmp(nodeType(nscope_inner), "class") == 0 && cparse_cplusplus && ignore_nested_classes && !GetFlag((yyval.node), "feature:flatnested"))
		       (yyval.node) = nested_forward_declaration((yyvsp[-9].id), (yyvsp[-8].id), (yyvsp[-7].str), Copy((yyvsp[-7].str)), (yyvsp[0].node));
		     appendChild(nscope_inner, (yyval.node));
		     Swig_symbol_setscope(Getattr(nscope_inner, "symtab"));
		     Delete(Namespaceprefix);
		     Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		     yyrename = Copy(Getattr((yyval.node), "class_rename"));
		     add_symbols((yyval.node));
		     Delattr((yyval.node), "class_rename");
		     /* but the variable definition in the current scope */
		     Swig_symbol_setscope(cscope);
		     Delete(Namespaceprefix);
		     Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		     add_symbols((yyvsp[0].node));
		     if (nscope) {
		       (yyval.node) = nscope; /* here we return recreated namespace tower instead of the class itself */
		       if ((yyvsp[0].node)) {
			 appendSibling((yyval.node), (yyvsp[0].node));
		       }
		     } else if (!SwigType_istemplate(ty) && template_parameters == 0) { /* for template we need the class itself */
		       (yyval.node) = (yyvsp[0].node);
		     }
		   } else {
		     Delete(yyrename);
		     yyrename = 0;
		     if (!cparse_cplusplus && currentOuterClass) { /* nested C structs go into global scope*/
		       Node *outer = currentOuterClass;
		       while (Getattr(outer, "nested:outer"))
			 outer = Getattr(outer, "nested:outer");
		       appendSibling(outer, (yyval.node));
		       Swig_symbol_setscope(cscope); /* declaration goes in the parent scope */
		       add_symbols((yyvsp[0].node));
		       set_scope_to_global();
		       Delete(Namespaceprefix);
		       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		       yyrename = Copy(Getattr((yyval.node), "class_rename"));
		       add_symbols((yyval.node));
		       if (!cparse_cplusplusout)
			 Delattr((yyval.node), "nested:outer");
		       Delattr((yyval.node), "class_rename");
		       (yyval.node) = 0;
		     } else {
		       yyrename = Copy(Getattr((yyval.node), "class_rename"));
		       add_symbols((yyval.node));
		       add_symbols((yyvsp[0].node));
		       Delattr((yyval.node), "class_rename");
		     }
		   }
		   Delete(ty);
		   Swig_symbol_setscope(cscope);
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   Classprefix = currentOuterClass ? Getattr(currentOuterClass, "Classprefix") : 0;
	       }
#line 7964 "y.tab.c"
    break;

  case 167: /* @6: %empty  */
#line 3965 "parser.y"
                                                    {
	       String *unnamed;
	       String *code;
	       unnamed = make_unnamed();
	       (yyval.node) = new_node("class");
	       Setline((yyval.node),cparse_start_line);
	       Setattr((yyval.node),"kind",(yyvsp[-2].id));
	       if ((yyvsp[-1].bases)) {
		 Setattr((yyval.node),"baselist", Getattr((yyvsp[-1].bases),"public"));
		 Setattr((yyval.node),"protectedbaselist", Getattr((yyvsp[-1].bases),"protected"));
		 Setattr((yyval.node),"privatebaselist", Getattr((yyvsp[-1].bases),"private"));
	       }
	       Setattr((yyval.node),"storage",(yyvsp[-3].id));
	       Setattr((yyval.node),"unnamed",unnamed);
	       Setattr((yyval.node),"allows_typedef","1");
	       if (currentOuterClass) {
		 SetFlag((yyval.node), "nested");
		 Setattr((yyval.node), "nested:outer", currentOuterClass);
		 set_access_mode((yyval.node));
	       }
	       Swig_features_get(Swig_cparse_features(), Namespaceprefix, 0, 0, (yyval.node));
	       /* save yyrename to the class attribute, to be used later in add_symbols()*/
	       Setattr((yyval.node), "class_rename", make_name((yyval.node),0,0));
	       if (strcmp((yyvsp[-2].id),"class") == 0) {
		 cplus_mode = CPLUS_PRIVATE;
	       } else {
		 cplus_mode = CPLUS_PUBLIC;
	       }
	       Swig_symbol_newscope();
	       cparse_start_line = cparse_line;
	       currentOuterClass = (yyval.node);
	       inclass = 1;
	       Classprefix = 0;
	       Delete(Namespaceprefix);
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       /* save the structure declaration to make a typedef for it later*/
	       code = get_raw_text_balanced('{', '}');
	       Setattr((yyval.node), "code", code);
	       Delete(code);
	     }
#line 8009 "y.tab.c"
    break;

  case 168: /* cpp_class_decl: storage_class cpptype inherit LBRACE @6 cpp_members RBRACE cpp_opt_declarators  */
#line 4004 "parser.y"
                                                      {
	       String *unnamed;
               List *bases = 0;
	       String *name = 0;
	       Node *n;
	       Classprefix = 0;
	       (void)(yyvsp[-3].node);
	       (yyval.node) = currentOuterClass;
	       currentOuterClass = Getattr((yyval.node), "nested:outer");
	       if (!currentOuterClass)
		 inclass = 0;
	       else
		 restore_access_mode((yyval.node));
	       unnamed = Getattr((yyval.node),"unnamed");
               /* Check for pure-abstract class */
	       Setattr((yyval.node),"abstracts", pure_abstracts((yyvsp[-2].node)));
	       n = (yyvsp[0].node);
	       if (cparse_cplusplus && currentOuterClass && ignore_nested_classes && !GetFlag((yyval.node), "feature:flatnested")) {
		 String *name = n ? Copy(Getattr(n, "name")) : 0;
		 (yyval.node) = nested_forward_declaration((yyvsp[-7].id), (yyvsp[-6].id), 0, name, n);
		 Swig_symbol_popscope();
	         Delete(Namespaceprefix);
		 Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       } else if (n) {
	         appendSibling((yyval.node),n);
		 /* If a proper typedef name was given, we'll use it to set the scope name */
		 name = try_to_find_a_name_for_unnamed_structure((yyvsp[-7].id), n);
		 if (name) {
		   String *scpname = 0;
		   SwigType *ty;
		   Setattr((yyval.node),"tdname",name);
		   Setattr((yyval.node),"name",name);
		   Swig_symbol_setscopename(name);
		   if ((yyvsp[-5].bases))
		     bases = Swig_make_inherit_list(name,Getattr((yyvsp[-5].bases),"public"),Namespaceprefix);
		   Swig_inherit_base_symbols(bases);

		     /* If a proper name was given, we use that as the typedef, not unnamed */
		   Clear(unnamed);
		   Append(unnamed, name);
		   if (cparse_cplusplus && !cparse_externc) {
		     ty = NewString(name);
		   } else {
		     ty = NewStringf("%s %s", (yyvsp[-6].id),name);
		   }
		   while (n) {
		     Setattr(n,"storage",(yyvsp[-7].id));
		     Setattr(n, "type", ty);
		     if (!cparse_cplusplus && currentOuterClass && (!Getattr(currentOuterClass, "name"))) {
		       SetFlag(n,"hasconsttype");
		       SetFlag(n,"feature:immutable");
		     }
		     n = nextSibling(n);
		   }
		   n = (yyvsp[0].node);

		   /* Check for previous extensions */
		   {
		     String *clsname = Swig_symbol_qualifiedscopename(0);
		     Node *am = Getattr(Swig_extend_hash(),clsname);
		     if (am) {
		       /* Merge the extension into the symbol table */
		       Swig_extend_merge((yyval.node),am);
		       Swig_extend_append_previous((yyval.node),am);
		       Delattr(Swig_extend_hash(),clsname);
		     }
		     Delete(clsname);
		   }
		   if (!classes) classes = NewHash();
		   scpname = Swig_symbol_qualifiedscopename(0);
		   Setattr(classes,scpname,(yyval.node));
		   Delete(scpname);
		 } else { /* no suitable name was found for a struct */
		   Setattr((yyval.node), "nested:unnamed", Getattr(n, "name")); /* save the name of the first declarator for later use in name generation*/
		   while (n) { /* attach unnamed struct to the declarators, so that they would receive proper type later*/
		     Setattr(n, "nested:unnamedtype", (yyval.node));
		     Setattr(n, "storage", (yyvsp[-7].id));
		     n = nextSibling(n);
		   }
		   n = (yyvsp[0].node);
		   Swig_symbol_setscopename("<unnamed>");
		 }
		 appendChild((yyval.node),(yyvsp[-2].node));
		 /* Pop the scope */
		 Setattr((yyval.node),"symtab",Swig_symbol_popscope());
		 if (name) {
		   Delete(yyrename);
		   yyrename = Copy(Getattr((yyval.node), "class_rename"));
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   add_symbols((yyval.node));
		   add_symbols(n);
		   Delattr((yyval.node), "class_rename");
		 }else if (cparse_cplusplus)
		   (yyval.node) = 0; /* ignore unnamed structs for C++ */
	         Delete(unnamed);
	       } else { /* unnamed struct w/o declarator*/
		 Swig_symbol_popscope();
	         Delete(Namespaceprefix);
		 Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		 add_symbols((yyvsp[-2].node));
		 Delete((yyval.node));
		 (yyval.node) = (yyvsp[-2].node); /* pass member list to outer class/namespace (instead of self)*/
	       }
	       Classprefix = currentOuterClass ? Getattr(currentOuterClass, "Classprefix") : 0;
              }
#line 8120 "y.tab.c"
    break;

  case 169: /* cpp_opt_declarators: SEMI  */
#line 4112 "parser.y"
                            { (yyval.node) = 0; }
#line 8126 "y.tab.c"
    break;

  case 170: /* cpp_opt_declarators: declarator cpp_const initializer c_decl_tail  */
#line 4113 "parser.y"
                                                                    {
                        (yyval.node) = new_node("cdecl");
                        Setattr((yyval.node),"name",(yyvsp[-3].decl).id);
                        Setattr((yyval.node),"decl",(yyvsp[-3].decl).type);
                        Setattr((yyval.node),"parms",(yyvsp[-3].decl).parms);
			set_nextSibling((yyval.node), (yyvsp[0].node));
                    }
#line 8138 "y.tab.c"
    break;

  case 171: /* cpp_forward_class_decl: storage_class cpptype idcolon SEMI  */
#line 4125 "parser.y"
                                                            {
              if ((yyvsp[-3].id) && (Strcmp((yyvsp[-3].id),"friend") == 0)) {
		/* Ignore */
                (yyval.node) = 0; 
	      } else {
		(yyval.node) = new_node("classforward");
		Setattr((yyval.node),"kind",(yyvsp[-2].id));
		Setattr((yyval.node),"name",(yyvsp[-1].str));
		Setattr((yyval.node),"sym:weak", "1");
		add_symbols((yyval.node));
	      }
             }
#line 8155 "y.tab.c"
    break;

  case 172: /* $@7: %empty  */
#line 4143 "parser.y"
                                                                 { 
		    if (currentOuterClass)
		      Setattr(currentOuterClass, "template_parameters", template_parameters);
		    template_parameters = (yyvsp[-1].tparms); 
		    parsing_template_declaration = 1;
		  }
#line 8166 "y.tab.c"
    break;

  case 173: /* cpp_template_decl: TEMPLATE LESSTHAN template_parms GREATERTHAN $@7 cpp_template_possible  */
#line 4148 "parser.y"
                                          {
			String *tname = 0;
			int     error = 0;

			/* check if we get a namespace node with a class declaration, and retrieve the class */
			Symtab *cscope = Swig_symbol_current();
			Symtab *sti = 0;
			Node *ntop = (yyvsp[0].node);
			Node *ni = ntop;
			SwigType *ntype = ni ? nodeType(ni) : 0;
			while (ni && Strcmp(ntype,"namespace") == 0) {
			  sti = Getattr(ni,"symtab");
			  ni = firstChild(ni);
			  ntype = nodeType(ni);
			}
			if (sti) {
			  Swig_symbol_setscope(sti);
			  Delete(Namespaceprefix);
			  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
			  (yyvsp[0].node) = ni;
			}

			(yyval.node) = (yyvsp[0].node);
			if ((yyval.node)) tname = Getattr((yyval.node),"name");
			
			/* Check if the class is a template specialization */
			if (((yyval.node)) && (Strchr(tname,'<')) && (!is_operator(tname))) {
			  /* If a specialization.  Check if defined. */
			  Node *tempn = 0;
			  {
			    String *tbase = SwigType_templateprefix(tname);
			    tempn = Swig_symbol_clookup_local(tbase,0);
			    if (!tempn || (Strcmp(nodeType(tempn),"template") != 0)) {
			      SWIG_WARN_NODE_BEGIN(tempn);
			      Swig_warning(WARN_PARSE_TEMPLATE_SP_UNDEF, Getfile((yyval.node)),Getline((yyval.node)),"Specialization of non-template '%s'.\n", tbase);
			      SWIG_WARN_NODE_END(tempn);
			      tempn = 0;
			      error = 1;
			    }
			    Delete(tbase);
			  }
			  Setattr((yyval.node),"specialization","1");
			  Setattr((yyval.node),"templatetype",nodeType((yyval.node)));
			  set_nodeType((yyval.node),"template");
			  /* Template partial specialization */
			  if (tempn && ((yyvsp[-3].tparms)) && ((yyvsp[0].node))) {
			    List   *tlist;
			    String *targs = SwigType_templateargs(tname);
			    tlist = SwigType_parmlist(targs);
			    /*			  Printf(stdout,"targs = '%s' %s\n", targs, tlist); */
			    if (!Getattr((yyval.node),"sym:weak")) {
			      Setattr((yyval.node),"sym:typename","1");
			    }
			    
			    if (Len(tlist) != ParmList_len(Getattr(tempn,"templateparms"))) {
			      Swig_error(Getfile((yyval.node)),Getline((yyval.node)),"Inconsistent argument count in template partial specialization. %d %d\n", Len(tlist), ParmList_len(Getattr(tempn,"templateparms")));
			      
			    } else {

			    /* This code builds the argument list for the partial template
			       specialization.  This is a little hairy, but the idea is as
			       follows:

			       $3 contains a list of arguments supplied for the template.
			       For example template<class T>.

			       tlist is a list of the specialization arguments--which may be
			       different.  For example class<int,T>.

			       tp is a copy of the arguments in the original template definition.
       
			       The patching algorithm walks through the list of supplied
			       arguments ($3), finds the position in the specialization arguments
			       (tlist), and then patches the name in the argument list of the
			       original template.
			    */

			    {
			      String *pn;
			      Parm *p, *p1;
			      int i, nargs;
			      Parm *tp = CopyParmList(Getattr(tempn,"templateparms"));
			      nargs = Len(tlist);
			      p = (yyvsp[-3].tparms);
			      while (p) {
				for (i = 0; i < nargs; i++){
				  pn = Getattr(p,"name");
				  if (Strcmp(pn,SwigType_base(Getitem(tlist,i))) == 0) {
				    int j;
				    Parm *p1 = tp;
				    for (j = 0; j < i; j++) {
				      p1 = nextSibling(p1);
				    }
				    Setattr(p1,"name",pn);
				    Setattr(p1,"partialarg","1");
				  }
				}
				p = nextSibling(p);
			      }
			      p1 = tp;
			      i = 0;
			      while (p1) {
				if (!Getattr(p1,"partialarg")) {
				  Delattr(p1,"name");
				  Setattr(p1,"type", Getitem(tlist,i));
				} 
				i++;
				p1 = nextSibling(p1);
			      }
			      Setattr((yyval.node),"templateparms",tp);
			      Delete(tp);
			    }
  #if 0
			    /* Patch the parameter list */
			    if (tempn) {
			      Parm *p,*p1;
			      ParmList *tp = CopyParmList(Getattr(tempn,"templateparms"));
			      p = (yyvsp[-3].tparms);
			      p1 = tp;
			      while (p && p1) {
				String *pn = Getattr(p,"name");
				Printf(stdout,"pn = '%s'\n", pn);
				if (pn) Setattr(p1,"name",pn);
				else Delattr(p1,"name");
				pn = Getattr(p,"type");
				if (pn) Setattr(p1,"type",pn);
				p = nextSibling(p);
				p1 = nextSibling(p1);
			      }
			      Setattr((yyval.node),"templateparms",tp);
			      Delete(tp);
			    } else {
			      Setattr((yyval.node),"templateparms",(yyvsp[-3].tparms));
			    }
  #endif
			    Delattr((yyval.node),"specialization");
			    Setattr((yyval.node),"partialspecialization","1");
			    /* Create a specialized name for matching */
			    {
			      Parm *p = (yyvsp[-3].tparms);
			      String *fname = NewString(Getattr((yyval.node),"name"));
			      String *ffname = 0;
			      ParmList *partialparms = 0;

			      char   tmp[32];
			      int    i, ilen;
			      while (p) {
				String *n = Getattr(p,"name");
				if (!n) {
				  p = nextSibling(p);
				  continue;
				}
				ilen = Len(tlist);
				for (i = 0; i < ilen; i++) {
				  if (Strstr(Getitem(tlist,i),n)) {
				    sprintf(tmp,"$%d",i+1);
				    Replaceid(fname,n,tmp);
				  }
				}
				p = nextSibling(p);
			      }
			      /* Patch argument names with typedef */
			      {
				Iterator tt;
				Parm *parm_current = 0;
				List *tparms = SwigType_parmlist(fname);
				ffname = SwigType_templateprefix(fname);
				Append(ffname,"<(");
				for (tt = First(tparms); tt.item; ) {
				  SwigType *rtt = Swig_symbol_typedef_reduce(tt.item,0);
				  SwigType *ttr = Swig_symbol_type_qualify(rtt,0);

				  Parm *newp = NewParmWithoutFileLineInfo(ttr, 0);
				  if (partialparms)
				    set_nextSibling(parm_current, newp);
				  else
				    partialparms = newp;
				  parm_current = newp;

				  Append(ffname,ttr);
				  tt = Next(tt);
				  if (tt.item) Putc(',',ffname);
				  Delete(rtt);
				  Delete(ttr);
				}
				Delete(tparms);
				Append(ffname,")>");
			      }
			      {
				Node *new_partial = NewHash();
				String *partials = Getattr(tempn,"partials");
				if (!partials) {
				  partials = NewList();
				  Setattr(tempn,"partials",partials);
				  Delete(partials);
				}
				/*			      Printf(stdout,"partial: fname = '%s', '%s'\n", fname, Swig_symbol_typedef_reduce(fname,0)); */
				Setattr(new_partial, "partialparms", partialparms);
				Setattr(new_partial, "templcsymname", ffname);
				Append(partials, new_partial);
			      }
			      Setattr((yyval.node),"partialargs",ffname);
			      Swig_symbol_cadd(ffname,(yyval.node));
			    }
			    }
			    Delete(tlist);
			    Delete(targs);
			  } else {
			    /* An explicit template specialization */
			    /* add default args from primary (unspecialized) template */
			    String *ty = Swig_symbol_template_deftype(tname,0);
			    String *fname = Swig_symbol_type_qualify(ty,0);
			    Swig_symbol_cadd(fname,(yyval.node));
			    Delete(ty);
			    Delete(fname);
			  }
			}  else if ((yyval.node)) {
			  Setattr((yyval.node),"templatetype",nodeType((yyvsp[0].node)));
			  set_nodeType((yyval.node),"template");
			  Setattr((yyval.node),"templateparms", (yyvsp[-3].tparms));
			  if (!Getattr((yyval.node),"sym:weak")) {
			    Setattr((yyval.node),"sym:typename","1");
			  }
			  add_symbols((yyval.node));
			  default_arguments((yyval.node));
			  /* We also place a fully parameterized version in the symbol table */
			  {
			    Parm *p;
			    String *fname = NewStringf("%s<(", Getattr((yyval.node),"name"));
			    p = (yyvsp[-3].tparms);
			    while (p) {
			      String *n = Getattr(p,"name");
			      if (!n) n = Getattr(p,"type");
			      Append(fname,n);
			      p = nextSibling(p);
			      if (p) Putc(',',fname);
			    }
			    Append(fname,")>");
			    Swig_symbol_cadd(fname,(yyval.node));
			  }
			}
			(yyval.node) = ntop;
			Swig_symbol_setscope(cscope);
			Delete(Namespaceprefix);
			Namespaceprefix = Swig_symbol_qualifiedscopename(0);
			if (error || (nscope_inner && Strcmp(nodeType(nscope_inner), "class") == 0)) {
			  (yyval.node) = 0;
			}
			if (currentOuterClass)
			  template_parameters = Getattr(currentOuterClass, "template_parameters");
			else
			  template_parameters = 0;
			parsing_template_declaration = 0;
                }
#line 8425 "y.tab.c"
    break;

  case 174: /* cpp_template_decl: TEMPLATE cpptype idcolon  */
#line 4404 "parser.y"
                                           {
		  Swig_warning(WARN_PARSE_EXPLICIT_TEMPLATE, cparse_file, cparse_line, "Explicit template instantiation ignored.\n");
                  (yyval.node) = 0; 
		}
#line 8434 "y.tab.c"
    break;

  case 175: /* cpp_template_decl: TEMPLATE cpp_alternate_rettype idcolon LPAREN parms RPAREN  */
#line 4410 "parser.y"
                                                                             {
			Swig_warning(WARN_PARSE_EXPLICIT_TEMPLATE, cparse_file, cparse_line, "Explicit template instantiation ignored.\n");
                  (yyval.node) = 0; 
		}
#line 8443 "y.tab.c"
    break;

  case 176: /* cpp_template_decl: EXTERN TEMPLATE cpptype idcolon  */
#line 4416 "parser.y"
                                                  {
		  Swig_warning(WARN_PARSE_EXTERN_TEMPLATE, cparse_file, cparse_line, "Extern template ignored.\n");
                  (yyval.node) = 0; 
                }
#line 8452 "y.tab.c"
    break;

  case 177: /* cpp_template_decl: EXTERN TEMPLATE cpp_alternate_rettype idcolon LPAREN parms RPAREN  */
#line 4422 "parser.y"
                                                                                    {
			Swig_warning(WARN_PARSE_EXTERN_TEMPLATE, cparse_file, cparse_line, "Extern template ignored.\n");
                  (yyval.node) = 0; 
		}
#line 8461 "y.tab.c"
    break;

  case 178: /* cpp_template_possible: c_decl  */
#line 4428 "parser.y"
                               {
		  (yyval.node) = (yyvsp[0].node);
                }
#line 8469 "y.tab.c"
    break;

  case 179: /* cpp_template_possible: cpp_class_decl  */
#line 4431 "parser.y"
                                 {
                   (yyval.node) = (yyvsp[0].node);
                }
#line 8477 "y.tab.c"
    break;

  case 180: /* cpp_template_possible: cpp_constructor_decl  */
#line 4434 "parser.y"
                                       {
                   (yyval.node) = (yyvsp[0].node);
                }
#line 8485 "y.tab.c"
    break;

  case 181: /* cpp_template_possible: cpp_template_decl  */
#line 4437 "parser.y"
                                    {
		  (yyval.node) = 0;
                }
#line 8493 "y.tab.c"
    break;

  case 182: /* cpp_template_possible: cpp_forward_class_decl  */
#line 4440 "parser.y"
                                         {
                  (yyval.node) = (yyvsp[0].node);
                }
#line 8501 "y.tab.c"
    break;

  case 183: /* cpp_template_possible: cpp_conversion_operator  */
#line 4443 "parser.y"
                                          {
                  (yyval.node) = (yyvsp[0].node);
                }
#line 8509 "y.tab.c"
    break;

  case 184: /* template_parms: templateparameters  */
#line 4448 "parser.y"
                                     {
		   /* Rip out the parameter names */
		  Parm *p = (yyvsp[0].pl);
		  (yyval.tparms) = (yyvsp[0].pl);

		  while (p) {
		    String *name = Getattr(p,"name");
		    if (!name) {
		      /* Hmmm. Maybe it's a 'class T' parameter */
		      char *type = Char(Getattr(p,"type"));
		      /* Template template parameter */
		      if (strncmp(type,"template<class> ",16) == 0) {
			type += 16;
		      }
		      if ((strncmp(type,"class ",6) == 0) || (strncmp(type,"typename ", 9) == 0)) {
			char *t = strchr(type,' ');
			Setattr(p,"name", t+1);
		      } else 
                      /* Variadic template args */
		      if ((strncmp(type,"class... ",9) == 0) || (strncmp(type,"typename... ", 12) == 0)) {
			char *t = strchr(type,' ');
			Setattr(p,"name", t+1);
			Setattr(p,"variadic", "1");
		      } else {
			/*
			 Swig_error(cparse_file, cparse_line, "Missing template parameter name\n");
			 $$.rparms = 0;
			 $$.parms = 0;
			 break; */
		      }
		    }
		    p = nextSibling(p);
		  }
                 }
#line 8548 "y.tab.c"
    break;

  case 185: /* templateparameters: templateparameter templateparameterstail  */
#line 4484 "parser.y"
                                                              {
                      set_nextSibling((yyvsp[-1].p),(yyvsp[0].pl));
                      (yyval.pl) = (yyvsp[-1].p);
                   }
#line 8557 "y.tab.c"
    break;

  case 186: /* templateparameters: empty  */
#line 4488 "parser.y"
                           { (yyval.pl) = 0; }
#line 8563 "y.tab.c"
    break;

  case 187: /* templateparameter: templcpptype  */
#line 4491 "parser.y"
                                 {
		    (yyval.p) = NewParmWithoutFileLineInfo(NewString((yyvsp[0].id)), 0);
                  }
#line 8571 "y.tab.c"
    break;

  case 188: /* templateparameter: parm  */
#line 4494 "parser.y"
                         {
                    (yyval.p) = (yyvsp[0].p);
                  }
#line 8579 "y.tab.c"
    break;

  case 189: /* templateparameterstail: COMMA templateparameter templateparameterstail  */
#line 4499 "parser.y"
                                                                        {
                         set_nextSibling((yyvsp[-1].p),(yyvsp[0].pl));
                         (yyval.pl) = (yyvsp[-1].p);
                       }
#line 8588 "y.tab.c"
    break;

  case 190: /* templateparameterstail: empty  */
#line 4503 "parser.y"
                               { (yyval.pl) = 0; }
#line 8594 "y.tab.c"
    break;

  case 191: /* cpp_using_decl: USING idcolon SEMI  */
#line 4508 "parser.y"
                                    {
                  String *uname = Swig_symbol_type_qualify((yyvsp[-1].str),0);
		  String *name = Swig_scopename_last((yyvsp[-1].str));
                  (yyval.node) = new_node("using");
		  Setattr((yyval.node),"uname",uname);
		  Setattr((yyval.node),"name", name);
		  Delete(uname);
		  Delete(name);
		  add_symbols((yyval.node));
             }
#line 8609 "y.tab.c"
    break;

  case 192: /* cpp_using_decl: USING NAMESPACE idcolon SEMI  */
#line 4518 "parser.y"
                                            {
	       Node *n = Swig_symbol_clookup((yyvsp[-1].str),0);
	       if (!n) {
		 Swig_error(cparse_file, cparse_line, "Nothing known about namespace '%s'\n", (yyvsp[-1].str));
		 (yyval.node) = 0;
	       } else {

		 while (Strcmp(nodeType(n),"using") == 0) {
		   n = Getattr(n,"node");
		 }
		 if (n) {
		   if (Strcmp(nodeType(n),"namespace") == 0) {
		     Symtab *current = Swig_symbol_current();
		     Symtab *symtab = Getattr(n,"symtab");
		     (yyval.node) = new_node("using");
		     Setattr((yyval.node),"node",n);
		     Setattr((yyval.node),"namespace", (yyvsp[-1].str));
		     if (current != symtab) {
		       Swig_symbol_inherit(symtab);
		     }
		   } else {
		     Swig_error(cparse_file, cparse_line, "'%s' is not a namespace.\n", (yyvsp[-1].str));
		     (yyval.node) = 0;
		   }
		 } else {
		   (yyval.node) = 0;
		 }
	       }
             }
#line 8643 "y.tab.c"
    break;

  case 193: /* @8: %empty  */
#line 4549 "parser.y"
                                              { 
                Hash *h;
		Node *parent_ns = 0;
		List *scopes = Swig_scopename_tolist((yyvsp[-1].str));
		int ilen = Len(scopes);
		int i;

/*
Printf(stdout, "==== Namespace %s creation...\n", $2);
*/
		(yyval.node) = 0;
		for (i = 0; i < ilen; i++) {
		  Node *ns = new_node("namespace");
		  Symtab *current_symtab = Swig_symbol_current();
		  String *scopename = Getitem(scopes, i);
		  Setattr(ns, "name", scopename);
		  (yyval.node) = ns;
		  if (parent_ns)
		    appendChild(parent_ns, ns);
		  parent_ns = ns;
		  h = Swig_symbol_clookup(scopename, 0);
		  if (h && (current_symtab == Getattr(h, "sym:symtab")) && (Strcmp(nodeType(h), "namespace") == 0)) {
/*
Printf(stdout, "  Scope %s [found C++17 style]\n", scopename);
*/
		    if (Getattr(h, "alias")) {
		      h = Getattr(h, "namespace");
		      Swig_warning(WARN_PARSE_NAMESPACE_ALIAS, cparse_file, cparse_line, "Namespace alias '%s' not allowed here. Assuming '%s'\n",
				   scopename, Getattr(h, "name"));
		      scopename = Getattr(h, "name");
		    }
		    Swig_symbol_setscope(Getattr(h, "symtab"));
		  } else {
/*
Printf(stdout, "  Scope %s [creating single scope C++17 style]\n", scopename);
*/
		    h = Swig_symbol_newscope();
		    Swig_symbol_setscopename(scopename);
		  }
		  Delete(Namespaceprefix);
		  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		}
		Delete(scopes);
             }
#line 8692 "y.tab.c"
    break;

  case 194: /* cpp_namespace_decl: NAMESPACE idcolon LBRACE @8 interface RBRACE  */
#line 4592 "parser.y"
                                {
		Node *n = (yyvsp[-2].node);
		Node *top_ns = 0;
		do {
		  Setattr(n, "symtab", Swig_symbol_popscope());
		  Delete(Namespaceprefix);
		  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		  add_symbols(n);
		  top_ns = n;
		  n = parentNode(n);
		} while(n);
		appendChild((yyvsp[-2].node), firstChild((yyvsp[-1].node)));
		Delete((yyvsp[-1].node));
		(yyval.node) = top_ns;
             }
#line 8712 "y.tab.c"
    break;

  case 195: /* $@9: %empty  */
#line 4607 "parser.y"
                                {
	       Hash *h;
	       (yyvsp[-1].node) = Swig_symbol_current();
	       h = Swig_symbol_clookup("    ",0);
	       if (h && (Strcmp(nodeType(h),"namespace") == 0)) {
		 Swig_symbol_setscope(Getattr(h,"symtab"));
	       } else {
		 Swig_symbol_newscope();
		 /* we don't use "__unnamed__", but a long 'empty' name */
		 Swig_symbol_setscopename("    ");
	       }
	       Namespaceprefix = 0;
             }
#line 8730 "y.tab.c"
    break;

  case 196: /* cpp_namespace_decl: NAMESPACE LBRACE $@9 interface RBRACE  */
#line 4619 "parser.y"
                                {
	       (yyval.node) = (yyvsp[-1].node);
	       set_nodeType((yyval.node),"namespace");
	       Setattr((yyval.node),"unnamed","1");
	       Setattr((yyval.node),"symtab", Swig_symbol_popscope());
	       Swig_symbol_setscope((yyvsp[-4].node));
	       Delete(Namespaceprefix);
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       add_symbols((yyval.node));
             }
#line 8745 "y.tab.c"
    break;

  case 197: /* cpp_namespace_decl: NAMESPACE identifier EQUAL idcolon SEMI  */
#line 4629 "parser.y"
                                                       {
	       /* Namespace alias */
	       Node *n;
	       (yyval.node) = new_node("namespace");
	       Setattr((yyval.node),"name",(yyvsp[-3].id));
	       Setattr((yyval.node),"alias",(yyvsp[-1].str));
	       n = Swig_symbol_clookup((yyvsp[-1].str),0);
	       if (!n) {
		 Swig_error(cparse_file, cparse_line, "Unknown namespace '%s'\n", (yyvsp[-1].str));
		 (yyval.node) = 0;
	       } else {
		 if (Strcmp(nodeType(n),"namespace") != 0) {
		   Swig_error(cparse_file, cparse_line, "'%s' is not a namespace\n",(yyvsp[-1].str));
		   (yyval.node) = 0;
		 } else {
		   while (Getattr(n,"alias")) {
		     n = Getattr(n,"namespace");
		   }
		   Setattr((yyval.node),"namespace",n);
		   add_symbols((yyval.node));
		   /* Set up a scope alias */
		   Swig_symbol_alias((yyvsp[-3].id),Getattr(n,"symtab"));
		 }
	       }
             }
#line 8775 "y.tab.c"
    break;

  case 198: /* cpp_members: cpp_member cpp_members  */
#line 4656 "parser.y"
                                      {
                   (yyval.node) = (yyvsp[-1].node);
                   /* Insert cpp_member (including any siblings) to the front of the cpp_members linked list */
		   if ((yyval.node)) {
		     Node *p = (yyval.node);
		     Node *pp =0;
		     while (p) {
		       pp = p;
		       p = nextSibling(p);
		     }
		     set_nextSibling(pp,(yyvsp[0].node));
		     if ((yyvsp[0].node))
		       set_previousSibling((yyvsp[0].node), pp);
		   } else {
		     (yyval.node) = (yyvsp[0].node);
		   }
             }
#line 8797 "y.tab.c"
    break;

  case 199: /* $@10: %empty  */
#line 4673 "parser.y"
                             { 
	       extendmode = 1;
	       if (cplus_mode != CPLUS_PUBLIC) {
		 Swig_error(cparse_file,cparse_line,"%%extend can only be used in a public section\n");
	       }
             }
#line 8808 "y.tab.c"
    break;

  case 200: /* $@11: %empty  */
#line 4678 "parser.y"
                                  {
	       extendmode = 0;
	     }
#line 8816 "y.tab.c"
    break;

  case 201: /* cpp_members: EXTEND LBRACE $@10 cpp_members RBRACE $@11 cpp_members  */
#line 4680 "parser.y"
                           {
	       (yyval.node) = new_node("extend");
	       mark_nodes_as_extend((yyvsp[-3].node));
	       appendChild((yyval.node),(yyvsp[-3].node));
	       set_nextSibling((yyval.node),(yyvsp[0].node));
	     }
#line 8827 "y.tab.c"
    break;

  case 202: /* cpp_members: include_directive  */
#line 4686 "parser.y"
                                 { (yyval.node) = (yyvsp[0].node); }
#line 8833 "y.tab.c"
    break;

  case 203: /* cpp_members: empty  */
#line 4687 "parser.y"
                     { (yyval.node) = 0;}
#line 8839 "y.tab.c"
    break;

  case 204: /* $@12: %empty  */
#line 4688 "parser.y"
                     {
	       Swig_error(cparse_file,cparse_line,"Syntax error in input(3).\n");
	       Exit(EXIT_FAILURE);
	       }
#line 8848 "y.tab.c"
    break;

  case 205: /* cpp_members: error $@12 cpp_members  */
#line 4691 "parser.y"
                             { 
		 (yyval.node) = (yyvsp[0].node);
   	     }
#line 8856 "y.tab.c"
    break;

  case 206: /* cpp_member_no_dox: c_declaration  */
#line 4702 "parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 8862 "y.tab.c"
    break;

  case 207: /* cpp_member_no_dox: cpp_constructor_decl  */
#line 4703 "parser.y"
                                    { 
                 (yyval.node) = (yyvsp[0].node); 
		 if (extendmode && current_class) {
		   String *symname;
		   symname= make_name((yyval.node),Getattr((yyval.node),"name"), Getattr((yyval.node),"decl"));
		   if (Strcmp(symname,Getattr((yyval.node),"name")) == 0) {
		     /* No renaming operation.  Set name to class name */
		     Delete(yyrename);
		     yyrename = NewString(Getattr(current_class,"sym:name"));
		   } else {
		     Delete(yyrename);
		     yyrename = symname;
		   }
		 }
		 add_symbols((yyval.node));
                 default_arguments((yyval.node));
             }
#line 8884 "y.tab.c"
    break;

  case 208: /* cpp_member_no_dox: cpp_destructor_decl  */
#line 4720 "parser.y"
                                   { (yyval.node) = (yyvsp[0].node); }
#line 8890 "y.tab.c"
    break;

  case 209: /* cpp_member_no_dox: cpp_protection_decl  */
#line 4721 "parser.y"
                                   { (yyval.node) = (yyvsp[0].node); }
#line 8896 "y.tab.c"
    break;

  case 210: /* cpp_member_no_dox: cpp_swig_directive  */
#line 4722 "parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 8902 "y.tab.c"
    break;

  case 211: /* cpp_member_no_dox: cpp_conversion_operator  */
#line 4723 "parser.y"
                                       { (yyval.node) = (yyvsp[0].node); }
#line 8908 "y.tab.c"
    break;

  case 212: /* cpp_member_no_dox: cpp_forward_class_decl  */
#line 4724 "parser.y"
                                      { (yyval.node) = (yyvsp[0].node); }
#line 8914 "y.tab.c"
    break;

  case 213: /* cpp_member_no_dox: cpp_class_decl  */
#line 4725 "parser.y"
                              { (yyval.node) = (yyvsp[0].node); }
#line 8920 "y.tab.c"
    break;

  case 214: /* cpp_member_no_dox: storage_class idcolon SEMI  */
#line 4726 "parser.y"
                                          { (yyval.node) = 0; }
#line 8926 "y.tab.c"
    break;

  case 215: /* cpp_member_no_dox: cpp_using_decl  */
#line 4727 "parser.y"
                              { (yyval.node) = (yyvsp[0].node); }
#line 8932 "y.tab.c"
    break;

  case 216: /* cpp_member_no_dox: cpp_template_decl  */
#line 4728 "parser.y"
                                 { (yyval.node) = (yyvsp[0].node); }
#line 8938 "y.tab.c"
    break;

  case 217: /* cpp_member_no_dox: cpp_catch_decl  */
#line 4729 "parser.y"
                              { (yyval.node) = 0; }
#line 8944 "y.tab.c"
    break;

  case 218: /* cpp_member_no_dox: template_directive  */
#line 4730 "parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 8950 "y.tab.c"
    break;

  case 219: /* cpp_member_no_dox: warn_directive  */
#line 4731 "parser.y"
                              { (yyval.node) = (yyvsp[0].node); }
#line 8956 "y.tab.c"
    break;

  case 220: /* cpp_member_no_dox: anonymous_bitfield  */
#line 4732 "parser.y"
                                  { (yyval.node) = 0; }
#line 8962 "y.tab.c"
    break;

  case 221: /* cpp_member_no_dox: fragment_directive  */
#line 4733 "parser.y"
                                  {(yyval.node) = (yyvsp[0].node); }
#line 8968 "y.tab.c"
    break;

  case 222: /* cpp_member_no_dox: types_directive  */
#line 4734 "parser.y"
                               {(yyval.node) = (yyvsp[0].node); }
#line 8974 "y.tab.c"
    break;

  case 223: /* cpp_member_no_dox: SEMI  */
#line 4735 "parser.y"
                    { (yyval.node) = 0; }
#line 8980 "y.tab.c"
    break;

  case 224: /* cpp_member: cpp_member_no_dox  */
#line 4737 "parser.y"
                                 {
		(yyval.node) = (yyvsp[0].node);
	     }
#line 8988 "y.tab.c"
    break;

  case 225: /* cpp_member: DOXYGENSTRING cpp_member_no_dox  */
#line 4740 "parser.y"
                                               {
	         (yyval.node) = (yyvsp[0].node);
		 set_comment((yyvsp[0].node), (yyvsp[-1].str));
	     }
#line 8997 "y.tab.c"
    break;

  case 226: /* cpp_member: cpp_member_no_dox DOXYGENPOSTSTRING  */
#line 4744 "parser.y"
                                                   {
	         (yyval.node) = (yyvsp[-1].node);
		 set_comment((yyvsp[-1].node), (yyvsp[0].str));
	     }
#line 9006 "y.tab.c"
    break;

  case 227: /* cpp_constructor_decl: storage_class type LPAREN parms RPAREN ctor_end  */
#line 4756 "parser.y"
                                                                       {
              if (inclass || extendmode) {
		SwigType *decl = NewStringEmpty();
		(yyval.node) = new_node("constructor");
		Setattr((yyval.node),"storage",(yyvsp[-5].id));
		Setattr((yyval.node),"name",(yyvsp[-4].type));
		Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		SwigType_add_function(decl,(yyvsp[-2].pl));
		Setattr((yyval.node),"decl",decl);
		Setattr((yyval.node),"throws",(yyvsp[0].decl).throws);
		Setattr((yyval.node),"throw",(yyvsp[0].decl).throwf);
		Setattr((yyval.node),"noexcept",(yyvsp[0].decl).nexcept);
		Setattr((yyval.node),"final",(yyvsp[0].decl).final);
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
		SetFlag((yyval.node),"feature:new");
		if ((yyvsp[0].decl).defarg)
		  Setattr((yyval.node),"value",(yyvsp[0].decl).defarg);
	      } else {
		(yyval.node) = 0;
              }
              }
#line 9036 "y.tab.c"
    break;

  case 228: /* cpp_destructor_decl: NOT idtemplate LPAREN parms RPAREN cpp_end  */
#line 4785 "parser.y"
                                                                 {
               String *name = NewStringf("%s",(yyvsp[-4].str));
	       if (*(Char(name)) != '~') Insert(name,0,"~");
               (yyval.node) = new_node("destructor");
	       Setattr((yyval.node),"name",name);
	       Delete(name);
	       if (Len(scanner_ccode)) {
		 String *code = Copy(scanner_ccode);
		 Setattr((yyval.node),"code",code);
		 Delete(code);
	       }
	       {
		 String *decl = NewStringEmpty();
		 SwigType_add_function(decl,(yyvsp[-2].pl));
		 Setattr((yyval.node),"decl",decl);
		 Delete(decl);
	       }
	       Setattr((yyval.node),"throws",(yyvsp[0].dtype).throws);
	       Setattr((yyval.node),"throw",(yyvsp[0].dtype).throwf);
	       Setattr((yyval.node),"noexcept",(yyvsp[0].dtype).nexcept);
	       Setattr((yyval.node),"final",(yyvsp[0].dtype).final);
	       if ((yyvsp[0].dtype).val)
	         Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
	       if ((yyvsp[0].dtype).qualifier)
		 Swig_error(cparse_file, cparse_line, "Destructor %s %s cannot have a qualifier.\n", Swig_name_decl((yyval.node)), SwigType_str((yyvsp[0].dtype).qualifier, 0));
	       add_symbols((yyval.node));
	      }
#line 9068 "y.tab.c"
    break;

  case 229: /* cpp_destructor_decl: VIRTUAL NOT idtemplate LPAREN parms RPAREN cpp_vend  */
#line 4815 "parser.y"
                                                                    {
		String *name;
		(yyval.node) = new_node("destructor");
		Setattr((yyval.node),"storage","virtual");
	        name = NewStringf("%s",(yyvsp[-4].str));
		if (*(Char(name)) != '~') Insert(name,0,"~");
		Setattr((yyval.node),"name",name);
		Delete(name);
		Setattr((yyval.node),"throws",(yyvsp[0].dtype).throws);
		Setattr((yyval.node),"throw",(yyvsp[0].dtype).throwf);
		Setattr((yyval.node),"noexcept",(yyvsp[0].dtype).nexcept);
		Setattr((yyval.node),"final",(yyvsp[0].dtype).final);
		if ((yyvsp[0].dtype).val)
		  Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
		{
		  String *decl = NewStringEmpty();
		  SwigType_add_function(decl,(yyvsp[-2].pl));
		  Setattr((yyval.node),"decl",decl);
		  Delete(decl);
		}
		if ((yyvsp[0].dtype).qualifier)
		  Swig_error(cparse_file, cparse_line, "Destructor %s %s cannot have a qualifier.\n", Swig_name_decl((yyval.node)), SwigType_str((yyvsp[0].dtype).qualifier, 0));
		add_symbols((yyval.node));
	      }
#line 9102 "y.tab.c"
    break;

  case 230: /* cpp_conversion_operator: storage_class CONVERSIONOPERATOR type pointer LPAREN parms RPAREN cpp_vend  */
#line 4848 "parser.y"
                                                                                                     {
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[-5].type));
		 Setattr((yyval.node),"name",(yyvsp[-6].str));
		 Setattr((yyval.node),"storage",(yyvsp[-7].id));

		 SwigType_add_function((yyvsp[-4].type),(yyvsp[-2].pl));
		 if ((yyvsp[0].dtype).qualifier) {
		   SwigType_push((yyvsp[-4].type),(yyvsp[0].dtype).qualifier);
		 }
		 if ((yyvsp[0].dtype).val) {
		   Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
		 }
		 Setattr((yyval.node),"refqualifier",(yyvsp[0].dtype).refqualifier);
		 Setattr((yyval.node),"decl",(yyvsp[-4].type));
		 Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
              }
#line 9126 "y.tab.c"
    break;

  case 231: /* cpp_conversion_operator: storage_class CONVERSIONOPERATOR type AND LPAREN parms RPAREN cpp_vend  */
#line 4867 "parser.y"
                                                                                        {
		 SwigType *decl;
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[-5].type));
		 Setattr((yyval.node),"name",(yyvsp[-6].str));
		 Setattr((yyval.node),"storage",(yyvsp[-7].id));
		 decl = NewStringEmpty();
		 SwigType_add_reference(decl);
		 SwigType_add_function(decl,(yyvsp[-2].pl));
		 if ((yyvsp[0].dtype).qualifier) {
		   SwigType_push(decl,(yyvsp[0].dtype).qualifier);
		 }
		 if ((yyvsp[0].dtype).val) {
		   Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
		 }
		 Setattr((yyval.node),"refqualifier",(yyvsp[0].dtype).refqualifier);
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
	       }
#line 9152 "y.tab.c"
    break;

  case 232: /* cpp_conversion_operator: storage_class CONVERSIONOPERATOR type LAND LPAREN parms RPAREN cpp_vend  */
#line 4888 "parser.y"
                                                                                         {
		 SwigType *decl;
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[-5].type));
		 Setattr((yyval.node),"name",(yyvsp[-6].str));
		 Setattr((yyval.node),"storage",(yyvsp[-7].id));
		 decl = NewStringEmpty();
		 SwigType_add_rvalue_reference(decl);
		 SwigType_add_function(decl,(yyvsp[-2].pl));
		 if ((yyvsp[0].dtype).qualifier) {
		   SwigType_push(decl,(yyvsp[0].dtype).qualifier);
		 }
		 if ((yyvsp[0].dtype).val) {
		   Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
		 }
		 Setattr((yyval.node),"refqualifier",(yyvsp[0].dtype).refqualifier);
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
	       }
#line 9178 "y.tab.c"
    break;

  case 233: /* cpp_conversion_operator: storage_class CONVERSIONOPERATOR type pointer AND LPAREN parms RPAREN cpp_vend  */
#line 4910 "parser.y"
                                                                                                {
		 SwigType *decl;
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[-6].type));
		 Setattr((yyval.node),"name",(yyvsp[-7].str));
		 Setattr((yyval.node),"storage",(yyvsp[-8].id));
		 decl = NewStringEmpty();
		 SwigType_add_pointer(decl);
		 SwigType_add_reference(decl);
		 SwigType_add_function(decl,(yyvsp[-2].pl));
		 if ((yyvsp[0].dtype).qualifier) {
		   SwigType_push(decl,(yyvsp[0].dtype).qualifier);
		 }
		 if ((yyvsp[0].dtype).val) {
		   Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
		 }
		 Setattr((yyval.node),"refqualifier",(yyvsp[0].dtype).refqualifier);
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
	       }
#line 9205 "y.tab.c"
    break;

  case 234: /* cpp_conversion_operator: storage_class CONVERSIONOPERATOR type LPAREN parms RPAREN cpp_vend  */
#line 4933 "parser.y"
                                                                                   {
		String *t = NewStringEmpty();
		(yyval.node) = new_node("cdecl");
		Setattr((yyval.node),"type",(yyvsp[-4].type));
		Setattr((yyval.node),"name",(yyvsp[-5].str));
		 Setattr((yyval.node),"storage",(yyvsp[-6].id));
		SwigType_add_function(t,(yyvsp[-2].pl));
		if ((yyvsp[0].dtype).qualifier) {
		  SwigType_push(t,(yyvsp[0].dtype).qualifier);
		}
		if ((yyvsp[0].dtype).val) {
		  Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
		}
		Setattr((yyval.node),"refqualifier",(yyvsp[0].dtype).refqualifier);
		Setattr((yyval.node),"decl",t);
		Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		Setattr((yyval.node),"conversion_operator","1");
		add_symbols((yyval.node));
              }
#line 9229 "y.tab.c"
    break;

  case 235: /* cpp_catch_decl: CATCH LPAREN parms RPAREN LBRACE  */
#line 4956 "parser.y"
                                                  {
                 skip_balanced('{','}');
                 (yyval.node) = 0;
               }
#line 9238 "y.tab.c"
    break;

  case 236: /* cpp_static_assert: STATIC_ASSERT LPAREN  */
#line 4964 "parser.y"
                                         {
                skip_balanced('(',')');
                (yyval.node) = 0;
              }
#line 9247 "y.tab.c"
    break;

  case 237: /* cpp_protection_decl: PUBLIC COLON  */
#line 4971 "parser.y"
                                   { 
                (yyval.node) = new_node("access");
		Setattr((yyval.node),"kind","public");
                cplus_mode = CPLUS_PUBLIC;
              }
#line 9257 "y.tab.c"
    break;

  case 238: /* cpp_protection_decl: PRIVATE COLON  */
#line 4978 "parser.y"
                              { 
                (yyval.node) = new_node("access");
                Setattr((yyval.node),"kind","private");
		cplus_mode = CPLUS_PRIVATE;
	      }
#line 9267 "y.tab.c"
    break;

  case 239: /* cpp_protection_decl: PROTECTED COLON  */
#line 4986 "parser.y"
                                { 
		(yyval.node) = new_node("access");
		Setattr((yyval.node),"kind","protected");
		cplus_mode = CPLUS_PROTECTED;
	      }
#line 9277 "y.tab.c"
    break;

  case 240: /* cpp_swig_directive: pragma_directive  */
#line 4994 "parser.y"
                                     { (yyval.node) = (yyvsp[0].node); }
#line 9283 "y.tab.c"
    break;

  case 241: /* cpp_swig_directive: constant_directive  */
#line 4997 "parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 9289 "y.tab.c"
    break;

  case 242: /* cpp_swig_directive: name_directive  */
#line 5001 "parser.y"
                              { (yyval.node) = (yyvsp[0].node); }
#line 9295 "y.tab.c"
    break;

  case 243: /* cpp_swig_directive: rename_directive  */
#line 5004 "parser.y"
                                { (yyval.node) = (yyvsp[0].node); }
#line 9301 "y.tab.c"
    break;

  case 244: /* cpp_swig_directive: feature_directive  */
#line 5005 "parser.y"
                                 { (yyval.node) = (yyvsp[0].node); }
#line 9307 "y.tab.c"
    break;

  case 245: /* cpp_swig_directive: varargs_directive  */
#line 5006 "parser.y"
                                 { (yyval.node) = (yyvsp[0].node); }
#line 9313 "y.tab.c"
    break;

  case 246: /* cpp_swig_directive: insert_directive  */
#line 5007 "parser.y"
                                { (yyval.node) = (yyvsp[0].node); }
#line 9319 "y.tab.c"
    break;

  case 247: /* cpp_swig_directive: typemap_directive  */
#line 5008 "parser.y"
                                 { (yyval.node) = (yyvsp[0].node); }
#line 9325 "y.tab.c"
    break;

  case 248: /* cpp_swig_directive: apply_directive  */
#line 5009 "parser.y"
                               { (yyval.node) = (yyvsp[0].node); }
#line 9331 "y.tab.c"
    break;

  case 249: /* cpp_swig_directive: clear_directive  */
#line 5010 "parser.y"
                               { (yyval.node) = (yyvsp[0].node); }
#line 9337 "y.tab.c"
    break;

  case 250: /* cpp_swig_directive: echo_directive  */
#line 5011 "parser.y"
                              { (yyval.node) = (yyvsp[0].node); }
#line 9343 "y.tab.c"
    break;

  case 251: /* cpp_end: cpp_const SEMI  */
#line 5014 "parser.y"
                                {
	            Clear(scanner_ccode);
		    (yyval.dtype).val = 0;
		    (yyval.dtype).qualifier = (yyvsp[-1].dtype).qualifier;
		    (yyval.dtype).refqualifier = (yyvsp[-1].dtype).refqualifier;
		    (yyval.dtype).bitfield = 0;
		    (yyval.dtype).throws = (yyvsp[-1].dtype).throws;
		    (yyval.dtype).throwf = (yyvsp[-1].dtype).throwf;
		    (yyval.dtype).nexcept = (yyvsp[-1].dtype).nexcept;
		    (yyval.dtype).final = (yyvsp[-1].dtype).final;
               }
#line 9359 "y.tab.c"
    break;

  case 252: /* cpp_end: cpp_const EQUAL default_delete SEMI  */
#line 5025 "parser.y"
                                                     {
	            Clear(scanner_ccode);
		    (yyval.dtype).val = (yyvsp[-1].dtype).val;
		    (yyval.dtype).qualifier = (yyvsp[-3].dtype).qualifier;
		    (yyval.dtype).refqualifier = (yyvsp[-3].dtype).refqualifier;
		    (yyval.dtype).bitfield = 0;
		    (yyval.dtype).throws = (yyvsp[-3].dtype).throws;
		    (yyval.dtype).throwf = (yyvsp[-3].dtype).throwf;
		    (yyval.dtype).nexcept = (yyvsp[-3].dtype).nexcept;
		    (yyval.dtype).final = (yyvsp[-3].dtype).final;
               }
#line 9375 "y.tab.c"
    break;

  case 253: /* cpp_end: cpp_const LBRACE  */
#line 5036 "parser.y"
                                  { 
		    skip_balanced('{','}'); 
		    (yyval.dtype).val = 0;
		    (yyval.dtype).qualifier = (yyvsp[-1].dtype).qualifier;
		    (yyval.dtype).refqualifier = (yyvsp[-1].dtype).refqualifier;
		    (yyval.dtype).bitfield = 0;
		    (yyval.dtype).throws = (yyvsp[-1].dtype).throws;
		    (yyval.dtype).throwf = (yyvsp[-1].dtype).throwf;
		    (yyval.dtype).nexcept = (yyvsp[-1].dtype).nexcept;
		    (yyval.dtype).final = (yyvsp[-1].dtype).final;
	       }
#line 9391 "y.tab.c"
    break;

  case 254: /* cpp_vend: cpp_const SEMI  */
#line 5049 "parser.y"
                                { 
                     Clear(scanner_ccode);
                     (yyval.dtype).val = 0;
                     (yyval.dtype).qualifier = (yyvsp[-1].dtype).qualifier;
                     (yyval.dtype).refqualifier = (yyvsp[-1].dtype).refqualifier;
                     (yyval.dtype).bitfield = 0;
                     (yyval.dtype).throws = (yyvsp[-1].dtype).throws;
                     (yyval.dtype).throwf = (yyvsp[-1].dtype).throwf;
                     (yyval.dtype).nexcept = (yyvsp[-1].dtype).nexcept;
                     (yyval.dtype).final = (yyvsp[-1].dtype).final;
                }
#line 9407 "y.tab.c"
    break;

  case 255: /* cpp_vend: cpp_const EQUAL definetype SEMI  */
#line 5060 "parser.y"
                                                 { 
                     Clear(scanner_ccode);
                     (yyval.dtype).val = (yyvsp[-1].dtype).val;
                     (yyval.dtype).qualifier = (yyvsp[-3].dtype).qualifier;
                     (yyval.dtype).refqualifier = (yyvsp[-3].dtype).refqualifier;
                     (yyval.dtype).bitfield = 0;
                     (yyval.dtype).throws = (yyvsp[-3].dtype).throws; 
                     (yyval.dtype).throwf = (yyvsp[-3].dtype).throwf; 
                     (yyval.dtype).nexcept = (yyvsp[-3].dtype).nexcept;
                     (yyval.dtype).final = (yyvsp[-3].dtype).final;
               }
#line 9423 "y.tab.c"
    break;

  case 256: /* cpp_vend: cpp_const LBRACE  */
#line 5071 "parser.y"
                                  { 
                     skip_balanced('{','}');
                     (yyval.dtype).val = 0;
                     (yyval.dtype).qualifier = (yyvsp[-1].dtype).qualifier;
                     (yyval.dtype).refqualifier = (yyvsp[-1].dtype).refqualifier;
                     (yyval.dtype).bitfield = 0;
                     (yyval.dtype).throws = (yyvsp[-1].dtype).throws; 
                     (yyval.dtype).throwf = (yyvsp[-1].dtype).throwf; 
                     (yyval.dtype).nexcept = (yyvsp[-1].dtype).nexcept;
                     (yyval.dtype).final = (yyvsp[-1].dtype).final;
               }
#line 9439 "y.tab.c"
    break;

  case 257: /* anonymous_bitfield: storage_class anon_bitfield_type COLON expr SEMI  */
#line 5085 "parser.y"
                                                                       { }
#line 9445 "y.tab.c"
    break;

  case 258: /* anon_bitfield_type: primitive_type  */
#line 5088 "parser.y"
                                    { (yyval.type) = (yyvsp[0].type);
                  /* Printf(stdout,"primitive = '%s'\n", $$);*/
                }
#line 9453 "y.tab.c"
    break;

  case 259: /* anon_bitfield_type: TYPE_BOOL  */
#line 5091 "parser.y"
                           { (yyval.type) = (yyvsp[0].type); }
#line 9459 "y.tab.c"
    break;

  case 260: /* anon_bitfield_type: TYPE_VOID  */
#line 5092 "parser.y"
                           { (yyval.type) = (yyvsp[0].type); }
#line 9465 "y.tab.c"
    break;

  case 261: /* anon_bitfield_type: TYPE_RAW  */
#line 5096 "parser.y"
                          { (yyval.type) = (yyvsp[0].type); }
#line 9471 "y.tab.c"
    break;

  case 262: /* anon_bitfield_type: idcolon  */
#line 5098 "parser.y"
                         {
		  (yyval.type) = (yyvsp[0].str);
               }
#line 9479 "y.tab.c"
    break;

  case 263: /* extern_string: EXTERN string  */
#line 5106 "parser.y"
                               {
                   if (Strcmp((yyvsp[0].str),"C") == 0) {
		     (yyval.id) = "externc";
                   } else if (Strcmp((yyvsp[0].str),"C++") == 0) {
		     (yyval.id) = "extern";
		   } else {
		     Swig_warning(WARN_PARSE_UNDEFINED_EXTERN,cparse_file, cparse_line,"Unrecognized extern type \"%s\".\n", (yyvsp[0].str));
		     (yyval.id) = 0;
		   }
               }
#line 9494 "y.tab.c"
    break;

  case 264: /* storage_class: EXTERN  */
#line 5118 "parser.y"
                        { (yyval.id) = "extern"; }
#line 9500 "y.tab.c"
    break;

  case 265: /* storage_class: extern_string  */
#line 5119 "parser.y"
                               { (yyval.id) = (yyvsp[0].id); }
#line 9506 "y.tab.c"
    break;

  case 266: /* storage_class: extern_string THREAD_LOCAL  */
#line 5120 "parser.y"
                                            {
                if (Equal((yyvsp[-1].id), "extern")) {
                  (yyval.id) = "extern thread_local";
                } else {
                  (yyval.id) = "externc thread_local";
                }
	       }
#line 9518 "y.tab.c"
    break;

  case 267: /* storage_class: extern_string TYPEDEF  */
#line 5127 "parser.y"
                                       { (yyval.id) = "typedef"; }
#line 9524 "y.tab.c"
    break;

  case 268: /* storage_class: STATIC  */
#line 5128 "parser.y"
                        { (yyval.id) = "static"; }
#line 9530 "y.tab.c"
    break;

  case 269: /* storage_class: TYPEDEF  */
#line 5129 "parser.y"
                         { (yyval.id) = "typedef"; }
#line 9536 "y.tab.c"
    break;

  case 270: /* storage_class: VIRTUAL  */
#line 5130 "parser.y"
                         { (yyval.id) = "virtual"; }
#line 9542 "y.tab.c"
    break;

  case 271: /* storage_class: FRIEND  */
#line 5131 "parser.y"
                        { (yyval.id) = "friend"; }
#line 9548 "y.tab.c"
    break;

  case 272: /* storage_class: EXPLICIT  */
#line 5132 "parser.y"
                          { (yyval.id) = "explicit"; }
#line 9554 "y.tab.c"
    break;

  case 273: /* storage_class: CONSTEXPR  */
#line 5133 "parser.y"
                           { (yyval.id) = "constexpr"; }
#line 9560 "y.tab.c"
    break;

  case 274: /* storage_class: EXPLICIT CONSTEXPR  */
#line 5134 "parser.y"
                                    { (yyval.id) = "explicit constexpr"; }
#line 9566 "y.tab.c"
    break;

  case 275: /* storage_class: CONSTEXPR EXPLICIT  */
#line 5135 "parser.y"
                                    { (yyval.id) = "explicit constexpr"; }
#line 9572 "y.tab.c"
    break;

  case 276: /* storage_class: STATIC CONSTEXPR  */
#line 5136 "parser.y"
                                  { (yyval.id) = "static constexpr"; }
#line 9578 "y.tab.c"
    break;

  case 277: /* storage_class: CONSTEXPR STATIC  */
#line 5137 "parser.y"
                                  { (yyval.id) = "static constexpr"; }
#line 9584 "y.tab.c"
    break;

  case 278: /* storage_class: THREAD_LOCAL  */
#line 5138 "parser.y"
                              { (yyval.id) = "thread_local"; }
#line 9590 "y.tab.c"
    break;

  case 279: /* storage_class: THREAD_LOCAL STATIC  */
#line 5139 "parser.y"
                                     { (yyval.id) = "static thread_local"; }
#line 9596 "y.tab.c"
    break;

  case 280: /* storage_class: STATIC THREAD_LOCAL  */
#line 5140 "parser.y"
                                     { (yyval.id) = "static thread_local"; }
#line 9602 "y.tab.c"
    break;

  case 281: /* storage_class: EXTERN THREAD_LOCAL  */
#line 5141 "parser.y"
                                     { (yyval.id) = "extern thread_local"; }
#line 9608 "y.tab.c"
    break;

  case 282: /* storage_class: THREAD_LOCAL EXTERN  */
#line 5142 "parser.y"
                                     { (yyval.id) = "extern thread_local"; }
#line 9614 "y.tab.c"
    break;

  case 283: /* storage_class: empty  */
#line 5143 "parser.y"
                       { (yyval.id) = 0; }
#line 9620 "y.tab.c"
    break;

  case 284: /* parms: rawparms  */
#line 5150 "parser.y"
                          {
                 Parm *p;
		 (yyval.pl) = (yyvsp[0].pl);
		 p = (yyvsp[0].pl);
                 while (p) {
		   Replace(Getattr(p,"type"),"typename ", "", DOH_REPLACE_ANY);
		   p = nextSibling(p);
                 }
               }
#line 9634 "y.tab.c"
    break;

  case 285: /* rawparms: parm ptail  */
#line 5161 "parser.y"
                               {
                  set_nextSibling((yyvsp[-1].p),(yyvsp[0].pl));
                  (yyval.pl) = (yyvsp[-1].p);
		}
#line 9643 "y.tab.c"
    break;

  case 286: /* rawparms: empty  */
#line 5165 "parser.y"
                       {
		  (yyval.pl) = 0;
		  previousNode = currentNode;
		  currentNode=0;
	       }
#line 9653 "y.tab.c"
    break;

  case 287: /* ptail: COMMA parm ptail  */
#line 5172 "parser.y"
                                  {
                 set_nextSibling((yyvsp[-1].p),(yyvsp[0].pl));
		 (yyval.pl) = (yyvsp[-1].p);
                }
#line 9662 "y.tab.c"
    break;

  case 288: /* ptail: COMMA DOXYGENPOSTSTRING parm ptail  */
#line 5176 "parser.y"
                                                    {
		 set_comment(previousNode, (yyvsp[-2].str));
                 set_nextSibling((yyvsp[-1].p), (yyvsp[0].pl));
		 (yyval.pl) = (yyvsp[-1].p);
               }
#line 9672 "y.tab.c"
    break;

  case 289: /* ptail: empty  */
#line 5181 "parser.y"
                       { (yyval.pl) = 0; }
#line 9678 "y.tab.c"
    break;

  case 290: /* parm_no_dox: rawtype parameter_declarator  */
#line 5185 "parser.y"
                                               {
                   SwigType_push((yyvsp[-1].type),(yyvsp[0].decl).type);
		   (yyval.p) = NewParmWithoutFileLineInfo((yyvsp[-1].type),(yyvsp[0].decl).id);
		   previousNode = currentNode;
		   currentNode = (yyval.p);
		   Setfile((yyval.p),cparse_file);
		   Setline((yyval.p),cparse_line);
		   if ((yyvsp[0].decl).defarg) {
		     Setattr((yyval.p),"value",(yyvsp[0].decl).defarg);
		   }
		}
#line 9694 "y.tab.c"
    break;

  case 291: /* parm_no_dox: TEMPLATE LESSTHAN cpptype GREATERTHAN cpptype idcolon def_args  */
#line 5197 "parser.y"
                                                                                 {
                  (yyval.p) = NewParmWithoutFileLineInfo(NewStringf("template<class> %s %s", (yyvsp[-2].id),(yyvsp[-1].str)), 0);
		  previousNode = currentNode;
		  currentNode = (yyval.p);
		  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
                  if ((yyvsp[0].dtype).val) {
                    Setattr((yyval.p),"value",(yyvsp[0].dtype).val);
                  }
                }
#line 9709 "y.tab.c"
    break;

  case 292: /* parm_no_dox: ELLIPSIS  */
#line 5207 "parser.y"
                           {
		  SwigType *t = NewString("v(...)");
		  (yyval.p) = NewParmWithoutFileLineInfo(t, 0);
		  previousNode = currentNode;
		  currentNode = (yyval.p);
		  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
		}
#line 9722 "y.tab.c"
    break;

  case 293: /* parm: parm_no_dox  */
#line 5217 "parser.y"
                              {
		  (yyval.p) = (yyvsp[0].p);
		}
#line 9730 "y.tab.c"
    break;

  case 294: /* parm: DOXYGENSTRING parm_no_dox  */
#line 5220 "parser.y"
                                            {
		  (yyval.p) = (yyvsp[0].p);
		  set_comment((yyvsp[0].p), (yyvsp[-1].str));
		}
#line 9739 "y.tab.c"
    break;

  case 295: /* parm: parm_no_dox DOXYGENPOSTSTRING  */
#line 5224 "parser.y"
                                                {
		  (yyval.p) = (yyvsp[-1].p);
		  set_comment((yyvsp[-1].p), (yyvsp[0].str));
		}
#line 9748 "y.tab.c"
    break;

  case 296: /* valparms: rawvalparms  */
#line 5230 "parser.y"
                              {
                 Parm *p;
		 (yyval.p) = (yyvsp[0].p);
		 p = (yyvsp[0].p);
                 while (p) {
		   if (Getattr(p,"type")) {
		     Replace(Getattr(p,"type"),"typename ", "", DOH_REPLACE_ANY);
		   }
		   p = nextSibling(p);
                 }
               }
#line 9764 "y.tab.c"
    break;

  case 297: /* rawvalparms: valparm valptail  */
#line 5243 "parser.y"
                                   {
                  set_nextSibling((yyvsp[-1].p),(yyvsp[0].p));
                  (yyval.p) = (yyvsp[-1].p);
		}
#line 9773 "y.tab.c"
    break;

  case 298: /* rawvalparms: empty  */
#line 5247 "parser.y"
                       { (yyval.p) = 0; }
#line 9779 "y.tab.c"
    break;

  case 299: /* valptail: COMMA valparm valptail  */
#line 5250 "parser.y"
                                        {
                 set_nextSibling((yyvsp[-1].p),(yyvsp[0].p));
		 (yyval.p) = (yyvsp[-1].p);
                }
#line 9788 "y.tab.c"
    break;

  case 300: /* valptail: empty  */
#line 5254 "parser.y"
                       { (yyval.p) = 0; }
#line 9794 "y.tab.c"
    break;

  case 301: /* valparm: parm  */
#line 5258 "parser.y"
                      {
		  (yyval.p) = (yyvsp[0].p);
		  {
		    /* We need to make a possible adjustment for integer parameters. */
		    SwigType *type;
		    Node     *n = 0;

		    while (!n) {
		      type = Getattr((yyvsp[0].p),"type");
		      n = Swig_symbol_clookup(type,0);     /* See if we can find a node that matches the typename */
		      if ((n) && (Strcmp(nodeType(n),"cdecl") == 0)) {
			SwigType *decl = Getattr(n,"decl");
			if (!SwigType_isfunction(decl)) {
			  String *value = Getattr(n,"value");
			  if (value) {
			    String *v = Copy(value);
			    Setattr((yyvsp[0].p),"type",v);
			    Delete(v);
			    n = 0;
			  }
			}
		      } else {
			break;
		      }
		    }
		  }

               }
#line 9827 "y.tab.c"
    break;

  case 302: /* valparm: valexpr  */
#line 5286 "parser.y"
                         {
                  (yyval.p) = NewParmWithoutFileLineInfo(0,0);
                  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
		  Setattr((yyval.p),"value",(yyvsp[0].dtype).val);
               }
#line 9838 "y.tab.c"
    break;

  case 303: /* callparms: valexpr callptail  */
#line 5294 "parser.y"
                                   {
		 (yyval.dtype) = (yyvsp[-1].dtype);
		 Printf((yyval.dtype).val, "%s", (yyvsp[0].dtype));
	       }
#line 9847 "y.tab.c"
    break;

  case 304: /* callparms: empty  */
#line 5298 "parser.y"
                       { (yyval.dtype).val = NewStringEmpty(); }
#line 9853 "y.tab.c"
    break;

  case 305: /* callptail: COMMA valexpr callptail  */
#line 5301 "parser.y"
                                         {
		 (yyval.dtype).val = NewStringf(",%s%s", (yyvsp[-1].dtype), (yyvsp[0].dtype));
		 (yyval.dtype).type = 0;
	       }
#line 9862 "y.tab.c"
    break;

  case 306: /* callptail: empty  */
#line 5305 "parser.y"
                       { (yyval.dtype).val = NewStringEmpty(); }
#line 9868 "y.tab.c"
    break;

  case 307: /* def_args: EQUAL definetype  */
#line 5308 "parser.y"
                                  { 
                  (yyval.dtype) = (yyvsp[0].dtype); 
		  if ((yyvsp[0].dtype).type == T_ERROR) {
		    Swig_warning(WARN_PARSE_BAD_DEFAULT,cparse_file, cparse_line, "Can't set default argument (ignored)\n");
		    (yyval.dtype).val = 0;
		    (yyval.dtype).rawval = 0;
		    (yyval.dtype).bitfield = 0;
		    (yyval.dtype).throws = 0;
		    (yyval.dtype).throwf = 0;
		    (yyval.dtype).nexcept = 0;
		    (yyval.dtype).final = 0;
		  }
               }
#line 9886 "y.tab.c"
    break;

  case 308: /* def_args: EQUAL definetype LBRACKET expr RBRACKET  */
#line 5321 "parser.y"
                                                         { 
		  (yyval.dtype) = (yyvsp[-3].dtype);
		  if ((yyvsp[-3].dtype).type == T_ERROR) {
		    Swig_warning(WARN_PARSE_BAD_DEFAULT,cparse_file, cparse_line, "Can't set default argument (ignored)\n");
		    (yyval.dtype) = (yyvsp[-3].dtype);
		    (yyval.dtype).val = 0;
		    (yyval.dtype).rawval = 0;
		    (yyval.dtype).bitfield = 0;
		    (yyval.dtype).throws = 0;
		    (yyval.dtype).throwf = 0;
		    (yyval.dtype).nexcept = 0;
		    (yyval.dtype).final = 0;
		  } else {
		    (yyval.dtype).val = NewStringf("%s[%s]",(yyvsp[-3].dtype).val,(yyvsp[-1].dtype).val); 
		  }		  
               }
#line 9907 "y.tab.c"
    break;

  case 309: /* def_args: EQUAL LBRACE  */
#line 5337 "parser.y"
                              {
		 skip_balanced('{','}');
		 (yyval.dtype).val = NewString(scanner_ccode);
		 (yyval.dtype).rawval = 0;
                 (yyval.dtype).type = T_INT;
		 (yyval.dtype).bitfield = 0;
		 (yyval.dtype).throws = 0;
		 (yyval.dtype).throwf = 0;
		 (yyval.dtype).nexcept = 0;
		 (yyval.dtype).final = 0;
	       }
#line 9923 "y.tab.c"
    break;

  case 310: /* def_args: COLON expr  */
#line 5348 "parser.y"
                            { 
		 (yyval.dtype).val = 0;
		 (yyval.dtype).rawval = 0;
		 (yyval.dtype).type = 0;
		 (yyval.dtype).bitfield = (yyvsp[0].dtype).val;
		 (yyval.dtype).throws = 0;
		 (yyval.dtype).throwf = 0;
		 (yyval.dtype).nexcept = 0;
		 (yyval.dtype).final = 0;
	       }
#line 9938 "y.tab.c"
    break;

  case 311: /* def_args: empty  */
#line 5358 "parser.y"
                       {
                 (yyval.dtype).val = 0;
                 (yyval.dtype).rawval = 0;
                 (yyval.dtype).type = T_INT;
		 (yyval.dtype).bitfield = 0;
		 (yyval.dtype).throws = 0;
		 (yyval.dtype).throwf = 0;
		 (yyval.dtype).nexcept = 0;
		 (yyval.dtype).final = 0;
               }
#line 9953 "y.tab.c"
    break;

  case 312: /* parameter_declarator: declarator def_args  */
#line 5370 "parser.y"
                                           {
                 (yyval.decl) = (yyvsp[-1].decl);
		 (yyval.decl).defarg = (yyvsp[0].dtype).rawval ? (yyvsp[0].dtype).rawval : (yyvsp[0].dtype).val;
            }
#line 9962 "y.tab.c"
    break;

  case 313: /* parameter_declarator: abstract_declarator def_args  */
#line 5374 "parser.y"
                                           {
              (yyval.decl) = (yyvsp[-1].decl);
	      (yyval.decl).defarg = (yyvsp[0].dtype).rawval ? (yyvsp[0].dtype).rawval : (yyvsp[0].dtype).val;
            }
#line 9971 "y.tab.c"
    break;

  case 314: /* parameter_declarator: def_args  */
#line 5378 "parser.y"
                       {
   	      (yyval.decl).type = 0;
              (yyval.decl).id = 0;
	      (yyval.decl).defarg = (yyvsp[0].dtype).rawval ? (yyvsp[0].dtype).rawval : (yyvsp[0].dtype).val;
            }
#line 9981 "y.tab.c"
    break;

  case 315: /* parameter_declarator: direct_declarator LPAREN parms RPAREN cv_ref_qualifier  */
#line 5385 "parser.y"
                                                                     {
	      SwigType *t;
	      (yyval.decl) = (yyvsp[-4].decl);
	      t = NewStringEmpty();
	      SwigType_add_function(t,(yyvsp[-2].pl));
	      if ((yyvsp[0].dtype).qualifier)
	        SwigType_push(t, (yyvsp[0].dtype).qualifier);
	      if (!(yyval.decl).have_parms) {
		(yyval.decl).parms = (yyvsp[-2].pl);
		(yyval.decl).have_parms = 1;
	      }
	      if (!(yyval.decl).type) {
		(yyval.decl).type = t;
	      } else {
		SwigType_push(t, (yyval.decl).type);
		Delete((yyval.decl).type);
		(yyval.decl).type = t;
	      }
	      (yyval.decl).defarg = 0;
	    }
#line 10006 "y.tab.c"
    break;

  case 316: /* plain_declarator: declarator  */
#line 5407 "parser.y"
                              {
                 (yyval.decl) = (yyvsp[0].decl);
		 if (SwigType_isfunction((yyvsp[0].decl).type)) {
		   Delete(SwigType_pop_function((yyvsp[0].decl).type));
		 } else if (SwigType_isarray((yyvsp[0].decl).type)) {
		   SwigType *ta = SwigType_pop_arrays((yyvsp[0].decl).type);
		   if (SwigType_isfunction((yyvsp[0].decl).type)) {
		     Delete(SwigType_pop_function((yyvsp[0].decl).type));
		   } else {
		     (yyval.decl).parms = 0;
		   }
		   SwigType_push((yyvsp[0].decl).type,ta);
		   Delete(ta);
		 } else {
		   (yyval.decl).parms = 0;
		 }
            }
#line 10028 "y.tab.c"
    break;

  case 317: /* plain_declarator: abstract_declarator  */
#line 5424 "parser.y"
                                  {
              (yyval.decl) = (yyvsp[0].decl);
	      if (SwigType_isfunction((yyvsp[0].decl).type)) {
		Delete(SwigType_pop_function((yyvsp[0].decl).type));
	      } else if (SwigType_isarray((yyvsp[0].decl).type)) {
		SwigType *ta = SwigType_pop_arrays((yyvsp[0].decl).type);
		if (SwigType_isfunction((yyvsp[0].decl).type)) {
		  Delete(SwigType_pop_function((yyvsp[0].decl).type));
		} else {
		  (yyval.decl).parms = 0;
		}
		SwigType_push((yyvsp[0].decl).type,ta);
		Delete(ta);
	      } else {
		(yyval.decl).parms = 0;
	      }
            }
#line 10050 "y.tab.c"
    break;

  case 318: /* plain_declarator: direct_declarator LPAREN parms RPAREN cv_ref_qualifier  */
#line 5443 "parser.y"
                                                                     {
	      SwigType *t;
	      (yyval.decl) = (yyvsp[-4].decl);
	      t = NewStringEmpty();
	      SwigType_add_function(t, (yyvsp[-2].pl));
	      if ((yyvsp[0].dtype).qualifier)
	        SwigType_push(t, (yyvsp[0].dtype).qualifier);
	      if (!(yyval.decl).have_parms) {
		(yyval.decl).parms = (yyvsp[-2].pl);
		(yyval.decl).have_parms = 1;
	      }
	      if (!(yyval.decl).type) {
		(yyval.decl).type = t;
	      } else {
		SwigType_push(t, (yyval.decl).type);
		Delete((yyval.decl).type);
		(yyval.decl).type = t;
	      }
	    }
#line 10074 "y.tab.c"
    break;

  case 319: /* plain_declarator: empty  */
#line 5462 "parser.y"
                    {
   	      (yyval.decl).type = 0;
              (yyval.decl).id = 0;
	      (yyval.decl).parms = 0;
	      }
#line 10084 "y.tab.c"
    break;

  case 320: /* declarator: pointer notso_direct_declarator  */
#line 5469 "parser.y"
                                              {
              (yyval.decl) = (yyvsp[0].decl);
	      if ((yyval.decl).type) {
		SwigType_push((yyvsp[-1].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-1].type);
           }
#line 10097 "y.tab.c"
    break;

  case 321: /* declarator: pointer AND notso_direct_declarator  */
#line 5477 "parser.y"
                                                 {
              (yyval.decl) = (yyvsp[0].decl);
	      SwigType_add_reference((yyvsp[-2].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-2].type);
           }
#line 10111 "y.tab.c"
    break;

  case 322: /* declarator: pointer LAND notso_direct_declarator  */
#line 5486 "parser.y"
                                                  {
              (yyval.decl) = (yyvsp[0].decl);
	      SwigType_add_rvalue_reference((yyvsp[-2].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-2].type);
           }
#line 10125 "y.tab.c"
    break;

  case 323: /* declarator: direct_declarator  */
#line 5495 "parser.y"
                               {
              (yyval.decl) = (yyvsp[0].decl);
	      if (!(yyval.decl).type) (yyval.decl).type = NewStringEmpty();
           }
#line 10134 "y.tab.c"
    break;

  case 324: /* declarator: AND notso_direct_declarator  */
#line 5499 "parser.y"
                                         {
	     (yyval.decl) = (yyvsp[0].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_reference((yyval.decl).type);
	     if ((yyvsp[0].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
	       Delete((yyvsp[0].decl).type);
	     }
           }
#line 10148 "y.tab.c"
    break;

  case 325: /* declarator: LAND notso_direct_declarator  */
#line 5508 "parser.y"
                                          {
	     /* Introduced in C++11, move operator && */
             /* Adds one S/R conflict */
	     (yyval.decl) = (yyvsp[0].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_rvalue_reference((yyval.decl).type);
	     if ((yyvsp[0].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
	       Delete((yyvsp[0].decl).type);
	     }
           }
#line 10164 "y.tab.c"
    break;

  case 326: /* declarator: idcolon DSTAR notso_direct_declarator  */
#line 5519 "parser.y"
                                                   { 
	     SwigType *t = NewStringEmpty();

	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-2].str));
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = t;
	     }
#line 10180 "y.tab.c"
    break;

  case 327: /* declarator: pointer idcolon DSTAR notso_direct_declarator  */
#line 5530 "parser.y"
                                                           { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-2].str));
	     SwigType_push((yyvsp[-3].type),t);
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-3].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-3].type);
	     Delete(t);
	   }
#line 10197 "y.tab.c"
    break;

  case 328: /* declarator: pointer idcolon DSTAR AND notso_direct_declarator  */
#line 5542 "parser.y"
                                                               { 
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer((yyvsp[-4].type),(yyvsp[-3].str));
	     SwigType_add_reference((yyvsp[-4].type));
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-4].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-4].type);
	   }
#line 10212 "y.tab.c"
    break;

  case 329: /* declarator: idcolon DSTAR AND notso_direct_declarator  */
#line 5552 "parser.y"
                                                       { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-3].str));
	     SwigType_add_reference(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     } 
	     (yyval.decl).type = t;
	   }
#line 10228 "y.tab.c"
    break;

  case 330: /* declarator: pointer ELLIPSIS notso_direct_declarator  */
#line 5566 "parser.y"
                                                       {
              (yyval.decl) = (yyvsp[0].decl);
	      if ((yyval.decl).type) {
		SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-2].type);
           }
#line 10241 "y.tab.c"
    break;

  case 331: /* declarator: pointer AND ELLIPSIS notso_direct_declarator  */
#line 5574 "parser.y"
                                                          {
              (yyval.decl) = (yyvsp[0].decl);
	      SwigType_add_reference((yyvsp[-3].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[-3].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-3].type);
           }
#line 10255 "y.tab.c"
    break;

  case 332: /* declarator: pointer LAND ELLIPSIS notso_direct_declarator  */
#line 5583 "parser.y"
                                                           {
              (yyval.decl) = (yyvsp[0].decl);
	      SwigType_add_rvalue_reference((yyvsp[-3].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[-3].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-3].type);
           }
#line 10269 "y.tab.c"
    break;

  case 333: /* declarator: ELLIPSIS direct_declarator  */
#line 5592 "parser.y"
                                        {
              (yyval.decl) = (yyvsp[0].decl);
	      if (!(yyval.decl).type) (yyval.decl).type = NewStringEmpty();
           }
#line 10278 "y.tab.c"
    break;

  case 334: /* declarator: AND ELLIPSIS notso_direct_declarator  */
#line 5596 "parser.y"
                                                  {
	     (yyval.decl) = (yyvsp[0].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_reference((yyval.decl).type);
	     if ((yyvsp[0].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
	       Delete((yyvsp[0].decl).type);
	     }
           }
#line 10292 "y.tab.c"
    break;

  case 335: /* declarator: LAND ELLIPSIS notso_direct_declarator  */
#line 5605 "parser.y"
                                                   {
	     /* Introduced in C++11, move operator && */
             /* Adds one S/R conflict */
	     (yyval.decl) = (yyvsp[0].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_rvalue_reference((yyval.decl).type);
	     if ((yyvsp[0].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
	       Delete((yyvsp[0].decl).type);
	     }
           }
#line 10308 "y.tab.c"
    break;

  case 336: /* declarator: idcolon DSTAR ELLIPSIS notso_direct_declarator  */
#line 5616 "parser.y"
                                                            {
	     SwigType *t = NewStringEmpty();

	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-3].str));
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = t;
	     }
#line 10324 "y.tab.c"
    break;

  case 337: /* declarator: pointer idcolon DSTAR ELLIPSIS notso_direct_declarator  */
#line 5627 "parser.y"
                                                                    {
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-3].str));
	     SwigType_push((yyvsp[-4].type),t);
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-4].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-4].type);
	     Delete(t);
	   }
#line 10341 "y.tab.c"
    break;

  case 338: /* declarator: pointer idcolon DSTAR AND ELLIPSIS notso_direct_declarator  */
#line 5639 "parser.y"
                                                                        {
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer((yyvsp[-5].type),(yyvsp[-4].str));
	     SwigType_add_reference((yyvsp[-5].type));
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-5].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-5].type);
	   }
#line 10356 "y.tab.c"
    break;

  case 339: /* declarator: pointer idcolon DSTAR LAND ELLIPSIS notso_direct_declarator  */
#line 5649 "parser.y"
                                                                         {
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer((yyvsp[-5].type),(yyvsp[-4].str));
	     SwigType_add_rvalue_reference((yyvsp[-5].type));
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-5].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-5].type);
	   }
#line 10371 "y.tab.c"
    break;

  case 340: /* declarator: idcolon DSTAR AND ELLIPSIS notso_direct_declarator  */
#line 5659 "parser.y"
                                                                {
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-4].str));
	     SwigType_add_reference(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     } 
	     (yyval.decl).type = t;
	   }
#line 10387 "y.tab.c"
    break;

  case 341: /* declarator: idcolon DSTAR LAND ELLIPSIS notso_direct_declarator  */
#line 5670 "parser.y"
                                                                 {
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-4].str));
	     SwigType_add_rvalue_reference(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     } 
	     (yyval.decl).type = t;
	   }
#line 10403 "y.tab.c"
    break;

  case 342: /* notso_direct_declarator: idcolon  */
#line 5683 "parser.y"
                                  {
                /* Note: This is non-standard C.  Template declarator is allowed to follow an identifier */
                 (yyval.decl).id = Char((yyvsp[0].str));
		 (yyval.decl).type = 0;
		 (yyval.decl).parms = 0;
		 (yyval.decl).have_parms = 0;
                  }
#line 10415 "y.tab.c"
    break;

  case 343: /* notso_direct_declarator: NOT idcolon  */
#line 5690 "parser.y"
                                {
                  (yyval.decl).id = Char(NewStringf("~%s",(yyvsp[0].str)));
                  (yyval.decl).type = 0;
                  (yyval.decl).parms = 0;
                  (yyval.decl).have_parms = 0;
                  }
#line 10426 "y.tab.c"
    break;

  case 344: /* notso_direct_declarator: LPAREN idcolon RPAREN  */
#line 5698 "parser.y"
                                         {
                  (yyval.decl).id = Char((yyvsp[-1].str));
                  (yyval.decl).type = 0;
                  (yyval.decl).parms = 0;
                  (yyval.decl).have_parms = 0;
                  }
#line 10437 "y.tab.c"
    break;

  case 345: /* notso_direct_declarator: LPAREN pointer notso_direct_declarator RPAREN  */
#line 5714 "parser.y"
                                                                  {
		    (yyval.decl) = (yyvsp[-1].decl);
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-2].type);
                  }
#line 10450 "y.tab.c"
    break;

  case 346: /* notso_direct_declarator: LPAREN idcolon DSTAR notso_direct_declarator RPAREN  */
#line 5722 "parser.y"
                                                                        {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-1].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t,(yyvsp[-3].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		    }
#line 10466 "y.tab.c"
    break;

  case 347: /* notso_direct_declarator: notso_direct_declarator LBRACKET RBRACKET  */
#line 5733 "parser.y"
                                                              { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-2].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 10482 "y.tab.c"
    break;

  case 348: /* notso_direct_declarator: notso_direct_declarator LBRACKET expr RBRACKET  */
#line 5744 "parser.y"
                                                                   { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[-1].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 10498 "y.tab.c"
    break;

  case 349: /* notso_direct_declarator: notso_direct_declarator LPAREN parms RPAREN  */
#line 5755 "parser.y"
                                                                {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[-1].pl));
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[-1].pl);
		      (yyval.decl).have_parms = 1;
		    }
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		  }
#line 10520 "y.tab.c"
    break;

  case 350: /* direct_declarator: idcolon  */
#line 5774 "parser.y"
                            {
                /* Note: This is non-standard C.  Template declarator is allowed to follow an identifier */
                 (yyval.decl).id = Char((yyvsp[0].str));
		 (yyval.decl).type = 0;
		 (yyval.decl).parms = 0;
		 (yyval.decl).have_parms = 0;
                  }
#line 10532 "y.tab.c"
    break;

  case 351: /* direct_declarator: NOT idcolon  */
#line 5782 "parser.y"
                                {
                  (yyval.decl).id = Char(NewStringf("~%s",(yyvsp[0].str)));
                  (yyval.decl).type = 0;
                  (yyval.decl).parms = 0;
                  (yyval.decl).have_parms = 0;
                  }
#line 10543 "y.tab.c"
    break;

  case 352: /* direct_declarator: LPAREN pointer direct_declarator RPAREN  */
#line 5799 "parser.y"
                                                            {
		    (yyval.decl) = (yyvsp[-1].decl);
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-2].type);
                  }
#line 10556 "y.tab.c"
    break;

  case 353: /* direct_declarator: LPAREN AND direct_declarator RPAREN  */
#line 5807 "parser.y"
                                                        {
                    (yyval.decl) = (yyvsp[-1].decl);
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = NewStringEmpty();
		    }
		    SwigType_add_reference((yyval.decl).type);
                  }
#line 10568 "y.tab.c"
    break;

  case 354: /* direct_declarator: LPAREN LAND direct_declarator RPAREN  */
#line 5814 "parser.y"
                                                         {
                    (yyval.decl) = (yyvsp[-1].decl);
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = NewStringEmpty();
		    }
		    SwigType_add_rvalue_reference((yyval.decl).type);
                  }
#line 10580 "y.tab.c"
    break;

  case 355: /* direct_declarator: LPAREN idcolon DSTAR declarator RPAREN  */
#line 5821 "parser.y"
                                                           {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-1].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t,(yyvsp[-3].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		  }
#line 10596 "y.tab.c"
    break;

  case 356: /* direct_declarator: LPAREN idcolon DSTAR type_qualifier declarator RPAREN  */
#line 5832 "parser.y"
                                                                          {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-1].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t, (yyvsp[-4].str));
		    SwigType_push(t, (yyvsp[-2].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		  }
#line 10613 "y.tab.c"
    break;

  case 357: /* direct_declarator: LPAREN idcolon DSTAR abstract_declarator RPAREN  */
#line 5844 "parser.y"
                                                                    {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-1].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t, (yyvsp[-3].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		  }
#line 10629 "y.tab.c"
    break;

  case 358: /* direct_declarator: LPAREN idcolon DSTAR type_qualifier abstract_declarator RPAREN  */
#line 5855 "parser.y"
                                                                                   {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-1].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t, (yyvsp[-4].str));
		    SwigType_push(t, (yyvsp[-2].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		  }
#line 10646 "y.tab.c"
    break;

  case 359: /* direct_declarator: direct_declarator LBRACKET RBRACKET  */
#line 5867 "parser.y"
                                                        { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-2].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 10662 "y.tab.c"
    break;

  case 360: /* direct_declarator: direct_declarator LBRACKET expr RBRACKET  */
#line 5878 "parser.y"
                                                             { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[-1].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 10678 "y.tab.c"
    break;

  case 361: /* direct_declarator: direct_declarator LPAREN parms RPAREN  */
#line 5889 "parser.y"
                                                          {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[-1].pl));
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[-1].pl);
		      (yyval.decl).have_parms = 1;
		    }
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		  }
#line 10700 "y.tab.c"
    break;

  case 362: /* direct_declarator: OPERATOR ID LPAREN parms RPAREN  */
#line 5909 "parser.y"
                                                   {
		    SwigType *t;
                    Append((yyvsp[-4].str), " "); /* intervening space is mandatory */
                    Append((yyvsp[-4].str), Char((yyvsp[-3].id)));
		    (yyval.decl).id = Char((yyvsp[-4].str));
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[-1].pl));
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[-1].pl);
		      (yyval.decl).have_parms = 1;
		    }
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		  }
#line 10724 "y.tab.c"
    break;

  case 363: /* abstract_declarator: pointer  */
#line 5930 "parser.y"
                              {
		    (yyval.decl).type = (yyvsp[0].type);
                    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
                  }
#line 10735 "y.tab.c"
    break;

  case 364: /* abstract_declarator: pointer direct_abstract_declarator  */
#line 5936 "parser.y"
                                                       { 
                     (yyval.decl) = (yyvsp[0].decl);
                     SwigType_push((yyvsp[-1].type),(yyvsp[0].decl).type);
		     (yyval.decl).type = (yyvsp[-1].type);
		     Delete((yyvsp[0].decl).type);
                  }
#line 10746 "y.tab.c"
    break;

  case 365: /* abstract_declarator: pointer AND  */
#line 5942 "parser.y"
                                {
		    (yyval.decl).type = (yyvsp[-1].type);
		    SwigType_add_reference((yyval.decl).type);
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		  }
#line 10758 "y.tab.c"
    break;

  case 366: /* abstract_declarator: pointer LAND  */
#line 5949 "parser.y"
                                 {
		    (yyval.decl).type = (yyvsp[-1].type);
		    SwigType_add_rvalue_reference((yyval.decl).type);
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		  }
#line 10770 "y.tab.c"
    break;

  case 367: /* abstract_declarator: pointer AND direct_abstract_declarator  */
#line 5956 "parser.y"
                                                           {
		    (yyval.decl) = (yyvsp[0].decl);
		    SwigType_add_reference((yyvsp[-2].type));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-2].type);
                  }
#line 10784 "y.tab.c"
    break;

  case 368: /* abstract_declarator: pointer LAND direct_abstract_declarator  */
#line 5965 "parser.y"
                                                            {
		    (yyval.decl) = (yyvsp[0].decl);
		    SwigType_add_rvalue_reference((yyvsp[-2].type));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-2].type);
                  }
#line 10798 "y.tab.c"
    break;

  case 369: /* abstract_declarator: direct_abstract_declarator  */
#line 5974 "parser.y"
                                               {
		    (yyval.decl) = (yyvsp[0].decl);
                  }
#line 10806 "y.tab.c"
    break;

  case 370: /* abstract_declarator: AND direct_abstract_declarator  */
#line 5977 "parser.y"
                                                   {
		    (yyval.decl) = (yyvsp[0].decl);
		    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_reference((yyval.decl).type);
		    if ((yyvsp[0].decl).type) {
		      SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
		      Delete((yyvsp[0].decl).type);
		    }
                  }
#line 10820 "y.tab.c"
    break;

  case 371: /* abstract_declarator: LAND direct_abstract_declarator  */
#line 5986 "parser.y"
                                                    {
		    (yyval.decl) = (yyvsp[0].decl);
		    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_rvalue_reference((yyval.decl).type);
		    if ((yyvsp[0].decl).type) {
		      SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
		      Delete((yyvsp[0].decl).type);
		    }
                  }
#line 10834 "y.tab.c"
    break;

  case 372: /* abstract_declarator: AND  */
#line 5995 "parser.y"
                        {
                    (yyval.decl).id = 0;
                    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
                    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_reference((yyval.decl).type);
                  }
#line 10846 "y.tab.c"
    break;

  case 373: /* abstract_declarator: LAND  */
#line 6002 "parser.y"
                         {
                    (yyval.decl).id = 0;
                    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
                    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_rvalue_reference((yyval.decl).type);
                  }
#line 10858 "y.tab.c"
    break;

  case 374: /* abstract_declarator: idcolon DSTAR  */
#line 6009 "parser.y"
                                  { 
		    (yyval.decl).type = NewStringEmpty();
                    SwigType_add_memberpointer((yyval.decl).type,(yyvsp[-1].str));
                    (yyval.decl).id = 0;
                    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
      	          }
#line 10870 "y.tab.c"
    break;

  case 375: /* abstract_declarator: idcolon DSTAR type_qualifier  */
#line 6016 "parser.y"
                                                 {
		    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_memberpointer((yyval.decl).type, (yyvsp[-2].str));
		    SwigType_push((yyval.decl).type, (yyvsp[0].str));
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		  }
#line 10883 "y.tab.c"
    break;

  case 376: /* abstract_declarator: pointer idcolon DSTAR  */
#line 6024 "parser.y"
                                          { 
		    SwigType *t = NewStringEmpty();
                    (yyval.decl).type = (yyvsp[-2].type);
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		    SwigType_add_memberpointer(t,(yyvsp[-1].str));
		    SwigType_push((yyval.decl).type,t);
		    Delete(t);
                  }
#line 10898 "y.tab.c"
    break;

  case 377: /* abstract_declarator: pointer idcolon DSTAR direct_abstract_declarator  */
#line 6034 "parser.y"
                                                                     { 
		    (yyval.decl) = (yyvsp[0].decl);
		    SwigType_add_memberpointer((yyvsp[-3].type),(yyvsp[-2].str));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-3].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-3].type);
                  }
#line 10912 "y.tab.c"
    break;

  case 378: /* direct_abstract_declarator: direct_abstract_declarator LBRACKET RBRACKET  */
#line 6045 "parser.y"
                                                                          { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-2].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 10928 "y.tab.c"
    break;

  case 379: /* direct_abstract_declarator: direct_abstract_declarator LBRACKET expr RBRACKET  */
#line 6056 "parser.y"
                                                                      { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[-1].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 10944 "y.tab.c"
    break;

  case 380: /* direct_abstract_declarator: LBRACKET RBRACKET  */
#line 6067 "parser.y"
                                      { 
		    (yyval.decl).type = NewStringEmpty();
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		    SwigType_add_array((yyval.decl).type,"");
                  }
#line 10956 "y.tab.c"
    break;

  case 381: /* direct_abstract_declarator: LBRACKET expr RBRACKET  */
#line 6074 "parser.y"
                                           { 
		    (yyval.decl).type = NewStringEmpty();
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		    SwigType_add_array((yyval.decl).type,(yyvsp[-1].dtype).val);
		  }
#line 10968 "y.tab.c"
    break;

  case 382: /* direct_abstract_declarator: LPAREN abstract_declarator RPAREN  */
#line 6081 "parser.y"
                                                      {
                    (yyval.decl) = (yyvsp[-1].decl);
		  }
#line 10976 "y.tab.c"
    break;

  case 383: /* direct_abstract_declarator: direct_abstract_declarator LPAREN parms RPAREN  */
#line 6084 "parser.y"
                                                                   {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
                    SwigType_add_function(t,(yyvsp[-1].pl));
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[-1].pl);
		      (yyval.decl).have_parms = 1;
		    }
		  }
#line 10998 "y.tab.c"
    break;

  case 384: /* direct_abstract_declarator: direct_abstract_declarator LPAREN parms RPAREN cv_ref_qualifier  */
#line 6101 "parser.y"
                                                                                    {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[-4].decl);
		    t = NewStringEmpty();
                    SwigType_add_function(t,(yyvsp[-2].pl));
		    SwigType_push(t, (yyvsp[0].dtype).qualifier);
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[-2].pl);
		      (yyval.decl).have_parms = 1;
		    }
		  }
#line 11021 "y.tab.c"
    break;

  case 385: /* direct_abstract_declarator: LPAREN parms RPAREN  */
#line 6119 "parser.y"
                                        {
                    (yyval.decl).type = NewStringEmpty();
                    SwigType_add_function((yyval.decl).type,(yyvsp[-1].pl));
		    (yyval.decl).parms = (yyvsp[-1].pl);
		    (yyval.decl).have_parms = 1;
		    (yyval.decl).id = 0;
                  }
#line 11033 "y.tab.c"
    break;

  case 386: /* pointer: STAR type_qualifier pointer  */
#line 6129 "parser.y"
                                         { 
             (yyval.type) = NewStringEmpty();
             SwigType_add_pointer((yyval.type));
	     SwigType_push((yyval.type),(yyvsp[-1].str));
	     SwigType_push((yyval.type),(yyvsp[0].type));
	     Delete((yyvsp[0].type));
           }
#line 11045 "y.tab.c"
    break;

  case 387: /* pointer: STAR pointer  */
#line 6136 "parser.y"
                          {
	     (yyval.type) = NewStringEmpty();
	     SwigType_add_pointer((yyval.type));
	     SwigType_push((yyval.type),(yyvsp[0].type));
	     Delete((yyvsp[0].type));
	   }
#line 11056 "y.tab.c"
    break;

  case 388: /* pointer: STAR type_qualifier  */
#line 6142 "parser.y"
                                 { 
	     (yyval.type) = NewStringEmpty();
	     SwigType_add_pointer((yyval.type));
	     SwigType_push((yyval.type),(yyvsp[0].str));
           }
#line 11066 "y.tab.c"
    break;

  case 389: /* pointer: STAR  */
#line 6147 "parser.y"
                  {
	     (yyval.type) = NewStringEmpty();
	     SwigType_add_pointer((yyval.type));
           }
#line 11075 "y.tab.c"
    break;

  case 390: /* cv_ref_qualifier: type_qualifier  */
#line 6154 "parser.y"
                                  {
		  (yyval.dtype).qualifier = (yyvsp[0].str);
		  (yyval.dtype).refqualifier = 0;
	       }
#line 11084 "y.tab.c"
    break;

  case 391: /* cv_ref_qualifier: type_qualifier ref_qualifier  */
#line 6158 "parser.y"
                                              {
		  (yyval.dtype).qualifier = (yyvsp[-1].str);
		  (yyval.dtype).refqualifier = (yyvsp[0].str);
		  SwigType_push((yyval.dtype).qualifier, (yyvsp[0].str));
	       }
#line 11094 "y.tab.c"
    break;

  case 392: /* cv_ref_qualifier: ref_qualifier  */
#line 6163 "parser.y"
                               {
		  (yyval.dtype).qualifier = NewStringEmpty();
		  (yyval.dtype).refqualifier = (yyvsp[0].str);
		  SwigType_push((yyval.dtype).qualifier, (yyvsp[0].str));
	       }
#line 11104 "y.tab.c"
    break;

  case 393: /* ref_qualifier: AND  */
#line 6170 "parser.y"
                    {
	          (yyval.str) = NewStringEmpty();
	          SwigType_add_reference((yyval.str));
	       }
#line 11113 "y.tab.c"
    break;

  case 394: /* ref_qualifier: LAND  */
#line 6174 "parser.y"
                      {
	          (yyval.str) = NewStringEmpty();
	          SwigType_add_rvalue_reference((yyval.str));
	       }
#line 11122 "y.tab.c"
    break;

  case 395: /* type_qualifier: type_qualifier_raw  */
#line 6180 "parser.y"
                                    {
	          (yyval.str) = NewStringEmpty();
	          if ((yyvsp[0].id)) SwigType_add_qualifier((yyval.str),(yyvsp[0].id));
               }
#line 11131 "y.tab.c"
    break;

  case 396: /* type_qualifier: type_qualifier_raw type_qualifier  */
#line 6184 "parser.y"
                                                   {
		  (yyval.str) = (yyvsp[0].str);
	          if ((yyvsp[-1].id)) SwigType_add_qualifier((yyval.str),(yyvsp[-1].id));
               }
#line 11140 "y.tab.c"
    break;

  case 397: /* type_qualifier_raw: CONST_QUAL  */
#line 6190 "parser.y"
                                 { (yyval.id) = "const"; }
#line 11146 "y.tab.c"
    break;

  case 398: /* type_qualifier_raw: VOLATILE  */
#line 6191 "parser.y"
                               { (yyval.id) = "volatile"; }
#line 11152 "y.tab.c"
    break;

  case 399: /* type_qualifier_raw: REGISTER  */
#line 6192 "parser.y"
                               { (yyval.id) = 0; }
#line 11158 "y.tab.c"
    break;

  case 400: /* type: rawtype  */
#line 6198 "parser.y"
                          {
                   (yyval.type) = (yyvsp[0].type);
                   Replace((yyval.type),"typename ","", DOH_REPLACE_ANY);
                }
#line 11167 "y.tab.c"
    break;

  case 401: /* rawtype: type_qualifier type_right  */
#line 6204 "parser.y"
                                           {
                   (yyval.type) = (yyvsp[0].type);
	           SwigType_push((yyval.type),(yyvsp[-1].str));
               }
#line 11176 "y.tab.c"
    break;

  case 402: /* rawtype: type_right  */
#line 6208 "parser.y"
                            { (yyval.type) = (yyvsp[0].type); }
#line 11182 "y.tab.c"
    break;

  case 403: /* rawtype: type_right type_qualifier  */
#line 6209 "parser.y"
                                           {
		  (yyval.type) = (yyvsp[-1].type);
	          SwigType_push((yyval.type),(yyvsp[0].str));
	       }
#line 11191 "y.tab.c"
    break;

  case 404: /* rawtype: type_qualifier type_right type_qualifier  */
#line 6213 "parser.y"
                                                          {
		  (yyval.type) = (yyvsp[-1].type);
	          SwigType_push((yyval.type),(yyvsp[0].str));
	          SwigType_push((yyval.type),(yyvsp[-2].str));
	       }
#line 11201 "y.tab.c"
    break;

  case 405: /* type_right: primitive_type  */
#line 6220 "parser.y"
                                { (yyval.type) = (yyvsp[0].type);
                  /* Printf(stdout,"primitive = '%s'\n", $$);*/
               }
#line 11209 "y.tab.c"
    break;

  case 406: /* type_right: TYPE_BOOL  */
#line 6223 "parser.y"
                           { (yyval.type) = (yyvsp[0].type); }
#line 11215 "y.tab.c"
    break;

  case 407: /* type_right: TYPE_VOID  */
#line 6224 "parser.y"
                           { (yyval.type) = (yyvsp[0].type); }
#line 11221 "y.tab.c"
    break;

  case 408: /* type_right: c_enum_key idcolon  */
#line 6228 "parser.y"
                                    { (yyval.type) = NewStringf("enum %s", (yyvsp[0].str)); }
#line 11227 "y.tab.c"
    break;

  case 409: /* type_right: TYPE_RAW  */
#line 6229 "parser.y"
                          { (yyval.type) = (yyvsp[0].type); }
#line 11233 "y.tab.c"
    break;

  case 410: /* type_right: idcolon  */
#line 6231 "parser.y"
                         {
		  (yyval.type) = (yyvsp[0].str);
               }
#line 11241 "y.tab.c"
    break;

  case 411: /* type_right: cpptype idcolon  */
#line 6234 "parser.y"
                                 { 
		 (yyval.type) = NewStringf("%s %s", (yyvsp[-1].id), (yyvsp[0].str));
               }
#line 11249 "y.tab.c"
    break;

  case 412: /* type_right: decltype  */
#line 6237 "parser.y"
                          {
                 (yyval.type) = (yyvsp[0].type);
               }
#line 11257 "y.tab.c"
    break;

  case 413: /* decltype: DECLTYPE LPAREN idcolon RPAREN  */
#line 6242 "parser.y"
                                                {
                 Node *n = Swig_symbol_clookup((yyvsp[-1].str),0);
                 if (!n) {
		   Swig_error(cparse_file, cparse_line, "Identifier %s not defined.\n", (yyvsp[-1].str));
                   (yyval.type) = (yyvsp[-1].str);
                 } else {
                   (yyval.type) = Getattr(n, "type");
                 }
               }
#line 11271 "y.tab.c"
    break;

  case 414: /* primitive_type: primitive_type_list  */
#line 6253 "parser.y"
                                     {
		 if (!(yyvsp[0].ptype).type) (yyvsp[0].ptype).type = NewString("int");
		 if ((yyvsp[0].ptype).us) {
		   (yyval.type) = NewStringf("%s %s", (yyvsp[0].ptype).us, (yyvsp[0].ptype).type);
		   Delete((yyvsp[0].ptype).us);
                   Delete((yyvsp[0].ptype).type);
		 } else {
                   (yyval.type) = (yyvsp[0].ptype).type;
		 }
		 if (Cmp((yyval.type),"signed int") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("int");
                 } else if (Cmp((yyval.type),"signed long") == 0) {
		   Delete((yyval.type));
                   (yyval.type) = NewString("long");
                 } else if (Cmp((yyval.type),"signed short") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("short");
		 } else if (Cmp((yyval.type),"signed long long") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("long long");
		 }
               }
#line 11299 "y.tab.c"
    break;

  case 415: /* primitive_type_list: type_specifier  */
#line 6278 "parser.y"
                                     { 
                 (yyval.ptype) = (yyvsp[0].ptype);
               }
#line 11307 "y.tab.c"
    break;

  case 416: /* primitive_type_list: type_specifier primitive_type_list  */
#line 6281 "parser.y"
                                                    {
                    if ((yyvsp[-1].ptype).us && (yyvsp[0].ptype).us) {
		      Swig_error(cparse_file, cparse_line, "Extra %s specifier.\n", (yyvsp[0].ptype).us);
		    }
                    (yyval.ptype) = (yyvsp[0].ptype);
                    if ((yyvsp[-1].ptype).us) (yyval.ptype).us = (yyvsp[-1].ptype).us;
		    if ((yyvsp[-1].ptype).type) {
		      if (!(yyvsp[0].ptype).type) (yyval.ptype).type = (yyvsp[-1].ptype).type;
		      else {
			int err = 0;
			if ((Cmp((yyvsp[-1].ptype).type,"long") == 0)) {
			  if ((Cmp((yyvsp[0].ptype).type,"long") == 0) || (Strncmp((yyvsp[0].ptype).type,"double",6) == 0)) {
			    (yyval.ptype).type = NewStringf("long %s", (yyvsp[0].ptype).type);
			  } else if (Cmp((yyvsp[0].ptype).type,"int") == 0) {
			    (yyval.ptype).type = (yyvsp[-1].ptype).type;
			  } else {
			    err = 1;
			  }
			} else if ((Cmp((yyvsp[-1].ptype).type,"short")) == 0) {
			  if (Cmp((yyvsp[0].ptype).type,"int") == 0) {
			    (yyval.ptype).type = (yyvsp[-1].ptype).type;
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[-1].ptype).type,"int") == 0) {
			  (yyval.ptype).type = (yyvsp[0].ptype).type;
			} else if (Cmp((yyvsp[-1].ptype).type,"double") == 0) {
			  if (Cmp((yyvsp[0].ptype).type,"long") == 0) {
			    (yyval.ptype).type = NewString("long double");
			  } else if (Cmp((yyvsp[0].ptype).type,"_Complex") == 0) {
			    (yyval.ptype).type = NewString("double _Complex");
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[-1].ptype).type,"float") == 0) {
			  if (Cmp((yyvsp[0].ptype).type,"_Complex") == 0) {
			    (yyval.ptype).type = NewString("float _Complex");
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[-1].ptype).type,"_Complex") == 0) {
			  (yyval.ptype).type = NewStringf("%s _Complex", (yyvsp[0].ptype).type);
			} else {
			  err = 1;
			}
			if (err) {
			  Swig_error(cparse_file, cparse_line, "Extra %s specifier.\n", (yyvsp[-1].ptype).type);
			}
		      }
		    }
               }
#line 11363 "y.tab.c"
    break;

  case 417: /* type_specifier: TYPE_INT  */
#line 6335 "parser.y"
                          { 
		    (yyval.ptype).type = NewString("int");
                    (yyval.ptype).us = 0;
               }
#line 11372 "y.tab.c"
    break;

  case 418: /* type_specifier: TYPE_SHORT  */
#line 6339 "parser.y"
                            { 
                    (yyval.ptype).type = NewString("short");
                    (yyval.ptype).us = 0;
                }
#line 11381 "y.tab.c"
    break;

  case 419: /* type_specifier: TYPE_LONG  */
#line 6343 "parser.y"
                           { 
                    (yyval.ptype).type = NewString("long");
                    (yyval.ptype).us = 0;
                }
#line 11390 "y.tab.c"
    break;

  case 420: /* type_specifier: TYPE_CHAR  */
#line 6347 "parser.y"
                           { 
                    (yyval.ptype).type = NewString("char");
                    (yyval.ptype).us = 0;
                }
#line 11399 "y.tab.c"
    break;

  case 421: /* type_specifier: TYPE_WCHAR  */
#line 6351 "parser.y"
                            { 
                    (yyval.ptype).type = NewString("wchar_t");
                    (yyval.ptype).us = 0;
                }
#line 11408 "y.tab.c"
    break;

  case 422: /* type_specifier: TYPE_FLOAT  */
#line 6355 "parser.y"
                            { 
                    (yyval.ptype).type = NewString("float");
                    (yyval.ptype).us = 0;
                }
#line 11417 "y.tab.c"
    break;

  case 423: /* type_specifier: TYPE_DOUBLE  */
#line 6359 "parser.y"
                             { 
                    (yyval.ptype).type = NewString("double");
                    (yyval.ptype).us = 0;
                }
#line 11426 "y.tab.c"
    break;

  case 424: /* type_specifier: TYPE_SIGNED  */
#line 6363 "parser.y"
                             { 
                    (yyval.ptype).us = NewString("signed");
                    (yyval.ptype).type = 0;
                }
#line 11435 "y.tab.c"
    break;

  case 425: /* type_specifier: TYPE_UNSIGNED  */
#line 6367 "parser.y"
                               { 
                    (yyval.ptype).us = NewString("unsigned");
                    (yyval.ptype).type = 0;
                }
#line 11444 "y.tab.c"
    break;

  case 426: /* type_specifier: TYPE_COMPLEX  */
#line 6371 "parser.y"
                              { 
                    (yyval.ptype).type = NewString("_Complex");
                    (yyval.ptype).us = 0;
                }
#line 11453 "y.tab.c"
    break;

  case 427: /* type_specifier: TYPE_NON_ISO_INT8  */
#line 6375 "parser.y"
                                   { 
                    (yyval.ptype).type = NewString("__int8");
                    (yyval.ptype).us = 0;
                }
#line 11462 "y.tab.c"
    break;

  case 428: /* type_specifier: TYPE_NON_ISO_INT16  */
#line 6379 "parser.y"
                                    { 
                    (yyval.ptype).type = NewString("__int16");
                    (yyval.ptype).us = 0;
                }
#line 11471 "y.tab.c"
    break;

  case 429: /* type_specifier: TYPE_NON_ISO_INT32  */
#line 6383 "parser.y"
                                    { 
                    (yyval.ptype).type = NewString("__int32");
                    (yyval.ptype).us = 0;
                }
#line 11480 "y.tab.c"
    break;

  case 430: /* type_specifier: TYPE_NON_ISO_INT64  */
#line 6387 "parser.y"
                                    { 
                    (yyval.ptype).type = NewString("__int64");
                    (yyval.ptype).us = 0;
                }
#line 11489 "y.tab.c"
    break;

  case 431: /* $@13: %empty  */
#line 6393 "parser.y"
                 { /* scanner_check_typedef(); */ }
#line 11495 "y.tab.c"
    break;

  case 432: /* definetype: $@13 expr  */
#line 6393 "parser.y"
                                                         {
                   (yyval.dtype) = (yyvsp[0].dtype);
		   if ((yyval.dtype).type == T_STRING) {
		     (yyval.dtype).rawval = NewStringf("\"%(escape)s\"",(yyval.dtype).val);
		   } else if ((yyval.dtype).type != T_CHAR && (yyval.dtype).type != T_WSTRING && (yyval.dtype).type != T_WCHAR) {
		     (yyval.dtype).rawval = NewStringf("%s", (yyval.dtype).val);
		   }
		   (yyval.dtype).qualifier = 0;
		   (yyval.dtype).refqualifier = 0;
		   (yyval.dtype).bitfield = 0;
		   (yyval.dtype).throws = 0;
		   (yyval.dtype).throwf = 0;
		   (yyval.dtype).nexcept = 0;
		   (yyval.dtype).final = 0;
		   scanner_ignore_typedef();
                }
#line 11516 "y.tab.c"
    break;

  case 433: /* definetype: default_delete  */
#line 6409 "parser.y"
                                 {
		  (yyval.dtype) = (yyvsp[0].dtype);
		}
#line 11524 "y.tab.c"
    break;

  case 434: /* default_delete: deleted_definition  */
#line 6414 "parser.y"
                                    {
		  (yyval.dtype) = (yyvsp[0].dtype);
		}
#line 11532 "y.tab.c"
    break;

  case 435: /* default_delete: explicit_default  */
#line 6417 "parser.y"
                                   {
		  (yyval.dtype) = (yyvsp[0].dtype);
		}
#line 11540 "y.tab.c"
    break;

  case 436: /* deleted_definition: DELETE_KW  */
#line 6423 "parser.y"
                               {
		  (yyval.dtype).val = NewString("delete");
		  (yyval.dtype).rawval = 0;
		  (yyval.dtype).type = T_STRING;
		  (yyval.dtype).qualifier = 0;
		  (yyval.dtype).refqualifier = 0;
		  (yyval.dtype).bitfield = 0;
		  (yyval.dtype).throws = 0;
		  (yyval.dtype).throwf = 0;
		  (yyval.dtype).nexcept = 0;
		  (yyval.dtype).final = 0;
		}
#line 11557 "y.tab.c"
    break;

  case 437: /* explicit_default: DEFAULT  */
#line 6438 "parser.y"
                           {
		  (yyval.dtype).val = NewString("default");
		  (yyval.dtype).rawval = 0;
		  (yyval.dtype).type = T_STRING;
		  (yyval.dtype).qualifier = 0;
		  (yyval.dtype).refqualifier = 0;
		  (yyval.dtype).bitfield = 0;
		  (yyval.dtype).throws = 0;
		  (yyval.dtype).throwf = 0;
		  (yyval.dtype).nexcept = 0;
		  (yyval.dtype).final = 0;
		}
#line 11574 "y.tab.c"
    break;

  case 438: /* ename: identifier  */
#line 6454 "parser.y"
                             { (yyval.id) = (yyvsp[0].id); }
#line 11580 "y.tab.c"
    break;

  case 439: /* ename: empty  */
#line 6455 "parser.y"
                        { (yyval.id) = (char *) 0;}
#line 11586 "y.tab.c"
    break;

  case 444: /* enumlist: enumlist_item  */
#line 6474 "parser.y"
                                {
		  Setattr((yyvsp[0].node),"_last",(yyvsp[0].node));
		  (yyval.node) = (yyvsp[0].node);
		}
#line 11595 "y.tab.c"
    break;

  case 445: /* enumlist: enumlist_item DOXYGENPOSTSTRING  */
#line 6478 "parser.y"
                                                  {
		  Setattr((yyvsp[-1].node),"_last",(yyvsp[-1].node));
		  set_comment((yyvsp[-1].node), (yyvsp[0].str));
		  (yyval.node) = (yyvsp[-1].node);
		}
#line 11605 "y.tab.c"
    break;

  case 446: /* enumlist: enumlist_item COMMA enumlist  */
#line 6483 "parser.y"
                                               {
		  if ((yyvsp[0].node)) {
		    set_nextSibling((yyvsp[-2].node), (yyvsp[0].node));
		    Setattr((yyvsp[-2].node),"_last",Getattr((yyvsp[0].node),"_last"));
		    Setattr((yyvsp[0].node),"_last",NULL);
		  } else {
		    Setattr((yyvsp[-2].node),"_last",(yyvsp[-2].node));
		  }
		  (yyval.node) = (yyvsp[-2].node);
		}
#line 11620 "y.tab.c"
    break;

  case 447: /* enumlist: enumlist_item COMMA DOXYGENPOSTSTRING enumlist  */
#line 6493 "parser.y"
                                                                 {
		  if ((yyvsp[0].node)) {
		    set_nextSibling((yyvsp[-3].node), (yyvsp[0].node));
		    Setattr((yyvsp[-3].node),"_last",Getattr((yyvsp[0].node),"_last"));
		    Setattr((yyvsp[0].node),"_last",NULL);
		  } else {
		    Setattr((yyvsp[-3].node),"_last",(yyvsp[-3].node));
		  }
		  set_comment((yyvsp[-3].node), (yyvsp[-1].str));
		  (yyval.node) = (yyvsp[-3].node);
		}
#line 11636 "y.tab.c"
    break;

  case 448: /* enumlist: optional_ignored_defines  */
#line 6504 "parser.y"
                                           {
		  (yyval.node) = 0;
		}
#line 11644 "y.tab.c"
    break;

  case 449: /* enumlist_item: optional_ignored_defines edecl_with_dox optional_ignored_defines  */
#line 6509 "parser.y"
                                                                                   {
		  (yyval.node) = (yyvsp[-1].node);
		}
#line 11652 "y.tab.c"
    break;

  case 450: /* edecl_with_dox: edecl  */
#line 6514 "parser.y"
                        {
		  (yyval.node) = (yyvsp[0].node);
		}
#line 11660 "y.tab.c"
    break;

  case 451: /* edecl_with_dox: DOXYGENSTRING edecl  */
#line 6517 "parser.y"
                                      {
		  (yyval.node) = (yyvsp[0].node);
		  set_comment((yyvsp[0].node), (yyvsp[-1].str));
		}
#line 11669 "y.tab.c"
    break;

  case 452: /* edecl: identifier  */
#line 6523 "parser.y"
                             {
		   SwigType *type = NewSwigType(T_INT);
		   (yyval.node) = new_node("enumitem");
		   Setattr((yyval.node),"name",(yyvsp[0].id));
		   Setattr((yyval.node),"type",type);
		   SetFlag((yyval.node),"feature:immutable");
		   Delete(type);
		 }
#line 11682 "y.tab.c"
    break;

  case 453: /* edecl: identifier EQUAL etype  */
#line 6531 "parser.y"
                                          {
		   SwigType *type = NewSwigType((yyvsp[0].dtype).type == T_BOOL ? T_BOOL : ((yyvsp[0].dtype).type == T_CHAR ? T_CHAR : T_INT));
		   (yyval.node) = new_node("enumitem");
		   Setattr((yyval.node),"name",(yyvsp[-2].id));
		   Setattr((yyval.node),"type",type);
		   SetFlag((yyval.node),"feature:immutable");
		   Setattr((yyval.node),"enumvalue", (yyvsp[0].dtype).val);
		   Setattr((yyval.node),"value",(yyvsp[-2].id));
		   Delete(type);
                 }
#line 11697 "y.tab.c"
    break;

  case 454: /* etype: expr  */
#line 6543 "parser.y"
                        {
                   (yyval.dtype) = (yyvsp[0].dtype);
		   if (((yyval.dtype).type != T_INT) && ((yyval.dtype).type != T_UINT) &&
		       ((yyval.dtype).type != T_LONG) && ((yyval.dtype).type != T_ULONG) &&
		       ((yyval.dtype).type != T_LONGLONG) && ((yyval.dtype).type != T_ULONGLONG) &&
		       ((yyval.dtype).type != T_SHORT) && ((yyval.dtype).type != T_USHORT) &&
		       ((yyval.dtype).type != T_SCHAR) && ((yyval.dtype).type != T_UCHAR) &&
		       ((yyval.dtype).type != T_CHAR) && ((yyval.dtype).type != T_BOOL)) {
		     Swig_error(cparse_file,cparse_line,"Type error. Expecting an integral type\n");
		   }
                }
#line 11713 "y.tab.c"
    break;

  case 455: /* expr: valexpr  */
#line 6558 "parser.y"
                         { (yyval.dtype) = (yyvsp[0].dtype); }
#line 11719 "y.tab.c"
    break;

  case 456: /* expr: type  */
#line 6559 "parser.y"
                      {
		 Node *n;
		 (yyval.dtype).val = (yyvsp[0].type);
		 (yyval.dtype).type = T_INT;
		 /* Check if value is in scope */
		 n = Swig_symbol_clookup((yyvsp[0].type),0);
		 if (n) {
                   /* A band-aid for enum values used in expressions. */
                   if (Strcmp(nodeType(n),"enumitem") == 0) {
                     String *q = Swig_symbol_qualified(n);
                     if (q) {
                       (yyval.dtype).val = NewStringf("%s::%s", q, Getattr(n,"name"));
                       Delete(q);
                     }
                   }
		 }
               }
#line 11741 "y.tab.c"
    break;

  case 457: /* exprmem: ID ARROW ID  */
#line 6579 "parser.y"
                             {
		 (yyval.dtype).val = NewStringf("%s->%s", (yyvsp[-2].id), (yyvsp[0].id));
		 (yyval.dtype).type = 0;
	       }
#line 11750 "y.tab.c"
    break;

  case 458: /* exprmem: ID ARROW ID LPAREN callparms RPAREN  */
#line 6583 "parser.y"
                                                     {
		 (yyval.dtype).val = NewStringf("%s->%s(%s)", (yyvsp[-5].id), (yyvsp[-3].id), (yyvsp[-1].dtype));
		 (yyval.dtype).type = 0;
	       }
#line 11759 "y.tab.c"
    break;

  case 459: /* exprmem: exprmem ARROW ID  */
#line 6587 "parser.y"
                                  {
		 (yyval.dtype) = (yyvsp[-2].dtype);
		 Printf((yyval.dtype).val, "->%s", (yyvsp[0].id));
	       }
#line 11768 "y.tab.c"
    break;

  case 460: /* exprmem: exprmem ARROW ID LPAREN callparms RPAREN  */
#line 6591 "parser.y"
                                                          {
		 (yyval.dtype) = (yyvsp[-5].dtype);
		 Printf((yyval.dtype).val, "->%s(%s)", (yyvsp[-3].id), (yyvsp[-1].dtype));
	       }
#line 11777 "y.tab.c"
    break;

  case 461: /* exprmem: ID PERIOD ID  */
#line 6595 "parser.y"
                              {
		 (yyval.dtype).val = NewStringf("%s.%s", (yyvsp[-2].id), (yyvsp[0].id));
		 (yyval.dtype).type = 0;
	       }
#line 11786 "y.tab.c"
    break;

  case 462: /* exprmem: ID PERIOD ID LPAREN callparms RPAREN  */
#line 6599 "parser.y"
                                                      {
		 (yyval.dtype).val = NewStringf("%s.%s(%s)", (yyvsp[-5].id), (yyvsp[-3].id), (yyvsp[-1].dtype));
		 (yyval.dtype).type = 0;
	       }
#line 11795 "y.tab.c"
    break;

  case 463: /* exprmem: exprmem PERIOD ID  */
#line 6603 "parser.y"
                                   {
		 (yyval.dtype) = (yyvsp[-2].dtype);
		 Printf((yyval.dtype).val, ".%s", (yyvsp[0].id));
	       }
#line 11804 "y.tab.c"
    break;

  case 464: /* exprmem: exprmem PERIOD ID LPAREN callparms RPAREN  */
#line 6607 "parser.y"
                                                           {
		 (yyval.dtype) = (yyvsp[-5].dtype);
		 Printf((yyval.dtype).val, ".%s(%s)", (yyvsp[-3].id), (yyvsp[-1].dtype));
	       }
#line 11813 "y.tab.c"
    break;

  case 465: /* exprsimple: exprnum  */
#line 6614 "parser.y"
                         {
		    (yyval.dtype) = (yyvsp[0].dtype);
               }
#line 11821 "y.tab.c"
    break;

  case 466: /* exprsimple: exprmem  */
#line 6617 "parser.y"
                         {
		    (yyval.dtype) = (yyvsp[0].dtype);
               }
#line 11829 "y.tab.c"
    break;

  case 467: /* exprsimple: string  */
#line 6620 "parser.y"
                        {
		    (yyval.dtype).val = (yyvsp[0].str);
                    (yyval.dtype).type = T_STRING;
               }
#line 11838 "y.tab.c"
    break;

  case 468: /* exprsimple: SIZEOF LPAREN type parameter_declarator RPAREN  */
#line 6624 "parser.y"
                                                                {
		  SwigType_push((yyvsp[-2].type),(yyvsp[-1].decl).type);
		  (yyval.dtype).val = NewStringf("sizeof(%s)",SwigType_str((yyvsp[-2].type),0));
		  (yyval.dtype).type = T_ULONG;
               }
#line 11848 "y.tab.c"
    break;

  case 469: /* exprsimple: SIZEOF ELLIPSIS LPAREN type parameter_declarator RPAREN  */
#line 6629 "parser.y"
                                                                         {
		  SwigType_push((yyvsp[-2].type),(yyvsp[-1].decl).type);
		  (yyval.dtype).val = NewStringf("sizeof...(%s)",SwigType_str((yyvsp[-2].type),0));
		  (yyval.dtype).type = T_ULONG;
               }
#line 11858 "y.tab.c"
    break;

  case 470: /* exprsimple: SIZEOF LPAREN exprsimple RPAREN  */
#line 6641 "parser.y"
                                                 {
		  (yyval.dtype).val = NewStringf("sizeof(%s)", (yyvsp[-1].dtype).val);
		 (yyval.dtype).type = T_ULONG;
	       }
#line 11867 "y.tab.c"
    break;

  case 471: /* exprsimple: SIZEOF exprsimple  */
#line 6649 "parser.y"
                                   {
		  (yyval.dtype).val = NewStringf("sizeof(%s)", (yyvsp[0].dtype).val);
		  (yyval.dtype).type = T_ULONG;
	       }
#line 11876 "y.tab.c"
    break;

  case 472: /* exprsimple: wstring  */
#line 6653 "parser.y"
                         {
		    (yyval.dtype).val = (yyvsp[0].str);
		    (yyval.dtype).rawval = NewStringf("L\"%s\"", (yyval.dtype).val);
                    (yyval.dtype).type = T_WSTRING;
	       }
#line 11886 "y.tab.c"
    break;

  case 473: /* exprsimple: CHARCONST  */
#line 6658 "parser.y"
                           {
		  (yyval.dtype).val = NewString((yyvsp[0].str));
		  if (Len((yyval.dtype).val)) {
		    (yyval.dtype).rawval = NewStringf("'%(escape)s'", (yyval.dtype).val);
		  } else {
		    (yyval.dtype).rawval = NewString("'\\0'");
		  }
		  (yyval.dtype).type = T_CHAR;
		  (yyval.dtype).bitfield = 0;
		  (yyval.dtype).throws = 0;
		  (yyval.dtype).throwf = 0;
		  (yyval.dtype).nexcept = 0;
		  (yyval.dtype).final = 0;
	       }
#line 11905 "y.tab.c"
    break;

  case 474: /* exprsimple: WCHARCONST  */
#line 6672 "parser.y"
                            {
		  (yyval.dtype).val = NewString((yyvsp[0].str));
		  if (Len((yyval.dtype).val)) {
		    (yyval.dtype).rawval = NewStringf("L\'%s\'", (yyval.dtype).val);
		  } else {
		    (yyval.dtype).rawval = NewString("L'\\0'");
		  }
		  (yyval.dtype).type = T_WCHAR;
		  (yyval.dtype).bitfield = 0;
		  (yyval.dtype).throws = 0;
		  (yyval.dtype).throwf = 0;
		  (yyval.dtype).nexcept = 0;
		  (yyval.dtype).final = 0;
	       }
#line 11924 "y.tab.c"
    break;

  case 475: /* valexpr: exprsimple  */
#line 6689 "parser.y"
                            { (yyval.dtype) = (yyvsp[0].dtype); }
#line 11930 "y.tab.c"
    break;

  case 476: /* valexpr: exprcompound  */
#line 6690 "parser.y"
                              { (yyval.dtype) = (yyvsp[0].dtype); }
#line 11936 "y.tab.c"
    break;

  case 477: /* valexpr: LPAREN expr RPAREN  */
#line 6693 "parser.y"
                                                {
		    (yyval.dtype).val = NewStringf("(%s)",(yyvsp[-1].dtype).val);
		    if ((yyvsp[-1].dtype).rawval) {
		      (yyval.dtype).rawval = NewStringf("(%s)",(yyvsp[-1].dtype).rawval);
		    }
		    (yyval.dtype).type = (yyvsp[-1].dtype).type;
	       }
#line 11948 "y.tab.c"
    break;

  case 478: /* valexpr: LPAREN expr RPAREN expr  */
#line 6703 "parser.y"
                                                    {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   switch ((yyvsp[-2].dtype).type) {
		     case T_FLOAT:
		     case T_DOUBLE:
		     case T_LONGDOUBLE:
		     case T_FLTCPLX:
		     case T_DBLCPLX:
		       (yyval.dtype).val = NewStringf("(%s)%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val); /* SwigType_str and decimal points don't mix! */
		       break;
		     default:
		       (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-2].dtype).val,0), (yyvsp[0].dtype).val);
		       break;
		   }
		 }
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type, (yyvsp[0].dtype).type);
 	       }
#line 11971 "y.tab.c"
    break;

  case 479: /* valexpr: LPAREN expr pointer RPAREN expr  */
#line 6721 "parser.y"
                                                            {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[-3].dtype).val,(yyvsp[-2].type));
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-3].dtype).val,0), (yyvsp[0].dtype).val);
		 }
 	       }
#line 11983 "y.tab.c"
    break;

  case 480: /* valexpr: LPAREN expr AND RPAREN expr  */
#line 6728 "parser.y"
                                                        {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_add_reference((yyvsp[-3].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-3].dtype).val,0), (yyvsp[0].dtype).val);
		 }
 	       }
#line 11995 "y.tab.c"
    break;

  case 481: /* valexpr: LPAREN expr LAND RPAREN expr  */
#line 6735 "parser.y"
                                                         {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_add_rvalue_reference((yyvsp[-3].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-3].dtype).val,0), (yyvsp[0].dtype).val);
		 }
 	       }
#line 12007 "y.tab.c"
    break;

  case 482: /* valexpr: LPAREN expr pointer AND RPAREN expr  */
#line 6742 "parser.y"
                                                                {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[-4].dtype).val,(yyvsp[-3].type));
		   SwigType_add_reference((yyvsp[-4].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-4].dtype).val,0), (yyvsp[0].dtype).val);
		 }
 	       }
#line 12020 "y.tab.c"
    break;

  case 483: /* valexpr: LPAREN expr pointer LAND RPAREN expr  */
#line 6750 "parser.y"
                                                                 {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[-4].dtype).val,(yyvsp[-3].type));
		   SwigType_add_rvalue_reference((yyvsp[-4].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-4].dtype).val,0), (yyvsp[0].dtype).val);
		 }
 	       }
#line 12033 "y.tab.c"
    break;

  case 484: /* valexpr: AND expr  */
#line 6758 "parser.y"
                          {
		 (yyval.dtype) = (yyvsp[0].dtype);
                 (yyval.dtype).val = NewStringf("&%s",(yyvsp[0].dtype).val);
	       }
#line 12042 "y.tab.c"
    break;

  case 485: /* valexpr: STAR expr  */
#line 6762 "parser.y"
                           {
		 (yyval.dtype) = (yyvsp[0].dtype);
                 (yyval.dtype).val = NewStringf("*%s",(yyvsp[0].dtype).val);
	       }
#line 12051 "y.tab.c"
    break;

  case 486: /* exprnum: NUM_INT  */
#line 6768 "parser.y"
                          { (yyval.dtype) = (yyvsp[0].dtype); }
#line 12057 "y.tab.c"
    break;

  case 487: /* exprnum: NUM_FLOAT  */
#line 6769 "parser.y"
                            { (yyval.dtype) = (yyvsp[0].dtype); }
#line 12063 "y.tab.c"
    break;

  case 488: /* exprnum: NUM_UNSIGNED  */
#line 6770 "parser.y"
                               { (yyval.dtype) = (yyvsp[0].dtype); }
#line 12069 "y.tab.c"
    break;

  case 489: /* exprnum: NUM_LONG  */
#line 6771 "parser.y"
                           { (yyval.dtype) = (yyvsp[0].dtype); }
#line 12075 "y.tab.c"
    break;

  case 490: /* exprnum: NUM_ULONG  */
#line 6772 "parser.y"
                            { (yyval.dtype) = (yyvsp[0].dtype); }
#line 12081 "y.tab.c"
    break;

  case 491: /* exprnum: NUM_LONGLONG  */
#line 6773 "parser.y"
                               { (yyval.dtype) = (yyvsp[0].dtype); }
#line 12087 "y.tab.c"
    break;

  case 492: /* exprnum: NUM_ULONGLONG  */
#line 6774 "parser.y"
                                { (yyval.dtype) = (yyvsp[0].dtype); }
#line 12093 "y.tab.c"
    break;

  case 493: /* exprnum: NUM_BOOL  */
#line 6775 "parser.y"
                           { (yyval.dtype) = (yyvsp[0].dtype); }
#line 12099 "y.tab.c"
    break;

  case 494: /* exprcompound: expr PLUS expr  */
#line 6778 "parser.y"
                                {
		 (yyval.dtype).val = NewStringf("%s+%s", COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 12108 "y.tab.c"
    break;

  case 495: /* exprcompound: expr MINUS expr  */
#line 6782 "parser.y"
                                 {
		 (yyval.dtype).val = NewStringf("%s-%s",COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 12117 "y.tab.c"
    break;

  case 496: /* exprcompound: expr STAR expr  */
#line 6786 "parser.y"
                                {
		 (yyval.dtype).val = NewStringf("%s*%s",COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 12126 "y.tab.c"
    break;

  case 497: /* exprcompound: expr SLASH expr  */
#line 6790 "parser.y"
                                 {
		 (yyval.dtype).val = NewStringf("%s/%s",COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 12135 "y.tab.c"
    break;

  case 498: /* exprcompound: expr MODULO expr  */
#line 6794 "parser.y"
                                  {
		 (yyval.dtype).val = NewStringf("%s%%%s",COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 12144 "y.tab.c"
    break;

  case 499: /* exprcompound: expr AND expr  */
#line 6798 "parser.y"
                               {
		 (yyval.dtype).val = NewStringf("%s&%s",COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 12153 "y.tab.c"
    break;

  case 500: /* exprcompound: expr OR expr  */
#line 6802 "parser.y"
                              {
		 (yyval.dtype).val = NewStringf("%s|%s",COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 12162 "y.tab.c"
    break;

  case 501: /* exprcompound: expr XOR expr  */
#line 6806 "parser.y"
                               {
		 (yyval.dtype).val = NewStringf("%s^%s",COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 12171 "y.tab.c"
    break;

  case 502: /* exprcompound: expr LSHIFT expr  */
#line 6810 "parser.y"
                                  {
		 (yyval.dtype).val = NewStringf("%s << %s",COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 12180 "y.tab.c"
    break;

  case 503: /* exprcompound: expr RSHIFT expr  */
#line 6814 "parser.y"
                                  {
		 (yyval.dtype).val = NewStringf("%s >> %s",COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 12189 "y.tab.c"
    break;

  case 504: /* exprcompound: expr LAND expr  */
#line 6818 "parser.y"
                                {
		 (yyval.dtype).val = NewStringf("%s&&%s",COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12198 "y.tab.c"
    break;

  case 505: /* exprcompound: expr LOR expr  */
#line 6822 "parser.y"
                               {
		 (yyval.dtype).val = NewStringf("%s||%s",COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12207 "y.tab.c"
    break;

  case 506: /* exprcompound: expr EQUALTO expr  */
#line 6826 "parser.y"
                                   {
		 (yyval.dtype).val = NewStringf("%s==%s",COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12216 "y.tab.c"
    break;

  case 507: /* exprcompound: expr NOTEQUALTO expr  */
#line 6830 "parser.y"
                                      {
		 (yyval.dtype).val = NewStringf("%s!=%s",COMPOUND_EXPR_VAL((yyvsp[-2].dtype)),COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12225 "y.tab.c"
    break;

  case 508: /* exprcompound: LPAREN expr GREATERTHAN expr RPAREN  */
#line 6838 "parser.y"
                                                     {
		 (yyval.dtype).val = NewStringf("%s > %s", COMPOUND_EXPR_VAL((yyvsp[-3].dtype)), COMPOUND_EXPR_VAL((yyvsp[-1].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12234 "y.tab.c"
    break;

  case 509: /* exprcompound: LPAREN exprsimple LESSTHAN expr RPAREN  */
#line 6848 "parser.y"
                                                        {
		 (yyval.dtype).val = NewStringf("%s < %s", COMPOUND_EXPR_VAL((yyvsp[-3].dtype)), COMPOUND_EXPR_VAL((yyvsp[-1].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12243 "y.tab.c"
    break;

  case 510: /* exprcompound: expr GREATERTHANOREQUALTO expr  */
#line 6852 "parser.y"
                                                {
		 (yyval.dtype).val = NewStringf("%s >= %s", COMPOUND_EXPR_VAL((yyvsp[-2].dtype)), COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12252 "y.tab.c"
    break;

  case 511: /* exprcompound: expr LESSTHANOREQUALTO expr  */
#line 6856 "parser.y"
                                             {
		 (yyval.dtype).val = NewStringf("%s <= %s", COMPOUND_EXPR_VAL((yyvsp[-2].dtype)), COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 12261 "y.tab.c"
    break;

  case 512: /* exprcompound: expr LESSEQUALGREATER expr  */
#line 6860 "parser.y"
                                            {
		 (yyval.dtype).val = NewStringf("%s <=> %s", COMPOUND_EXPR_VAL((yyvsp[-2].dtype)), COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 /* Really `<=>` returns one of `std::strong_ordering`,
		  * `std::partial_ordering` or `std::weak_ordering`, but we
		  * fake it by treating the return value as `int`.  The main
		  * thing to do with the return value in this context is to
		  * compare it with 0, for which `int` does the job. */
		 (yyval.dtype).type = T_INT;
	       }
#line 12275 "y.tab.c"
    break;

  case 513: /* exprcompound: expr QUESTIONMARK expr COLON expr  */
#line 6869 "parser.y"
                                                                      {
		 (yyval.dtype).val = NewStringf("%s?%s:%s", COMPOUND_EXPR_VAL((yyvsp[-4].dtype)), COMPOUND_EXPR_VAL((yyvsp[-2].dtype)), COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 /* This may not be exactly right, but is probably good enough
		  * for the purposes of parsing constant expressions. */
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type, (yyvsp[0].dtype).type);
	       }
#line 12286 "y.tab.c"
    break;

  case 514: /* exprcompound: MINUS expr  */
#line 6875 "parser.y"
                                         {
		 (yyval.dtype).val = NewStringf("-%s",(yyvsp[0].dtype).val);
		 (yyval.dtype).type = (yyvsp[0].dtype).type;
	       }
#line 12295 "y.tab.c"
    break;

  case 515: /* exprcompound: PLUS expr  */
#line 6879 "parser.y"
                                        {
                 (yyval.dtype).val = NewStringf("+%s",(yyvsp[0].dtype).val);
		 (yyval.dtype).type = (yyvsp[0].dtype).type;
	       }
#line 12304 "y.tab.c"
    break;

  case 516: /* exprcompound: NOT expr  */
#line 6883 "parser.y"
                          {
		 (yyval.dtype).val = NewStringf("~%s",(yyvsp[0].dtype).val);
		 (yyval.dtype).type = (yyvsp[0].dtype).type;
	       }
#line 12313 "y.tab.c"
    break;

  case 517: /* exprcompound: LNOT expr  */
#line 6887 "parser.y"
                           {
                 (yyval.dtype).val = NewStringf("!%s",COMPOUND_EXPR_VAL((yyvsp[0].dtype)));
		 (yyval.dtype).type = T_INT;
	       }
#line 12322 "y.tab.c"
    break;

  case 518: /* exprcompound: type LPAREN  */
#line 6891 "parser.y"
                             {
		 String *qty;
                 skip_balanced('(',')');
		 qty = Swig_symbol_type_qualify((yyvsp[-1].type),0);
		 if (SwigType_istemplate(qty)) {
		   String *nstr = SwigType_namestr(qty);
		   Delete(qty);
		   qty = nstr;
		 }
		 (yyval.dtype).val = NewStringf("%s%s",qty,scanner_ccode);
		 Clear(scanner_ccode);
		 (yyval.dtype).type = T_INT;
		 Delete(qty);
               }
#line 12341 "y.tab.c"
    break;

  case 519: /* variadic: ELLIPSIS  */
#line 6907 "parser.y"
                         {
	        (yyval.str) = NewString("...");
	      }
#line 12349 "y.tab.c"
    break;

  case 520: /* variadic: empty  */
#line 6910 "parser.y"
                      {
	        (yyval.str) = 0;
	      }
#line 12357 "y.tab.c"
    break;

  case 521: /* inherit: raw_inherit  */
#line 6915 "parser.y"
                             {
		 (yyval.bases) = (yyvsp[0].bases);
               }
#line 12365 "y.tab.c"
    break;

  case 522: /* $@14: %empty  */
#line 6920 "parser.y"
                        { inherit_list = 1; }
#line 12371 "y.tab.c"
    break;

  case 523: /* raw_inherit: COLON $@14 base_list  */
#line 6920 "parser.y"
                                                        { (yyval.bases) = (yyvsp[0].bases); inherit_list = 0; }
#line 12377 "y.tab.c"
    break;

  case 524: /* raw_inherit: empty  */
#line 6921 "parser.y"
                        { (yyval.bases) = 0; }
#line 12383 "y.tab.c"
    break;

  case 525: /* base_list: base_specifier  */
#line 6924 "parser.y"
                                {
		   Hash *list = NewHash();
		   Node *base = (yyvsp[0].node);
		   Node *name = Getattr(base,"name");
		   List *lpublic = NewList();
		   List *lprotected = NewList();
		   List *lprivate = NewList();
		   Setattr(list,"public",lpublic);
		   Setattr(list,"protected",lprotected);
		   Setattr(list,"private",lprivate);
		   Delete(lpublic);
		   Delete(lprotected);
		   Delete(lprivate);
		   Append(Getattr(list,Getattr(base,"access")),name);
	           (yyval.bases) = list;
               }
#line 12404 "y.tab.c"
    break;

  case 526: /* base_list: base_list COMMA base_specifier  */
#line 6941 "parser.y"
                                                {
		   Hash *list = (yyvsp[-2].bases);
		   Node *base = (yyvsp[0].node);
		   Node *name = Getattr(base,"name");
		   Append(Getattr(list,Getattr(base,"access")),name);
                   (yyval.bases) = list;
               }
#line 12416 "y.tab.c"
    break;

  case 527: /* @15: %empty  */
#line 6950 "parser.y"
                             {
		 (yyval.intvalue) = cparse_line;
	       }
#line 12424 "y.tab.c"
    break;

  case 528: /* base_specifier: opt_virtual @15 idcolon variadic  */
#line 6952 "parser.y"
                                  {
		 (yyval.node) = NewHash();
		 Setfile((yyval.node),cparse_file);
		 Setline((yyval.node),(yyvsp[-2].intvalue));
		 Setattr((yyval.node),"name",(yyvsp[-1].str));
		 Setfile((yyvsp[-1].str),cparse_file);
		 Setline((yyvsp[-1].str),(yyvsp[-2].intvalue));
                 if (last_cpptype && (Strcmp(last_cpptype,"struct") != 0)) {
		   Setattr((yyval.node),"access","private");
		   Swig_warning(WARN_PARSE_NO_ACCESS, Getfile((yyval.node)), Getline((yyval.node)), "No access specifier given for base class '%s' (ignored).\n", SwigType_namestr((yyvsp[-1].str)));
                 } else {
		   Setattr((yyval.node),"access","public");
		 }
		 if ((yyvsp[0].str))
		   SetFlag((yyval.node), "variadic");
               }
#line 12445 "y.tab.c"
    break;

  case 529: /* @16: %empty  */
#line 6968 "parser.y"
                                              {
		 (yyval.intvalue) = cparse_line;
	       }
#line 12453 "y.tab.c"
    break;

  case 530: /* base_specifier: opt_virtual access_specifier @16 opt_virtual idcolon variadic  */
#line 6970 "parser.y"
                                              {
		 (yyval.node) = NewHash();
		 Setfile((yyval.node),cparse_file);
		 Setline((yyval.node),(yyvsp[-3].intvalue));
		 Setattr((yyval.node),"name",(yyvsp[-1].str));
		 Setfile((yyvsp[-1].str),cparse_file);
		 Setline((yyvsp[-1].str),(yyvsp[-3].intvalue));
		 Setattr((yyval.node),"access",(yyvsp[-4].id));
	         if (Strcmp((yyvsp[-4].id),"public") != 0) {
		   Swig_warning(WARN_PARSE_PRIVATE_INHERIT, Getfile((yyval.node)), Getline((yyval.node)), "%s inheritance from base '%s' (ignored).\n", (yyvsp[-4].id), SwigType_namestr((yyvsp[-1].str)));
		 }
		 if ((yyvsp[0].str))
		   SetFlag((yyval.node), "variadic");
               }
#line 12472 "y.tab.c"
    break;

  case 531: /* access_specifier: PUBLIC  */
#line 6986 "parser.y"
                           { (yyval.id) = (char*)"public"; }
#line 12478 "y.tab.c"
    break;

  case 532: /* access_specifier: PRIVATE  */
#line 6987 "parser.y"
                         { (yyval.id) = (char*)"private"; }
#line 12484 "y.tab.c"
    break;

  case 533: /* access_specifier: PROTECTED  */
#line 6988 "parser.y"
                           { (yyval.id) = (char*)"protected"; }
#line 12490 "y.tab.c"
    break;

  case 534: /* templcpptype: CLASS  */
#line 6991 "parser.y"
                       { 
                   (yyval.id) = (char*)"class"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 12499 "y.tab.c"
    break;

  case 535: /* templcpptype: TYPENAME  */
#line 6995 "parser.y"
                          { 
                   (yyval.id) = (char *)"typename"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 12508 "y.tab.c"
    break;

  case 536: /* templcpptype: CLASS ELLIPSIS  */
#line 6999 "parser.y"
                                {
                   (yyval.id) = (char *)"class..."; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 12517 "y.tab.c"
    break;

  case 537: /* templcpptype: TYPENAME ELLIPSIS  */
#line 7003 "parser.y"
                                   {
                   (yyval.id) = (char *)"typename..."; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 12526 "y.tab.c"
    break;

  case 538: /* cpptype: templcpptype  */
#line 7009 "parser.y"
                              {
                 (yyval.id) = (yyvsp[0].id);
               }
#line 12534 "y.tab.c"
    break;

  case 539: /* cpptype: STRUCT  */
#line 7012 "parser.y"
                        { 
                   (yyval.id) = (char*)"struct"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 12543 "y.tab.c"
    break;

  case 540: /* cpptype: UNION  */
#line 7016 "parser.y"
                       {
                   (yyval.id) = (char*)"union"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 12552 "y.tab.c"
    break;

  case 541: /* classkey: CLASS  */
#line 7022 "parser.y"
                       {
                   (yyval.id) = (char*)"class";
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 12561 "y.tab.c"
    break;

  case 542: /* classkey: STRUCT  */
#line 7026 "parser.y"
                        {
                   (yyval.id) = (char*)"struct";
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 12570 "y.tab.c"
    break;

  case 543: /* classkey: UNION  */
#line 7030 "parser.y"
                       {
                   (yyval.id) = (char*)"union";
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 12579 "y.tab.c"
    break;

  case 544: /* classkeyopt: classkey  */
#line 7036 "parser.y"
                          {
		   (yyval.id) = (yyvsp[0].id);
               }
#line 12587 "y.tab.c"
    break;

  case 545: /* classkeyopt: empty  */
#line 7039 "parser.y"
                       {
		   (yyval.id) = 0;
               }
#line 12595 "y.tab.c"
    break;

  case 548: /* virt_specifier_seq: OVERRIDE  */
#line 7048 "parser.y"
                              {
                   (yyval.str) = 0;
	       }
#line 12603 "y.tab.c"
    break;

  case 549: /* virt_specifier_seq: FINAL  */
#line 7051 "parser.y"
                       {
                   (yyval.str) = NewString("1");
	       }
#line 12611 "y.tab.c"
    break;

  case 550: /* virt_specifier_seq: FINAL OVERRIDE  */
#line 7054 "parser.y"
                                {
                   (yyval.str) = NewString("1");
	       }
#line 12619 "y.tab.c"
    break;

  case 551: /* virt_specifier_seq: OVERRIDE FINAL  */
#line 7057 "parser.y"
                                {
                   (yyval.str) = NewString("1");
	       }
#line 12627 "y.tab.c"
    break;

  case 552: /* virt_specifier_seq_opt: virt_specifier_seq  */
#line 7062 "parser.y"
                                            {
                   (yyval.str) = (yyvsp[0].str);
               }
#line 12635 "y.tab.c"
    break;

  case 553: /* virt_specifier_seq_opt: empty  */
#line 7065 "parser.y"
                       {
                   (yyval.str) = 0;
               }
#line 12643 "y.tab.c"
    break;

  case 554: /* class_virt_specifier_opt: FINAL  */
#line 7070 "parser.y"
                                 {
                   (yyval.str) = NewString("1");
               }
#line 12651 "y.tab.c"
    break;

  case 555: /* class_virt_specifier_opt: empty  */
#line 7073 "parser.y"
                       {
                   (yyval.str) = 0;
               }
#line 12659 "y.tab.c"
    break;

  case 556: /* exception_specification: THROW LPAREN parms RPAREN  */
#line 7078 "parser.y"
                                                    {
                    (yyval.dtype).throws = (yyvsp[-1].pl);
                    (yyval.dtype).throwf = NewString("1");
                    (yyval.dtype).nexcept = 0;
                    (yyval.dtype).final = 0;
	       }
#line 12670 "y.tab.c"
    break;

  case 557: /* exception_specification: NOEXCEPT  */
#line 7084 "parser.y"
                          {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = NewString("true");
                    (yyval.dtype).final = 0;
	       }
#line 12681 "y.tab.c"
    break;

  case 558: /* exception_specification: virt_specifier_seq  */
#line 7090 "parser.y"
                                    {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = 0;
                    (yyval.dtype).final = (yyvsp[0].str);
	       }
#line 12692 "y.tab.c"
    break;

  case 559: /* exception_specification: THROW LPAREN parms RPAREN virt_specifier_seq  */
#line 7096 "parser.y"
                                                              {
                    (yyval.dtype).throws = (yyvsp[-2].pl);
                    (yyval.dtype).throwf = NewString("1");
                    (yyval.dtype).nexcept = 0;
                    (yyval.dtype).final = (yyvsp[0].str);
	       }
#line 12703 "y.tab.c"
    break;

  case 560: /* exception_specification: NOEXCEPT virt_specifier_seq  */
#line 7102 "parser.y"
                                             {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = NewString("true");
                    (yyval.dtype).final = (yyvsp[0].str);
	       }
#line 12714 "y.tab.c"
    break;

  case 561: /* exception_specification: NOEXCEPT LPAREN expr RPAREN  */
#line 7108 "parser.y"
                                             {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = (yyvsp[-1].dtype).val;
                    (yyval.dtype).final = 0;
	       }
#line 12725 "y.tab.c"
    break;

  case 562: /* qualifiers_exception_specification: cv_ref_qualifier  */
#line 7116 "parser.y"
                                                      {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = 0;
                    (yyval.dtype).final = 0;
                    (yyval.dtype).qualifier = (yyvsp[0].dtype).qualifier;
                    (yyval.dtype).refqualifier = (yyvsp[0].dtype).refqualifier;
               }
#line 12738 "y.tab.c"
    break;

  case 563: /* qualifiers_exception_specification: exception_specification  */
#line 7124 "parser.y"
                                         {
		    (yyval.dtype) = (yyvsp[0].dtype);
                    (yyval.dtype).qualifier = 0;
                    (yyval.dtype).refqualifier = 0;
               }
#line 12748 "y.tab.c"
    break;

  case 564: /* qualifiers_exception_specification: cv_ref_qualifier exception_specification  */
#line 7129 "parser.y"
                                                          {
		    (yyval.dtype) = (yyvsp[0].dtype);
                    (yyval.dtype).qualifier = (yyvsp[-1].dtype).qualifier;
                    (yyval.dtype).refqualifier = (yyvsp[-1].dtype).refqualifier;
               }
#line 12758 "y.tab.c"
    break;

  case 565: /* cpp_const: qualifiers_exception_specification  */
#line 7136 "parser.y"
                                                    {
                    (yyval.dtype) = (yyvsp[0].dtype);
               }
#line 12766 "y.tab.c"
    break;

  case 566: /* cpp_const: empty  */
#line 7139 "parser.y"
                       { 
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = 0;
                    (yyval.dtype).final = 0;
                    (yyval.dtype).qualifier = 0;
                    (yyval.dtype).refqualifier = 0;
               }
#line 12779 "y.tab.c"
    break;

  case 567: /* ctor_end: cpp_const ctor_initializer SEMI  */
#line 7149 "parser.y"
                                                 { 
                    Clear(scanner_ccode); 
                    (yyval.decl).have_parms = 0; 
                    (yyval.decl).defarg = 0; 
		    (yyval.decl).throws = (yyvsp[-2].dtype).throws;
		    (yyval.decl).throwf = (yyvsp[-2].dtype).throwf;
		    (yyval.decl).nexcept = (yyvsp[-2].dtype).nexcept;
		    (yyval.decl).final = (yyvsp[-2].dtype).final;
                    if ((yyvsp[-2].dtype).qualifier)
                      Swig_error(cparse_file, cparse_line, "Constructor cannot have a qualifier.\n");
               }
#line 12795 "y.tab.c"
    break;

  case 568: /* ctor_end: cpp_const ctor_initializer LBRACE  */
#line 7160 "parser.y"
                                                   { 
                    skip_balanced('{','}'); 
                    (yyval.decl).have_parms = 0; 
                    (yyval.decl).defarg = 0; 
                    (yyval.decl).throws = (yyvsp[-2].dtype).throws;
                    (yyval.decl).throwf = (yyvsp[-2].dtype).throwf;
                    (yyval.decl).nexcept = (yyvsp[-2].dtype).nexcept;
                    (yyval.decl).final = (yyvsp[-2].dtype).final;
                    if ((yyvsp[-2].dtype).qualifier)
                      Swig_error(cparse_file, cparse_line, "Constructor cannot have a qualifier.\n");
               }
#line 12811 "y.tab.c"
    break;

  case 569: /* ctor_end: LPAREN parms RPAREN SEMI  */
#line 7171 "parser.y"
                                          { 
                    Clear(scanner_ccode); 
                    (yyval.decl).parms = (yyvsp[-2].pl); 
                    (yyval.decl).have_parms = 1; 
                    (yyval.decl).defarg = 0; 
		    (yyval.decl).throws = 0;
		    (yyval.decl).throwf = 0;
		    (yyval.decl).nexcept = 0;
		    (yyval.decl).final = 0;
               }
#line 12826 "y.tab.c"
    break;

  case 570: /* ctor_end: LPAREN parms RPAREN LBRACE  */
#line 7181 "parser.y"
                                            {
                    skip_balanced('{','}'); 
                    (yyval.decl).parms = (yyvsp[-2].pl); 
                    (yyval.decl).have_parms = 1; 
                    (yyval.decl).defarg = 0; 
                    (yyval.decl).throws = 0;
                    (yyval.decl).throwf = 0;
                    (yyval.decl).nexcept = 0;
                    (yyval.decl).final = 0;
               }
#line 12841 "y.tab.c"
    break;

  case 571: /* ctor_end: EQUAL definetype SEMI  */
#line 7191 "parser.y"
                                       { 
                    (yyval.decl).have_parms = 0; 
                    (yyval.decl).defarg = (yyvsp[-1].dtype).val; 
                    (yyval.decl).throws = 0;
                    (yyval.decl).throwf = 0;
                    (yyval.decl).nexcept = 0;
                    (yyval.decl).final = 0;
               }
#line 12854 "y.tab.c"
    break;

  case 572: /* ctor_end: exception_specification EQUAL default_delete SEMI  */
#line 7199 "parser.y"
                                                                   {
                    (yyval.decl).have_parms = 0;
                    (yyval.decl).defarg = (yyvsp[-1].dtype).val;
                    (yyval.decl).throws = (yyvsp[-3].dtype).throws;
                    (yyval.decl).throwf = (yyvsp[-3].dtype).throwf;
                    (yyval.decl).nexcept = (yyvsp[-3].dtype).nexcept;
                    (yyval.decl).final = (yyvsp[-3].dtype).final;
                    if ((yyvsp[-3].dtype).qualifier)
                      Swig_error(cparse_file, cparse_line, "Constructor cannot have a qualifier.\n");
               }
#line 12869 "y.tab.c"
    break;

  case 579: /* mem_initializer: idcolon LPAREN  */
#line 7221 "parser.y"
                                 {
		  skip_balanced('(',')');
		  Clear(scanner_ccode);
		}
#line 12878 "y.tab.c"
    break;

  case 580: /* mem_initializer: idcolon LBRACE  */
#line 7233 "parser.y"
                                 {
		  skip_balanced('{','}');
		  Clear(scanner_ccode);
		}
#line 12887 "y.tab.c"
    break;

  case 581: /* less_valparms_greater: LESSTHAN valparms GREATERTHAN  */
#line 7239 "parser.y"
                                                      {
                     String *s = NewStringEmpty();
                     SwigType_add_template(s,(yyvsp[-1].p));
                     (yyval.id) = Char(s);
		     scanner_last_id(1);
                }
#line 12898 "y.tab.c"
    break;

  case 582: /* identifier: ID  */
#line 7248 "parser.y"
                    { (yyval.id) = (yyvsp[0].id); }
#line 12904 "y.tab.c"
    break;

  case 583: /* identifier: OVERRIDE  */
#line 7249 "parser.y"
                          { (yyval.id) = Swig_copy_string("override"); }
#line 12910 "y.tab.c"
    break;

  case 584: /* identifier: FINAL  */
#line 7250 "parser.y"
                       { (yyval.id) = Swig_copy_string("final"); }
#line 12916 "y.tab.c"
    break;

  case 585: /* idstring: identifier  */
#line 7253 "parser.y"
                            { (yyval.id) = (yyvsp[0].id); }
#line 12922 "y.tab.c"
    break;

  case 586: /* idstring: default_delete  */
#line 7254 "parser.y"
                                { (yyval.id) = Char((yyvsp[0].dtype).val); }
#line 12928 "y.tab.c"
    break;

  case 587: /* idstring: string  */
#line 7255 "parser.y"
                        { (yyval.id) = Char((yyvsp[0].str)); }
#line 12934 "y.tab.c"
    break;

  case 588: /* idstringopt: idstring  */
#line 7258 "parser.y"
                          { (yyval.id) = (yyvsp[0].id); }
#line 12940 "y.tab.c"
    break;

  case 589: /* idstringopt: empty  */
#line 7259 "parser.y"
                       { (yyval.id) = 0; }
#line 12946 "y.tab.c"
    break;

  case 590: /* idcolon: idtemplate idcolontail  */
#line 7262 "parser.y"
                                        { 
                  (yyval.str) = 0;
		  if (!(yyval.str)) (yyval.str) = NewStringf("%s%s", (yyvsp[-1].str),(yyvsp[0].str));
      	          Delete((yyvsp[0].str));
               }
#line 12956 "y.tab.c"
    break;

  case 591: /* idcolon: NONID DCOLON idtemplatetemplate idcolontail  */
#line 7267 "parser.y"
                                                             {
		 (yyval.str) = NewStringf("::%s%s",(yyvsp[-1].str),(yyvsp[0].str));
                 Delete((yyvsp[0].str));
               }
#line 12965 "y.tab.c"
    break;

  case 592: /* idcolon: idtemplate  */
#line 7271 "parser.y"
                            {
		 (yyval.str) = NewString((yyvsp[0].str));
   	       }
#line 12973 "y.tab.c"
    break;

  case 593: /* idcolon: NONID DCOLON idtemplatetemplate  */
#line 7274 "parser.y"
                                                 {
		 (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 12981 "y.tab.c"
    break;

  case 594: /* idcolon: OPERATOR  */
#line 7277 "parser.y"
                          {
                 (yyval.str) = NewStringf("%s", (yyvsp[0].str));
	       }
#line 12989 "y.tab.c"
    break;

  case 595: /* idcolon: OPERATOR less_valparms_greater  */
#line 7280 "parser.y"
                                                {
                 (yyval.str) = NewStringf("%s%s", (yyvsp[-1].str), (yyvsp[0].id));
	       }
#line 12997 "y.tab.c"
    break;

  case 596: /* idcolon: NONID DCOLON OPERATOR  */
#line 7283 "parser.y"
                                       {
                 (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 13005 "y.tab.c"
    break;

  case 597: /* idcolontail: DCOLON idtemplatetemplate idcolontail  */
#line 7288 "parser.y"
                                                       {
                   (yyval.str) = NewStringf("::%s%s",(yyvsp[-1].str),(yyvsp[0].str));
		   Delete((yyvsp[0].str));
               }
#line 13014 "y.tab.c"
    break;

  case 598: /* idcolontail: DCOLON idtemplatetemplate  */
#line 7292 "parser.y"
                                           {
                   (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 13022 "y.tab.c"
    break;

  case 599: /* idcolontail: DCOLON OPERATOR  */
#line 7295 "parser.y"
                                 {
                   (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 13030 "y.tab.c"
    break;

  case 600: /* idcolontail: DCNOT idtemplate  */
#line 7302 "parser.y"
                                  {
		 (yyval.str) = NewStringf("::~%s",(yyvsp[0].str));
               }
#line 13038 "y.tab.c"
    break;

  case 601: /* idtemplate: identifier  */
#line 7308 "parser.y"
                           {
		(yyval.str) = NewStringf("%s", (yyvsp[0].id));
	      }
#line 13046 "y.tab.c"
    break;

  case 602: /* idtemplate: identifier less_valparms_greater  */
#line 7311 "parser.y"
                                                 {
		(yyval.str) = NewStringf("%s%s", (yyvsp[-1].id), (yyvsp[0].id));
	      }
#line 13054 "y.tab.c"
    break;

  case 603: /* idtemplatetemplate: idtemplate  */
#line 7316 "parser.y"
                                {
		(yyval.str) = (yyvsp[0].str);
	      }
#line 13062 "y.tab.c"
    break;

  case 604: /* idtemplatetemplate: TEMPLATE identifier less_valparms_greater  */
#line 7319 "parser.y"
                                                          {
		(yyval.str) = NewStringf("%s%s", (yyvsp[-1].id), (yyvsp[0].id));
	      }
#line 13070 "y.tab.c"
    break;

  case 605: /* idcolonnt: identifier idcolontailnt  */
#line 7325 "parser.y"
                                         {
                  (yyval.str) = 0;
		  if (!(yyval.str)) (yyval.str) = NewStringf("%s%s", (yyvsp[-1].id),(yyvsp[0].str));
      	          Delete((yyvsp[0].str));
               }
#line 13080 "y.tab.c"
    break;

  case 606: /* idcolonnt: NONID DCOLON identifier idcolontailnt  */
#line 7330 "parser.y"
                                                       {
		 (yyval.str) = NewStringf("::%s%s",(yyvsp[-1].id),(yyvsp[0].str));
                 Delete((yyvsp[0].str));
               }
#line 13089 "y.tab.c"
    break;

  case 607: /* idcolonnt: identifier  */
#line 7334 "parser.y"
                            {
		 (yyval.str) = NewString((yyvsp[0].id));
   	       }
#line 13097 "y.tab.c"
    break;

  case 608: /* idcolonnt: NONID DCOLON identifier  */
#line 7337 "parser.y"
                                         {
		 (yyval.str) = NewStringf("::%s",(yyvsp[0].id));
               }
#line 13105 "y.tab.c"
    break;

  case 609: /* idcolonnt: OPERATOR  */
#line 7340 "parser.y"
                          {
                 (yyval.str) = NewString((yyvsp[0].str));
	       }
#line 13113 "y.tab.c"
    break;

  case 610: /* idcolonnt: NONID DCOLON OPERATOR  */
#line 7343 "parser.y"
                                       {
                 (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 13121 "y.tab.c"
    break;

  case 611: /* idcolontailnt: DCOLON identifier idcolontailnt  */
#line 7348 "parser.y"
                                                  {
                   (yyval.str) = NewStringf("::%s%s",(yyvsp[-1].id),(yyvsp[0].str));
		   Delete((yyvsp[0].str));
               }
#line 13130 "y.tab.c"
    break;

  case 612: /* idcolontailnt: DCOLON identifier  */
#line 7352 "parser.y"
                                   {
                   (yyval.str) = NewStringf("::%s",(yyvsp[0].id));
               }
#line 13138 "y.tab.c"
    break;

  case 613: /* idcolontailnt: DCOLON OPERATOR  */
#line 7355 "parser.y"
                                 {
                   (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 13146 "y.tab.c"
    break;

  case 614: /* idcolontailnt: DCNOT identifier  */
#line 7358 "parser.y"
                                  {
		 (yyval.str) = NewStringf("::~%s",(yyvsp[0].id));
               }
#line 13154 "y.tab.c"
    break;

  case 615: /* string: string STRING  */
#line 7364 "parser.y"
                               { 
                   (yyval.str) = NewStringf("%s%s", (yyvsp[-1].str), (yyvsp[0].id));
               }
#line 13162 "y.tab.c"
    break;

  case 616: /* string: STRING  */
#line 7367 "parser.y"
                        { (yyval.str) = NewString((yyvsp[0].id));}
#line 13168 "y.tab.c"
    break;

  case 617: /* wstring: wstring WSTRING  */
#line 7370 "parser.y"
                                  {
                   (yyval.str) = NewStringf("%s%s", (yyvsp[-1].str), (yyvsp[0].id));
               }
#line 13176 "y.tab.c"
    break;

  case 618: /* wstring: WSTRING  */
#line 7378 "parser.y"
                         { (yyval.str) = NewString((yyvsp[0].id));}
#line 13182 "y.tab.c"
    break;

  case 619: /* stringbrace: string  */
#line 7381 "parser.y"
                        {
		 (yyval.str) = (yyvsp[0].str);
               }
#line 13190 "y.tab.c"
    break;

  case 620: /* stringbrace: LBRACE  */
#line 7384 "parser.y"
                        {
                  skip_balanced('{','}');
		  (yyval.str) = NewString(scanner_ccode);
               }
#line 13199 "y.tab.c"
    break;

  case 621: /* stringbrace: HBLOCK  */
#line 7388 "parser.y"
                       {
		 (yyval.str) = (yyvsp[0].str);
              }
#line 13207 "y.tab.c"
    break;

  case 622: /* options: LPAREN kwargs RPAREN  */
#line 7393 "parser.y"
                                      {
                  Hash *n;
                  (yyval.node) = NewHash();
                  n = (yyvsp[-1].node);
                  while(n) {
                     String *name, *value;
                     name = Getattr(n,"name");
                     value = Getattr(n,"value");
		     if (!value) value = (String *) "1";
                     Setattr((yyval.node),name, value);
		     n = nextSibling(n);
		  }
               }
#line 13225 "y.tab.c"
    break;

  case 623: /* options: empty  */
#line 7406 "parser.y"
                       { (yyval.node) = 0; }
#line 13231 "y.tab.c"
    break;

  case 624: /* kwargs: idstring EQUAL stringnum  */
#line 7410 "parser.y"
                                          {
		 (yyval.node) = NewHash();
		 Setattr((yyval.node),"name",(yyvsp[-2].id));
		 Setattr((yyval.node),"value",(yyvsp[0].str));
               }
#line 13241 "y.tab.c"
    break;

  case 625: /* kwargs: idstring EQUAL stringnum COMMA kwargs  */
#line 7415 "parser.y"
                                                       {
		 (yyval.node) = NewHash();
		 Setattr((yyval.node),"name",(yyvsp[-4].id));
		 Setattr((yyval.node),"value",(yyvsp[-2].str));
		 set_nextSibling((yyval.node),(yyvsp[0].node));
               }
#line 13252 "y.tab.c"
    break;

  case 626: /* kwargs: idstring  */
#line 7421 "parser.y"
                          {
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"name",(yyvsp[0].id));
	       }
#line 13261 "y.tab.c"
    break;

  case 627: /* kwargs: idstring COMMA kwargs  */
#line 7425 "parser.y"
                                       {
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"name",(yyvsp[-2].id));
                 set_nextSibling((yyval.node),(yyvsp[0].node));
               }
#line 13271 "y.tab.c"
    break;

  case 628: /* kwargs: idstring EQUAL stringtype  */
#line 7430 "parser.y"
                                            {
                 (yyval.node) = (yyvsp[0].node);
		 Setattr((yyval.node),"name",(yyvsp[-2].id));
               }
#line 13280 "y.tab.c"
    break;

  case 629: /* kwargs: idstring EQUAL stringtype COMMA kwargs  */
#line 7434 "parser.y"
                                                        {
                 (yyval.node) = (yyvsp[-2].node);
		 Setattr((yyval.node),"name",(yyvsp[-4].id));
		 set_nextSibling((yyval.node),(yyvsp[0].node));
               }
#line 13290 "y.tab.c"
    break;

  case 630: /* stringnum: string  */
#line 7441 "parser.y"
                        {
		 (yyval.str) = (yyvsp[0].str);
               }
#line 13298 "y.tab.c"
    break;

  case 631: /* stringnum: exprnum  */
#line 7444 "parser.y"
                         {
                 (yyval.str) = Char((yyvsp[0].dtype).val);
               }
#line 13306 "y.tab.c"
    break;


#line 13310 "y.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= END)
        {
          /* Return failure if at end of input.  */
          if (yychar == END)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 7451 "parser.y"


SwigType *Swig_cparse_type(String *s) {
   String *ns;
   ns = NewStringf("%s;",s);
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSETYPE);
   yyparse();
   /*   Printf(stdout,"typeparse: '%s' ---> '%s'\n", s, top); */
   return top;
}


Parm *Swig_cparse_parm(String *s) {
   String *ns;
   ns = NewStringf("%s;",s);
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSEPARM);
   yyparse();
   /*   Printf(stdout,"typeparse: '%s' ---> '%s'\n", s, top); */
   Delete(ns);
   return top;
}


ParmList *Swig_cparse_parms(String *s, Node *file_line_node) {
   String *ns;
   char *cs = Char(s);
   if (cs && cs[0] != '(') {
     ns = NewStringf("(%s);",s);
   } else {
     ns = NewStringf("%s;",s);
   }
   Setfile(ns, Getfile(file_line_node));
   Setline(ns, Getline(file_line_node));
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSEPARMS);
   yyparse();
   /*   Printf(stdout,"typeparse: '%s' ---> '%s'\n", s, top); */
   return top;
}

