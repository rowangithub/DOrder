/* GPA version 5 is based on gpa-4.1.14, but handles sparse data by using
   integer arrays to represent attributes rather than bit fields. */

#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <getopt.h>
#include <time.h>
#include <glib.h>
#include "foreach.h"
/* #include "procinfo.h" */

#define VERBOSE 1
int msg_quiet = 0;
#include "msg.h"

/* #define G_DISABLE_ASSERT */

typedef GPtrArray *Array;
#define make_array() g_ptr_array_new()
#define free_array(a) g_ptr_array_free((a),1)
#define aref(a,i) ((a)->pdata[i])
#define aput(a,x) g_ptr_array_add((a),(gpointer)(x))
#define alen(a) ((a)->len)
#define reset_array(a) g_ptr_array_set_size((a), 0)
#define delete_last(a) g_ptr_array_set_size((a), alen(a)-1);

/* This is from garray.c in glib - not quite portable */

typedef struct _GRealPtrArray  GRealPtrArray;

struct _GRealPtrArray
{
  gpointer *pdata;
  guint     len;
  guint     alloc;
};

Array make_array_sized(int n) {
  GRealPtrArray *array = (GRealPtrArray *) make_array();
  array->len = 0;
  array->alloc = n;
  array->pdata = g_malloc(sizeof (gpointer) * array->alloc);
  return (Array) array;
}

Array copy_array(Array a, Array b) {
  reset_array(a);
  foreach(gpointer, x, b) {
    aput(a, x);
  }
  return a;
}

int afind(Array a, int x) {
  foreach(int, i, a) {
    if (i == x) return 1;
  }
  return 0;
}

typedef GHashTable *Hash;
#define make_hash() g_hash_table_new(g_str_hash, g_str_equal)
#define free_hash(h) g_hash_table_destroy(h)
#define hget(h,k) g_hash_table_lookup(h,k)
#define hset(h,k,v) g_hash_table_insert(h,k,(gpointer)(v))
#define hlen(h) g_hash_table_size(h)

typedef gint32 Class;
typedef gint32 Attr;

typedef struct _Instance {
  Array attr;			/* attr array */
  Class class;			/* correct class of instance */
  Class guess;			/* current guess by dlist */
} *Instance;

Instance make_instance() {
  Instance x = g_new0(struct _Instance, 1);
  x->attr = make_array();
  x->class = 0;
  x->guess = -1;
  return x;
}

Instance make_instance_using (Class class, Array attr) {
  Instance x = g_new0(struct _Instance, 1);
  x->attr = make_array_sized(alen(attr));
  copy_array(x->attr, attr);
  x->class = class;
  x->guess = -1;
  return x;
}

void free_instance(Instance x) {
  free_array(x->attr);
  g_free(x);
}

typedef struct _Rule {
  Array attr;			/* attribute array */
  Class class;			/* predicted class */
  int gain;			/* gain on training set */
  int correct;			/* number of correct instances */
  int domain;			/* number of instances in domain */
  int vgain;			/* gain on validation set */
  int vcorrect;			/* number of correct instances in validation*/
  int vdomain;			/* number of instances in domain in validation */
} *Rule;

Rule make_rule() {
  Rule x = g_new0(struct _Rule, 1);
  x->attr = make_array();
  return x;
}

void free_rule(Rule x) {
  free_array(x->attr);
  g_free(x);
}

int rmatch(Rule r, Instance x) {
  foreach (Attr, ra, r->attr) {
    int m = 0;
    foreach (Attr, rx, x->attr) {
      if (rx == ra) {
	m = 1; break;
      }
    }
    if (m == 0) return 0;
  }
  return 1;
}

typedef struct _Data {
  Array instances;		/* array of instances */
  Array attributes;		/* array of strings */
  Array classes;		/* array of strings */
  Array train;			/* training set (subset of instances) */
  Array validate;		/* validation set (subset of instances) */
} *Data;

Data make_data() {
  Data d = g_new0(struct _Data, 1);
  d->instances = make_array();
  d->attributes = make_array();
  d->classes = make_array();
  d->train = make_array();
  d->validate = make_array();
  return d;
}

void free_data(Data d) {
  foreach(Instance, x, d->instances) free_instance(x);
  foreach(gpointer, s, d->attributes) g_free(s);
  foreach(gpointer, s, d->classes) g_free(s);
  free_array(d->instances);
  free_array(d->attributes);
  free_array(d->classes);
  free_array(d->train);
  free_array(d->validate);
  g_free(d);
}

Data read_data() {
  msg1 ("reading data begins");
  Data d = make_data();		/* return value */
  Hash attr_hash = make_hash();	/* index attr strings  */
  Hash class_hash = make_hash(); /* index class strings */
  Array raw = make_array();	/* read data in here first */
  
  msg1 ("reading data begins2");
  foreach_line (buf, "../data/vote.dat") {	/* convert all class/attr to uniq int */
	msg1("reading a line ...");
    Hash h = class_hash;	/* first token is class */
    Array a = d->classes;
    foreach_token (tok, buf) {
      if (*tok == '#') break;
      gint32 i = (gint32) hget(h, tok);
      if (i == 0) {		/* not seen before */
	gchar *str = g_strdup(tok);
	i = hlen(h);
	if (i == 0) i = -1;	/* swap 0 and -1, 0 means not seen */
	hset(h, str, i);
	aput(a, str);
      }
      if (i == -1) i = 0;	/* swap them back */
      aput(raw, i);
      if (h == class_hash) {	/* the rest are attributes */
	h = attr_hash;
	a = d->attributes;
      }
    }
    aput(raw, -1);		/* mark the end of instance */
  }
  /* create instances */
  Class class = -1;
  Array attr_array = make_array();
  foreach (gint32, i, raw) {	
    if (i == -1) {		/* end of instance */
      Instance x = make_instance_using(class, attr_array);
      aput(d->instances, x);
      reset_array(attr_array);
      class = -1;
    } else if (class == -1) {	/* new instance, class */
      class = i;
    } else if (!afind(attr_array, i)) {	/* attribute */
      aput(attr_array, i);
    }
  }
  free_hash(attr_hash);
  free_hash(class_hash);
  free_array(raw);
  free_array(attr_array);
  return d;
}

void write_data(Data d) {
  foreach(Instance, x, d->instances) {
    fputs(aref(d->classes, x->class), stdout);
    foreach (Attr, i, x->attr) {
      putchar(' ');
      fputs(aref(d->attributes, i), stdout);
    }
    putchar('\n');
  }
}

typedef struct _SearchContext {
  Data data;			/* data */
  int nattr;			/* total number of attributes */
  int nclass;			/* total number of classes */
  Array attr;			/* currently searching this */
  Array best_attr;		/* best rule so far */
  int best_gain;		/* gain of best rule */
  Class best_class;		/* class of best rule */
  int *class_cnt;		/* utility array to count classes in domain */
  int *class_mistakes;		/* utility array to count mistakes in domain */
  guint memsize;		/* size of memory buffer */
  void **mem;			/* allocated memory buffer */
  void **free;			/* free pointer for memory buffer */
  guint memusage;		/* track the max mem usage */
  Array index;			/* attr -> instances array */
} *SearchContext;

SearchContext make_search_context(Data d) {
  SearchContext s = g_new0(struct _SearchContext, 1);
  s->data = d;
  s->nattr = alen(d->attributes);
  s->nclass = alen(d->classes);
  s->attr = make_array();
  s->best_attr = make_array();
  s->best_gain = 0;
  s->best_class = 0;
  s->class_cnt = g_new0(int, s->nclass);
  s->class_mistakes = g_new0(int, s->nclass);
  s->memsize = 16 * s->nattr + 4 * alen(d->train);
  msg1("Allocating %u kB of memory", (s->memsize >> 8));
  s->mem = g_malloc(s->memsize * sizeof(gpointer));
  s->free = s->mem;
  s->memusage = 0;
  s->index = make_array();

  /* Index optimization */
  msg1("Indexing training instances");
  foreach (gpointer, a, d->attributes) {
    aput(s->index, make_array());
  }
  foreach (Instance, x, d->train) {
    foreach (Attr, a, x->attr) {
      aput(aref(s->index, a), x);
    }
  }
  msg1("Indexing done");
  return s;
}

void free_search_context(SearchContext s) {
  free_array(s->attr);
  free_array(s->best_attr);
  g_free(s->class_cnt);
  g_free(s->class_mistakes);
  g_free(s->mem);
  foreach(Array, a, s->index) free_array(a);
  free_array(s->index);
  g_free(s);
}

void reset_search_context(SearchContext s) {
  reset_array(s->attr);
  reset_array(s->best_attr);
  s->best_gain = 0;
  s->best_class = 0;
  for (int i = 0; i < s->nclass; i++) {
    s->class_cnt[i] = 0;
    s->class_mistakes[i] = 0;
  }
  s->free = s->mem;
  s->memusage = 0;
  /* Note: does not touch the index! */
}

static GString *gstr;

char *attr2str(Array attr, Data d) {
  if (gstr == NULL) gstr = g_string_new("");
  g_string_assign(gstr, "");
  foreach (Attr, j, attr) {
    g_string_append(gstr, aref(d->attributes, j));
    g_string_append_c(gstr, ' ');
  }
  return gstr->str;
}


/* search: implements opus search for the best rule.  Returns 0 on
   success and -1 on "Out of Memory" error, in which case the memory
   buffer in SearchContext should be increased.  

   s: The answer is returned in the SearchContext structure which also
   contains temporary space and information for the search.  In
   particular the current rule in the search is represented in
   s->attr.
   
   inst, ninst: The instances covered by the current rule (initially
   set to the full data set.)
   
   try, ntry: The attributes to be added to the current rule.
*/
int mybrk(int x) { return x; }
#define checkmem(n) if(fptr + (n) >= s->mem + s->memsize){return mybrk(-1);} 

int search(SearchContext s, Instance *inst, int ninst, Attr *try, int ntry, int depth) {
  void **fptr = s->free;	/* free pointer for efficiency */
  msg2("Entering search [%s] ninst=%d ntry=%d", 
       attr2str(s->attr, s->data), ninst, ntry);

  /* Compute maxgain and intersection for each attribute in try */
  checkmem(3 * s->nattr);
  /* inst_ptr[a]: Pointer to the instance domain of attribute a */
  Instance **inst_ptr = (Instance **) fptr; 
  fptr += s->nattr;
  /* inst_len[a]: The length of the instance domain of attribute a */
  int *inst_len = (int *) fptr; 
  fptr += s->nattr;
  /* maxgain[a]: The maxgain for attribute a */
  int *maxgain = (int *) fptr; 
  fptr += s->nattr;

  for (int i = 0; i < ntry; i++) {
    Attr a = try[i];
    maxgain[a] = 0;
    msg2("Trying attribute [%s]", (char*)aref(s->data->attributes, a));
    g_assert(afind(s->attr, a) == 0);

    /* Compute intersection */
    if (alen(s->attr) > 0) {
      checkmem(ninst);
      inst_ptr[a] = (Instance *) fptr;
      for (int j = 0; j < ninst; j++) {
	Instance x = inst[j];
	if (afind(x->attr, a)) {
	  *fptr++ = x;
	}
      }
      inst_len[a] = (Instance *) fptr - inst_ptr[a];
    } else {
      Array z = (Array) aref(s->index, a);
      inst_ptr[a] = (Instance *) z->pdata;
      inst_len[a] = alen(z);
    }
    msg2("Domain has [%d] elements", inst_len[a]);

    /* If the new attribute has no effect, skip it */
    if (inst_len[a] == ninst) {
      msg2("Attribute has no effect, skipping");
      continue;
    }

    /* If num elements < best gain skip it */
    if ((inst_len[a] < s->best_gain) ||
	((inst_len[a] == s->best_gain) &&
	 (alen(s->attr) + 1 >= alen(s->best_attr)))) {
      msg2("Domain smaller than best gain, skipping");
      continue;
    }
    
    /* Count classes and mistakes for gain calculation */
    int nmistakes = 0;
    memset(s->class_cnt, 0, s->nclass * sizeof(int));
    memset(s->class_mistakes, 0, s->nclass * sizeof(int));
    for (int j = 0; j < inst_len[a]; j++) {
      Instance x = inst_ptr[a][j];
      s->class_cnt[x->class]++;
      if (x->class != x->guess) {
	nmistakes++;
	s->class_mistakes[x->class]++;
      }
    }
    msg2("Mistakes in domain [%d]", nmistakes);

    /* Compute best class, gain, maxgain */
    int class = 0;
    int class_cnt = s->class_cnt[0];
    int maxg = s->class_mistakes[0];
    for (int j = 1; j < s->nclass; j++) {
      if (s->class_cnt[j] > class_cnt) {
	class_cnt = s->class_cnt[j];
	class = j;
      }
      if (s->class_mistakes[j] > maxg) {
	maxg = s->class_mistakes[j];
      }
    }

    msg2("Best class is [%s] with [%d] instances",
	(char*)aref(s->data->classes, class), s->class_cnt[class]);
    msg2("Maxgain is [%d]", maxg);

    /* Skip if maxgain is less than best gain */
    if ((maxg < s->best_gain)||
	((maxg == s->best_gain) &&
	 (alen(s->attr) + 1 >= alen(s->best_attr)))) {
      msg2("Maxgain less than best gain, skipping");
      continue;
    }

    /* We have a keeper */
    maxgain[a] = maxg;

    /* Update best if new gain is higher */
    int gain = class_cnt + nmistakes - inst_len[a];
    msg2("Gain is [%d]", gain);

    if ((gain > s->best_gain) ||
	((gain == s->best_gain) &&
	 (alen(s->attr) + 1 < alen(s->best_attr)))) {
      msg2("Updating [%s %s] gain=%d best_gain=%d",
	   (char*)aref(s->data->classes, class),
	   attr2str(s->attr, s->data), gain, s->best_gain);
      s->best_gain = gain;
      s->best_class = class;
      copy_array(s->best_attr, s->attr);
      aput(s->best_attr, a);
    }
  }
  /* If this is the maxdepth, we are done */
  if (depth == 1)
    return 0;

  /* Sort attributes in try based on their maxgain */
  int compar(const void *p1, const void *p2) {
    return (maxgain[*(int*)p2] - maxgain[*(int*)p1]);
  }
  qsort(try, ntry, sizeof(int), compar);

  /* Allocate next try */
  checkmem(ntry);
  Attr *try_next = (Attr *) fptr; 
  fptr += ntry;
  int ntry_next = 0;

  /* Prepare memory for recursive calls */
  g_assert(fptr < s->mem + s->memsize);
  if (fptr - s->mem > s->memusage)
    s->memusage = fptr - s->mem;
  void **fsave = s->free;
  s->free = fptr;

  /* Do the recursive calls */
  for (int i = 0; i < ntry; i++) {
    Attr a = try[i];
    if ((maxgain[a] < s->best_gain) ||
	(maxgain[a] == 0))
      break;
    msg2("Recursing on attribute [%s]", (char*)aref(s->data->attributes, a));
    if (ntry_next > 0) {
      aput(s->attr, a);
      int rval = search(s, inst_ptr[a], inst_len[a], try_next, ntry_next, depth - 1);
      if (rval != 0) return rval;
      delete_last(s->attr);
    } else {
      msg2("Nothing to try, no recursive call.");
    }
    try_next[ntry_next++] = a;
  }

  /* Restore memory buffer */
  s->free = fsave;
  return 0;
}

Class init_class(SearchContext s, Instance *inst, int ninst) {
  msg2("Computing initial class");
  /* Count classes and mistakes */
  int nmistakes = 0;
  for (int j = 0; j < s->nclass; j++) {
    s->class_cnt[j] = 0;
    s->class_mistakes[j] = 0;
  }
  for (int j = 0; j < ninst; j++) {
    Instance x = inst[j];
    s->class_cnt[x->class]++;
    if (x->class != x->guess) {
      nmistakes++;
      s->class_mistakes[x->class]++;
    }
  }
  msg2("Mistakes in domain [%d]", nmistakes);

  /* Compute best class, gain, maxgain */
  int class = 0;
  int nclass = s->class_cnt[0];
  int maxgain = s->class_mistakes[0];
  for (int j = 1; j < s->nclass; j++) {
    if (s->class_cnt[j] > nclass) {
      nclass = s->class_cnt[j];
      class = j;
    }
    if (s->class_mistakes[j] > maxgain) {
      maxgain = s->class_mistakes[j];
    }
  }
  msg2("Best class is [%s] with [%d] instances",
	    (char*)aref(s->data->classes, class), s->class_cnt[class]);
  msg2("Maxgain is [%d]", maxgain);
  msg2("Gain is [%d]", nclass + nmistakes - ninst);
  return class;
}

/* Command line options */
void usage(char *cmd) {
  g_error("Usage: %s [-d maxdepth] [-v nvalidate] [-p pruning mode] [-w cutoff] [-r] [-q] < data > model", cmd);
}

void rule_apply(Rule r, Data d) {
  /* updates inst->guess, rule stats */
  r->gain = r->domain = r->correct = 0;
  r->vgain = r->vdomain = r->vcorrect = 0;
  foreach(Instance, x, d->train) {
    if (rmatch(r, x)) {
      r->domain++;
      if (r->class == x->class) {
	r->correct++;
	if (x->guess != x->class) {
	  r->gain++;
	}
      } else if (x->guess == x->class) {
	r->gain--;
      }
      x->guess = r->class;
    }
  }
  foreach(Instance, x, d->validate) {
    if (rmatch(r, x)) {
      r->vdomain++;
      if (r->class == x->class) {
	r->vcorrect++;
	if (x->guess != x->class) {
	  r->vgain++;
	}
      } else if (x->guess == x->class) {
	r->vgain--;
      }
      x->guess = r->class;
    }
  }
}

int main(int argc, char **argv) {
  /* g_log_set_handler(NULL, G_LOG_LEVEL_MESSAGE, my_log_func, NULL); */
  /* g_mem_set_vtable(glib_mem_profiler_table); */
  int o;
  int maxdepth = 0;
  int nvalidate = 0;
  int nwait = 0;
  int prune = 2;
  int rand_opt = 0;
  gstr = g_string_new("");

  while ((o = getopt(argc, argv, "d:m:v:p:w:rq")) != -1) {
    switch(o) {
    case 'd': maxdepth = atoi(optarg); break;
    case 'v': nvalidate = atoi(optarg); break;
    case 'w': nwait = atoi(optarg); break;
    case 'p': prune = atoi(optarg); break;
    case 'r': rand_opt = 1; srand(time(NULL)); break;
    case 'q': msg_quiet = 1; break;
    default: usage(argv[0]); break;
    }
  }

  if (maxdepth <= 0) maxdepth = G_MAXINT;
  if ((nwait <= 0) || (prune == 0)) nwait = G_MAXINT;
  if ((prune < 0) || (prune > 2)) usage(argv[0]);
  if (nvalidate < 2) prune = 0;
  msg1("$Id: gpa.c,v 1.13 2006/05/28 09:33:23 dyuret Exp $\n");
  msg1("maxdepth=%d nvalidate=%d nwait=%d prune=%d rand=%d",
       maxdepth, nvalidate, nwait, prune, rand_opt);

  Array dlist = make_array();
  msg1 ("Reading data ...");
  Data d = read_data();
  msg1("Read %d instances, %d classes, %d attributes",
       alen(d->instances), alen(d->classes), alen(d->attributes));
  if (maxdepth > alen(d->attributes)) {
    maxdepth = alen(d->attributes);
    msg1("Reset maxdepth=%d", maxdepth);
  }

  msg1 ("Spliting ...");
  /* Split the training and validation sets */
  if (nvalidate > 1) {
    foreach (Instance, x, d->instances) {
      if (0 == ((rand()>>10)%nvalidate)) {
	aput(d->validate, x);
      } else {
	aput(d->train, x);
      }
    }
  } else {
    free_array(d->train);
    d->train = d->instances;
  }
  msg1("Split data into %d training and %d validation instances",
       alen(d->train), alen(d->validate));

  SearchContext s = make_search_context(d);
  int nattr = alen(d->attributes);
  Attr *attrlist = g_new0(Attr, nattr);
  for (int j = 0; j < nattr; j++) attrlist[j] = j;

  Class c = init_class(s, (Instance *) d->train->pdata, alen(d->train));
  Rule r = make_rule();
  r->class = c;
  rule_apply(r, d);
  aput(dlist, r);

  int total = r->gain;
  int vtotal = r->vgain;
  printf("%s  # gain=%d correct=%d/%d total=%d/%d vgain=%d vcorrect=%d/%d vtotal=%d/%d\n", 
	 (char*)aref(d->classes, r->class), 
	 r->gain, r->correct, r->domain, total, alen(d->train),
	 r->vgain, r->vcorrect, r->vdomain, vtotal, alen(d->validate));
  msg1  ("%s  # gain=%d correct=%d/%d total=%d/%d vgain=%d vcorrect=%d/%d vtotal=%d/%d", 
	 (char*)aref(d->classes, r->class), 
	 r->gain, r->correct, r->domain, total, alen(d->train),
	 r->vgain, r->vcorrect, r->vdomain, vtotal, alen(d->validate));
  fflush(NULL);

  int vmax = vtotal;
  int nprint = 1;		/* num rules printed */

  for (;;) {
	msg1 ("continuing");
    reset_search_context(s);
    int rval = search(s, (Instance *) d->train->pdata, 
		      alen(d->train), attrlist, nattr, maxdepth);
    if (rval == -1) {		/* out of memory */
      s->memsize *= 2;
      msg1("Doubling memory to %u kB", (s->memsize >> 8));
      s->mem = g_realloc(s->mem, s->memsize * sizeof(gpointer));
      continue;
    }
    if (s->best_gain <= 0) break;

    Rule r = make_rule();
    copy_array(r->attr, s->best_attr);
    r->class = s->best_class;
    rule_apply(r, d);
    aput(dlist, r);
    //g_assert(r->gain == s->best_gain);

    total += r->gain;
    vtotal += r->vgain;

    msg1("[%d kB %d rules] %s %s # gain=%d correct=%d/%d total=%d/%d vgain=%d vcorrect=%d/%d vtotal=%d/%d", 
	 (s->memusage >> 8), alen(dlist),
	 (char*)aref(d->classes, r->class), attr2str(r->attr, d), 
	 r->gain, r->correct, r->domain, total, alen(d->train),
	 r->vgain, r->vcorrect, r->vdomain, vtotal, alen(d->validate));

    if ((prune == 0) ||
	((prune == 1) && (vtotal > vmax)) ||
	((prune == 2) && (vtotal >= vmax))) {
      for ( ; nprint < alen(dlist); nprint++) {
	r = aref(dlist, nprint);
	printf("%s %s # gain=%d correct=%d/%d total=%d/%d vgain=%d vcorrect=%d/%d vtotal=%d/%d\n", 
	       (char*)aref(d->classes, r->class), attr2str(r->attr, d),
	       r->gain, r->correct, r->domain, total, alen(d->train),
	       r->vgain, r->vcorrect, r->vdomain, vtotal, alen(d->validate));
      }
      fflush(NULL);
    }
    if (vtotal > vmax) {
      vmax = vtotal;
    }
    if ((prune != 0) && (alen(dlist) - nprint >= nwait)) {
      break;
    }
  }
  
  /* Clean up */
  free_search_context(s);
  free_data(d);
  g_free(attrlist);
  foreach (Rule, r, dlist) { free_rule(r); }
  free_array(dlist);
  g_string_free(gstr, TRUE);
  /* g_mem_profile(); */
}

