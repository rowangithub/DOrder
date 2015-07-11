#ifndef VERBOSE
#define VERBOSE 0
#endif

extern int msg_quiet;
#define NOMSG G_STMT_START{ (void)0; }G_STMT_END

#if (VERBOSE == 0)
#define msg0(...) if(!msg_quiet){g_message(__VA_ARGS__);}
#define msg1(...) NOMSG
#define msg2(...) NOMSG
#elif (VERBOSE == 1)
#define msg0(...) if(!msg_quiet){g_message(__VA_ARGS__);}
#define msg1(...) if(!msg_quiet){g_message(__VA_ARGS__);}
#define msg2(...) NOMSG
#elif (VERBOSE >= 2)
#define msg0(...) if(!msg_quiet){g_message(__VA_ARGS__);}
#define msg1(...) if(!msg_quiet){g_message(__VA_ARGS__);}
#define msg2(...) if(!msg_quiet){g_message(__VA_ARGS__);}
#endif
