#include <errno.h>
#ifdef _WIN32
/* Unconditionally require Windows XP (0x501) for getaddrinfo.  It is not known
   how to autodetect missing getaddrinfo support.  These funcs are supported
   on W2k and even earlier with native SDK (Wspiapi.h), but MinGW does not support them. */
#define _WIN32_WINNT 0x501

# if (defined(HAVE_WINSOCK2_H) && defined(HAVE_WS2TCPIP_H))
#  include <winsock2.h>
#  include <ws2tcpip.h>
# else
#  include <winsock.h>
# endif
/* Beware: winsock2.h must come BEFORE windows.h */
# define socklen_t       int
static WSADATA wsa;
# define fcntl(a, b, c)  0

#ifndef SHUT_RD
# define SHUT_RD SD_RECEIVE
#endif
#ifndef SHUT_WR
# define SHUT_WR SD_SEND
#endif
#ifndef SHUT_RDWR
# define SHUT_RDWR SD_BOTH
#endif

#define typecorrect_getsockopt(socket, level, optname, optval, optlen)	\
    getsockopt(socket, level, optname, (char *)optval, optlen)

/* NB Might move this out of header file */
/* On Windows < Vista, getnameinfo is broken, erroring out if it cannot resolve the
   service to a name.  Try to detect this case and rerun the call with NUMERICSERV set
   to avoid the error.  Note: we should check Windows version but do not. */
int WSAAPI skt_getnameinfo(const struct sockaddr *sa, socklen_t salen, char *node,
  DWORD nodelen, char *service, DWORD servicelen, int flags) {
    int rc = getnameinfo(sa,salen,node,nodelen,service,servicelen,flags);
    int err = WSAGetLastError();      /* rc *might* not be gai error, though it should be */
    if (rc != 0 && !(flags & NI_NUMERICSERV) && err == WSANO_DATA) {
        rc = getnameinfo(sa,salen,node,nodelen,service,servicelen,flags | NI_NUMERICSERV);
        err = WSAGetLastError();
    }
    return rc ? err : 0;
}

#define ECONNREFUSED WSAECONNREFUSED
#define ETIMEDOUT WSAETIMEDOUT
/* May need to test WSAEHOSTDOWN as well */
#define ENETUNREACH WSAENETUNREACH
#define EHOSTUNREACH WSAEHOSTUNREACH
#define EWOULDBLOCK WSAEWOULDBLOCK
#define EINPROGRESS WSAEINPROGRESS
#define ENOTCONN WSAENOTCONN
#define ENOPROTOOPT WSAENOPROTOOPT
/* Might incur redefinition, but WSAEINVAL != EINVAL by default */
#define EINVAL WSAEINVAL
/* Note that it may be possible to replace all references to errno/GetLastError() with
   a getsockopt(SO_ERROR). */
#define errno (WSAGetLastError())

char *skt_strerror(int err) {
    static char msg[1024];
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS |
                  FORMAT_MESSAGE_MAX_WIDTH_MASK, NULL, err,
                  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (char*)msg, 1024, NULL);
    return msg;
}

#else
# include <fcntl.h>
# include <sys/types.h>
# include <sys/socket.h>
# include <sys/time.h>
# include <netinet/in.h>
# include <netinet/tcp.h>        /* only for TCP_* socket options */
# include <unistd.h>
# include <netdb.h>
# include <signal.h>
# define closesocket     close
# define INVALID_SOCKET  -1
# define typecorrect_getsockopt getsockopt
# define skt_getnameinfo getnameinfo
# define skt_strerror strerror
/* assume unix sockets always available on non-WIN32 */
# define HAVE_SYS_UN_H
#endif

#ifdef ECOS
#include <sys/sockio.h>
#endif

#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#else
#undef AF_UNIX
#endif

/* For testing */
/* #undef IPV6_V6ONLY */
