#include <stdint.h>

#if unix || __APPLE__

#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <sys/types.h>

typedef socklen_t my_socklen_t;

#else

#include <winsock2.h>

typedef int my_socklen_t;

#endif

#if __APPLE__
#include <sys/time.h>
#endif

int c_getsockopt_int(int sockfd, int level, int optname, int *opt_out)
{
    my_socklen_t optlen = sizeof(*opt_out);
    return getsockopt(sockfd, level, optname, (void *) opt_out, &optlen);
}

int c_setsockopt_int(int sockfd, int level, int optname, int optval)
{
    return setsockopt(sockfd, level, optname, (const void *) &optval, sizeof(optval));
}

int c_getsockopt_time(int sockfd, int level, int optname, int64_t *usec_out)
{
#if unix || __APPLE__
    struct timeval tv;
    my_socklen_t optlen = sizeof(tv);
    int rc = getsockopt(sockfd, level, optname, (void *) &tv, &optlen);

    if (rc != -1)
        *usec_out = (int64_t)tv.tv_sec * 1000000 + tv.tv_usec;

    return rc;
#else
    DWORD msec;
    my_socklen_t optlen = sizeof(msec);
    int rc = getsockopt(sockfd, level, optname, (void *) &msec, &optlen);

    if (rc != -1)
        *usec_out = (int64_t)msec * 1000;

    return rc;
#endif
}

int c_setsockopt_time(int sockfd, int level, int optname, int64_t usec)
{
#if unix || __APPLE__
    struct timeval tv;

    if (usec <= 0) {
        tv.tv_sec  = 0;
        tv.tv_usec = 0;
    } else {
        tv.tv_sec  = usec / 1000000;
        tv.tv_usec = usec % 1000000;
    }

    return setsockopt(sockfd, level, optname, (const void *) &tv, sizeof(tv));
#else
    DWORD msec;

    if (usec <= 0) {
        msec = 0;
    } else {
        msec = usec / 1000;

        /* Prevent non-zero usec value from being treated as "never time out". */
        if (msec == 0)
            msec = 1;
    }

    return setsockopt(sockfd, level, optname, (const void *) &msec, sizeof(msec));
#endif
}

int c_getsockopt_linger(int sockfd, int *l_onoff, int *l_linger)
{
    struct linger linger;
    my_socklen_t optlen = sizeof(linger);
    int rc = getsockopt(sockfd, SOL_SOCKET, SO_LINGER, (void *) &linger, &optlen);

    if (rc != -1) {
        *l_onoff  = linger.l_onoff;
        *l_linger = linger.l_linger;
    }

    return rc;
}

int c_setsockopt_linger(int sockfd, int l_onoff, int l_linger)
{
    struct linger linger = {
        l_onoff  = l_onoff,
        l_linger = l_linger,
    };
    return setsockopt(sockfd, SOL_SOCKET, SO_LINGER, (const void *) &linger, sizeof(linger));
}
