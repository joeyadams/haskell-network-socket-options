#include <stdint.h>

#if unix

#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <sys/types.h>

#else

#include <winsock2.h>

#endif

int c_getsockopt_int(int sockfd, int level, int optname, int *opt_out)
{
#if unix
    socklen_t optlen = sizeof(*opt_out);
#else
    int optlen = sizeof(*opt_out);
#endif
    return getsockopt(sockfd, level, optname, (void *) opt_out, &optlen);
}

int c_setsockopt_int(int sockfd, int level, int optname, int optval)
{
    return setsockopt(sockfd, level, optname, (const void *) &optval, sizeof(optval));
}

int c_setsockopt_time(int sockfd, int level, int optname, int64_t usec)
{
#if unix
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

int c_setsockopt_linger(int sockfd, int l_onoff, int l_linger)
{
    struct linger linger = {
        l_onoff  = l_onoff,
        l_linger = l_linger,
    };
    return setsockopt(sockfd, SOL_SOCKET, SO_LINGER, (const void *) &linger, sizeof(linger));
}
