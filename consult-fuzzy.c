//#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <unistd.h>

static void chk(const char* str, int err) {
    if (err) {
        perror(str);
        exit(1);
    }
}

static void source_end(pid_t* pid, int* fd) {
    close(*fd);
    chk("waitpid failed", waitpid(*pid, 0, 0) < 0);
    *pid = *fd = -1;
}

static void source_start(pid_t* pid, int* fd, const char* cmd) {
    int p[2];
    chk("pipe failed", pipe(p) < 0);
    chk("fork failed", (*pid = fork()) < 0);
    if (!*pid) {
        chk("dup2 failed", dup2(p[1], STDOUT_FILENO) < 0);
        close(p[0]);
        execl("/bin/sh", "sh", "-c", cmd, 0);
        chk("execl failed", 1);
    }
    close(p[1]);
    chk("fcntl failed", fcntl(p[0], F_SETFL, O_NONBLOCK) < 0);
    //chk("fcntl failed", fcntl(p[0], F_SETPIPE_SZ, 0x100000) < 0);
    *fd = p[0];
}

static void filter_end(pid_t* pid, int* wfd, int* rfd) {
    if (*wfd >= -1)
        close(*wfd);
    close(*rfd);
    chk("waitpid failed", waitpid(*pid, 0, 0) < 0);
    *pid = *wfd = *rfd = -1;
}

static void filter_start(pid_t* pid, int* wfd, int* rfd, const char* cmd, const char* filter) {
    if (*pid >= 0) {
        chk("kill failed", kill(*pid, SIGKILL) < 0);
        filter_end(pid, wfd, rfd);
    }
    int p[2], q[2];
    chk("pipe failed", pipe(p) < 0 || pipe(q) < 0);
    chk("fork failed", (*pid = fork()) < 0);
    if (!*pid) {
        chk("dup2 failed", dup2(p[0], STDIN_FILENO) < 0 || dup2(q[1], STDOUT_FILENO) < 0);
        close(p[1]);
        close(q[0]);
        execl("/bin/sh", "sh", "-c", cmd, filter, 0);
        chk("execl failed", 1);
    }
    close(p[0]);
    close(q[1]);
    chk("fcntl failed", fcntl(p[1], F_SETFL, O_NONBLOCK) < 0 || fcntl(q[0], F_SETFL, O_NONBLOCK) < 0);
    //chk("fcntl failed", fcntl(p[1], F_SETPIPE_SZ, 0x100000) < 0 || fcntl(q[0], F_SETPIPE_SZ, 0x100000) < 0);
    *wfd = p[1];
    *rfd = q[0];
}

int main(int argc, char* argv[]) {
    pid_t filter_pid = -1, source_pid = -1;
    int filter_wfd = -1, filter_rfd = -1, source_fd = -1;
    char* data = 0;
    size_t data_size = 0, data_written = 0, data_avail = 0;
    char* lines = 0;
    size_t lines_size = 0, lines_avail = 0;

    if (argc != 3) {
        fprintf(stderr, "Usage: %s <source> <filter>\n", argv[0]);
        exit(1);
    }

    // Block pipe signal since filter may terminate early
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGPIPE);
    chk("sigprocmask failed", sigprocmask(SIG_BLOCK, &set, 0) < 0);

    source_start(&source_pid, &source_fd, argv[1]);

    for (;;) {
        int max_fd = STDIN_FILENO;
        fd_set read_fds, write_fds;
        FD_ZERO(&read_fds);
        FD_SET(STDIN_FILENO, &read_fds);
        if (source_fd >= 0) {
            if (source_fd > max_fd)
                max_fd = source_fd;
            FD_SET(source_fd, &read_fds);
        }
        if (filter_rfd >= 0) {
            if (filter_rfd > max_fd)
                max_fd = filter_rfd;
            FD_SET(filter_rfd, &read_fds);
        } // TODO helper

        FD_ZERO(&write_fds);
        if (filter_wfd >= 0 && data_written < data_size) {
            if (filter_wfd > max_fd)
                max_fd = filter_wfd;
            FD_SET(filter_wfd, &write_fds);
        }

        chk("select failed", select(max_fd + 1, &read_fds, &write_fds, 0, 0) < 0);

        // Read new filter string from stdin
        if (FD_ISSET(STDIN_FILENO, &read_fds)) {
            char buf[0x400];
            ssize_t len = read(STDIN_FILENO, buf, sizeof (buf));
            chk("read from stdin failed", len < 0);
            if (!len)
                break;
            if (buf[len - 1] != '\n') {
                fputs("invalid input\n", stderr);
                exit(1);
            }
            if (len > 1) {
                buf[len - 1] = 0;
                filter_start(&filter_pid, &filter_wfd, &filter_rfd, argv[2], buf);
                data_written = 0;
                if (!lines_avail) {
                    lines_avail = 0x100000;
                    chk("malloc failed", !(lines = malloc(lines_avail)));
                }
                lines_size = 1;
                *lines = 2; // STX
            }
        }

        // Read filtered data
        if (filter_rfd >= 0 && FD_ISSET(filter_rfd, &read_fds)) {
            if (lines_size + 0x100000 > lines_avail) {
                lines_avail = lines_avail ? 2 * lines_avail : 0x100000;
                chk("realloc failed", !(lines = realloc(lines, lines_avail)));
            }
            ssize_t len = read(filter_rfd, lines + lines_size, lines_avail - lines_size - 1);
            if (len < 0)
                chk("read from filter failed", errno != EAGAIN);
            else if (len)
                lines_size += len;
            else
                filter_end(&filter_pid, &filter_wfd, &filter_rfd);

            char* p = lines, *end = lines + lines_size;
            size_t count = 0;
            while (p < end && count < 1000) {
                char* q = memchr(p, '\n', end - p);
                if (!q)
                    break;
                p = q + 1;
                ++count;
            }

            printf("COUNT %zu\n", count);

            if (count == 1000 && filter_rfd >= 0) {
                chk("kill failed", kill(filter_pid, SIGKILL) < 0);
                filter_end(&filter_pid, &filter_wfd, &filter_rfd);
            }

            if (filter_rfd < 0) {
                *p = 3; // ETX
                end = p + 1;
                p = lines;
                while (p < end) {
                    ssize_t n = write(STDOUT_FILENO, p, end - p);
                    chk("write to stdout failed", n < 0);
                    p += n;
                }
            }
        }

        // Read new data from source
        if (source_fd >= 0 && FD_ISSET(source_fd, &read_fds)) {
            if (data_size + 0x100000 > data_avail) {
                data_avail = data_avail ? 2 * data_avail : 0x100000;
                chk("realloc failed", !(data = realloc(data, data_avail)));
            }
            ssize_t len = read(source_fd, data + data_size, data_avail - data_size);
            if (len < 0)
                chk("read from source failed", errno != EAGAIN);
            else if (len)
                data_size += len;
            else
                source_end(&source_pid, &source_fd);
        }

        // Write new data to filter
        if (filter_wfd >= 0 && FD_ISSET(filter_wfd, &write_fds) && data_written < data_size) {
            ssize_t len = write(filter_wfd, data + data_written, data_size - data_written);
            if (len < 0) {
                if (errno == EPIPE)
                    filter_end(&filter_pid, &filter_wfd, &filter_rfd);
                else
                    chk("write failed", errno != EAGAIN);
            } else {
                data_written += len;
            }
        }

        // Source is done, all data has been written, close filter
        if (source_fd < 0 && filter_wfd >= 0 && data_written == data_size) {
            close(filter_wfd);
            filter_wfd = -1;
        }
    }

    return 0;
}
