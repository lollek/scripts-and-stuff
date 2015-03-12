#include <sys/inotify.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <time.h>
#include <signal.h>
#include <dirent.h>
#include <errno.h>

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define fail0(errmsg) do { perror(errmsg); return 0; } while (0)
#define fail1(errmsg) do { perror(errmsg); return 1; } while (0)

static int grep(char const * filepath, char const * string);
static int restart_kodi(void);
static int check_for_notifications(int fd);
static int reset_watcher(int *fd, int *watcher, char const * const watchpath, int mask);

static char const * const path = "/home/pi/.kodi/temp/kodi.log";
static int inotify_fd = -1;
static int inotify_watcher = -1;

/**
 * grep
 * 		find a string in a file
 * @filepath	file to read
 * @string	string to search for
 *
 * @returns
 * 		1 if found
 * 		otherwise 0
 *
 * @bugs
 * 		Can probably fail if the file contains a line longer than BUFSIZ
 */
static int grep(char const * filepath, char const * string)
{
    FILE * file = fopen(filepath, "r");
    int status = 0;
    char buf[BUFSIZ];

    if (file == NULL)
    {
        fprintf(stderr, "open: %s: %s\n", filepath, strerror(errno));
        return 0;
    }

    while (fgets(buf, sizeof(buf), file) != NULL)
        if (strstr(buf, string) != NULL)
        {
            status = 1;
            break;
        }

    fclose(file);
    return status;
}

/**
 * find_kodi_pid
 * 		finds and returns a process named *kodi*
 *
 * @returns
 * 		pid of process
 * 		0 if none is found
 *
 * @bugs
 * 		Will only return 1 process, even if there are many that match
 */
static int find_kodi_pid(void)
{
    DIR * dir = opendir("/proc");
    struct dirent *dirp;
    if (dir == NULL)
        fail0("opendir");

    while ((dirp = readdir(dir)) != NULL)
    {
        int error = 0;
        char buf[BUFSIZ];
        int i;

        for (i = 0; dirp->d_name[i] != '\0'; ++i)
            if (!isdigit(dirp->d_name[i]))
            {
                error = 1;
                break;
            }
        if (error)
            continue;

        strcpy(buf, "/proc/");
        strcpy(&buf[6], dirp->d_name);
        strcat(&buf[6], "/stat");

        if (grep(buf, "kodi"))
            return atoi(dirp->d_name);
    }

    closedir(dir);
    return 0;
}

/**
 * restart_kodi
 * 		find and restart kodi, also updates the inotify watcher
 *
 * @returns
 * 		0 on success
 * 		1 otherwise
 */
static int restart_kodi(void)
{
    struct timespec const shortwait = { 0, 100000000L };
    struct timespec const longwait = { 10, 0L };
    struct stat statbuf;
    char buf[BUFSIZ];
    int pid = find_kodi_pid();

    if (pid == 0)
        return 1;

    sprintf(buf, "/proc/%d", pid);

    printf("killing kodi\n");
    kill(pid, SIGHUP);

    printf("waiting for kodi cleanup\n");
    while (stat(buf, &statbuf) != -1)
        nanosleep(&shortwait, NULL);

    printf("waiting for kodi to restart\n");
    while (find_kodi_pid() == 0)
        nanosleep(&shortwait, NULL);

    printf("Waiting a while to make sure kodi can change the logfile\n");
    nanosleep(&longwait, NULL);

    printf("Resetting watcher\n");
    reset_watcher(&inotify_fd, &inotify_watcher, path, IN_MODIFY);

    printf("Done\n");
    return 0;
}

/**
 * check_for_notifications
 * 		wait for inotify_data, and restart kodi if neccessary
 *
 * @returns
 * 		always 0
 */
static int check_for_notifications(int fd)
{
    char const * badstr = "Recorder 1 (1): power status changed from 'standby' to 'on'";
    char buf[BUFSIZ];
    static struct tm last_event;

    time_t t = time(NULL);
    struct tm tm = *localtime(&t);

    read(fd, buf, BUFSIZ);
    if (tm.tm_mday == last_event.tm_mday &&
        tm.tm_hour == last_event.tm_hour &&
        tm.tm_min == last_event.tm_min &&
        tm.tm_sec == last_event.tm_sec)
        return 0;

    if (grep(path, badstr))
        restart_kodi();
    last_event = tm;

    return 0;
}

/**
 * reset_watcher
 * 		(re)sets a inotify watcher
 * @fd		inotify fd
 * @watcher	inotify watcher fd
 * @watchpath	the path to watch
 * @mask	mask for watch for (see inotify(7))
 *
 * @returns
 * 		0 on success
 * 		exits application otherwise
 */
static int reset_watcher(int *fd, int *watcher, char const * const watchpath, int mask)
{
    if (*fd == -1)
    {
        *fd = inotify_init();
        if (*fd == -1)
        {
            fprintf(stderr, "inotify_init: %s\n", strerror(errno));
            exit(1);
        }
    }
    else if (inotify_rm_watch(*fd, *watcher) == -1)
    {
        fprintf(stderr, "inotify_rm_watch: %s\n", strerror(errno));
        exit(1);
    }

    *watcher = inotify_add_watch(*fd, watchpath, mask);
    if (*watcher == -1)
    {
        fprintf(stderr, "inotify_add_watch: %s\n", strerror(errno));
        exit(1);
    }

    return 0;
}

int main(void)
{
    reset_watcher(&inotify_fd, &inotify_watcher, path, IN_MODIFY);
    for (;;)
        check_for_notifications(inotify_fd);

    close(inotify_fd);
    return 0;
}
