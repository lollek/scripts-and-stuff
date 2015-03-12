/**
 * This program is a workaround for an issue with kodi on raspberry pi with
 * HDMI-CEC on a LG-TV
 *
 * Issue:
 *  Start the raspberry pi, switch source from the raspberry (e.g. to
 *  cable) and shut down the TV. When you restart the TV and switch source back
 *  to the raspberry pi, the remote will not work any more, and only starts
 *  working when kodi restarts (raspi reboot is not needed)
 *
 * Workaround:
 *  This program waits for the situation described above by checking data
 *  written to the ~pi/.kodi/temp/kodi.log, if a special kind of string appears
 *  in the log, the pi will find the kodi process and send a SIGHUP.
 *
 * It's possible that you might need to enable some extra logging in order for
 * this to work. You might need to add this data to
 * ~pi/.kodi/userdata/advancedsettings.xml:
 * <advancedsettings>
 *   <loglevel>1</loglevel>
 *   <debug>
 *     <extralogging>true</extralogging>
 *     <setextraloglevel>64,2048,32768</setextraloglevel>
 *     <showloginfo>false</showloginfo>
 *   </debug>
 * </advancedsettings>
 *
 * Compiled for raspberry pi here:
 * http://iix.se/files/fix-standby-cec-armv6l-kodi
 */

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
    int status = 0;
    struct dirent *dirp;

    if (dir == NULL)
        fail0("opendir");

    while ((dirp = readdir(dir)) != NULL)
    {
        char buf[BUFSIZ];

        if (!isdigit(dirp->d_name[0]))
            continue;

        sprintf(buf, "/proc/%s/stat", dirp->d_name);
        if (grep(buf, "kodi"))
        {
            status = atoi(dirp->d_name);
            break;
        }
    }

    closedir(dir);
    return status;
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

    kill(pid, SIGHUP);

    /* Wait for pid to disappear */
    sprintf(buf, "/proc/%d", pid);
    while (stat(buf, &statbuf) != -1)
        nanosleep(&shortwait, NULL);

    /* Wait for kodi to appear */
    while (!find_kodi_pid())
        nanosleep(&shortwait, NULL);

    /* Wait for kodi to rotate logfile */
    nanosleep(&longwait, NULL);

    reset_watcher(&inotify_fd, &inotify_watcher, path, IN_MODIFY);
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
    static time_t last_event;
    time_t now;

    /* Wait for event */
    read(fd, buf, BUFSIZ);

    now = time(NULL);
    if (!difftime(last_event, now))
        return 0;

    if (grep(path, badstr))
        restart_kodi();
    last_event = now;
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
