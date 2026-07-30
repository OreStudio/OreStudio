/* HEALTHCHECK probe: reports healthy once any log file under /app/log
 * contains "Service ready." -- automates the same string
 * process_supervisor's wait_for_log_ready polls for natively, as a
 * container-native probe. No shell/grep available in the final
 * chainguard glibc-dynamic image, hence a standalone binary. */
#include <dirent.h>
#include <stdio.h>
#include <string.h>

static int file_has_marker(const char *path, const char *marker) {
    FILE *f = fopen(path, "r");
    if (!f) return 0;
    char line[4096];
    int found = 0;
    while (fgets(line, sizeof(line), f)) {
        if (strstr(line, marker)) { found = 1; break; }
    }
    fclose(f);
    return found;
}

int main(void) {
    const char *log_dir = "/app/log";
    const char *marker = "Service ready.";
    DIR *d = opendir(log_dir);
    if (!d) return 1;
    struct dirent *entry;
    int healthy = 0;
    while ((entry = readdir(d)) != NULL) {
        size_t len = strlen(entry->d_name);
        if (len < 4 || strcmp(entry->d_name + len - 4, ".log") != 0) continue;
        char path[4160];
        snprintf(path, sizeof(path), "%s/%s", log_dir, entry->d_name);
        if (file_has_marker(path, marker)) { healthy = 1; break; }
    }
    closedir(d);
    return healthy ? 0 : 1;
}
