diff --git a/2bwm.c b/2bwm.c
index 28f05ad..ffb4c7b 100644
--- a/2bwm.c
+++ b/2bwm.c
@@ -21,6 +21,7 @@
 #include <unistd.h>
 #include <string.h>
 #include <signal.h>
+#include <sys/stat.h>
 #include <xcb/randr.h>
 #include <xcb/xcb_keysyms.h>
 #include <xcb/xcb_icccm.h>
@@ -57,6 +58,7 @@ static const char *atomnames[NB_ATOMS][1] = {
 xcb_atom_t ATOM[NB_ATOMS];
 ///---Functions prototypes---///
 static void run(void);
+static void runautostart(void);
 static bool setup(int);
 static void install_sig_handlers(void);
 static void start(const Arg *);
@@ -156,6 +158,10 @@ static void getmonsize(int8_t, int16_t *, int16_t *, uint16_t *, uint16_t *,cons
 static void noborder(int16_t *,struct client *, bool);
 static void movepointerback(const int16_t, const int16_t, const struct client *);
 static void snapwindow(struct client *);
+static const char autostartblocksh[] = "autostart_blocking.sh";
+static const char autostartsh[] = "autostart.sh";
+static const char twobwmdir[] = "2bwm";
+static const char localshare[] = ".local/share";
 #include "config.h"
 
 ///---Function bodies---///
@@ -3105,6 +3111,83 @@ run(void)
 	}
 }
 
+void
+runautostart(void)
+{
+	char *pathpfx;
+	char *path;
+	char *xdgdatahome;
+	char *home;
+	struct stat sb;
+
+	/* highly unlikely that this will happen */
+	if ((home = getenv("HOME")) == NULL)
+		return;
+
+	/* if $XDG_DATA_HOME is set and not empty, use $XDG_DATA_HOME/2bwm,
+	 * otherwise use ~/.local/share/2bwm as autostart script directory
+	 */
+	xdgdatahome = getenv("XDG_DATA_HOME");
+	if (xdgdatahome != NULL && *xdgdatahome != '\0') {
+		/* space for path segments, separators and nul */
+		pathpfx = calloc(1, strlen(xdgdatahome) + strlen(twobwmdir) + 2);
+
+		if (sprintf(pathpfx, "%s/%s", xdgdatahome, twobwmdir) <= 0) {
+			free(pathpfx);
+			return;
+		}
+	} else {
+		/* space for path segments, separators and nul */
+		pathpfx = calloc(1, strlen(home) + strlen(localshare)
+		                     + strlen(twobwmdir) + 3);
+
+		if (sprintf(pathpfx, "%s/%s/%s", home, localshare, twobwmdir) < 0) {
+			free(pathpfx);
+			return;
+		}
+	}
+
+	/* check if the autostart script directory exists */
+	if (! (stat(pathpfx, &sb) == 0 && S_ISDIR(sb.st_mode))) {
+		/* the XDG conformant path does not exist or is no directory
+		 * so we try ~/.2bwm instead
+		 */
+		char *pathpfx_new = realloc(pathpfx, strlen(home) + strlen(twobwmdir) + 3);
+		if(pathpfx_new == NULL) {
+			free(pathpfx);
+			return;
+		}
+		pathpfx = pathpfx_new;
+
+		if (sprintf(pathpfx, "%s/.%s", home, twobwmdir) <= 0) {
+			free(pathpfx);
+			return;
+		}
+	}
+
+	/* try the blocking script first */
+	path = calloc(1, strlen(pathpfx) + strlen(autostartblocksh) + 2);
+	if (sprintf(path, "%s/%s", pathpfx, autostartblocksh) <= 0) {
+		free(path);
+		free(pathpfx);
+	}
+
+	if (access(path, X_OK) == 0)
+		system(path);
+
+	/* now the non-blocking script */
+	if (sprintf(path, "%s/%s", pathpfx, autostartsh) <= 0) {
+		free(path);
+		free(pathpfx);
+	}
+
+	if (access(path, X_OK) == 0)
+		system(strcat(path, " &"));
+
+	free(pathpfx);
+	free(path);
+}
+
 /* Get a defined atom from the X server. */
 xcb_atom_t
 getatom(const char *atom_name)
@@ -3310,6 +3393,8 @@ setup(int scrno)
 	events[XCB_BUTTON_PRESS]        = buttonpress;
 	events[XCB_CLIENT_MESSAGE]      = clientmessage;
 
+	runautostart();
+
 	return true;
 }
 
