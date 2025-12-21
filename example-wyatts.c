#include <stdio.h>
#include <unistd.h>
#include <locale.h>
#include <notcurses/notcurses.h>

int main(){
    setlocale(LC_ALL, "");
    notcurses_options ncopt;

    memset(&ncopt, 0, sizeof(ncopt));

    struct notcurses* nc = notcurses_init(&ncopt, stdout);
    struct ncplane* stdplane = notcurses_stdplane(nc);
    for (int i = 0; i < 25; i++){
        for (int j = 0; j < 25; j++){
            ncplane_putchar_yx(stdplane, i, j, '*');
            notcurses_render(nc);
            sleep (0.05);
            // int response = nanosleep(&request, &remaining);
        }
    }
    sleep(2);
    notcurses_stop(nc);
    return 0;
}
