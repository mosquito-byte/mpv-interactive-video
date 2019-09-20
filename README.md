# mpv-interactive-video

mpv Lua script for watching interactive videos (such as Netflix's *Black
Mirror: Bandersnatch*).

![Screenshot of first interactive moment in Black Mirror: Bandersnatch](img/bandersnatch.jpg)

[Source](https://www.netflix.com/title/80988062) (Credit: Netflix)

## Usage

Copy `interactive-video.lua` file to the `scripts` subdirectory of the mpv
configuration directory (usually `~/.config/mpv/scripts/`) for automatic
loading.

You can also load the script manually by passing the
`--script=path/to/interactive-video.lua` command-line parameter to mpv.

In order for mpv to play an interactive movie, you should have in the same
directory:
- the movie file (say `movie.mkv`);
- the *segments* JSON data file (which contains the `segments` element),
  renamed as `movie.seg.json`;
- and the *interactive video moments* JSON data file (which contains the
  `interactiveVideoMoments` element), renamed as `movie.ivm.json`.

You can then type `mpv movie.mkv` to play the movie. If the two JSON data files
are detected properly, the script should display something like

    Playing: movie.mkv
    [interactive_video] Found JSON data files for interactive video playback:
    [interactive_video]   seg: ./movie.seg.json
    [interactive_video]   ivm: ./movie.ivm.json 
     (+) Video --vid=1 (*) (h264 1280x720 25.000fps)
     (+) Audio --aid=1 --alang=eng (*) (aac 2ch 48000Hz)

on the terminal.

Example JSON data files are provided in the `json` directory.

## Controls

Most of the usual mpv controls are unchanged.

Navigation keys (*left*, *right*, *up*, *down*, *page-up*, *page-down*) jump
forward or backward by 10 seconds, 1 minute, or 10 minutes, respectively,
unless mpv encounters an interactive moment before reaching the desired
position. Using the *shift* modifier will force mpv to jump by the selected
amount of time, oblivious to any interactive moment.

When a choice appears on the OSD, the arrow keys can be used to highlight the
answer, and then *enter* to submit it. Likewise, numbers can be typed in using
the number keys.
