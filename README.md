# Elo Anything!

I love using [idea fight](https://idea-fight.hoelz.ro) to rank things.
It's pretty nice!
I can get an idea of the top item in a list in `O(n)` time, and finding all values is `O(n log n)` (see [Rob Hoelz's blog post about idea fight](https://hoelz.ro/blog/idea-fight).)
I've used idea fight for years, but recently I've been running into some problems when using it:

1. I second-guess my choices. I'd like them to sort out over time instead of having to commit once.
2. Some things are pretty equal, but the selection method means that choices below the "losing" choice are automatically lowered when I'd kind of like them to be higher.
3. I can't see what's going on in the process when it's running.
4. There's no undo.

I think most of these are caused by the fact that I often do not put a series of items with a strict objective ordering into the tool!
So I could give up on the nice `O(n)` sorting time if I could get some of those benefits.

This is an exploration of sorting things with an [Elo rating system](https://en.wikipedia.org/wiki/Elo_rating_system).
Essentially, it solves my problems in the following ways:

1. If I second-guess a choice, it's fine. The match may come up again, and this style of ranking is only a rough order--it's fine if it's not strictly ordered!
2. The rating system models ties, so if two things really are equal they will become closer in rank over time.
3. I can see the rankings as I'm making choices.
4. There's still no undo, but I can add it if I find that it's really a problem! (So far I haven't.)

## Other Implementations

Urs Ganse has made a TUI version of ELO Anything in Common Lisp, available at [github.com/ursg/eloTodo](https://github.com/ursg/eloTodo). It is intended to be compatible with the save files produced by the web version.

## Building

You don't have to build anything.
Go play with a live version at [elo.bytes.zone](https://elo.bytes.zone).

But if you want to hack locally:

1. have direnv and nix
2. clone and `cd` here 
3. run `modd` to start the development server (it will tell you where to go to view the result)
3. run `nix-build` to build a deployable version

## License

The code in this repo is licensed under the BSD 3-Clause license (located at `LICENSE` in the source.)
Open Sans (the font in the UI) is released under an Apache 2.0 license available at [opensans.com](https://www.opensans.com).
