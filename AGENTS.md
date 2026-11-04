# ORE Studio — Agent Instructions

Run `./compass.sh bearings` to orient yourself before acting.

## Pull requests

Never open a pull request unless the user asks for it in that turn. When they
ask, raise it with `compass pr create`, merge it with `compass pr merge`, read
its CI with `compass pr checks`, and record it on the task with
`compass pr record`. Do not use `gh pr create` or `gh pr merge`: they skip the
title check, the fixed body, the Traceability table and the guard rails. Run
the local check gate first. See
[How do I create a PR?](doc/recipes/github/how_do_i_create_a_pr.org).
