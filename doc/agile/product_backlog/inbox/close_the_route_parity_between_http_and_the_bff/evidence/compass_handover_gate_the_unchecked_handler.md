# Handover: gate the handler that checks no permission

For the agent working in **MASD-Project/compass** on the machine that owns that
checkout. Written 2026-11-25 against compass commit `8259d8b2ae` (v0.0.13).

## What is wrong

compass-ng's `handler_permissions` codec checks one direction only: every
permission code a scanned handler **checks** is seeded. A handler that checks
nothing therefore passes silently, and that is the hole the ORE Studio task
`CCB1D9D1-B5F8-43D5-B5E5-A6A3A5BDC9E7` names. 48 handlers in the tree today
check no permission (the census beside this file lists them), and the gate reads
none of them.

The registry half of that task is **not** yours: the `components` option in
`.compass.org` can name every component, and ORE Studio will do so once the 102
codes it surfaces are seeded and granted. What needs compass is the second rule.

## The change

In `projects/compass.sql/src/codecs/handler_permissions.cpp`, at v0.0.13:

- the rules array at line ~41 declares one rule, `unseeded_permission`;
- `scanned()` at line ~104 builds the file list;
- the descriptor is at line ~192.

Add a second rule to the same codec:

```
{"unchecked_handler", "Every handler of a scanned component calls has_permission.", <sources>, ct::blame_method::line}
```

and make the run report, for each scanned file, one diagnostic at the line of
the handler's class declaration (or at line 1 when the scan cannot find it) when
the file holds no `has_permission` call. The existing exception mechanism is the
answer for the handlers that legitimately need none, which is most of them: 13
DQ publish consumers, the sign-in and bootstrap handlers, the compute workers
and the server's health read. They will carry a `compass:except` comment naming
the rule and the reason, so every exemption is a line in the code rather than a
missing finding.

Two details that decide whether the rule is usable:

1. **The exemption must be able to state why.** The census groups the 48 by
   kind, and the reasons are of a few sorts — "the caller is the DQ service",
   "no session exists yet", "public by design". The exception comment should be
   able to hold a sentence, as the other codecs' exceptions do.
2. **A file that holds the call anywhere passes.** The check is coarse on
   purpose: it is a reminder that a hand-written handler must decide about
   authority, not a proof that the decision is the right one. Proving the code
   is the right one is what the *first* rule and the seeds are for.

## Proving it

- A fixture component with a handler that checks a permission and one that does
  not: the second must report, and the first must not.
- A second fixture where the unchecked handler carries the exception comment:
  it must not report.
- `compass.docs`/`compass.sql` tests as the existing codec tests are written.

## Release and what ORE Studio does after it

Same as the recipe-body handover: bump the version in `CMakeLists.txt:33`,
merge, tag, publish, then ORE Studio raises `build/scripts/compass_ng.pin`.

Then, in ORE Studio:

1. Widen `components` in `.compass.org` to every component with handlers.
2. Seed the 102 codes the widened registry reports, and grant each to the roles
   that need it — a product decision per code, which is why it is not in this
   handover.
3. Add the `compass:except` reasons to the handlers the census calls legitimate,
   and `has_permission` to the ones it calls holes.
4. Add the same enforcement to the HTTP side, where the router has no permission
   hook: that half is ORE Studio's own and does not wait on compass.
