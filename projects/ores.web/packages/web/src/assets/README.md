# Artwork

The icon set is `icons/`: Microsoft Fluent UI System Icons, the vocabulary the
interface draws on. `doc/knowledge/ui/icon_guidelines.org` is the source of
truth for the artwork, and `projects/ores.web/modeling/icon_reference.org`
lists the subset this client uses.

Two brand images, both from the main site at `orestudio.github.io/OreStudio`,
which is the source of truth. If the branding changes there, copy the new files
across rather than editing them here.

| File                    | Used for                        | Comes from                        |
| ----------------------- | ------------------------------- | --------------------------------- |
| `ore-studio-splash.png` | The landing page hero           | `assets/images/splash-screen.png` |
| `ore-studio-icon.png`   | The header mark and the favicon | `assets/images/modern-icon.png`   |

`brand.ts` is the one place they are imported, so a screen reaches them by
importing it rather than by naming a file path.

The artwork is GPL-3, the same licence as this repository, and comes from the
same project, so it is used here rather than redrawn.
