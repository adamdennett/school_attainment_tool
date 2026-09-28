# school_attainment_tool

**Outstanding:** read `MACHINE_SYNC_TODO.md` first. There are setup fixes to make
on the main machine (where the ignored data lives) so the repo runs from a
fresh clone. Mention it to the user at the start of a session until it's done.

- Other repos are expected as sibling folders (e.g. `../bh-school-system`).
  Never use absolute drive paths like `E:/...`; use `here::here()` and
  `dirname(here::here())`.
