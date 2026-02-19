#!/usr/bin/env python3
"""Fix SQL convention issues in existing create files.

Applies the following fixes:
1. Remove 'set schema 'public';' lines (with trailing blank line)
2. Remove schema qualification "public"."table" -> "table"
3. Remove 'public.' prefix from function/trigger declarations
4. Remove spurious check ("change_reason_code" <> '') constraints
5. Fix trailing comma left after removing last constraint before closing paren
6. Replace old modified_by/performed_by null-check with validate fn call
"""

import re
import sys
from pathlib import Path


def fix_file(path: Path) -> bool:
    """Apply all convention fixes to a SQL file. Returns True if changed."""
    original = path.read_text()
    text = original

    # 1. Remove 'set schema 'public';' line (with optional trailing blank line)
    text = re.sub(r"^set schema 'public';\n(\n)?", "", text, flags=re.MULTILINE)

    # 2. Remove "public". schema qualification in various contexts
    # Covers: ON "public"."...", FROM "public"."...", UPDATE "public"."...",
    #         ON DELETE TO "public"."...", BEFORE INSERT ON "public"."..."
    text = re.sub(r'"public"\."(ores_[^"]+)"', r'"\1"', text)

    # 3. Remove 'public.' prefix from function declarations
    #    create or replace function public.func_name
    text = re.sub(
        r'(create or replace function )public\.(ores_\w+)',
        r'\1\2',
        text,
        flags=re.IGNORECASE
    )
    #    execute function public.func_name
    text = re.sub(
        r'(execute function )public\.(ores_\w+)',
        r'\1\2',
        text,
        flags=re.IGNORECASE
    )

    # 4. Remove check ("change_reason_code" <> '') constraint lines
    #    May have a trailing comma (middle of list) or not (last constraint)
    text = re.sub(
        r'^\s+check \("change_reason_code" <> \'\'\),?\n',
        '',
        text,
        flags=re.MULTILINE
    )

    # 5. Fix trailing comma before closing paren left after removing the last constraint
    #    Pattern: ,\n); -> \n);
    text = re.sub(r',\n(\);)', r'\n\1', text)

    # 6a. Replace full modified_by + performed_by null-check block
    old_block_both = (
        "    if new.modified_by is null or new.modified_by = '' then\n"
        "        new.modified_by = current_user;\n"
        "    end if;\n"
        "    if new.performed_by is null or new.performed_by = '' then\n"
        "        new.performed_by = current_user;\n"
        "    end if;\n"
    )
    new_block_both = (
        "    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);\n"
        "    new.performed_by = current_user;\n"
    )
    text = text.replace(old_block_both, new_block_both)

    # 6b. Replace modified_by-only null-check block (no performed_by)
    old_block_mod_only = (
        "    if new.modified_by is null or new.modified_by = '' then\n"
        "        new.modified_by = current_user;\n"
        "    end if;\n"
    )
    new_block_mod_only = (
        "    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);\n"
    )
    text = text.replace(old_block_mod_only, new_block_mod_only)

    if text != original:
        path.write_text(text)
        return True
    return False


def main():
    sql_dir = Path(__file__).parent.parent / "create"
    if not sql_dir.exists():
        print(f"Directory not found: {sql_dir}", file=sys.stderr)
        sys.exit(1)

    changed = []
    unchanged = []

    for sql_file in sorted(sql_dir.rglob("*.sql")):
        if fix_file(sql_file):
            changed.append(sql_file)
        else:
            unchanged.append(sql_file)

    print(f"Fixed {len(changed)} files:")
    for f in changed:
        print(f"  {f.relative_to(sql_dir.parent.parent)}")

    print(f"\nUnchanged: {len(unchanged)} files")


if __name__ == "__main__":
    main()
