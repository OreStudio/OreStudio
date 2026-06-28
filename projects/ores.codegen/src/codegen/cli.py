import argparse
import logging
import sys
from pathlib import Path

log = logging.getLogger(__name__)


def _base_dir() -> Path:
    # src/codegen/cli.py → src/codegen → src → projects/ores.codegen
    return Path(__file__).resolve().parent.parent.parent


def main() -> None:
    from .logging_config import configure  # noqa: PLC0415
    from .generate import cmd_generate, cmd_regenerate  # noqa: PLC0415
    from .list_cmd import cmd_list  # noqa: PLC0415
    from .diff_cmd import cmd_diff  # noqa: PLC0415

    parser = argparse.ArgumentParser(
        prog="codegen.py",
        description="ores.codegen: SQL and C++ code generator for ORE Studio.",
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="Enable debug logging"
    )

    sub = parser.add_subparsers(dest="command", required=True)

    # --- generate ---
    gen_p = sub.add_parser(
        "generate",
        aliases=["gen"],
        help="Generate SQL/C++ from a single model file.",
    )
    gen_p.add_argument(
        "--model",
        required=True,
        metavar="PATH",
        help="Path to the model file (e.g. models/refdata/currency_entity.json)",
    )
    gen_p.add_argument(
        "--profile",
        default="sql",
        metavar="PROFILE",
        help="DEPRECATED (use --address): legacy profile (sql, all-cpp, ...); default: sql",
    )
    gen_p.add_argument(
        "--address",
        default=None,
        metavar="ADDRESS",
        help="Physical-space address to generate (e.g. ores.sql, ores.cpp.qt); "
             "overrides --profile.",
    )
    gen_p.add_argument(
        "--dry-run",
        action="store_true",
        help="Print output paths that would be written without writing them",
    )

    # --- regenerate ---
    regen_p = sub.add_parser(
        "regenerate",
        aliases=["regen"],
        help="Regenerate SQL for a named component or all components.",
    )
    regen_scope = regen_p.add_mutually_exclusive_group(required=True)
    regen_scope.add_argument(
        "--component",
        metavar="NAME",
        help="Regenerate all entity models for a component (e.g. refdata)",
    )
    regen_scope.add_argument(
        "--all",
        action="store_true",
        help="Regenerate all components",
    )
    regen_p.add_argument(
        "--profile",
        default="sql",
        metavar="PROFILE",
        help="DEPRECATED (use --address): legacy profile; default: sql",
    )
    regen_p.add_argument(
        "--address",
        default=None,
        metavar="ADDRESS",
        help="Physical-space address to generate; overrides --profile.",
    )
    regen_p.add_argument(
        "--dry-run",
        action="store_true",
        help="Print output paths that would be written without writing them",
    )

    # --- list ---
    list_p = sub.add_parser(
        "list",
        help="List models or components.",
    )
    list_p.add_argument(
        "what",
        choices=["models", "components"],
        help="What to list",
    )
    list_p.add_argument(
        "--component",
        metavar="NAME",
        help="Component to scope the listing (required for 'list models')",
    )

    # --- diff ---
    diff_p = sub.add_parser(
        "diff",
        help="Show what would change in output files without writing "
             "(requires unified template; not yet available).",
    )
    diff_scope = diff_p.add_mutually_exclusive_group(required=True)
    diff_scope.add_argument(
        "--component",
        metavar="NAME",
        help="Diff all entity models for a component",
    )
    diff_scope.add_argument(
        "--all",
        action="store_true",
        help="Diff all components",
    )
    diff_p.add_argument(
        "--profile",
        default="sql",
        metavar="PROFILE",
        help="Generation profile; default: sql",
    )

    args = parser.parse_args()
    configure(verbose=args.verbose)
    base_dir = _base_dir()

    command = args.command
    if command in ("generate", "gen"):
        sys.exit(cmd_generate(args, base_dir))
    elif command in ("regenerate", "regen"):
        sys.exit(cmd_regenerate(args, base_dir))
    elif command == "list":
        sys.exit(cmd_list(args, base_dir))
    elif command == "diff":
        sys.exit(cmd_diff(args, base_dir))
