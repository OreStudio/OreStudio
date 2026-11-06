"""Run Compass database commands using psql in a Linux PostgreSQL container.

This is an explicit setup path for Windows hosts whose database already runs
in Docker. The checkout must be mounted in that container. Shared SQL and its
schema fingerprint remain unchanged; its Unix shell commands run on Linux.
"""
import argparse
import os
from pathlib import Path, PurePosixPath
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]


def container_psql(root, container, container_root='/workspace', port=None,
                   runner=subprocess.run):
    """Return the psql runner Compass expects, forwarding machine secrets by name."""
    if not re.fullmatch(r'[a-zA-Z0-9][a-zA-Z0-9_.-]*', container):
        raise ValueError('Provide a Docker container name or ID.')
    mounted_root = PurePosixPath(container_root)
    if not mounted_root.is_absolute() or '..' in mounted_root.parts:
        raise ValueError('The container checkout path must be absolute.')
    root = Path(root).resolve()

    def psql(env, *args, user='postgres', password=None, database=None,
             check=True, capture=False, cwd=None):
        child_env = os.environ.copy()
        child_env.update(env)
        if password is not None:
            child_env['PGPASSWORD'] = password
        if port is not None:
            child_env['PGPORT'] = str(port)
        working_dir = mounted_root
        if cwd is not None:
            relative = Path(cwd).resolve().relative_to(root)
            working_dir /= PurePosixPath(relative.as_posix())
        forwarded = sorted(key for key in child_env
                           if key in {'PGPASSWORD', 'PGPORT'}
                           or re.fullmatch(r'ORES_[A-Z0-9_]+_DB_PASSWORD', key))
        command = ['docker', 'exec', '-i']
        for key in forwarded:
            command += ['-e', key]
        command += ['-w', str(working_dir), container,
                    'psql', '-h', '127.0.0.1', '-U', user]
        if database:
            command += ['-d', database]
        command += list(args)
        # Do not echo commands: Compass's existing role-setup arguments can
        # contain passwords. Environment values themselves travel by name.
        return runner(command, env=child_env, check=check,
                      capture_output=capture, text=True)

    return psql


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--container', required=True,
                        help='The existing Linux PostgreSQL container.')
    parser.add_argument('--container-root', default='/workspace',
                        help='Where this checkout is mounted inside the container.')
    parser.add_argument('--container-port', type=int,
                        help='Internal PostgreSQL port, if different from PGPORT in .env.')
    parser.add_argument('db_args', nargs=argparse.REMAINDER,
                        help='Compass db arguments, for example: status')
    args = parser.parse_args(argv)
    if not args.db_args:
        parser.error('Supply a Compass db command; no database operation was run.')
    sys.path.insert(0, str(ROOT/'projects/ores.compass/src'))
    import compass_db
    compass_db._psql = container_psql(ROOT, args.container,
                                     args.container_root, args.container_port)
    return compass_db.run(args.db_args, ROOT)


if __name__ == '__main__':
    sys.exit(main())
