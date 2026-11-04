"""The explicit Docker psql adapter preserves secrets and scopes its paths."""
import importlib.util
from pathlib import Path
from unittest.mock import Mock

import pytest

spec = importlib.util.spec_from_file_location(
    'container_database', Path(__file__).resolve().parents[1]/'run_container_database.py')
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


def test_forwards_service_passwords_by_name_and_preserves_values(tmp_path):
    runner = Mock()
    machine_password = "literal '$ and spaces and Unicode £"
    run = module.container_psql(tmp_path, 'test-db', runner=runner)
    run({'ORES_HTTP_SERVER_DB_PASSWORD': machine_password,
         'ORES_IAM_SERVICE_DB_PASSWORD': 'second', 'PGPORT': '55432',
         'AUDIT_UNRELATED_SECRET': 'must-not-be-forwarded'},
        '-f', 'populate.sql', password='database-secret',
        database='test_database', cwd=tmp_path/'projects/ores.sql', capture=True)
    command = runner.call_args.args[0]
    assert machine_password not in command
    assert 'database-secret' not in command
    assert 'ORES_HTTP_SERVER_DB_PASSWORD' in command
    assert 'ORES_IAM_SERVICE_DB_PASSWORD' in command
    assert 'AUDIT_UNRELATED_SECRET' not in command
    assert command[command.index('-w')+1] == '/workspace/projects/ores.sql'
    assert command[-2:] == ['-f', 'populate.sql']
    assert runner.call_args.kwargs['env']['ORES_HTTP_SERVER_DB_PASSWORD'] == machine_password
    assert runner.call_args.kwargs['env']['PGPASSWORD'] == 'database-secret'
    assert runner.call_args.kwargs['capture_output'] is True


def test_internal_port_override_does_not_change_the_caller_environment(tmp_path):
    runner = Mock()
    env = {'PGPORT': '55432'}
    module.container_psql(tmp_path, 'test-db', port=5432, runner=runner)(env, '-c', 'select 1')
    assert env['PGPORT'] == '55432'
    assert runner.call_args.kwargs['env']['PGPORT'] == '5432'


def test_rejects_a_working_directory_outside_the_mounted_checkout(tmp_path):
    runner = Mock()
    with pytest.raises(ValueError):
        module.container_psql(tmp_path/'checkout', 'test-db', runner=runner)(
            {}, '-c', 'select 1', cwd=tmp_path/'elsewhere')
    runner.assert_not_called()


@pytest.mark.parametrize('name', ['--privileged', '', 'db other'])
def test_rejects_invalid_container_names(tmp_path, name):
    with pytest.raises(ValueError):
        module.container_psql(tmp_path, name)
