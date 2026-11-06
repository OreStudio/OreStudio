"""Packaging gates: missing DLLs, private runtimes, architecture and stale output."""
import importlib.util
from pathlib import Path
import struct
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location(
    'package_ore_windows', Path(__file__).resolve().parents[2] / 'package_ore_windows.py')
package = importlib.util.module_from_spec(spec)
spec.loader.exec_module(package)


class WindowsPackageTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.runtime = self.root / 'runtime'
        self.system = self.root / 'System32'
        self.runtime.mkdir()
        self.system.mkdir()

    def test_missing_transitive_dependency_fails(self):
        with self.assertRaisesRegex(FileNotFoundError, 'QuantExt.dll'):
            package.resolve_dependency('QuantExt.dll', [self.runtime], self.system)

    def test_vc_runtime_in_system32_does_not_mask_incomplete_package(self):
        (self.system / 'VCRUNTIME140.dll').touch()
        with self.assertRaises(FileNotFoundError):
            package.resolve_dependency('VCRUNTIME140.dll', [self.runtime], self.system)
        redist = self.runtime / 'VCRUNTIME140.dll'
        redist.touch()
        self.assertEqual(package.resolve_dependency(redist.name, [self.runtime], self.system), redist)

    def test_os_api_contracts_and_system_dlls_remain_host_dependencies(self):
        self.assertIsNone(package.resolve_dependency('api-ms-win-core-file-l1-1-0.dll', [], self.system))
        (self.system / 'KERNEL32.dll').touch()
        self.assertIsNone(package.resolve_dependency('KERNEL32.dll', [], self.system))

    def test_rejects_x86_and_accepts_x64_pe_header(self):
        binary = self.root / 'ore.exe'
        header = bytearray(128)
        header[:2] = b'MZ'
        struct.pack_into('<I', header, 0x3c, 64)
        header[64:68] = b'PE\0\0'
        struct.pack_into('<H', header, 68, 0x14c)
        binary.write_bytes(header)
        with self.assertRaisesRegex(ValueError, 'Not an x64'):
            package.require_x64(binary)
        struct.pack_into('<H', header, 68, 0x8664)
        binary.write_bytes(header)
        package.require_x64(binary)

    def test_sample_cannot_pass_with_previous_output(self):
        sample = self.root / 'sample'
        (sample / 'Input').mkdir(parents=True)
        (sample / 'Input/ore.xml').write_text('<ORE/>')
        (sample / 'Output').mkdir()
        (sample / 'Output/npv.csv').write_text('stale')
        job = self.root / 'job'
        package.extract_sample(sample, job)
        self.assertTrue((job / 'Input/ore.xml').is_file())
        self.assertFalse((job / 'Output').exists())

    def test_dependency_scan_rejects_missing_import_of_bundled_dll(self):
        executable = self.root / 'ore.exe'
        executable.touch()
        (self.runtime / 'first.dll').touch()
        stage = self.root / 'stage'
        stage.mkdir()
        def dependencies(binary):
            return ['first.dll'] if binary.name == 'ore.exe' else ['missing.dll']
        with patch.object(package, 'require_x64'), patch.object(package, 'imports', side_effect=dependencies):
            with self.assertRaisesRegex(FileNotFoundError, 'missing.dll'):
                package.bundle_dependencies(executable, stage, [self.runtime], self.system)


if __name__ == '__main__':
    unittest.main()
