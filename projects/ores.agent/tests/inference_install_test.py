import hashlib
import importlib.util
import io
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import zipfile

spec = importlib.util.spec_from_file_location(
    'inference_install', Path(__file__).resolve().parents[1] / 'scripts/install-inference.py')
installer = importlib.util.module_from_spec(spec)
spec.loader.exec_module(installer)


class InferenceInstallTests(unittest.TestCase):
    def test_truncated_download_is_retried_and_only_verified_bytes_are_cached(self):
        with tempfile.TemporaryDirectory() as directory:
            cache = Path(directory)
            expected = hashlib.sha256(b'complete fixture').hexdigest()
            with patch.object(installer.urllib.request, 'urlopen', side_effect=[
                    io.BytesIO(b'truncated'), io.BytesIO(b'complete fixture')]) as fetch, \
                    patch.object(installer.time, 'sleep'):
                result = installer.download(cache, 'https://fixture.invalid', 'model', expected)
            self.assertEqual(result.read_bytes(), b'complete fixture')
            self.assertEqual(fetch.call_count, 2)
            self.assertFalse((cache / 'model.part').exists())

    def test_repeated_checksum_mismatch_is_rejected_without_caching(self):
        with tempfile.TemporaryDirectory() as directory:
            cache = Path(directory)
            with patch.object(installer.urllib.request, 'urlopen', side_effect=lambda *a, **k: io.BytesIO(b'wrong')), \
                    patch.object(installer.time, 'sleep'):
                with self.assertRaisesRegex(ValueError, 'Checksum mismatch'):
                    installer.download(cache, 'https://fixture.invalid', 'archive', '0' * 64)
            self.assertEqual(list(cache.iterdir()), [])

    def test_verified_cache_needs_no_network(self):
        with tempfile.TemporaryDirectory() as directory:
            cache = Path(directory)
            (cache / 'archive').write_bytes(b'verified')
            with patch.object(installer.urllib.request, 'urlopen') as fetch:
                installer.download(cache, 'https://fixture.invalid', 'archive', hashlib.sha256(b'verified').hexdigest())
            fetch.assert_not_called()

    def test_cpu_install_cannot_overwrite_an_existing_cuda_root(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'inference-lock.json').write_text(json.dumps({'release': 'fixture'}))
            with patch.object(installer.urllib.request, 'urlopen') as fetch:
                with self.assertRaisesRegex(ValueError, 'separate runtime roots'):
                    installer.install(root, cpu=True)
            fetch.assert_not_called()
            self.assertEqual(sorted(p.name for p in root.iterdir()), ['inference-lock.json'])

    def test_archive_cannot_escape_its_release_directory(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            archive = root / 'bad.zip'
            with zipfile.ZipFile(archive, 'w') as z:
                z.writestr('../outside.txt', 'untrusted')
            release = root / 'release'
            release.mkdir()
            with self.assertRaisesRegex(ValueError, 'Unsafe archive path'):
                installer.extract(archive, release)
            self.assertFalse((root / 'outside.txt').exists())


if __name__ == '__main__':
    unittest.main()
