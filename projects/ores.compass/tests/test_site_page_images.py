"""
Tests for the images that compass site page publishes with a page.

Run with:  python -m pytest projects/ores.compass/tests/test_site_page_images.py -v
"""

import os
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass  # noqa: E402


@pytest.fixture
def site(tmp_path, monkeypatch):
    monkeypatch.setattr(compass, "PROJECT_ROOT", tmp_path)
    out_root = tmp_path / compass._SITE_OUTPUT
    page_dir = out_root / "doc" / "arch"
    page_dir.mkdir(parents=True)
    src_dir = tmp_path / "doc" / "arch"
    src_dir.mkdir(parents=True)
    return tmp_path, out_root, page_dir, src_dir


def write_page(page_dir, *srcs):
    html = page_dir / "page.html"
    html.write_text("".join(f'<img src="{s}" alt="x" />' for s in srcs))
    return html


def test_copies_rooted_and_relative_images(site):
    root, out_root, page_dir, src_dir = site
    (src_dir / "a.png").write_bytes(b"A")
    (src_dir / "b.svg").write_bytes(b"B")
    html = write_page(page_dir, "/OreStudio/doc/arch/a.png", "b.svg")

    assert compass._publish_page_images(html, out_root) == 2
    assert (page_dir / "a.png").read_bytes() == b"A"
    assert (page_dir / "b.svg").read_bytes() == b"B"


def test_skips_an_image_already_current(site):
    root, out_root, page_dir, src_dir = site
    (src_dir / "a.png").write_bytes(b"A")
    html = write_page(page_dir, "a.png")
    compass._publish_page_images(html, out_root)

    assert compass._publish_page_images(html, out_root) == 0


def test_recopies_an_image_whose_source_changed(site):
    root, out_root, page_dir, src_dir = site
    src = src_dir / "a.png"
    src.write_bytes(b"old")
    html = write_page(page_dir, "a.png")
    compass._publish_page_images(html, out_root)
    src.write_bytes(b"new")
    stamp = (page_dir / "a.png").stat().st_mtime + 10
    os.utime(src, (stamp, stamp))

    assert compass._publish_page_images(html, out_root) == 1
    assert (page_dir / "a.png").read_bytes() == b"new"


def test_ignores_external_missing_and_escaping_references(site):
    root, out_root, page_dir, src_dir = site
    (root / "secret.png").write_bytes(b"S")
    html = write_page(page_dir,
                      "https://example.com/x.png",
                      "/other/x.png",
                      "missing.png",
                      "../../../../../secret.png")

    assert compass._site_page_images(html, out_root) == []
    assert compass._publish_page_images(html, out_root) == 0
