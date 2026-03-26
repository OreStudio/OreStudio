/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.compute.wrapper/filesystem/archiver.hpp"

#include <array>
#include <fstream>
#include <memory>
#include <stdexcept>
#include <archive.h>
#include <archive_entry.h>

namespace fs = std::filesystem;

namespace {

struct archive_read_deleter {
    void operator()(archive* a) const noexcept { archive_read_free(a); }
};

struct archive_write_deleter {
    void operator()(archive* a) const noexcept { archive_write_free(a); }
};

struct archive_entry_deleter {
    void operator()(archive_entry* e) const noexcept { archive_entry_free(e); }
};

using archive_read_ptr  = std::unique_ptr<archive, archive_read_deleter>;
using archive_write_ptr = std::unique_ptr<archive, archive_write_deleter>;
using archive_entry_ptr = std::unique_ptr<archive_entry, archive_entry_deleter>;

}

namespace ores::compute::wrapper::filesystem {

void archiver::pack(const fs::path& source_dir,
    const fs::path& output_archive) {
    archive_write_ptr a(archive_write_new());
    archive_write_add_filter_gzip(a.get());
    archive_write_set_format_pax_restricted(a.get());

#ifdef _WIN32
    const int open_rc = archive_write_open_filename_w(
        a.get(), output_archive.c_str());
#else
    const int open_rc = archive_write_open_filename(
        a.get(), output_archive.c_str());
#endif
    if (open_rc != ARCHIVE_OK)
        throw std::runtime_error(std::string("Failed to create archive: ")
            + output_archive.string() + " — " + archive_error_string(a.get()));

    for (const auto& e : fs::recursive_directory_iterator(source_dir)) {
        if (!e.is_regular_file()) continue;
        const auto rel = fs::relative(e.path(), source_dir);
        archive_entry_ptr entry(archive_entry_new());
#ifdef _WIN32
        archive_entry_set_pathname_w(entry.get(), rel.c_str());
#else
        archive_entry_set_pathname(entry.get(), rel.c_str());
#endif
        archive_entry_set_size(entry.get(),
            static_cast<la_int64_t>(fs::file_size(e.path())));
        archive_entry_set_filetype(entry.get(), AE_IFREG);
        archive_entry_set_perm(entry.get(), 0644);
        if (archive_write_header(a.get(), entry.get()) < ARCHIVE_OK)
            throw std::runtime_error(std::string("archive write header error: ")
                + archive_error_string(a.get()));
        std::ifstream in(e.path(), std::ios::binary);
        std::array<char, 16384> buf{};
        while (in.read(buf.data(), buf.size()) || in.gcount()) {
            if (archive_write_data(a.get(), buf.data(),
                    static_cast<std::size_t>(in.gcount())) < ARCHIVE_OK)
                throw std::runtime_error(
                    std::string("archive write data error: ")
                    + archive_error_string(a.get()));
        }
    }
}

void archiver::extract(const fs::path& archive_path,
    const fs::path& dest_dir) {
    archive_read_ptr  a(archive_read_new());
    archive_write_ptr out(archive_write_disk_new());

    archive_read_support_filter_gzip(a.get());
    archive_read_support_format_tar(a.get());
    archive_write_disk_set_options(out.get(),
        ARCHIVE_EXTRACT_TIME | ARCHIVE_EXTRACT_PERM |
        ARCHIVE_EXTRACT_ACL  | ARCHIVE_EXTRACT_FFLAGS);
    archive_write_disk_set_standard_lookup(out.get());

#ifdef _WIN32
    const int open_rc = archive_read_open_filename_w(
        a.get(), archive_path.c_str(), 16384);
#else
    const int open_rc = archive_read_open_filename(
        a.get(), archive_path.c_str(), 16384);
#endif
    if (open_rc != ARCHIVE_OK)
        throw std::runtime_error(std::string("Failed to open archive: ")
            + archive_path.string() + " — " + archive_error_string(a.get()));

    archive_entry* entry = nullptr;
    while (true) {
        const int r = archive_read_next_header(a.get(), &entry);
        if (r == ARCHIVE_EOF) break;
        if (r < ARCHIVE_OK)
            throw std::runtime_error(std::string("archive read error: ")
                + archive_error_string(a.get()));

#ifdef _WIN32
        const fs::path rel = fs::u8path(archive_entry_pathname(entry));
        archive_entry_set_pathname_w(entry, (dest_dir / rel).c_str());
#else
        archive_entry_set_pathname(entry,
            (dest_dir / archive_entry_pathname(entry)).c_str());
#endif

        if (archive_write_header(out.get(), entry) < ARCHIVE_OK)
            throw std::runtime_error(std::string("archive write header error: ")
                + archive_error_string(out.get()));

        if (archive_entry_size(entry) > 0) {
            const void* buff = nullptr;
            std::size_t size = 0;
            la_int64_t offset = 0;
            while (true) {
                const int rd = archive_read_data_block(
                    a.get(), &buff, &size, &offset);
                if (rd == ARCHIVE_EOF) break;
                if (rd < ARCHIVE_OK)
                    throw std::runtime_error(
                        std::string("archive data read error: ")
                        + archive_error_string(a.get()));
                if (archive_write_data_block(
                        out.get(), buff, size, offset) < ARCHIVE_OK)
                    throw std::runtime_error(
                        std::string("archive data write error: ")
                        + archive_error_string(out.get()));
            }
        }
        archive_write_finish_entry(out.get());
    }
}

}
