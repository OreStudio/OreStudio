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

#include <fstream>
#include <stdexcept>
#include <archive.h>
#include <archive_entry.h>

namespace fs = std::filesystem;

namespace ores::compute::wrapper::filesystem {

void archiver::pack(const fs::path& source_dir,
    const fs::path& output_archive) {
    struct archive* a = archive_write_new();
    archive_write_add_filter_gzip(a);
    archive_write_set_format_pax_restricted(a);

#ifdef _WIN32
    const int open_rc = archive_write_open_filename_w(a, output_archive.c_str());
#else
    const int open_rc = archive_write_open_filename(a, output_archive.c_str());
#endif
    if (open_rc != ARCHIVE_OK) {
        const std::string msg = std::string("Failed to create archive: ")
            + output_archive.string() + " — " + archive_error_string(a);
        archive_write_free(a);
        throw std::runtime_error(msg);
    }

    for (const auto& e : fs::recursive_directory_iterator(source_dir)) {
        if (!e.is_regular_file()) continue;
        const auto rel = fs::relative(e.path(), source_dir);
        struct archive_entry* entry = archive_entry_new();
#ifdef _WIN32
        archive_entry_set_pathname_w(entry, rel.c_str());
#else
        archive_entry_set_pathname(entry, rel.c_str());
#endif
        archive_entry_set_size(entry,
            static_cast<la_int64_t>(fs::file_size(e.path())));
        archive_entry_set_filetype(entry, AE_IFREG);
        archive_entry_set_perm(entry, 0644);
        archive_write_header(a, entry);
        std::ifstream in(e.path(), std::ios::binary);
        char buf[16384];
        while (in.read(buf, sizeof(buf)) || in.gcount())
            archive_write_data(a, buf, static_cast<std::size_t>(in.gcount()));
        archive_entry_free(entry);
    }
    archive_write_close(a);
    archive_write_free(a);
}

void archiver::extract(const fs::path& archive_path,
    const fs::path& dest_dir) {
    struct archive* a = archive_read_new();
    archive_read_support_filter_gzip(a);
    archive_read_support_format_tar(a);

    struct archive* out = archive_write_disk_new();
    archive_write_disk_set_options(out,
        ARCHIVE_EXTRACT_TIME | ARCHIVE_EXTRACT_PERM |
        ARCHIVE_EXTRACT_ACL  | ARCHIVE_EXTRACT_FFLAGS);
    archive_write_disk_set_standard_lookup(out);

#ifdef _WIN32
    const int open_rc = archive_read_open_filename_w(
        a, archive_path.c_str(), 16384);
#else
    const int open_rc = archive_read_open_filename(
        a, archive_path.c_str(), 16384);
#endif
    if (open_rc != ARCHIVE_OK) {
        const std::string msg = "Failed to open archive: " +
            archive_path.string() + " — " + archive_error_string(a);
        archive_read_free(a);
        archive_write_free(out);
        throw std::runtime_error(msg);
    }

    struct archive_entry* entry = nullptr;
    while (true) {
        const int r = archive_read_next_header(a, &entry);
        if (r == ARCHIVE_EOF) break;
        if (r < ARCHIVE_OK)
            throw std::runtime_error(
                std::string("archive read error: ") + archive_error_string(a));

        const std::string entry_path =
            (dest_dir / archive_entry_pathname(entry)).string();
        archive_entry_set_pathname(entry, entry_path.c_str());

        const int wh = archive_write_header(out, entry);
        if (wh < ARCHIVE_OK)
            throw std::runtime_error(
                std::string("archive write header error: ") +
                archive_error_string(out));

        if (archive_entry_size(entry) > 0) {
            const void* buff = nullptr;
            std::size_t size = 0;
            la_int64_t offset = 0;
            while (true) {
                const int rd = archive_read_data_block(
                    a, &buff, &size, &offset);
                if (rd == ARCHIVE_EOF) break;
                if (rd < ARCHIVE_OK)
                    throw std::runtime_error(
                        std::string("archive data read error: ") +
                        archive_error_string(a));
                if (archive_write_data_block(
                        out, buff, size, offset) < ARCHIVE_OK)
                    throw std::runtime_error(
                        std::string("archive data write error: ") +
                        archive_error_string(out));
            }
        }
        archive_write_finish_entry(out);
    }

    archive_read_close(a);
    archive_read_free(a);
    archive_write_close(out);
    archive_write_free(out);
}

}
