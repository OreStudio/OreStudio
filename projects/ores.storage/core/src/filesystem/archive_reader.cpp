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
#include "ores.storage.core/filesystem/archive_reader.hpp"
#include <algorithm>
#include <archive.h>
#include <archive_entry.h>
#include <array>
#include <memory>
#include <stdexcept>

namespace ores::storage::filesystem {

namespace {

struct reader_deleter {
    void operator()(archive* a) const noexcept {
        archive_read_free(a);
    }
};

using reader_ptr = std::unique_ptr<archive, reader_deleter>;

/// The message libarchive states, or one of this build's own when it states none.
const char* reader_error_string(archive* a) noexcept {
    const char* err = archive_error_string(a);
    return err ? err : "unknown error";
}

/// Opens the bytes as an archive this build reads, or throws.
reader_ptr open_packed(std::span<const std::byte> packed) {
    reader_ptr a(archive_read_new());
    archive_read_support_filter_gzip(a.get());
    archive_read_support_format_tar(a.get());
    if (archive_read_open_memory(a.get(), packed.data(), packed.size()) != ARCHIVE_OK)
        throw std::runtime_error(std::string("archive_reader: cannot read the archive: ") +
                                 reader_error_string(a.get()));
    return a;
}

/// The next header's entry, or nullptr at the end of the archive.
archive_entry* next_archive_header(archive* a) {
    archive_entry* entry = nullptr;
    const auto rc = archive_read_next_header(a, &entry);
    if (rc == ARCHIVE_EOF)
        return nullptr;
    if (rc != ARCHIVE_OK)
        throw std::runtime_error(std::string("archive_reader: cannot read a header: ") +
                                 reader_error_string(a));
    return entry;
}

}

std::vector<archive_reader::entry> archive_reader::index(std::span<const std::byte> packed) {
    auto a = open_packed(packed);
    std::vector<entry> entries;
    while (auto* header = next_archive_header(a.get())) {
        if (archive_entry_filetype(header) != AE_IFREG)
            continue;
        const auto* name = archive_entry_pathname(header);
        entries.push_back({name == nullptr ? std::string() : std::string(name),
                           static_cast<std::uint64_t>(archive_entry_size(header))});
    }
    return entries;
}

std::vector<std::byte> archive_reader::extract(std::span<const std::byte> packed,
                                               std::string_view name) {
    auto a = open_packed(packed);
    std::vector<std::byte> out;
    while (auto* header = next_archive_header(a.get())) {
        if (archive_entry_filetype(header) != AE_IFREG)
            continue;
        const auto* entry_name = archive_entry_pathname(header);
        if (entry_name == nullptr || name != entry_name)
            continue;
        const auto size = static_cast<std::size_t>(archive_entry_size(header));
        out.resize(size);
        std::size_t read = 0;
        while (read < size) {
            const auto got = archive_read_data(a.get(), out.data() + read, size - read);
            if (got < 0)
                throw std::runtime_error(std::string("archive_reader: cannot read an entry: ") +
                                         reader_error_string(a.get()));
            if (got == 0)
                break;
            read += static_cast<std::size_t>(got);
        }
        out.resize(read);
        return out;
    }
    return {};
}

}
