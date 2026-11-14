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

/// Opens the file as an archive this build reads, or throws.
reader_ptr open_archive_file(const std::filesystem::path& packed) {
    reader_ptr a(archive_read_new());
    archive_read_support_filter_gzip(a.get());
    archive_read_support_format_tar(a.get());
#ifdef _WIN32
    const auto rc = archive_read_open_filename_w(a.get(), packed.c_str(), 16384);
#else
    const auto rc = archive_read_open_filename(a.get(), packed.c_str(), 16384);
#endif
    if (rc != ARCHIVE_OK)
        throw std::runtime_error(std::string("archive_reader: cannot read the archive ") +
                                 packed.string() + ": " + reader_error_string(a.get()));
    return a;
}

/// Whether a header names a regular file, and what that file is called.
bool named_regular_file(archive_entry* header, std::string& name) {
    if (archive_entry_filetype(header) != AE_IFREG)
        return false;
    const auto* stated = archive_entry_pathname(header);
    name = stated == nullptr ? std::string() : std::string(stated);
    return true;
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

std::vector<archive_reader::entry> archive_reader::index_file(const std::filesystem::path& packed) {
    auto a = open_archive_file(packed);
    std::vector<entry> entries;
    std::string name;
    while (auto* header = next_archive_header(a.get())) {
        if (!named_regular_file(header, name))
            continue;
        entries.push_back({name, static_cast<std::uint64_t>(archive_entry_size(header))});
    }
    return entries;
}

archive_reader::stream::stream(std::filesystem::path packed,
                               std::string_view name,
                               std::uint64_t offset,
                               std::uint64_t length) {
    auto a = open_archive_file(packed);
    std::string entry_name;
    while (auto* header = next_archive_header(a.get())) {
        if (!named_regular_file(header, entry_name) || name != entry_name)
            continue;
        const auto stated = static_cast<std::uint64_t>(archive_entry_size(header));
        const auto start = std::min(offset, stated);
        const auto rest = stated - start;
        skip_ = start;
        size_ = length == to_end ? rest : std::min(length, rest);
        found_ = true;
        break;
    }
    archive_ = a.release();
}

archive_reader::stream::~stream() {
    if (archive_ != nullptr)
        archive_read_free(archive_);
}

archive_reader::stream::stream(stream&& other) noexcept
    : archive_(other.archive_)
    , skip_(other.skip_)
    , size_(other.size_)
    , produced_(other.produced_)
    , found_(other.found_) {
    other.archive_ = nullptr;
    other.size_ = 0;
}

archive_reader::stream& archive_reader::stream::operator=(stream&& other) noexcept {
    if (this == &other)
        return *this;
    if (archive_ != nullptr)
        archive_read_free(archive_);
    archive_ = other.archive_;
    skip_ = other.skip_;
    size_ = other.size_;
    produced_ = other.produced_;
    found_ = other.found_;
    other.archive_ = nullptr;
    other.size_ = 0;
    return *this;
}

void archive_reader::stream::skip_to_the_range_start() {
    std::array<std::byte, 16384> discard{};
    while (skip_ > 0) {
        const auto want = static_cast<std::size_t>(std::min<std::uint64_t>(skip_, discard.size()));
        const auto got = archive_read_data(archive_, discard.data(), want);
        if (got < 0)
            throw std::runtime_error(std::string("archive_reader: cannot read an entry: ") +
                                     reader_error_string(archive_));
        if (got == 0) {
            skip_ = 0;
            return;
        }
        skip_ -= static_cast<std::uint64_t>(got);
    }
}

std::size_t archive_reader::stream::read(std::span<std::byte> into) {
    if (!found_ || archive_ == nullptr)
        return 0;
    if (skip_ > 0)
        skip_to_the_range_start();
    const auto remaining = size_ - produced_;
    if (remaining == 0 || into.empty())
        return 0;
    const auto want = static_cast<std::size_t>(std::min<std::uint64_t>(remaining, into.size()));
    const auto got = archive_read_data(archive_, into.data(), want);
    if (got < 0)
        throw std::runtime_error(std::string("archive_reader: cannot read an entry: ") +
                                 reader_error_string(archive_));
    produced_ += static_cast<std::uint64_t>(got);
    return static_cast<std::size_t>(got);
}

}
