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
#include "ores.storage.core/filesystem/local_store.hpp"
#include "ores.logging/make_logger.hpp"
#include <algorithm>
#include <fstream>
#include <iterator>

namespace ores::storage::filesystem {

using namespace ores::logging;

namespace {
inline auto& local_store_lg() {
    static auto instance = make_logger("ores.storage.filesystem.local_store");
    return instance;
}
}

object_not_found::object_not_found(const std::string& what)
    : std::runtime_error(what) {}

local_store::local_store(std::filesystem::path root)
    : root_(std::move(root)) {
    BOOST_LOG_SEV(local_store_lg(), debug) << "Storage root: " << root_.string();
}

bool local_store::is_valid_bucket(std::string_view bucket) {
    if (bucket.empty() || bucket == "." || bucket == "..")
        return false;
    return std::none_of(bucket.begin(), bucket.end(), [](const char c) {
        return c == '/' || c == '\\' || c == '\0';
    });
}

std::optional<std::filesystem::path> local_store::resolve(const std::string& bucket,
                                                          const std::string& key) const {
    if (!is_valid_bucket(bucket))
        return std::nullopt;

    const auto bucket_dir = (root_ / bucket).lexically_normal();
    const auto full_path = (bucket_dir / key).lexically_normal();

    // The resolved key must stay inside its bucket. A key that walks out of it
    // is refused here, at the one place that turns a key into a location, so
    // every caller inherits the guard.
    auto [it1, it2] =
        std::mismatch(bucket_dir.begin(), bucket_dir.end(), full_path.begin(), full_path.end());
    if (it1 != bucket_dir.end()) {
        BOOST_LOG_SEV(local_store_lg(), warn)
            << "Path traversal attempt: bucket=" << bucket << " key=" << key;
        return std::nullopt;
    }

    return full_path;
}

bool local_store::is_valid_key(const std::string& bucket, const std::string& key) const {
    return resolve(bucket, key).has_value();
}

void local_store::put(const std::string& bucket, const std::string& key, std::string_view data) {
    const auto path = resolve(bucket, key);
    if (!path)
        throw std::runtime_error("Invalid bucket or key: " + bucket + "/" + key);

    std::error_code ec;
    std::filesystem::create_directories(path->parent_path(), ec);
    if (ec)
        throw std::runtime_error("Cannot create bucket directory: " + ec.message());

    std::ofstream f(*path, std::ios::binary | std::ios::trunc);
    if (!f)
        throw std::runtime_error("Cannot write object: " + path->string());
    f.write(data.data(), static_cast<std::streamsize>(data.size()));
    if (!f)
        throw std::runtime_error("Cannot write object: " + path->string());

    BOOST_LOG_SEV(local_store_lg(), debug)
        << "put: bucket=" << bucket << " key=" << key << " bytes=" << data.size();
}

std::string local_store::get(const std::string& bucket, const std::string& key) const {
    const auto path = resolve(bucket, key);
    if (!path)
        throw object_not_found(bucket + "/" + key);

    std::ifstream f(*path, std::ios::binary);
    if (!f)
        throw object_not_found(bucket + "/" + key);

    BOOST_LOG_SEV(local_store_lg(), debug) << "get: bucket=" << bucket << " key=" << key;
    return std::string(std::istreambuf_iterator<char>(f), std::istreambuf_iterator<char>());
}

bool local_store::exists(const std::string& bucket, const std::string& key) const {
    return size(bucket, key).has_value();
}

std::optional<std::uintmax_t> local_store::size(const std::string& bucket,
                                                const std::string& key) const {
    const auto path = resolve(bucket, key);
    if (!path)
        return std::nullopt;

    std::error_code ec;
    if (!std::filesystem::is_regular_file(*path, ec))
        return std::nullopt;
    const auto bytes = std::filesystem::file_size(*path, ec);
    if (ec)
        return std::nullopt;
    return bytes;
}

bool local_store::remove(const std::string& bucket, const std::string& key) {
    const auto path = resolve(bucket, key);
    if (!path)
        return false;

    std::error_code ec;
    const bool existed = std::filesystem::is_regular_file(*path, ec);
    std::filesystem::remove(*path, ec);
    if (ec && ec != std::errc::no_such_file_or_directory)
        throw std::runtime_error("Cannot delete object: " + path->string() + ": " + ec.message());

    BOOST_LOG_SEV(local_store_lg(), debug)
        << "remove: bucket=" << bucket << " key=" << key << " existed=" << existed;
    return existed;
}

std::vector<local_store::entry> local_store::list(const std::string& bucket,
                                                  const std::string& prefix) const {
    std::vector<entry> entries;
    if (!is_valid_bucket(bucket))
        return entries;

    const auto dir = (root_ / bucket).lexically_normal();
    std::error_code ec;
    if (!std::filesystem::is_directory(dir, ec))
        return entries;

    for (const auto& item : std::filesystem::recursive_directory_iterator(dir, ec)) {
        std::error_code item_ec;
        if (!item.is_regular_file(item_ec))
            continue;

        auto key = std::filesystem::relative(item.path(), dir, item_ec).generic_string();
        if (item_ec)
            continue;
        if (!prefix.empty() && !key.starts_with(prefix))
            continue;

        entries.push_back(entry{std::move(key), item.file_size(item_ec)});
    }

    std::sort(entries.begin(), entries.end(), [](const entry& left, const entry& right) {
        return left.key < right.key;
    });
    return entries;
}

}
