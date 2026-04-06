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
#include "ores.storage/net/storage_transfer.hpp"

#include <chrono>
#include <fstream>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.storage/net/http_client.hpp"
#include "ores.storage/net/storage_paths.hpp"
#include "ores.storage/filesystem/archiver.hpp"

namespace ores::storage::net {

using namespace ores::logging;
namespace fs = std::filesystem;

namespace {

inline static std::string_view logger_name = "ores.storage.net.storage_transfer";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

fs::path make_temp_archive() {
    // Instantiate the generator on the stack per call for thread safety.
    boost::uuids::random_generator gen;
    return fs::temp_directory_path() / (boost::uuids::to_string(gen()) + ".tar.gz");
}

}

storage_transfer::storage_transfer(std::string http_base_url)
    : http_base_url_(std::move(http_base_url)) {}

void storage_transfer::pack(const fs::path& src_dir,
    const fs::path& dest_archive) {
    BOOST_LOG_SEV(lg(), debug) << "Packing directory: " << src_dir.string()
                               << " -> " << dest_archive.string();
    const auto t0 = std::chrono::steady_clock::now();
    filesystem::archiver::pack(src_dir, dest_archive);
    const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::steady_clock::now() - t0).count();
    BOOST_LOG_SEV(lg(), debug) << "Pack complete: " << dest_archive.string()
                               << " bytes=" << fs::file_size(dest_archive)
                               << " duration=" << ms << "ms";
}

void storage_transfer::unpack(const fs::path& archive,
    const fs::path& dest_dir) {
    BOOST_LOG_SEV(lg(), debug) << "Unpacking archive: " << archive.string()
                               << " -> " << dest_dir.string();
    const auto bytes = fs::file_size(archive);
    const auto t0 = std::chrono::steady_clock::now();
    fs::create_directories(dest_dir);
    filesystem::archiver::extract(archive, dest_dir);
    const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::steady_clock::now() - t0).count();
    BOOST_LOG_SEV(lg(), debug) << "Unpack complete: dest=" << dest_dir.string()
                               << " bytes=" << bytes
                               << " duration=" << ms << "ms";
}

void storage_transfer::upload(const std::string& bucket, const std::string& key,
    const fs::path& src_file) {
    const auto url = storage_paths::make_object_url(http_base_url_, bucket, key);
    const auto bytes = fs::file_size(src_file);
    BOOST_LOG_SEV(lg(), debug) << "Uploading: bucket=" << bucket
                               << " key=" << key
                               << " bytes=" << bytes;
    const auto t0 = std::chrono::steady_clock::now();
    http_client::put(url, src_file);
    const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::steady_clock::now() - t0).count();
    BOOST_LOG_SEV(lg(), debug) << "Upload complete: bucket=" << bucket
                               << " key=" << key
                               << " duration=" << ms << "ms";
}

void storage_transfer::download(const std::string& bucket, const std::string& key,
    const fs::path& dest_file) {
    const auto url = storage_paths::make_object_url(http_base_url_, bucket, key);
    BOOST_LOG_SEV(lg(), debug) << "Downloading: bucket=" << bucket
                               << " key=" << key;
    const auto t0 = std::chrono::steady_clock::now();
    http_client::get(url, dest_file);
    const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::steady_clock::now() - t0).count();
    BOOST_LOG_SEV(lg(), debug) << "Download complete: bucket=" << bucket
                               << " key=" << key
                               << " bytes=" << fs::file_size(dest_file)
                               << " duration=" << ms << "ms";
}

void storage_transfer::pack_and_upload(const fs::path& src_dir,
    const std::string& bucket, const std::string& key) {
    BOOST_LOG_SEV(lg(), debug) << "pack_and_upload: src=" << src_dir.string()
                               << " bucket=" << bucket << " key=" << key;
    const auto tmp = make_temp_archive();
    try {
        pack(src_dir, tmp);
        upload(bucket, key, tmp);
        fs::remove(tmp);
    } catch (...) {
        std::error_code ec;
        fs::remove(tmp, ec);
        throw;
    }
}

void storage_transfer::fetch_and_unpack(const std::string& bucket,
    const std::string& key, const fs::path& dest_dir) {
    BOOST_LOG_SEV(lg(), debug) << "fetch_and_unpack: bucket=" << bucket
                               << " key=" << key
                               << " dest=" << dest_dir.string();
    const auto tmp = make_temp_archive();
    try {
        download(bucket, key, tmp);
        unpack(tmp, dest_dir);
        fs::remove(tmp);
    } catch (...) {
        std::error_code ec;
        fs::remove(tmp, ec);
        throw;
    }
}

void storage_transfer::upload_blob(const std::string& bucket,
    const std::string& key, std::span<const char> data) {
    BOOST_LOG_SEV(lg(), debug) << "upload_blob: bucket=" << bucket
                               << " key=" << key
                               << " bytes=" << data.size();
    boost::uuids::random_generator gen;
    const auto tmp = fs::temp_directory_path() /
        (boost::uuids::to_string(gen()) + ".blob");
    try {
        {
            std::ofstream out(tmp, std::ios::binary);
            out.write(data.data(), static_cast<std::streamsize>(data.size()));
        }
        upload(bucket, key, tmp);
        fs::remove(tmp);
    } catch (...) {
        std::error_code ec;
        fs::remove(tmp, ec);
        throw;
    }
}

std::vector<char> storage_transfer::download_blob(const std::string& bucket,
    const std::string& key) {
    BOOST_LOG_SEV(lg(), debug) << "download_blob: bucket=" << bucket
                               << " key=" << key;
    boost::uuids::random_generator gen;
    const auto tmp = fs::temp_directory_path() /
        (boost::uuids::to_string(gen()) + ".blob");
    try {
        download(bucket, key, tmp);
        const auto size = fs::file_size(tmp);
        std::vector<char> buf(size);
        std::ifstream in(tmp, std::ios::binary);
        in.read(buf.data(), static_cast<std::streamsize>(size));
        fs::remove(tmp);
        return buf;
    } catch (...) {
        std::error_code ec;
        fs::remove(tmp, ec);
        throw;
    }
}

}
