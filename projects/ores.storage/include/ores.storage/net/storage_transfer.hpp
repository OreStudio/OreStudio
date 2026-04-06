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
#ifndef ORES_STORAGE_NET_STORAGE_TRANSFER_HPP
#define ORES_STORAGE_NET_STORAGE_TRANSFER_HPP

#include <cstddef>
#include <span>
#include <string>
#include <vector>
#include <filesystem>

namespace ores::storage::net {

/**
 * @brief Symmetric storage transfer helper.
 *
 * Provides atomic operations (pack, unpack, upload, download) that callers
 * can compose freely — e.g. to upload a single CSV or JSON file without
 * touching the archive layer — and composite helpers that combine pack+upload
 * or download+unpack for the common directory-tarball case.
 *
 * Composite helpers create a UUID-named temp file in
 * std::filesystem::temp_directory_path() and delete it after use.
 *
 * All operations log at DEBUG level including bucket, key, bytes transferred,
 * and wall-clock duration.
 */
class storage_transfer {
public:
    /**
     * @brief Constructs a transfer client.
     *
     * @param http_base_url  Base URL of the storage HTTP API,
     *                       e.g. "http://localhost:8080"
     */
    explicit storage_transfer(std::string http_base_url);

    // ── Atomic operations ─────────────────────────────────────────────────

    /**
     * @brief Pack a local directory into a .tar.gz archive.
     *
     * @param src_dir       Directory to archive (recursively).
     * @param dest_archive  Output archive path.
     * @throws std::runtime_error on failure.
     */
    void pack(const std::filesystem::path& src_dir,
        const std::filesystem::path& dest_archive);

    /**
     * @brief Extract a .tar.gz archive into a destination directory.
     *
     * @param archive   Local archive path.
     * @param dest_dir  Directory into which entries are extracted.
     * @throws std::runtime_error on failure.
     */
    void unpack(const std::filesystem::path& archive,
        const std::filesystem::path& dest_dir);

    /**
     * @brief Upload a local file to storage via HTTP PUT.
     *
     * @param bucket    Target storage bucket name.
     * @param key       Object key within the bucket.
     * @param src_file  Local file to upload.
     * @throws std::runtime_error on HTTP or I/O failure.
     */
    void upload(const std::string& bucket, const std::string& key,
        const std::filesystem::path& src_file);

    /**
     * @brief Download an object from storage via HTTP GET.
     *
     * @param bucket     Source storage bucket name.
     * @param key        Object key within the bucket.
     * @param dest_file  Local path to write the downloaded content.
     * @throws std::runtime_error on HTTP or I/O failure.
     */
    void download(const std::string& bucket, const std::string& key,
        const std::filesystem::path& dest_file);

    // ── Composite helpers ─────────────────────────────────────────────────

    /**
     * @brief Pack @p src_dir into a temp archive, then upload to storage.
     *
     * Equivalent to: pack(src_dir, tmp) + upload(bucket, key, tmp).
     * The temp file is removed after a successful upload.
     */
    void pack_and_upload(const std::filesystem::path& src_dir,
        const std::string& bucket, const std::string& key);

    /**
     * @brief Download from storage into a temp archive, then extract.
     *
     * Equivalent to: download(bucket, key, tmp) + unpack(tmp, dest_dir).
     * The temp file is removed after successful extraction.
     */
    void fetch_and_unpack(const std::string& bucket, const std::string& key,
        const std::filesystem::path& dest_dir);

    /**
     * @brief Upload an in-memory blob to storage.
     *
     * Writes @p data to a UUID-named temp file, uploads it, then removes
     * the temp file.  Useful for serialised binary payloads (e.g. MsgPack)
     * that are too large for NATS messages.
     */
    void upload_blob(const std::string& bucket, const std::string& key,
        std::span<const char> data);

    /**
     * @brief Download a blob from storage into memory.
     *
     * Downloads to a UUID-named temp file, reads it into a vector, then
     * removes the temp file.
     */
    std::vector<char> download_blob(const std::string& bucket,
        const std::string& key);

private:
    std::string http_base_url_;
};

}

#endif
