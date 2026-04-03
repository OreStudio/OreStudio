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
#ifndef ORES_STORAGE_NET_STORAGE_PATHS_HPP
#define ORES_STORAGE_NET_STORAGE_PATHS_HPP

#include <string>
#include <string_view>

namespace ores::storage::net {

/**
 * @brief URL path helpers for the generic object storage HTTP API.
 *
 * The storage API follows a bucket/key model identical in shape to S3:
 *
 *   PUT    /api/v1/storage/{bucket}/{key}   upload (create or replace)
 *   GET    /api/v1/storage/{bucket}/{key}   download
 *   DELETE /api/v1/storage/{bucket}/{key}   delete
 *   HEAD   /api/v1/storage/{bucket}/{key}   existence check / content-length
 *
 * Use @c make_object_path to construct the path component, then prepend
 * the HTTP base URL discovered via NATS (http-server.v1.info.get).
 */
struct storage_paths {
    /**
     * @brief API path prefix for all storage endpoints.
     */
    static constexpr std::string_view prefix = "/api/v1/storage";

    /**
     * @brief Constructs the URL path for a storage object.
     *
     * @param bucket  Bucket name (use @c buckets constants).
     * @param key     Object key; may contain slashes for hierarchical keys.
     * @return        Path string, e.g. "/api/v1/storage/compute-packages/abc123".
     */
    static std::string make_object_path(std::string_view bucket,
        std::string_view key) {
        std::string path;
        path.reserve(prefix.size() + 1 + bucket.size() + 1 + key.size());
        path += prefix;
        path += '/';
        path += bucket;
        path += '/';
        path += key;
        return path;
    }

    /**
     * @brief Constructs a full URL for a storage object.
     *
     * @param base_url  HTTP server base URL (e.g. "http://localhost:51000").
     * @param bucket    Bucket name (use @c buckets constants).
     * @param key       Object key.
     * @return          Full URL string.
     */
    static std::string make_object_url(std::string_view base_url,
        std::string_view bucket, std::string_view key) {
        std::string url(base_url);
        url += make_object_path(bucket, key);
        return url;
    }
};

}

#endif
