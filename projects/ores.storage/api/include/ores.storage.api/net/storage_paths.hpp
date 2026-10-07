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

#include <cstdint>
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
 * the HTTP base URL discovered via NATS (http.v1.info.get).
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
     * @return        Path string, e.g. "/api/v1/storage/ores/compute/packages/abc123".
     */
    static std::string make_object_path(std::string_view bucket, std::string_view key) {
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
     * @brief Splits an object path back into the bucket and key it names.
     *
     * The inverse of @ref make_object_path, for a caller that holds a path --
     * an assignment names its package, input and output as paths, and the
     * capability it carries must name the same objects as a bucket and a key.
     *
     * @return true when the path begins with the prefix and names a bucket and
     *         a non-empty key.
     */
    static bool
    split_object_path(std::string_view path, std::string& bucket, std::string& key) {
        if (!path.starts_with(prefix))
            return false;
        auto rest = path.substr(prefix.size());
        if (rest.empty() || rest.front() != '/')
            return false;
        rest.remove_prefix(1);
        const auto slash = rest.find('/');
        if (slash == std::string_view::npos || slash == 0 || slash + 1 >= rest.size())
            return false;
        bucket = std::string(rest.substr(0, slash));
        key = std::string(rest.substr(slash + 1));
        return true;
    }

    /**
     * @brief Constructs a full URL for a storage object.
     *
     * @param base_url  HTTP server base URL (e.g. "http://localhost:51000").
     * @param bucket    Bucket name (use @c buckets constants).
     * @param key       Object key.
     * @return          Full URL string.
     */
    static std::string
    make_object_url(std::string_view base_url, std::string_view bucket, std::string_view key) {
        std::string url(base_url);
        url += make_object_path(bucket, key);
        return url;
    }

    /**
     * @brief Constructs the URL path for a bucket itself, which lists its keys.
     */
    static std::string make_bucket_path(std::string_view bucket) {
        std::string path(prefix);
        path += '/';
        path += bucket;
        return path;
    }

    /**
     * @brief Constructs a full URL for one page of a bucket's keys.
     *
     * Every parameter is stated, including an empty prefix, so one shape covers
     * both a filtered and an unfiltered listing.
     */
    static std::string make_list_url(std::string_view base_url,
                                     std::string_view bucket,
                                     std::string_view key_prefix,
                                     std::uint32_t offset,
                                     std::uint32_t limit) {
        std::string url(base_url);
        url += make_bucket_path(bucket);
        url += "?prefix=";
        url += percent_encode_query(key_prefix);
        url += "&offset=";
        url += std::to_string(offset);
        url += "&limit=";
        url += std::to_string(limit);
        return url;
    }

private:
    /**
     * @brief Escapes a query-string value.
     *
     * Reserved bytes and anything outside the unreserved set become a percent
     * escape, so a prefix a caller typed reaches the server as the string it
     * was rather than as extra query parameters. A slash is left alone: a key
     * prefix is full of them.
     */
    static std::string percent_encode_query(std::string_view value) {
        constexpr char hex[] = "0123456789ABCDEF";
        std::string out;
        out.reserve(value.size());
        for (const char c : value) {
            const auto uc = static_cast<unsigned char>(c);
            const bool unreserved = (uc >= 'A' && uc <= 'Z') || (uc >= 'a' && uc <= 'z') ||
                                    (uc >= '0' && uc <= '9') || uc == '-' || uc == '_' ||
                                    uc == '.' || uc == '~' || uc == '/';
            if (unreserved) {
                out += c;
                continue;
            }
            out += '%';
            out += hex[uc >> 4];
            out += hex[uc & 0x0F];
        }
        return out;
    }
};

}

#endif
