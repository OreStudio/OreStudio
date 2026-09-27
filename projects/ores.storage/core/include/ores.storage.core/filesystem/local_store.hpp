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
#ifndef ORES_STORAGE_CORE_FILESYSTEM_LOCAL_STORE_HPP
#define ORES_STORAGE_CORE_FILESYSTEM_LOCAL_STORE_HPP

#include "ores.storage.core/export.hpp"
#include <cstdint>
#include <filesystem>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::storage::filesystem {

/**
 * @brief Thrown when an object is not there.
 *
 * A distinct type rather than a message, because "not there" is a normal
 * answer to a read and both interfaces have to tell it apart from a failure:
 * the HTTP routes answer 404 and the bus replies with =found = false=.
 */
class ORES_STORAGE_CORE_EXPORT object_not_found final : public std::runtime_error {
public:
    explicit object_not_found(const std::string& what);
};

/**
 * @brief A bucket-and-key store over a local directory tree.
 *
 * The store owns the whole filesystem contract both interfaces share: a bucket
 * is one directory under the root, a key is a relative path inside it, and a
 * put replaces a whole object. Nothing above this class touches the disk, so
 * the HTTP routes and the NATS handler cannot drift apart in how they resolve
 * a key, guard traversal or create a bucket.
 *
 * The store holds no bucket names. A bucket is created on first write, and
 * =is_valid_bucket= only rejects what could not be a single path segment, so
 * which buckets exist stays the caller's concern.
 *
 * Every operation is synchronous and blocking; callers run it on a background
 * thread or inside a coroutine that is allowed to block.
 */
class ORES_STORAGE_CORE_EXPORT local_store final {
public:
    /**
     * @brief One object in a listing, without its value.
     */
    struct entry {
        std::string key;
        std::uintmax_t size_bytes = 0;
    };

    explicit local_store(std::filesystem::path root);

    /**
     * @brief The directory this store keeps its buckets in.
     */
    [[nodiscard]] const std::filesystem::path& root() const noexcept {
        return root_;
    }

    /**
     * @brief Whether a bucket is a usable single path segment.
     *
     * Rejects the empty name, the two directory aliases, and anything holding
     * a separator, so a bucket can never name a location outside the root.
     */
    [[nodiscard]] static bool is_valid_bucket(std::string_view bucket);

    /**
     * @brief Whether a bucket may hold this key, and it stays inside the bucket.
     */
    [[nodiscard]] bool is_valid_key(const std::string& bucket, const std::string& key) const;

    /**
     * @brief The absolute path of a key, or nothing when it is not addressable.
     */
    [[nodiscard]] std::optional<std::filesystem::path> resolve(const std::string& bucket,
                                                               const std::string& key) const;

    /**
     * @brief Creates or replaces one object, creating the bucket if needed.
     *
     * @throws std::runtime_error when the object cannot be written.
     */
    void put(const std::string& bucket, const std::string& key, std::string_view data);

    /**
     * @brief Reads one object.
     *
     * @throws object_not_found when the bucket holds no object at the key.
     * @throws std::runtime_error when the object exists but cannot be read.
     */
    [[nodiscard]] std::string get(const std::string& bucket, const std::string& key) const;

    /**
     * @brief Whether the bucket holds an object at the key.
     */
    [[nodiscard]] bool exists(const std::string& bucket, const std::string& key) const;

    /**
     * @brief The object's length in bytes, or nothing when it is not there.
     */
    [[nodiscard]] std::optional<std::uintmax_t> size(const std::string& bucket,
                                                     const std::string& key) const;

    /**
     * @brief Removes one object.
     *
     * @return whether there was an object to remove.
     */
    bool remove(const std::string& bucket, const std::string& key);

    /**
     * @brief Every object in a bucket whose key starts with the prefix, sorted.
     *
     * A prefix is a filter over keys and not a directory: the store treats a
     * key as opaque, so a caller asks for what it wants by prefix and never by
     * path.
     */
    [[nodiscard]] std::vector<entry> list(const std::string& bucket,
                                          const std::string& prefix) const;

private:
    std::filesystem::path root_;
};

}

#endif
