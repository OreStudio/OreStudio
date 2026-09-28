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
#ifndef ORES_STORAGE_API_NET_OBJECT_KEYS_HPP
#define ORES_STORAGE_API_NET_OBJECT_KEYS_HPP

#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::storage::api {

/**
 * @brief The platform's bucket and key protocol.
 *
 * Every service writes to one bucket and names its objects with the same
 * grammar, so an operator can find an object without reading the code that
 * wrote it and two services cannot collide by accident:
 *
 *   bucket  ores
 *   key     <service>/<purpose>/<id>[/<name>]
 *
 * - _service_ is the owning domain component, lower snake case: =compute=,
 *   =ore=, =reporting=. One service per first segment; a service never
 *   writes under another's.
 * - _purpose_ says what the object is for within that service, in the plural
 *   where it is a collection: =packages=, =imports=, =runs=.
 * - _id_ is the domain identifier the object belongs to, as the domain stores
 *   it. It is what makes the key unique. An id may carry its own extension,
 *   where the object is the id's file and has no name of its own:
 *   =compute/input/<uuid>.tar.gz=.
 * - _name_, when present, is the file's own name, so two objects under one id
 *   -- =trades.msgpack= and =market_data.msgpack= in one run -- are
 *   distinguishable.
 *
 * A segment holds letters, digits, dots, hyphens and underscores, and never a
 * slash. Dot and dot dot are refused: they are a directory walk, not a
 * segment, and a slash is the only separator, so no key can leave the bucket
 * that owns it. Lower snake case is the convention for the service and purpose
 * segments, not a rule the protocol enforces.
 *
 * A key is still opaque to storage: the store never parses a segment, never
 * treats a slash as a directory it must create, and never learns what a
 * service or a purpose means. The protocol is a convention the domains keep,
 * and this header is where it is written down and checked, rather than a rule
 * the store enforces.
 *
 * The full target, and the reasoning behind this grammar, is in
 * [[id:286B7167-9B8A-4937-BBA5-E9D285AFECE3][Object Storage]].
 */
struct object_keys final {
    /**
     * @brief The single bucket every service writes to.
     *
     * One bucket for the platform, because a bucket is an allocation and the
     * allocation is the deployment's, not a service's. Which services exist,
     * and what each stores, is the key's business.
     */
    static constexpr std::string_view ores_bucket = "ores";

    /**
     * @brief A key, taken apart.
     *
     * @c name is empty when the key names no file: a purpose that holds one
     * object per id needs none.
     */
    struct parts {
        std::string service;
        std::string purpose;
        std::string id;
        std::string name;
    };

    /**
     * @brief Whether a segment may appear in a key.
     *
     * Letters, digits, dots, hyphens and underscores; nothing empty, and
     * neither dot nor dot dot. A dot is allowed because an id may carry its
     * own extension and a version reads as one, while a slash never is,
     * so a segment can never move a key out of the bucket it belongs to.
     */
    [[nodiscard]] static bool is_valid_segment(std::string_view segment) {
        if (segment.empty() || segment == "." || segment == "..")
            return false;

        for (const char c : segment) {
            const bool allowed = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
                                 (c >= '0' && c <= '9') || c == '_' || c == '-' || c == '.';
            if (!allowed)
                return false;
        }

        return true;
    }

    /**
     * @brief Builds a key from its parts.
     *
     * @throws std::invalid_argument when a part is empty or is not a valid
     *         segment, so a malformed key fails where it is built rather than
     *         where it is read back.
     */
    [[nodiscard]] static std::string make(std::string_view service,
                                          std::string_view purpose,
                                          std::string_view id,
                                          std::string_view name = {}) {
        if (!is_valid_segment(service))
            throw std::invalid_argument("object key has an invalid service segment");

        if (!is_valid_segment(purpose))
            throw std::invalid_argument("object key has an invalid purpose segment");

        if (!is_valid_segment(id))
            throw std::invalid_argument("object key has an invalid id segment");

        if (!name.empty() && !is_valid_segment(name))
            throw std::invalid_argument("object key has an invalid name segment");

        std::string key;
        key.reserve(service.size() + purpose.size() + id.size() + name.size() + 4);
        key.append(service).append(1, '/');
        key.append(purpose).append(1, '/');
        key.append(id);

        if (name.empty())
            return key;

        key.append(1, '/');
        key.append(name);
        return key;
    }

    /**
     * @brief Takes a key apart, or nothing when it does not follow the
     *        protocol.
     *
     * The inverse of @c make, so a reader can learn which service and which
     * domain object an object belongs to without an out-of-band index.
     */
    [[nodiscard]] static std::optional<parts> parse(std::string_view key) {
        std::vector<std::string_view> segments;
        std::size_t start = 0;
        while (true) {
            const auto end = key.find('/', start);
            const auto segment = (end == std::string_view::npos) ? key.substr(start) :
                                                                   key.substr(start, end - start);
            if (!is_valid_segment(segment))
                return std::nullopt;
            segments.push_back(segment);
            if (end == std::string_view::npos)
                break;
            start = end + 1;
        }

        if (segments.size() < 3 || segments.size() > 4)
            return std::nullopt;

        parts result;
        result.service = std::string(segments[0]);
        result.purpose = std::string(segments[1]);
        result.id = std::string(segments[2]);

        if (segments.size() == 4)
            result.name = std::string(segments[3]);

        return result;
    }
};

}

#endif
