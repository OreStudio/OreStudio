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
#ifndef ORES_COMPUTE_CORE_MESSAGING_ASSIGNMENT_GRANTS_HPP
#define ORES_COMPUTE_CORE_MESSAGING_ASSIGNMENT_GRANTS_HPP

#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.storage.api/net/storage_paths.hpp"
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace ores::compute::messaging {

/**
 * @brief The grant that admits one operation on the object a path names.
 *
 * The key is the whole prefix, so the capability admits exactly the object the
 * assignment names and nothing beside it. A path that does not name an object
 * gives nothing.
 */
inline std::optional<ores::security::jwt::storage_grant> grant_for_path(const std::string& path,
                                                                        std::string op) {
    std::string bucket;
    std::string key;
    if (!ores::storage::net::storage_paths::split_object_path(path, bucket, key))
        return std::nullopt;
    return ores::security::jwt::storage_grant{
        .bucket = std::move(bucket), .key_prefix = std::move(key), .op = std::move(op)};
}

/**
 * @brief The grants a node needs to run one job: read the package and the input,
 * and write the output.
 *
 * A node holds no standing storage credential, so each assignment carries a
 * capability for exactly these three objects. The whole set is refused when any
 * path does not name an object, because a job with a missing grant cannot run.
 */
inline std::optional<std::vector<ores::security::jwt::storage_grant>> storage_grants_for(
    const std::string& package_uri, const std::string& input_uri, const std::string& output_path) {
    std::vector<ores::security::jwt::storage_grant> grants;
    const struct {
        const std::string& path;
        const char* op;
    } sources[] = {{package_uri, "get"}, {input_uri, "get"}, {output_path, "put"}};
    for (const auto& source : sources) {
        auto grant = grant_for_path(source.path, source.op);
        if (!grant)
            return std::nullopt;
        grants.push_back(std::move(*grant));
    }
    return grants;
}

} // namespace ores::compute::messaging

#endif
