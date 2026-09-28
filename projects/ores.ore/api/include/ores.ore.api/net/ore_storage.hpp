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
#ifndef ORES_ORE_API_NET_ORE_STORAGE_HPP
#define ORES_ORE_API_NET_ORE_STORAGE_HPP

#include "ores.storage.api/net/object_keys.hpp"
#include "ores.storage.api/net/storage_paths.hpp"
#include <string>
#include <string_view>

namespace ores::ore::net {

/**
 * @brief Storage keys for an ORE import.
 *
 * Mirrors compute_storage in ores.compute.api -- ORE import tarballs live in
 * the platform bucket under the =ore= service, one key per import request:
 * =ore/imports/{request_id}.tar.gz=.
 */
struct ore_storage {
    /**
     * @brief The platform bucket every service writes to.
     */
    static constexpr std::string_view bucket = ores::storage::api::object_keys::ores_bucket;

    /**
     * @brief Object key for an ORE import tarball.
     *
     * @param request_id  UUID of the import request.
     * @return            e.g. "ore/imports/f47ac10b-58cc-4372-a567-0e02b2c3d479.tar.gz"
     */
    static std::string import_key(std::string_view request_id) {
        return ores::storage::api::object_keys::make(
            service, "imports", std::string(request_id) + ".tar.gz");
    }

    /**
     * @brief API path for an ORE import tarball.
     *
     * @param request_id  UUID of the import request.
     * @return            e.g. "/api/v1/storage/ores/ore/imports/{id}.tar.gz"
     */
    static std::string import_path(std::string_view request_id) {
        return ores::storage::net::storage_paths::make_object_path(bucket, import_key(request_id));
    }

private:
    /**
     * @brief The service segment every ORE key starts with.
     */
    static constexpr std::string_view service = "ore";
};

}

#endif
