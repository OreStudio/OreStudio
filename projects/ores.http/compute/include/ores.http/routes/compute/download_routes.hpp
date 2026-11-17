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
#ifndef ORES_HTTP_ROUTES_COMPUTE_DOWNLOAD_ROUTES_HPP
#define ORES_HTTP_ROUTES_COMPUTE_DOWNLOAD_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <optional>
#include <string>

namespace ores::http::routes::compute {

/**
 * @brief The stored bytes of the grid: a work unit's input and a run's output.
 */
class download_routes final {
public:
    /**
     * @brief Register the two download routes.
     *
     * @param storage_dir The root the object store is read under, so the bytes
     * a download answers with are the bytes the grid wrote.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session,
                                std::string storage_dir);
};

/**
 * @brief The object key a row's stored address names, or nothing when it names none.
 *
 * A row states the address of its object as the storage service serves it, so
 * an address outside that path names no object and a download refuses it rather
 * than reading something the row did not mean.
 */
std::optional<std::string> stored_key(const std::string& address);

/// The content type an object is served as, read from the name it is stored under.
std::string content_type_of(const std::string& key);

/// The name a browser saves an object as: the last segment of its key.
std::string download_name(const std::string& key);

/**
 * @brief The Content-Disposition a download answers with.
 *
 * A name reaches the header as printable ASCII, so no character of it can end
 * the field or start another one.
 */
std::string disposition_for(const std::string& name);

}

#endif
