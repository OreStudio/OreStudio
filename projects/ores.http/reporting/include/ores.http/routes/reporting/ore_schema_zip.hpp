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
#ifndef ORES_HTTP_ROUTES_REPORTING_ORE_SCHEMA_ZIP_HPP
#define ORES_HTTP_ROUTES_REPORTING_ORE_SCHEMA_ZIP_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace ores::http::routes::reporting {

/**
 * @brief One file of a schema's download.
 */
struct schema_entry final {
    std::string name;
    std::string content;
};

/**
 * @brief What a schema's download holds, and what the engine version does not
 * store.
 */
struct schema_bundle final {
    std::vector<schema_entry> files;
    std::vector<std::string> missing;
};

/**
 * @brief The schema with this name and every schema it includes, at once or
 * through another.
 *
 * A schema is complete only with its includes, so a download that left one out
 * could not validate a document. An include the version does not store is named
 * in @c missing rather than left out silently.
 *
 * @param root The schema's file name, as the catalogue states it.
 * @param contents The file contents of one engine version, by file name.
 */
[[nodiscard]] schema_bundle
schema_with_includes(const std::string& root, const std::map<std::string, std::string>& contents);

/**
 * @brief The bundle as a zip archive, one entry per file.
 *
 * @throws std::runtime_error when the archive cannot be written.
 */
[[nodiscard]] std::vector<char> zip_of(const std::vector<schema_entry>& files);

/**
 * @brief The ORE schema a screen downloads, with everything it includes.
 *
 * One route, and the only one in this area that answers a binary made of
 * several stored objects: the files are read at the engine version asked for and
 * answered as a zip. A missing include is a refusal that names what is missing
 * rather than a zip that cannot validate a document.
 */
class ore_schema_zip final {
public:
    /// Register GET /api/reporting/ore-files/{code}/schema.
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
