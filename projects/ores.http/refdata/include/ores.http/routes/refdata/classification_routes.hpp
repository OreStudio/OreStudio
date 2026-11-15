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
#ifndef ORES_HTTP_ROUTES_REFDATA_CLASSIFICATION_ROUTES_HPP
#define ORES_HTTP_ROUTES_REFDATA_CLASSIFICATION_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>

namespace ores::http::routes::refdata {

/**
 * @brief The classification screen's lists, its catalogue and the label catalogue.
 *
 * Each code list is a generated unit. What every list shares and what spans
 * them lives here: the labels of a code domain, a row's label write, the
 * catalogue of lists with their counts, and the label catalogue.
 */
class classification_routes {
public:
    /**
     * @brief Register every list's routes, the catalogue and the label catalogue.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief The badge each code of a code domain is painted with, by the row's code.
     */
    static std::map<std::string, std::string> labels_of(const ores::http::domain::http_request& req,
                                                        ores::nats::service::nats_client& session,
                                                        std::string_view code_domain);

    /**
     * @brief Maps a row's code to a badge, or removes the mapping when there is none.
     *
     * The label is a mapping in the catalogue, not a column of the row, so this
     * writes no new version of the row.
     */
    static void set_label(const ores::http::domain::http_request& req,
                          ores::nats::service::nats_client& session,
                          std::string_view code_domain,
                          const std::string& code,
                          const std::optional<std::string>& badge_code,
                          const ores::utility::domain::change_intent& intent);
};

}

#endif
