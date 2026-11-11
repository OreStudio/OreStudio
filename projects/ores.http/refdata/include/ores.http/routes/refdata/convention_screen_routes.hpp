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
#ifndef ORES_HTTP_ROUTES_REFDATA_CONVENTION_SCREEN_ROUTES_HPP
#define ORES_HTTP_ROUTES_REFDATA_CONVENTION_SCREEN_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::refdata {

/**
 * @brief The conventions screen's families: each family with its count, one
 * family's page, and the write of a family whose terms the screen draws.
 *
 * A plain family is listed through its own entity. A composite family is a
 * central convention row with parts, so its page is the convention entity
 * filtered by the family's kind, and the parts read for that page's keys are
 * laid over each row.
 */
class convention_screen_routes {
private:
    inline static std::string_view logger_name =
        "ores.http.routes.refdata.convention_screen_routes";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief One family as the screen lists it.
     *
     * kind_code is set only for a composite family, and names the kind every
     * central row of the family carries. shared marks a family the tenant holds
     * once and every party reads.
     */
    struct family final {
        std::string key;
        std::string entity;
        std::string kind_code;
        bool writable{};
        bool shared{};
    };

    /**
     * @brief Every family the screen lists, ordered by key.
     */
    [[nodiscard]] static std::vector<family> families();

    /**
     * @brief A central row with each group's columns under their own names.
     *
     * The row's own id, version and party stay, because they are the row's and
     * not links to a parent.
     */
    [[nodiscard]] static std::string flatten_central(std::string_view row);

    /**
     * @brief A flattened central row with the first populated row of each part
     * laid over it, less the columns the central row owns.
     */
    [[nodiscard]] static std::string zip_parts(std::string_view central, std::string_view item);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
