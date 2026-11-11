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
#ifndef ORES_HTTP_ROUTES_REFDATA_PARTY_PAGE_ROUTES_HPP
#define ORES_HTTP_ROUTES_REFDATA_PARTY_PAGE_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/domain/party.hpp"
#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::refdata {

/**
 * @brief One page of the parties of the session's own tenant, as the parties
 * screen lists them.
 *
 * Each row names its parent, read in one more call when the parent is on
 * another page, and the flag of its business centre's country. A page whose
 * flags cannot be read goes out without them, as the BFF sent it.
 */
class party_page_routes {
private:
    inline static std::string_view logger_name = "ores.http.routes.refdata.party_page_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    /**
     * @brief The flag of each business centre named, or none when they cannot
     * be read. A flag is decoration, so a page whose flags fail is still the
     * page, and the failure is logged.
     */
    static std::map<std::string, std::string>
    flags_or_none(const ores::http::domain::http_request& req,
                  ores::nats::service::nats_client& session,
                  const std::set<std::string>& codes);

public:
    /**
     * @brief What one page asks for, as the BFF took it from the address.
     */
    struct page_query final {
        std::string search;
        std::string sort;
        bool descending = false;
        std::uint32_t offset = 0;
        std::uint32_t limit = 20;
    };

    /**
     * @brief The page the address asks for, or nothing when a bound is not a
     * whole number in range or a text is too long.
     */
    [[nodiscard]] static std::optional<page_query>
    read_page_query(const ores::http::domain::http_request& req);

    /**
     * @brief One page read with the request's token, as the screen lists it.
     *
     * The token decides the tenant, so a caller that reads inside another
     * tenant passes a request that carries that tenant's token.
     */
    [[nodiscard]] static std::string read_page(const ores::http::domain::http_request& req,
                                               ores::nats::service::nats_client& session,
                                               const page_query& query);

    /**
     * @brief The page as the screen lists it.
     *
     * @param names Each known party's name by its id, the page's own and the
     * parents read from other pages.
     * @param flags The flag image of each business centre, by the centre's code.
     */
    [[nodiscard]] static std::string
    page_json(const std::vector<ores::refdata::domain::party>& parties,
              std::uint64_t total,
              const std::map<std::string, std::string>& names,
              const std::map<std::string, std::string>& flags);

    /**
     * @brief Register the parties page of the session's own tenant.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
