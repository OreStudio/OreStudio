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
#ifndef ORES_HTTP_ROUTES_REFDATA_COUNTERPARTY_SCREEN_ROUTES_HPP
#define ORES_HTTP_ROUTES_REFDATA_COUNTERPARTY_SCREEN_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/domain/counterparty_business_centre.hpp"
#include "ores.refdata.api/domain/counterparty_contact_information.hpp"
#include "ores.refdata.api/domain/counterparty_identifier.hpp"
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::refdata {

/**
 * @brief The counterparty onboarding screen's own reads and writes: one
 * counterparty with its identifiers, the children of a page, the composite
 * write, which parties see a counterparty, and its business centres.
 *
 * Each answers as the BFF did. A write answers the service's result whatever
 * its outcome, so the screen shows the field the service refused.
 */
class counterparty_screen_routes {
private:
    inline static std::string_view logger_name =
        "ores.http.routes.refdata.counterparty_screen_routes";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief The writes that take a counterparty's business centres from those
     * it holds to those wanted, in the order they are made.
     */
    struct centre_changes final {
        std::vector<std::string> links;
        std::vector<std::string> unlinks;
    };

    /**
     * @brief The links to add and the links to close. Each code appears once,
     * in the order first named, and links come first, so a failure part way
     * never leaves the counterparty with none.
     */
    [[nodiscard]] static centre_changes changes_of(const std::vector<std::string>& held,
                                                   const std::vector<std::string>& wanted);

    /**
     * @brief The identifiers a children read names, comma separated, or nothing
     * when one is malformed, when none is named, or when more than 200 are.
     */
    [[nodiscard]] static std::optional<std::vector<std::string>> read_ids(const std::string& text);

    /**
     * @brief The children of each counterparty named, in the order named.
     */
    [[nodiscard]] static std::string children_json(
        const std::vector<std::string>& ids,
        const std::vector<ores::refdata::domain::counterparty_identifier>& identifiers,
        const std::vector<ores::refdata::domain::counterparty_contact_information>& contacts,
        const std::vector<ores::refdata::domain::counterparty_business_centre>& centres);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
