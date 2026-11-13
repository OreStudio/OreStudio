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
#ifndef ORES_HTTP_ROUTES_REPORTING_REPORT_ROUTES_HPP
#define ORES_HTTP_ROUTES_REPORTING_REPORT_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.reporting.api/domain/report_definition.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace ores::http::routes::reporting {

/**
 * @brief The reports a party runs, one row per definition.
 *
 * The service answers the definition, the latest run of it, the health word of
 * that run, its problem counts, its next fire and the party's own name as six
 * reads. The row composes them, so a cell that cannot be read is absent and
 * says why rather than costing the screen.
 */
class report_routes {
public:
    /// A party the caller's account works in, as the row names it.
    struct party_name {
        boost::uuids::uuid id;
        std::string name;
        std::string short_code;
    };

    /// One definition's latest run.
    struct run_row {
        boost::uuids::uuid id;
        boost::uuids::uuid definition_id;
        std::optional<std::chrono::system_clock::time_point> started_at;
        std::optional<std::chrono::system_clock::time_point> completed_at;
    };

    /// The health word of one run, with the state it was derived from.
    struct health_row {
        boost::uuids::uuid run_id;
        std::string state_name;
        std::string health;
    };

    /// How many of one run's problems are errors and how many warnings.
    struct count_row {
        boost::uuids::uuid run_id;
        std::int64_t errors{};
        std::int64_t warnings{};
    };

    /// When the scheduler says one definition's job fires next.
    struct fire_row {
        boost::uuids::uuid definition_id;
        std::optional<std::string> next_fire_at;
    };

    /**
     * @brief The rows as the list draws them, in the order the definitions were
     * read in.
     *
     * A decoration that could not be read is passed as nothing, so its cell is
     * absent and the answer's unread list names the read that refused.
     */
    static std::string
    rows_json(const std::vector<ores::reporting::domain::report_definition>& definitions,
              const std::optional<std::vector<party_name>>& parties,
              const std::optional<std::vector<run_row>>& runs,
              const std::vector<health_row>& health,
              const std::optional<std::vector<count_row>>& counts,
              const std::optional<std::vector<fire_row>>& fires);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
