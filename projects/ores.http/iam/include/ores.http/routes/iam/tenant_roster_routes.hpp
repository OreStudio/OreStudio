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
#ifndef ORES_HTTP_ROUTES_IAM_TENANT_ROSTER_ROUTES_HPP
#define ORES_HTTP_ROUTES_IAM_TENANT_ROSTER_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.iam.api/domain/tenant.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::iam {

/**
 * @brief The deployment's tenants as system administration reads them: the
 * roster, one tenant's screen, its removal, and the home overview.
 *
 * Each route needs the deployment read. Each tenant is joined with the
 * provisioning run that set it up, and a failure to read the runs is not a
 * failure to read the tenants: the answer goes out without them and says so.
 */
class tenant_roster_routes {
private:
    inline static std::string_view logger_name = "ores.http.routes.iam.tenant_roster_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /// The tenant type that marks test infrastructure, hidden unless asked for.
    static constexpr std::string_view test_tenant_type = "automation";

    /// The tenant type of the deployment's own tenant, which the overview leaves out.
    static constexpr std::string_view system_tenant_type = "system";

    /**
     * @brief The provisioning run that set a tenant up, as a roster needs it.
     */
    struct tenant_setup final {
        std::string instance_id;
        std::string status;
        int steps_done = 0;
        int step_count = 0;
        std::string error;
    };

    /**
     * @brief What one roster page asks for, as the BFF took it from the address.
     */
    struct roster_query final {
        std::string search;
        std::string type;
        std::string status;
        bool include_test = false;
        std::string sort;
        bool descending = false;
        std::uint32_t offset = 0;
        std::uint32_t limit = 100;
    };

    /**
     * @brief One newest run named on the overview's activity list.
     */
    struct activity_entry final {
        std::string instance_id;
        std::string tenant_name;
        std::string status;
        int steps_done = 0;
        int step_count = 0;
        std::string error;
        std::string at;
    };

    /**
     * @brief One tenant on the overview's attention list, and why.
     */
    struct attention_entry final {
        ores::iam::domain::tenant tenant;
        std::optional<tenant_setup> setup;
        std::string reason;
    };

    /**
     * @brief The system administrator's home, as the overview answers it.
     */
    struct overview final {
        std::uint64_t in_service = 0;
        std::uint64_t on_evaluation = 0;
        std::uint64_t setting_up = 0;
        std::vector<attention_entry> attention;
        std::vector<ores::iam::domain::tenant> tenants;
        std::map<std::string, tenant_setup> setups;
        std::uint64_t total_count = 0;
        std::vector<activity_entry> activity;
        bool activity_unavailable = false;
    };

    /**
     * @brief The roster page the address asks for, or nothing when a bound is
     * not a whole number in range or a text is too long.
     */
    [[nodiscard]] static std::optional<roster_query>
    read_roster_query(const ores::http::domain::http_request& req);

    /**
     * @brief One tenant as a roster reads it, with its run or null.
     */
    [[nodiscard]] static std::string summary_json(const ores::iam::domain::tenant& t,
                                                  const std::optional<tenant_setup>& setup);

    /**
     * @brief One roster page, each tenant with the run its id names.
     */
    [[nodiscard]] static std::string
    page_json(const std::vector<ores::iam::domain::tenant>& tenants,
              const std::map<std::string, tenant_setup>& setups,
              std::uint64_t total,
              bool setup_unavailable,
              std::uint64_t hidden_test_count);

    /**
     * @brief One tenant's screen: the tenant with its row's provenance and run.
     */
    [[nodiscard]] static std::string detail_json(const ores::iam::domain::tenant& t,
                                                 const std::optional<tenant_setup>& setup,
                                                 bool setup_unavailable);

    /**
     * @brief The overview as the home reads it.
     */
    [[nodiscard]] static std::string overview_json(const overview& o);

    /**
     * @brief Register the roster, one tenant's read and removal, and the overview.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
