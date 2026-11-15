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
#include "ores.utility/codec/shaped_wire_traits.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

namespace ores::http::routes::iam {

/// A tenant's own setup run, as the deployment overview states it.
struct overview_setup final {
    std::string instanceId;
    std::string status;
    int stepsDone = 0;
    int stepCount = 0;
    std::string error;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<overview_setup>) {
    using ores::utility::codec::field;
    using v = overview_setup;
    return ::ores::utility::codec::field_list{field{"instanceId", &v::instanceId},
                                              field{"status", &v::status},
                                              field{"stepsDone", &v::stepsDone},
                                              field{"stepCount", &v::stepCount},
                                              field{"error", &v::error}};
}

/// A tenant as the deployment overview states it, with its setup run when it has one.
struct overview_tenant final {
    std::string id;
    std::string code;
    std::string name;
    std::string type;
    std::string description;
    std::string hostname;
    std::string status;
    bool registrationDefault = false;
    std::optional<overview_setup> setup;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<overview_tenant>) {
    using ores::utility::codec::field;
    using v = overview_tenant;
    return ::ores::utility::codec::field_list{field{"id", &v::id},
                                              field{"code", &v::code},
                                              field{"name", &v::name},
                                              field{"type", &v::type},
                                              field{"description", &v::description},
                                              field{"hostname", &v::hostname},
                                              field{"status", &v::status},
                                              field{"registrationDefault", &v::registrationDefault},
                                              field{"setup", &v::setup}};
}

/// A tenant that needs the administrator, and why.
struct overview_attention_entry final {
    overview_tenant tenant;
    std::string reason;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<overview_attention_entry>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"tenant", &overview_attention_entry::tenant},
                                              field{"reason", &overview_attention_entry::reason}};
}

/// One recent setup run, by the tenant it set up.
struct overview_activity_entry final {
    std::string instanceId;
    std::string tenantName;
    std::string status;
    int stepsDone = 0;
    int stepCount = 0;
    std::string error;
    std::string at;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<overview_activity_entry>) {
    using ores::utility::codec::field;
    using v = overview_activity_entry;
    return ::ores::utility::codec::field_list{field{"instanceId", &v::instanceId},
                                              field{"tenantName", &v::tenantName},
                                              field{"status", &v::status},
                                              field{"stepsDone", &v::stepsDone},
                                              field{"stepCount", &v::stepCount},
                                              field{"error", &v::error},
                                              field{"at", &v::at}};
}

/**
 * @brief The deployment overview: the tenant roster counted by status, the
 * tenants that need the administrator, the first page of the roster, and the
 * newest setups.
 */
struct deployment_overview final {
    std::uint64_t inService = 0;
    std::uint64_t onEvaluation = 0;
    std::uint64_t settingUp = 0;
    std::vector<overview_attention_entry> attention;
    std::vector<overview_tenant> tenants;
    std::uint64_t totalCount = 0;
    std::vector<overview_activity_entry> activity;
    bool activityUnavailable = false;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<deployment_overview>) {
    using ores::utility::codec::field;
    using v = deployment_overview;
    return ::ores::utility::codec::field_list{
        field{"inService", &v::inService},
        field{"onEvaluation", &v::onEvaluation},
        field{"settingUp", &v::settingUp},
        field{"attention", &v::attention},
        field{"tenants", &v::tenants},
        field{"totalCount", &v::totalCount},
        field{"activity", &v::activity},
        field{"activityUnavailable", &v::activityUnavailable}};
}

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
     * @brief One tenant by its code.
     *
     * @throws record_routes::refusal, not found, when no tenant holds the code.
     */
    [[nodiscard]] static ores::iam::domain::tenant
    known_tenant(const ores::http::domain::http_request& req,
                 ores::nats::service::nats_client& session,
                 const std::string& code);

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

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::iam::overview_setup>
    : shaped_wire_traits<ores::http::routes::iam::overview_setup> {};

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::iam::overview_tenant>
    : shaped_wire_traits<ores::http::routes::iam::overview_tenant> {};

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::iam::overview_attention_entry>
    : shaped_wire_traits<ores::http::routes::iam::overview_attention_entry> {};

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::iam::overview_activity_entry>
    : shaped_wire_traits<ores::http::routes::iam::overview_activity_entry> {};

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::iam::deployment_overview>
    : shaped_wire_traits<ores::http::routes::iam::deployment_overview> {};

#endif
