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
#ifndef ORES_HTTP_ROUTES_SYNTHETIC_CALIBRATION_ROUTES_HPP
#define ORES_HTTP_ROUTES_SYNTHETIC_CALIBRATION_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.utility/codec/shaped_wire_traits.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <memory>
#include <optional>
#include <string>
#include <type_traits>
#include <vector>

namespace ores::http::routes::synthetic {

/**
 * @brief The policy an asset class calibrates under, as the calibration view
 * states it: the service's summary and the version of the class's policy row,
 * or null when the class calibrates on the defaults.
 */
struct calibration_view_policy final {
    bool from_policy_row = false;
    int window_days = 0;
    int variance_window_days = 0;
    std::string variance_approval_mode;
    int variance_min_returns = 0;
    bool variance_require_starts_agree = false;
    double variance_min_persistence = 0.0;
    double variance_max_persistence = 0.0;
    double variance_min_reaction = 0.0;
    bool allow_inverse = false;
    std::string fallback_code;
    std::vector<std::string> provider_order;
    std::optional<int> version;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<calibration_view_policy>) {
    using ores::utility::codec::field;
    using p = calibration_view_policy;
    return ::ores::utility::codec::field_list{
        field{"from_policy_row", &p::from_policy_row},
        field{"window_days", &p::window_days},
        field{"variance_window_days", &p::variance_window_days},
        field{"variance_approval_mode", &p::variance_approval_mode},
        field{"variance_min_returns", &p::variance_min_returns},
        field{"variance_require_starts_agree", &p::variance_require_starts_agree},
        field{"variance_min_persistence", &p::variance_min_persistence},
        field{"variance_max_persistence", &p::variance_max_persistence},
        field{"variance_min_reaction", &p::variance_min_reaction},
        field{"allow_inverse", &p::allow_inverse},
        field{"fallback_code", &p::fallback_code},
        field{"provider_order", &p::provider_order},
        field{"version", &p::version}};
}

/**
 * @brief How many feeds of a class start from a fixing, keep their fixed level,
 * are refused, and override the class policy.
 */
struct calibration_coverage final {
    int total = 0;
    int fromFixing = 0;
    int keptFixed = 0;
    int refused = 0;
    int overridden = 0;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<calibration_coverage>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{
        field{"total", &calibration_coverage::total},
        field{"fromFixing", &calibration_coverage::fromFixing},
        field{"keptFixed", &calibration_coverage::keptFixed},
        field{"refused", &calibration_coverage::refused},
        field{"overridden", &calibration_coverage::overridden}};
}

/**
 * @brief The calibration of an asset class: the policy it calibrates under,
 * every feed of it resolved as a start would, with its candidates and their
 * reasons, and the coverage counts. Nothing starts.
 */
struct asset_class_calibration final {
    std::string assetClass;
    calibration_view_policy policy;
    bool policyWritable = false;
    std::vector<ores::synthetic::messaging::calibrated_feed> feeds;
    calibration_coverage coverage;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<asset_class_calibration>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{
        field{"assetClass", &asset_class_calibration::assetClass},
        field{"policy", &asset_class_calibration::policy},
        field{"policyWritable", &asset_class_calibration::policyWritable},
        field{"feeds", &asset_class_calibration::feeds},
        field{"coverage", &asset_class_calibration::coverage}};
}

/**
 * @brief The calibration screen's routes: the calibration view of a class, its
 * run, the review of a proposed variance calibration, the class's policy stated
 * or removed, and one feed's own candidate pinned or cleared.
 *
 * A policy and a review belong to the installation, so only the system tenant
 * makes them. A feed belongs to its tenant, so any tenant pins its own feeds.
 */
class calibration_routes {
public:
    /**
     * @brief The calibration view of a class from the service's resolution, the
     * version of the class's policy row, and whether the caller may write it.
     */
    static asset_class_calibration
    calibration_of(std::string asset_class,
                   ores::synthetic::messaging::calibrate_response resolved,
                   std::optional<int> policy_version,
                   bool policy_writable);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::synthetic::calibration_view_policy>
    : shaped_wire_traits<ores::http::routes::synthetic::calibration_view_policy> {};

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::synthetic::calibration_coverage>
    : shaped_wire_traits<ores::http::routes::synthetic::calibration_coverage> {};

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::synthetic::asset_class_calibration>
    : shaped_wire_traits<ores::http::routes::synthetic::asset_class_calibration> {};

#endif
