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
#include "ores.http/routes/synthetic/synthetic_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http/routes/refdata/instrument_code_web_routes.hpp"
#include "ores.http/routes/refdata/payment_frequency_web_routes.hpp"
#include "ores.http/routes/refdata/tenor_web_routes.hpp"
#include "ores.http/routes/synthetic/calibration_routes.hpp"
#include "ores.http/routes/synthetic/feed_key_routes.hpp"
#include "ores.http/routes/synthetic/feed_operation_routes.hpp"
#include "ores.http/routes/synthetic/feed_tick_routes.hpp"
#include "ores.http/routes/synthetic/folder_web_routes.hpp"
#include "ores.http/routes/synthetic/fx_spot_generation_config_web_routes.hpp"
#include "ores.http/routes/synthetic/fx_vol_contributor_web_routes.hpp"
#include "ores.http/routes/synthetic/fx_vol_surface_feed_contributor_web_routes.hpp"
#include "ores.http/routes/synthetic/fx_vol_surface_feed_expiry_web_routes.hpp"
#include "ores.http/routes/synthetic/fx_vol_surface_feed_web_routes.hpp"
#include "ores.http/routes/synthetic/gmm_component_web_routes.hpp"
#include "ores.http/routes/synthetic/ir_curve_generation_config_process_parameter_value_web_routes.hpp"
#include "ores.http/routes/synthetic/ir_curve_generation_config_web_routes.hpp"
#include "ores.http/routes/synthetic/ir_curve_template_entry_web_routes.hpp"
#include "ores.http/routes/synthetic/market_data_generation_config_web_routes.hpp"
#include "ores.http/routes/synthetic/market_data_provider_web_routes.hpp"
#include "ores.http/routes/synthetic/market_regime_routes.hpp"
#include "ores.http/routes/synthetic/process_preview_routes.hpp"
#include "ores.http/routes/synthetic/variance_calibration_input_point_web_routes.hpp"
#include "ores.http/routes/synthetic/variance_calibration_input_web_routes.hpp"
#include "ores.http/routes/synthetic/variance_calibration_web_routes.hpp"
#include "ores.http/routes/synthetic/yield_curve_process_parameter_definition_web_routes.hpp"
#include "ores.http/routes/synthetic/yield_curve_process_type_routes.hpp"
#include "ores.http/routes/synthetic/yield_curve_process_type_web_routes.hpp"
#include <boost/asio/awaitable.hpp>
#include <string>

namespace ores::http::routes::synthetic {

namespace {

/**
 * @brief The catalogue of the resources the synthetic screens name, for this session.
 *
 * The order is the BFF's, which is the order the screens list the resources in.
 */
std::string catalogue_body(const ores::http::domain::http_request& req) {
    const std::string entries[] = {
        folder_web_routes::catalogue_entry(req),
        market_data_generation_config_web_routes::catalogue_entry(req),
        market_data_provider_web_routes::catalogue_entry(req),
        fx_spot_generation_config_web_routes::catalogue_entry(req),
        gmm_component_web_routes::catalogue_entry(req),
        ir_curve_generation_config_web_routes::catalogue_entry(req),
        ir_curve_generation_config_process_parameter_value_web_routes::catalogue_entry(req),
        ir_curve_template_entry_web_routes::catalogue_entry(req),
        variance_calibration_web_routes::catalogue_entry(req),
        variance_calibration_input_web_routes::catalogue_entry(req),
        variance_calibration_input_point_web_routes::catalogue_entry(req),
        fx_vol_contributor_web_routes::catalogue_entry(req),
        fx_vol_surface_feed_web_routes::catalogue_entry(req),
        fx_vol_surface_feed_expiry_web_routes::catalogue_entry(req),
        fx_vol_surface_feed_contributor_web_routes::catalogue_entry(req),
        yield_curve_process_type_web_routes::catalogue_entry(req),
        yield_curve_process_parameter_definition_web_routes::catalogue_entry(req),
        ores::http::routes::refdata::tenor_web_routes::catalogue_entry(req),
        ores::http::routes::refdata::instrument_code_web_routes::catalogue_entry(req),
        ores::http::routes::refdata::payment_frequency_web_routes::catalogue_entry(req),
    };
    std::string out = "{\"resources\":[";
    for (std::size_t i = 0; i < std::size(entries); ++i) {
        if (i != 0)
            out += ",";
        out += entries[i];
    }
    return out + "]}";
}

}

void synthetic_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    ores::nats::service::nats_client& session) {

    yield_curve_process_type_routes::register_routes(router, registry, session);

    // The record routes the screens name each resource by, and the two feeds'
    // key reads, which are composed views and so are written by hand. The three
    // reference data lookups the screens read are registered by the refdata part,
    // which owns them; this catalogue only lists them.
    folder_web_routes::register_routes(router, registry, session);
    market_data_generation_config_web_routes::register_routes(router, registry, session);
    market_data_provider_web_routes::register_routes(router, registry, session);
    fx_spot_generation_config_web_routes::register_routes(router, registry, session);
    gmm_component_web_routes::register_routes(router, registry, session);
    ir_curve_generation_config_web_routes::register_routes(router, registry, session);
    ir_curve_generation_config_process_parameter_value_web_routes::register_routes(
        router, registry, session);
    ir_curve_template_entry_web_routes::register_routes(router, registry, session);
    variance_calibration_web_routes::register_routes(router, registry, session);
    variance_calibration_input_web_routes::register_routes(router, registry, session);
    variance_calibration_input_point_web_routes::register_routes(router, registry, session);
    fx_vol_contributor_web_routes::register_routes(router, registry, session);
    fx_vol_surface_feed_web_routes::register_routes(router, registry, session);
    fx_vol_surface_feed_expiry_web_routes::register_routes(router, registry, session);
    fx_vol_surface_feed_contributor_web_routes::register_routes(router, registry, session);
    yield_curve_process_type_web_routes::register_routes(router, registry, session);
    yield_curve_process_parameter_definition_web_routes::register_routes(router, registry, session);
    feed_key_routes::register_routes(router, registry, session);
    calibration_routes::register_routes(router, registry, session);
    feed_operation_routes::register_routes(router, registry, session);
    feed_tick_routes::register_routes(router, registry, session);
    process_preview_routes::register_routes(router, registry, session);
    market_regime_routes::register_routes(router, registry, session);

    // The screens read which resources they may name, and whether this session
    // may write each, from one catalogue.
    auto catalogue = router->get("/api/synthetic")
                         .summary("List the synthetic resources a screen may name")
                         .description("Each resource's key, key field, whether this session may "
                                      "write it and why not, search, sortable fields and "
                                      "permissions.")
                         .tags({"synthetic"})
                         .auth_required()
                         .handler([](const ores::http::domain::http_request& req)
                                      -> boost::asio::awaitable<ores::http::domain::http_response> {
                             co_return ores::http::domain::http_response::json(catalogue_body(req));
                         });
    const auto catalogue_built = catalogue.build();
    router->add_route(catalogue_built);
    registry->register_route(catalogue_built);
}

}
