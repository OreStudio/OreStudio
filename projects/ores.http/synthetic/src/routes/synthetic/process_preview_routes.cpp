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
#include "ores.http/routes/synthetic/process_preview_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.synthetic.api/messaging/convert_fx_mixture_protocol.hpp"
#include "ores.synthetic.api/messaging/convert_fx_simple_protocol.hpp"
#include "ores.synthetic.api/messaging/convert_ir_parameters_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_operations_protocol.hpp"
#include "ores.synthetic.api/messaging/simulate_fx_spot_paths_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <type_traits>
#include <vector>

namespace ores::http::routes::synthetic {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;
namespace forwarding = ores::http::forwarding;
namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace synthetic_messaging = ores::synthetic::messaging;

namespace {

/// The most components a mixture, or parameters a process, may name.
constexpr std::size_t preview_most_members = 100;

/// The bodies the screen sends, beside the wire_fields each is read with.
namespace process_preview {

/// An FX mixture or its simple view, either side of the conversion: the side not given is empty.
struct fx_simple_body final {
    std::string process_type;
    bool to_simple = false;
    double initial_price = 0.0;
    int ticks_per_hour = 0;
    std::optional<std::vector<double>> gmm_means;
    std::optional<std::vector<double>> gmm_stdevs;
    std::optional<std::vector<double>> gmm_weights;
    std::optional<double> trend_pct_per_hour;
    std::optional<double> volatility_pct_per_hour;
    std::optional<double> jump_share_pct;
    std::optional<double> half_life_minutes;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_simple_body>) {
    using ores::utility::codec::field;
    using body = fx_simple_body;
    return ::ores::utility::codec::field_list{
        field{"process_type", &body::process_type},
        field{"to_simple", &body::to_simple},
        field{"initial_price", &body::initial_price},
        field{"ticks_per_hour", &body::ticks_per_hour},
        field{"gmm_means", &body::gmm_means},
        field{"gmm_stdevs", &body::gmm_stdevs},
        field{"gmm_weights", &body::gmm_weights},
        field{"trend_pct_per_hour", &body::trend_pct_per_hour},
        field{"volatility_pct_per_hour", &body::volatility_pct_per_hour},
        field{"jump_share_pct", &body::jump_share_pct},
        field{"half_life_minutes", &body::half_life_minutes}};
}

/// An IR process's parameters by name, and which way to convert them.
struct ir_convert_body final {
    std::string process_type;
    bool to_simple = false;
    std::map<std::string, double> values;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<ir_convert_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"process_type", &ir_convert_body::process_type},
                                              field{"to_simple", &ir_convert_body::to_simple},
                                              field{"values", &ir_convert_body::values}};
}

}

/// The body read as its own shape, or a refusal naming what it must state.
template <typename Body>
Body body_of(const http_request& req, const std::string& sentence) {
    auto body = rr::read_body<Body>(req.body);
    if (!body)
        rr::invalid(sentence);
    return std::move(*body);
}

/// A preview the service refused is a bad input.
void require_preview(bool success, const std::string& message) {
    if (!success)
        rr::invalid(message);
}

/**
 * The engines an FX feed runs, which a conversion reads between. The service
 * publishes no list of them, so they are stated here, as the BFF stated them;
 * an engine the service adds is refused here until it is named.
 */
bool is_fx_engine(const std::string& engine) {
    static const std::set<std::string, std::less<>> engines{
        "geometric", "arithmetic", "ornstein_uhlenbeck"};
    return engines.contains(engine);
}

/// Whether a mixture names one mean, one deviation and one weight per component, at most 100.
bool is_mixture(const std::vector<double>& means,
                const std::vector<double>& stdevs,
                const std::vector<double>& weights) {
    return means.size() == stdevs.size() && means.size() == weights.size() &&
           means.size() <= preview_most_members;
}

/// Whether a process's parameters are at most 100, each named by a code.
bool are_parameters(std::vector<synthetic_messaging::parameter_spec>& parameters) {
    if (parameters.size() > preview_most_members)
        return false;
    for (auto& parameter : parameters) {
        parameter.parameter_name = rr::trimmed(parameter.parameter_name);
        if (parameter.parameter_name.empty() || rr::characters_of(parameter.parameter_name) > 100)
            return false;
    }
    return true;
}

/// A code of 1 to 100 characters, trimmed, or nothing.
std::optional<std::string> code_of(const std::string& raw) {
    auto value = rr::trimmed(raw);
    if (value.empty() || rr::characters_of(value) > 100)
        return std::nullopt;
    return value;
}

std::string paths_answer(const std::vector<std::vector<double>>& paths) {
    return "{\"paths\":" + json::write(paths) + "}";
}

}

void process_preview_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"synthetic"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->post("/api/synthetic/simulate/fx")
            .summary("Sample FX spot paths from the values on the screen")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::simulate_fx_spot_paths_request>([&] {
                    const std::string sentence =
                        "A simulation names a mixture of 1 to 100 components with one mean, one "
                        "deviation and one weight each, a process, a start price, and positive "
                        "tick and path counts.";
                    auto msg =
                        body_of<synthetic_messaging::simulate_fx_spot_paths_request>(req, sentence);
                    const auto process = code_of(msg.process_type);
                    if (!process || msg.gmm_means.empty() ||
                        !is_mixture(msg.gmm_means, msg.gmm_stdevs, msg.gmm_weights) ||
                        msg.num_ticks <= 0 || msg.num_paths <= 0)
                        rr::invalid(sentence);
                    msg.process_type = *process;
                    const auto reply = forwarding::forward(req, session, msg);
                    require_preview(reply.success, reply.message);
                    return http_response::json(paths_answer(reply.paths));
                });
            }));

    add(router->post("/api/synthetic/convert/fx")
            .summary("Convert an FX mixture from one engine to another")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::convert_fx_mixture_request>([&] {
                    const std::string sentence =
                        "A conversion names a mixture with one mean, one deviation and one weight "
                        "per component, the two engines, a positive start price and a positive "
                        "tick rate.";
                    const auto msg =
                        body_of<synthetic_messaging::convert_fx_mixture_request>(req, sentence);
                    if (!is_mixture(msg.gmm_means, msg.gmm_stdevs, msg.gmm_weights) ||
                        !is_fx_engine(msg.from_process_type) ||
                        !is_fx_engine(msg.to_process_type) || !(msg.initial_price > 0.0) ||
                        msg.ticks_per_hour <= 0)
                        rr::invalid(sentence);
                    const auto reply = forwarding::forward(req, session, msg);
                    require_preview(reply.success, reply.message);
                    return http_response::json(
                        "{\"gmm_means\":" + json::write(reply.gmm_means) +
                        ",\"gmm_stdevs\":" + json::write(reply.gmm_stdevs) +
                        ",\"gmm_weights\":" + json::write(reply.gmm_weights) + "}");
                });
            }));

    add(router->post("/api/synthetic/convert/fx-simple")
            .summary("Read an FX mixture as the simple view, or write the simple view as one")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::convert_fx_simple_request>([&] {
                    const std::string sentence =
                        "A conversion names the engine, the way to convert, a positive start "
                        "price and a positive tick rate, with up to 100 mixture components.";
                    const auto body = body_of<process_preview::fx_simple_body>(req, sentence);
                    synthetic_messaging::convert_fx_simple_request msg;
                    msg.process_type = body.process_type;
                    msg.to_simple = body.to_simple;
                    msg.initial_price = body.initial_price;
                    msg.ticks_per_hour = body.ticks_per_hour;
                    msg.gmm_means = body.gmm_means.value_or(std::vector<double>{});
                    msg.gmm_stdevs = body.gmm_stdevs.value_or(std::vector<double>{});
                    msg.gmm_weights = body.gmm_weights.value_or(std::vector<double>{});
                    msg.trend_pct_per_hour = body.trend_pct_per_hour.value_or(0.0);
                    msg.volatility_pct_per_hour = body.volatility_pct_per_hour.value_or(0.0);
                    msg.jump_share_pct = body.jump_share_pct.value_or(0.0);
                    msg.half_life_minutes = body.half_life_minutes.value_or(0.0);
                    if (!is_fx_engine(msg.process_type) || !(msg.initial_price > 0.0) ||
                        msg.ticks_per_hour <= 0 || msg.gmm_means.size() > preview_most_members ||
                        msg.gmm_stdevs.size() > preview_most_members ||
                        msg.gmm_weights.size() > preview_most_members)
                        rr::invalid(sentence);
                    const auto reply = forwarding::forward(req, session, msg);
                    require_preview(reply.success, reply.message);
                    return http_response::json(
                        "{\"message\":" + json::write(reply.message) +
                        ",\"gmm_means\":" + json::write(reply.gmm_means) +
                        ",\"gmm_stdevs\":" + json::write(reply.gmm_stdevs) +
                        ",\"gmm_weights\":" + json::write(reply.gmm_weights) +
                        ",\"has_simple_form\":" + json::write(reply.has_simple_form) +
                        ",\"trend_pct_per_hour\":" + json::write(reply.trend_pct_per_hour) +
                        ",\"volatility_pct_per_hour\":" +
                        json::write(reply.volatility_pct_per_hour) +
                        ",\"jump_share_pct\":" + json::write(reply.jump_share_pct) +
                        ",\"half_life_minutes\":" + json::write(reply.half_life_minutes) + "}");
                });
            }));

    add(router->post("/api/synthetic/convert/ir")
            .summary("Convert an IR process's parameters to or from the simple view")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::convert_ir_parameters_request>([&] {
                    const std::string sentence =
                        "A conversion names the process by its code, the way to convert, and "
                        "up to 100 parameter values, each by a name of 1 to 100 characters.";
                    const auto body = body_of<process_preview::ir_convert_body>(req, sentence);
                    const auto process = code_of(body.process_type);
                    if (!process || body.values.size() > preview_most_members)
                        rr::invalid(sentence);
                    synthetic_messaging::convert_ir_parameters_request msg;
                    msg.process_type = *process;
                    msg.to_simple = body.to_simple;
                    for (const auto& [raw, value] : body.values) {
                        const auto name = code_of(raw);
                        if (!name)
                            rr::invalid(sentence);
                        msg.parameter_names.push_back(*name);
                        msg.parameter_values.push_back(value);
                    }
                    const auto reply = forwarding::forward(req, session, msg);
                    require_preview(reply.success, reply.message);
                    std::map<std::string, double> values;
                    for (std::size_t i = 0; i < reply.parameter_names.size(); ++i)
                        values[reply.parameter_names[i]] =
                            i < reply.parameter_values.size() ? reply.parameter_values[i] : 0.0;
                    return http_response::json(
                        "{\"has_simple_form\":" + json::write(reply.has_simple_form) +
                        ",\"message\":" + json::write(reply.message) + ",\"caution\":" +
                        json::write(reply.caution) + ",\"values\":" + json::write(values) + "}");
                });
            }));

    add(router->post("/api/synthetic/simulate/ir")
            .summary("Sample short-rate paths from the process and parameters on the screen")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::simulate_ir_curve_paths_request>([&] {
                    const std::string sentence =
                        "A simulation names the process by its code, up to 100 parameters each "
                        "named by a code, and positive tick and path counts.";
                    auto msg = body_of<synthetic_messaging::simulate_ir_curve_paths_request>(
                        req, sentence);
                    const auto process = code_of(msg.process_type);
                    if (!process || !are_parameters(msg.parameters) || msg.num_ticks <= 0 ||
                        msg.num_paths <= 0)
                        rr::invalid(sentence);
                    msg.process_type = *process;
                    const auto reply = forwarding::forward(req, session, msg);
                    require_preview(reply.success, reply.message);
                    return http_response::json(paths_answer(reply.paths));
                });
            }));

    add(router->post("/api/synthetic/preview/ir-shape")
            .summary("Price the rate at each ladder row from the process on the screen")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::preview_ir_curve_shape_request>([&] {
                    const std::string sentence =
                        "A curve preview names the process by its code, up to 100 parameters, a "
                        "payment frequency of up to 100 characters, and 1 to 200 ladder rows "
                        "each naming its tenors and instrument by code.";
                    auto msg =
                        body_of<synthetic_messaging::preview_ir_curve_shape_request>(req, sentence);
                    const auto process = code_of(msg.process_type);
                    if (!process || !are_parameters(msg.parameters) ||
                        rr::characters_of(msg.fixed_leg_payment_frequency_code) > 100 ||
                        msg.entries.empty() || msg.entries.size() > 200)
                        rr::invalid(sentence);
                    msg.process_type = *process;
                    for (auto& entry : msg.entries) {
                        const auto start = code_of(entry.start_tenor_code);
                        const auto end = code_of(entry.end_tenor_code);
                        const auto instrument = code_of(entry.instrument_code);
                        if (entry.sequence_index < 0 || !start || !end || !instrument)
                            rr::invalid(sentence);
                        entry.start_tenor_code = *start;
                        entry.end_tenor_code = *end;
                        entry.instrument_code = *instrument;
                    }
                    const auto reply = forwarding::forward(req, session, msg);
                    require_preview(reply.success, reply.message);
                    return http_response::json("{\"points\":" + json::write(reply.points) + "}");
                });
            }));
}

}
