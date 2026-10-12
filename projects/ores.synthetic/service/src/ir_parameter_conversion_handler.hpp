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
#ifndef ORES_SYNTHETIC_SERVICE_IR_PARAMETER_CONVERSION_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_IR_PARAMETER_CONVERSION_HANDLER_HPP

#include "feed_kind_registry.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "ores.synthetic.api/feeds/ir_parameter_conversion.hpp"
#include "ores.synthetic.api/messaging/convert_ir_parameters_protocol.hpp"
#include <exception>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>

namespace ores::synthetic::service {

namespace {
inline auto& ir_parameter_conversion_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.ir_parameter_conversion_handler");
    return instance;
}
}

/**
 * @brief Converts an IR feed's parameters between the simple and the advanced view.
 *
 * Stateless: nothing is read or stored. Gated by the IR kind's config
 * permission, as the IR previews are.
 */
class ir_parameter_conversion_handler {
public:
    ir_parameter_conversion_handler(ores::nats::service::client& nats,
                                  ores::database::context ctx,
                                  std::optional<ores::security::jwt::jwt_authenticator> verifier,
                                  const feed_kind_registry& registry)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , registry_(registry) {}

    void convert(ores::nats::message msg) {
        using namespace ores::logging;
        using namespace ores::synthetic::messaging;
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::has_permission;
        using ores::service::messaging::reply;

        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            error_reply(nats_, msg, ctx.error());
            return;
        }
        const auto* kind = registry_.find(std::string(ores::synthetic::feed::ir_curve_feed_kind));
        if (kind == nullptr || !has_permission(*ctx, kind->config_permission)) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        convert_ir_parameters_response resp;
        auto req = decode<convert_ir_parameters_request>(msg);
        if (!req) {
            resp.message = "Failed to decode convert request.";
            reply(nats_, msg, resp);
            return;
        }

        try {
            if (req->parameter_names.size() != req->parameter_values.size())
                throw std::invalid_argument("The names and values differ in size.");
            std::map<std::string, double> given;
            for (std::size_t i = 0; i < req->parameter_names.size(); ++i)
                given[req->parameter_names[i]] = req->parameter_values[i];

            std::map<std::string, double> out;
            if (req->to_simple) {
                const auto reading =
                    ores::synthetic::feed::ir_simple_from_parameters(req->process_type, given);
                resp.has_simple_form = reading.view.has_value();
                resp.message = reading.reason;
                resp.caution = reading.caution;
                if (reading.view) {
                    out = {{"start_rate", reading.view->start_rate},
                           {"long_run_rate", reading.view->long_run_rate},
                           {"half_life_years", reading.view->half_life_years},
                           {"yearly_move_bp", reading.view->yearly_move_bp}};
                }
            } else {
                const auto number = [&](const std::string& name) {
                    const auto found = given.find(name);
                    if (found == given.end())
                        throw std::invalid_argument("The value " + name + " is missing.");
                    return found->second;
                };
                out = ores::synthetic::feed::ir_parameters_from_simple(
                    req->process_type,
                    {.start_rate = number("start_rate"),
                     .long_run_rate = number("long_run_rate"),
                     .half_life_years = number("half_life_years"),
                     .yearly_move_bp = number("yearly_move_bp")});
                resp.has_simple_form = true;
            }
            for (const auto& [name, value] : out) {
                resp.parameter_names.push_back(name);
                resp.parameter_values.push_back(value);
            }
            resp.success = true;
        } catch (const std::exception& e) {
            resp.message = e.what();
            BOOST_LOG_SEV(ir_parameter_conversion_handler_lg(), warn)
                << "convert_ir_parameters refused: " << e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    const feed_kind_registry& registry_;
};

}

#endif
