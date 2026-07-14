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
#ifndef ORES_SYNTHETIC_SERVICE_SIMULATE_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_SIMULATE_HANDLER_HPP

#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/messaging/simulate_fx_spot_paths_protocol.hpp"
#include <algorithm>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

namespace {
inline auto& simulate_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.simulate_handler");
    return instance;
}

// Compact "[a, b, c]" rendering of a vector for diagnostic logging.
inline std::string join_doubles(const std::vector<double>& xs) {
    std::string out = "[";
    for (std::size_t i = 0; i < xs.size(); ++i) {
        if (i)
            out += ", ";
        out += std::to_string(xs[i]);
    }
    out += "]";
    return out;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::synthetic::messaging::simulate_fx_spot_paths_request;
using ores::synthetic::messaging::simulate_fx_spot_paths_response;
using namespace ores::logging;

/**
 * @brief Stateless batch simulation of FX spot sample paths.
 *
 * Builds the real GMM price process and steps it to generate the whole batch in
 * one response (no streaming, no persistence, no publishing) so the UI can
 * preview/contrast the configured behaviour.
 */
class simulate_handler {
public:
    simulate_handler(ores::nats::service::client& nats,
                     ores::database::context ctx,
                     std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void simulate(ores::nats::message msg) {
        BOOST_LOG_SEV(simulate_handler_lg(), debug)
            << "Received fx_spot.simulate request (payload " << msg.data.size() << " bytes).";

        // Standard service auth: require a valid JWT, then RBAC.
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            BOOST_LOG_SEV(simulate_handler_lg(), warn)
                << "Rejecting simulate request: auth failed: "
                << static_cast<int>(ctx_expected.error());
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "synthetic::fx_spot_generation_configs:read")) {
            BOOST_LOG_SEV(simulate_handler_lg(), warn)
                << "Rejecting simulate request: missing permission "
                   "synthetic::fx_spot_generation_configs:read.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<simulate_fx_spot_paths_request>(msg);
        simulate_fx_spot_paths_response resp;
        if (!req) {
            BOOST_LOG_SEV(simulate_handler_lg(), error) << "Failed to decode simulate request.";
            resp.message = "Failed to decode simulate request.";
            reply(nats_, msg, resp);
            return;
        }

        // Clamp to sane bounds so a UI bug can't ask for a runaway batch.
        const int num_ticks =
            std::clamp(req->num_ticks, 1, simulate_fx_spot_paths_request::max_num_ticks);
        const int num_paths =
            std::clamp(req->num_paths, 1, simulate_fx_spot_paths_request::max_num_paths);

        BOOST_LOG_SEV(simulate_handler_lg(), debug)
            << "Decoded simulate request: process_type='" << req->process_type
            << "' components=" << req->gmm_means.size() << " means=" << join_doubles(req->gmm_means)
            << " stdevs=" << join_doubles(req->gmm_stdevs)
            << " weights=" << join_doubles(req->gmm_weights)
            << " initial_price=" << req->initial_price << " seed=" << req->seed
            << " num_paths=" << num_paths << "(req " << req->num_paths << ")"
            << " num_ticks=" << num_ticks << "(req " << req->num_ticks << ")";

        try {
            if (req->gmm_means.empty())
                throw std::invalid_argument("at least one GMM component is required");

            for (int p = 0; p < num_paths; ++p) {
                auto process = ores::analytics::quant::service::process_factory::make_process(
                    req->process_type,
                    req->gmm_means,
                    req->gmm_stdevs,
                    req->gmm_weights,
                    req->initial_price,
                    req->seed + static_cast<std::uint32_t>(p));
                std::vector<double> path;
                path.reserve(static_cast<std::size_t>(num_ticks));
                for (int t = 0; t < num_ticks; ++t)
                    path.push_back(process->next());
                resp.paths.push_back(std::move(path));
            }
            resp.success = true;
            BOOST_LOG_SEV(simulate_handler_lg(), debug)
                << "Simulated " << num_paths << " paths x " << num_ticks << " ticks; replying.";
        } catch (const std::exception& e) {
            resp.success = false;
            resp.message = e.what();
            BOOST_LOG_SEV(simulate_handler_lg(), error) << "Simulation failed: " << e.what();
        }
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(simulate_handler_lg(), debug)
            << "Reply sent for fx_spot.simulate (success=" << resp.success << ").";
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
