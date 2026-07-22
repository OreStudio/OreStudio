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
#ifndef ORES_SYNTHETIC_SERVICE_IR_CURVE_PREVIEW_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_IR_CURVE_PREVIEW_HANDLER_HPP

#include "ir_curve_template_resolver.hpp"
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/messaging/preview_ir_curve_shape_protocol.hpp"
#include "ores.synthetic.api/messaging/simulate_ir_curve_paths_protocol.hpp"
#include <algorithm>
#include <cctype>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::synthetic::service {

namespace {
inline auto& ir_curve_preview_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.ir_curve_preview_handler");
    return instance;
}

// 1 tick == 1 calendar day (see ir_curve_template_resolver.hpp's own doc comment for this
// convention) -- dt is real elapsed years per tick, matching make_ir_curve_feed()'s own dt, so a
// preview simulates the same model a real feed would.
constexpr double preview_dt = 1.0 / 365.0;

// process_factory::make_yield_curve_process() dispatches on lowercase engine names ("vasicek",
// "cir", "hull_white"); ir_curve_generation_config.process_type and this protocol's wire value
// both carry the catalog's uppercase code (VASICEK/CIR/HULL_WHITE) -- same lowercasing
// ir_curve_feed's make_ir_curve_feed() does before calling the factory.
inline std::string preview_lowercase(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return s;
}
} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief Stateless preview requests for an IR curve editor: sample short-rate paths (the process
 * behaviour, mirroring simulate_handler's FX equivalent) and the curve shape a set of Curve
 * Template entries implies at the process's current state (no FX equivalent -- FX has one scalar
 * spot, not a tenor grid). Nothing is persisted or published by either.
 */
class ir_curve_preview_handler {
public:
    ir_curve_preview_handler(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void simulate_paths(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;

        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "synthetic::ir_curve_generation_configs:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<simulate_ir_curve_paths_request>(msg);
        simulate_ir_curve_paths_response resp;
        if (!req) {
            resp.message = "Failed to decode simulate request.";
            reply(nats_, msg, resp);
            return;
        }

        const int num_ticks =
            std::clamp(req->num_ticks, 1, simulate_ir_curve_paths_request::max_num_ticks);
        const int num_paths =
            std::clamp(req->num_paths, 1, simulate_ir_curve_paths_request::max_num_paths);

        try {
            for (int p = 0; p < num_paths; ++p) {
                auto process =
                    ores::analytics::quant::service::process_factory::make_yield_curve_process(
                        preview_lowercase(req->process_type),
                        req->kappa,
                        {req->theta},
                        req->sigma,
                        req->initial_rate,
                        req->seed + static_cast<std::uint32_t>(p),
                        preview_dt);
                std::vector<double> path;
                path.reserve(static_cast<std::size_t>(num_ticks));
                for (int t = 0; t < num_ticks; ++t)
                    path.push_back(process->next());
                resp.paths.push_back(std::move(path));
            }
            resp.success = true;
        } catch (const std::exception& e) {
            resp.success = false;
            resp.message = e.what();
            BOOST_LOG_SEV(ir_curve_preview_handler_lg(), warn)
                << "simulate_paths failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    void preview_shape(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;

        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "synthetic::ir_curve_generation_configs:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<preview_ir_curve_shape_request>(msg);
        preview_ir_curve_shape_response resp;
        if (!req) {
            resp.message = "Failed to decode preview request.";
            reply(nats_, msg, resp);
            return;
        }

        try {
            if (req->entries.empty())
                throw std::invalid_argument("at least one Curve Template entry is required");
            if (req->entries.size() >
                static_cast<std::size_t>(preview_ir_curve_shape_request::max_entries))
                throw std::invalid_argument("too many Curve Template entries");

            auto refctx = build_ir_curve_refdata_context(ctx);
            if (!refctx)
                throw std::runtime_error("RATES_SPOT_FORWARD tenor convention not found");

            std::vector<ores::synthetic::domain::ir_curve_template_entry> entries;
            entries.reserve(req->entries.size());
            for (const auto& row : req->entries) {
                ores::synthetic::domain::ir_curve_template_entry e;
                e.sequence_index = row.sequence_index;
                e.start_tenor_code = row.start_tenor_code;
                e.end_tenor_code = row.end_tenor_code;
                e.instrument_code = row.instrument_code;
                entries.push_back(std::move(e));
            }

            const auto resolved = resolve(entries, *refctx, req->fixed_leg_payment_frequency_code);

            std::map<int, std::string> start_tenor_by_sequence;
            for (const auto& row : req->entries)
                start_tenor_by_sequence.emplace(row.sequence_index, row.start_tenor_code);

            auto process =
                ores::analytics::quant::service::process_factory::make_yield_curve_process(
                    preview_lowercase(req->process_type),
                    req->kappa,
                    {req->theta},
                    req->sigma,
                    req->initial_rate,
                    42,
                    preview_dt);

            for (const auto& re : resolved) {
                preview_ir_curve_shape_point pt;
                pt.sequence_index = re.sequence_index;
                if (auto it = start_tenor_by_sequence.find(re.sequence_index);
                    it != start_tenor_by_sequence.end())
                    pt.start_tenor_code = it->second;
                pt.end_tenor_code = re.point_id;
                pt.rate = price_ir_curve_entry(*process, re);
                resp.points.push_back(std::move(pt));
            }
            std::sort(resp.points.begin(), resp.points.end(), [](const auto& a, const auto& b) {
                return a.sequence_index < b.sequence_index;
            });
            resp.success = true;
        } catch (const std::exception& e) {
            resp.success = false;
            resp.message = e.what();
            BOOST_LOG_SEV(ir_curve_preview_handler_lg(), warn)
                << "preview_shape failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
