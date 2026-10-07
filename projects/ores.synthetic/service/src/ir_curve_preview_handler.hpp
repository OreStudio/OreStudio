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

#include "feed_kind_registry.hpp"
#include "ir_curve_preview_process.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/feeds/ir_curve_template_resolver.hpp"
#include "ores.synthetic.api/messaging/preview_ir_curve_shape_protocol.hpp"
#include <algorithm>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::synthetic::service {

using ores::synthetic::feed::ir_curve_feed_dt;

namespace {
inline auto& ir_curve_preview_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.ir_curve_preview_handler");
    return instance;
}
} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;
using namespace ores::logging;
using ores::synthetic::feed::build_ir_curve_refdata_context;
using ores::synthetic::feed::price_ir_curve_entry;
using ores::synthetic::feed::resolve;

/**
 * @brief The curve-shape dry run: the curve a set of Curve Template entries
 * implies at one process state.
 *
 * No FX equivalent -- FX has one scalar spot, not a tenor grid. Stateless:
 * nothing is persisted or published.
 */
class ir_curve_preview_handler {
public:
    ir_curve_preview_handler(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier,
                             const feed_kind_registry& registry)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , registry_(registry) {}

    void preview_shape(ores::nats::message msg, const std::string& kind) {
        using namespace ores::synthetic::messaging;

        const auto* e = registry_.find(kind);
        if (!e) {
            // No registered kind, so no permission to derive the gate from:
            // the caller gets one response shape regardless of what it asked
            // for.
            BOOST_LOG_SEV(ir_curve_preview_handler_lg(), error) << "Unknown feed kind: " << kind;
            reply(nats_,
                  msg,
                  preview_ir_curve_shape_response{.success = false,
                                                  .message = "Unknown feed kind: " + kind});
            return;
        }

        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, e->config_permission)) {
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

            auto refctx = build_ir_curve_refdata_context(ctx, "RATES_SPOT_FORWARD");
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

            auto process = make_preview_process(req->process_type, req->parameters, 42);

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
    const feed_kind_registry& registry_;
};

}

#endif
