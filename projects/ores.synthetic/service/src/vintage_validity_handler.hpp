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
#ifndef ORES_SYNTHETIC_SERVICE_VINTAGE_VALIDITY_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_VINTAGE_VALIDITY_HANDLER_HPP

#include "feed_controller.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/market_feed_config_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.core/repository/fx_spot_generation_config_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <memory>
#include <optional>

namespace ores::synthetic::service {

namespace {
inline auto& vintage_validity_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.vintage_validity_handler");
    return instance;
}
} // namespace

using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief NATS handler computing every feed's vintage-availability status
 * live, at read time -- not a persisted/cached column, so it is always
 * accurate, including immediately after a data import that added no new
 * feed rows, only new market_observation ones (see the design note on the
 * "Server-computed vintage-validity indicator" task for why a stored
 * column + triggers was rejected).
 */
class vintage_validity_handler {
public:
    vintage_validity_handler(ores::nats::service::client& nats,
                             std::shared_ptr<feed_controller> ctrl,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctrl_(std::move(ctrl))
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        using namespace ores::marketdata::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(vintage_validity_handler_lg(), msg);

        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            BOOST_LOG_SEV(vintage_validity_handler_lg(), warn)
                << "Rejecting vintage_validity request: auth failed: "
                << static_cast<int>(ctx_expected.error());
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "synthetic::fx_spot_generation_configs:read")) {
            BOOST_LOG_SEV(vintage_validity_handler_lg(), warn)
                << "Rejecting vintage_validity request: missing permission "
                   "synthetic::fx_spot_generation_configs:read.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        const auto bearer = ores::nats::service::extract_bearer(msg);

        namespace repo = ores::synthetic::repository;
        repo::fx_spot_generation_config_repository fx_repo;
        const auto fxs = fx_repo.read_latest(ctx);

        get_vintage_validity_response resp;
        resp.success = true;
        resp.entries.reserve(fxs.size());

        for (const auto& fx : fxs) {
            vintage_validity_entry entry;
            entry.fx_spot_generation_config_id = boost::uuids::to_string(fx.id);
            if (fx.price_source != "vintage") {
                entry.applicable = false;
                resp.entries.push_back(entry);
                continue;
            }
            entry.applicable = true;
            std::string error_detail;
            entry.valid =
                ctrl_->validate(fx.ore_key, fx.vintage_source, fx.vintage_date, error_detail, bearer);
            resp.entries.push_back(entry);
        }

        BOOST_LOG_SEV(vintage_validity_handler_lg(), info)
            << msg.subject << " — " << resp.entries.size() << " feed(s) checked.";
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    std::shared_ptr<feed_controller> ctrl_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
