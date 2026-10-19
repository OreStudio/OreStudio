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
#ifndef ORES_SYNTHETIC_SERVICE_RESTORE_FEEDS_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_RESTORE_FEEDS_HANDLER_HPP

#include "feed_kind_registry.hpp"
#include "feed_restore.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include <exception>
#include <memory>
#include <optional>
#include <string>

namespace ores::synthetic::service {

namespace {
inline auto& restore_feeds_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.restore_feeds_handler");
    return instance;
}
} // namespace

/**
 * @brief NATS handler that makes one restore pass for the caller's tenant.
 *
 * A provision calls it after it creates feeds that should run, so a new
 * installation runs them without a restart. The caller's token only
 * authorises the call. The pass runs in the caller's tenant and party, gives no
 * consent and creates no binding.
 *
 * @see The identity a feed runs as (doc/knowledge/architecture/feed_execution_identity.org)
 */
class restore_feeds_handler {
public:
    restore_feeds_handler(ores::nats::service::client& nats,
                          std::shared_ptr<feed_restorer> restorer,
                          ores::database::context ctx,
                          std::optional<ores::security::jwt::jwt_authenticator> verifier,
                          const feed_kind_registry& registry)
        : nats_(nats)
        , restorer_(std::move(restorer))
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , registry_(registry) {}

    void restore(ores::nats::message msg) {
        using ores::service::messaging::error_reply;
        using ores::service::messaging::log_handler_entry;
        using ores::service::messaging::reply;
        using namespace ores::logging;
        [[maybe_unused]] const auto cid = log_handler_entry(restore_feeds_handler_lg(), msg);

        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            BOOST_LOG_SEV(restore_feeds_handler_lg(), warn)
                << "Rejecting restore request: auth failed: " << static_cast<int>(ctx.error());
            error_reply(nats_, msg, ctx.error());
            return;
        }
        if (!registry_.permits_all_configs(*ctx) || !feed_kind_registry::permits_run(*ctx)) {
            BOOST_LOG_SEV(restore_feeds_handler_lg(), warn)
                << "Rejecting restore request: missing a feed config read permission or the run "
                   "permission.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        // The pass reads configs and builds feeds, so it can throw. An exception
        // that leaves this handler sends no reply, and the caller would wait out
        // its deadline for an answer this handler owed it.
        ores::synthetic::messaging::restore_feeds_response resp;
        try {
            const auto report = restorer_->pass(*ctx);
            resp.success = true;
            resp.started = report.started;
            for (const auto& f : report.failures)
                resp.failures.push_back(f.display_name + " — " + f.reason);
            resp.message = std::to_string(resp.started) + " started, " +
                           std::to_string(resp.failures.size()) + " failed";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(restore_feeds_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        BOOST_LOG_SEV(restore_feeds_handler_lg(), info) << msg.subject << " — " << resp.message;
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    std::shared_ptr<feed_restorer> restorer_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    const feed_kind_registry& registry_;
};

}

#endif
