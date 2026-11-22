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
#include "ores.orgmode.core/messaging/index_handler.hpp"
#include "ores.logging/logger.hpp"
#include "ores.orgmode.api/messaging/index_operations_protocol.hpp"
#include "ores.orgmode.core/service/knowledge_indexer.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <exception>
#include <utility>

namespace ores::orgmode::messaging {

using namespace ores::logging;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;

namespace {

inline std::string_view logger_name = "ores.orgmode.messaging.index_handler";
auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

}

index_handler::index_handler(ores::nats::service::client& nats,
                             const ores::database::context& ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier)
    : nats_(nats)
    , ctx_(ctx)
    , verifier_(std::move(verifier)) {}

void index_handler::reindex(ores::nats::message msg) {
    [[maybe_unused]] const auto correlation_id = log_handler_entry(lg(), msg);
    auto req_ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
    if (!req_ctx) {
        error_reply(nats_, msg, req_ctx.error());
        return;
    }
    if (!has_permission(*req_ctx, "installation::knowledge_index:rebuild")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }

    try {
        const auto r = service::knowledge_indexer(ctx_).reindex();
        reply(nats_,
              msg,
              reindex_knowledge_pages_response{
                  .pages = r.pages, .sections = r.sections, .links = r.links, .success = true});
    } catch (const std::exception& e) {
        ORES_LOG_SEV(lg(), error) << msg.subject << " failed: " << e.what();
        reply(nats_, msg, reindex_knowledge_pages_response{.success = false, .message = e.what()});
    }
}

}
