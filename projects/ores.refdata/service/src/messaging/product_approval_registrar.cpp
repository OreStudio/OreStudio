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
#include "ores.refdata.service/messaging/product_approval_registrar.hpp"
#include "ores.refdata.api/messaging/product_approval_operations_protocol.hpp"
#include "ores.refdata.service/messaging/product_approval_handler.hpp"
#include <memory>
#include <string_view>
#include <utility>

namespace ores::refdata::messaging {

namespace {

constexpr std::string_view product_approval_queue_group = "ores.refdata.service";

}

std::vector<ores::nats::service::subscription> product_approval_registrar::register_handlers(
    ores::nats::service::client& nats,
    const ores::database::context& ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    auto handler = std::make_shared<product_approval_handler>(nats, ctx, std::move(verifier));
    std::vector<ores::nats::service::subscription> subs;
    subs.push_back(
        nats.queue_subscribe(get_book_product_approvals_request::nats_subject,
                             product_approval_queue_group,
                             [handler](ores::nats::message msg) { handler->get(std::move(msg)); }));
    return subs;
}

}
